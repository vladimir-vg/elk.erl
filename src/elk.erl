-module(elk).
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-export([compile/1, get_value/2, render/1, render/2, render/3]).

-define(is_falsy(V), ((V =:= undefined) or (V =:= false))).
-record(state, {top, contexts, partials}).

compile(Source) ->
	Tree = elk_parser:parse(Source),
	{elk_template, Tree}.

render({elk_template, Tree}) ->
	render({elk_template, Tree}, {proplist, []}).

render({elk_template, Tree}, Context) ->
	render({elk_template, Tree}, Context, {proplist, []}).

render({elk_template, Tree}, Context, Partials) ->
	State = #state{contexts=[Context], top=Context, partials=Partials},
	IOList = render_iolist(Tree, State, []),
	iolist_to_binary(IOList).

is_standalone(Prefix, Postfix) ->
	not ((Prefix =:= <<>>) or (Postfix =:= <<>>)).

%% this function just checks standalone-ness and inserts (or not)
%% postfix and prefix whitespace
render_tag(Value, Prefix, Postfix, Acc, Escaped) ->
	case {is_standalone(Prefix, Postfix), ?is_falsy(Value)} of
		{true,  true}  -> Acc;
		{false, true}  -> [Postfix, Prefix | Acc];
		{_,     false} -> [Postfix, stringify(Value, Escaped), Prefix | Acc]
	end.

render_iolist([{text, Text} | Tree], State, Acc) ->
	render_iolist(Tree, State, [Text | Acc]);
render_iolist([{raw_var, Key, Prefix, Postfix} | Tree], State, Acc) ->
	Value = get(Key, State),
	render_iolist(Tree, State, render_tag(Value, Prefix, Postfix, Acc, false));
render_iolist([{var, Key, Prefix, Postfix} | Tree], State, Acc) ->
	Value = get(Key, State),
	render_iolist(Tree, State, render_tag(Value, Prefix, Postfix, Acc, true));
render_iolist([{block, Key, WS, SubTree} | Tree], State, Acc) ->
	[{StartPrefix, StartPostfix}, {EndPrefix, EndPostfix}] = WS,
	Value = get(Key, State),
	case {Value, ?is_falsy(Value)} of
		%% TODO: check is Kind from contexts list.
		%% What if user pass tuple just to use it as boolean?
		{{Kind, Context}, false} ->
			Contexts = [ {Kind, Context} | State#state.contexts ],
			SubText = render_iolist(SubTree, State#state{contexts=Contexts}, []),
			NewAcc = [EndPostfix, EndPrefix, SubText, StartPostfix, StartPrefix | Acc],
			render_iolist(Tree, State, NewAcc);
		{_, true} ->
			case {is_standalone(StartPrefix, StartPostfix), is_standalone(EndPrefix, EndPostfix)} of
				{true, true} -> render_iolist(Tree, State, Acc);
				{true, false} -> render_iolist(Tree, State, [EndPostfix | Acc]);
				{false, true} -> render_iolist(Tree, State, [StartPrefix | Acc]);
				{false, false} -> render_iolist(Tree, State, [StartPrefix, EndPrefix | Acc])
			end;
		{Value, false} ->
			SubText = render_iolist(SubTree, State, []),
			NewAcc = [EndPostfix, EndPrefix, SubText, StartPostfix, StartPrefix | Acc],
			render_iolist(Tree, State, NewAcc)
	end;
render_iolist([{inverse, Key, WS, SubTree} | Tree], State, Acc) ->
	[{StartPrefix, StartPostfix}, {EndPrefix, EndPostfix}] = WS,
	Value = get(Key, State),
	case ?is_falsy(Value) of
		false ->
			case {is_standalone(StartPrefix, StartPostfix), is_standalone(EndPrefix, EndPostfix)} of
				{true, true} -> render_iolist(Tree, State, Acc);
				{true, false} -> render_iolist(Tree, State, [EndPostfix | Acc]);
				{false, true} -> render_iolist(Tree, State, [StartPrefix | Acc]);
				{false, false} -> render_iolist(Tree, State, [StartPrefix, EndPrefix | Acc])
			end;
		true ->
			SubText = render_iolist(SubTree, State, []),
			NewAcc = [EndPostfix, EndPrefix, SubText, StartPostfix, StartPrefix | Acc],
			render_iolist(Tree, State, NewAcc)
	end;
render_iolist([{partial, Key, Prefix, Postfix} | Tree], State, Acc) ->
	Template = get_value(Key, State#state.partials),
	case Template of
		{elk_template, SubTree} ->
			IOList = render_iolist(SubTree, State, []),
			render_iolist(Tree, State, [Prefix, lists:reverse(IOList), Postfix | Acc]);
		_ ->
			case is_standalone(Prefix, Postfix) of
				true -> render_iolist(Tree, State, Acc);
				false -> render_iolist(Tree, State, [Prefix, Postfix | Acc])
			end
	end;
render_iolist([], _State, Acc) ->
	lists:reverse(Acc).

get(Key, State) ->
	Contexts = State#state.contexts,
	Value = get_from_contexts(Key, Contexts),
	case Value of
		Fun when is_function(Fun, 1) ->
			Fun(State#state.top);
		Fun when is_function(Fun, 2) ->
			Fun(State#state.top, State#state.partials);
		Any -> Any
	end.

get_value(Key, {Kind, Context}) ->
	Contexts = case application:get_env(elk, contexts) of
		undefined -> [{proplist, elk_proplist_context}];
		Value -> Value
	end,
	Module = proplists:get_value(Kind, Contexts),
	Module:get(Key, Context).

get_from_contexts(Key, [Context | ContextsList]) ->
	case get_value(Key, Context) of
		undefined -> get_from_contexts(Key, ContextsList);
		Value     -> Value
	end;
get_from_contexts(_Key, []) ->
	undefined.

stringify(true, _) -> "true";
stringify(false, _) -> "";
stringify(undefined, _) -> "";
stringify(Value, Escaped) when is_binary(Value); is_list(Value) ->
	case Escaped of
		true -> escape(binary_to_list(iolist_to_binary(Value)));
		false -> Value
	end.

escape(Text)             -> escape(Text, []).
escape([$< | Text], Acc) -> escape(Text, [ <<"&lt;">> | Acc]);
escape([$> | Text], Acc) -> escape(Text, [ <<"&gt;">> | Acc]);
escape([$& | Text], Acc) -> escape(Text, [ <<"&amp;">> | Acc]);
escape([$" | Text], Acc) -> escape(Text, [ <<"&quot;">> | Acc]);
escape([C  | Text], Acc) -> escape(Text, [C | Acc]);
escape([], Acc)          -> lists:reverse(Acc).