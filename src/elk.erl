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

render_tag(Value, {Standalone, Prefix, Postfix}, Escaped) ->
	case {?is_falsy(Value), Standalone} of
		{false, _} ->
			[Postfix, stringify(Value, Escaped), Prefix];
		{true, false} ->
			[Postfix, Prefix];
		{true, true} ->
			[]
	end.

render_iolist([{text, Text} | Tree], State, Acc) ->
	render_iolist(Tree, State, [Text | Acc]);
render_iolist([{raw_var, Key, {Standalone, Prefix, Postfix}} | Tree], State, Acc) ->
	Value = get(Key, State),
	Income = render_tag(Value, {Standalone, Prefix, Postfix}, false),
	render_iolist(Tree, State, Income ++ Acc);
render_iolist([{var, Key, {Standalone, Prefix, Postfix}} | Tree], State, Acc) ->
	Value = get(Key, State),
	Income = render_tag(Value, {Standalone, Prefix, Postfix}, true),
	render_iolist(Tree, State, Income ++ Acc);
render_iolist([{block, Key, WS, SubTree} | Tree], State, Acc) ->
	[{SStandalone, SPrefix, SPostfix}, {EStandalone, EPrefix, EPostfix}] = WS,
	Value = get(Key, State),
	case {?is_falsy(Value), Value} of
		%% TODO: check is Kind from contexts list.
		%% What if user pass tuple just to use it as boolean?
		{false, {Kind, Context}} ->
			NewContexts = [{Kind, Context} | State#state.contexts],
			SubText = render_iolist(SubTree, State#state{contexts=NewContexts}, []),
			NewAcc = [EPostfix, EPrefix, SubText, SPostfix, SPrefix | Acc],
			render_iolist(Tree, State, NewAcc);
		{false, Value} ->
			SubText = render_iolist(SubTree, State, []),
			NewAcc = [EPostfix, EPrefix, SubText, SPostfix, SPrefix | Acc],
			render_iolist(Tree, State, NewAcc);
		{true, _} ->
			case {SStandalone, EStandalone} of
				{true, true} -> render_iolist(Tree, State, Acc);
				{true, false} -> render_iolist(Tree, State, [EPostfix | Acc]);
				{false, true} -> render_iolist(Tree, State, [SPrefix | Acc]);
				{false, false} -> render_iolist(Tree, State, [EPostfix, SPrefix | Acc])
			end
	end;
render_iolist([{inverse, Key, WS, SubTree} | Tree], State, Acc) ->
	[{SStandalone, SPrefix, SPostfix}, {EStandalone, EPrefix, EPostfix}] = WS,
	Value = get(Key, State),
	case ?is_falsy(Value) of
		false ->
			case {SStandalone, EStandalone} of
				{true, true} -> render_iolist(Tree, State, Acc);
				{true, false} -> render_iolist(Tree, State, [EPostfix | Acc]);
				{false, true} -> render_iolist(Tree, State, [SPrefix | Acc]);
				{false, false} -> render_iolist(Tree, State, [SPrefix, EPostfix | Acc])
			end;
		true ->
			SubText = render_iolist(SubTree, State, []),
			NewAcc = [EPostfix, EPrefix, SubText, SPostfix, SPrefix | Acc],
			render_iolist(Tree, State, NewAcc)
	end;
render_iolist([{partial, Key, {Standalone, Prefix, Postfix}} | Tree], State, Acc) ->
	Template = get_value(Key, State#state.partials),
	case Template of
		{elk_template, SubTree} ->
			IOList = render_iolist(SubTree, State, []),
			render_iolist(Tree, State, [Postfix, lists:reverse(IOList), Prefix | Acc]);
		_ ->
			case Standalone of
				true -> render_iolist(Tree, State, Acc);
				false -> render_iolist(Tree, State, [Postfix, Prefix | Acc])
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
stringify(Value, _) when is_integer(Value) ->
	integer_to_list(Value);
stringify(Value, _) when is_float(Value) ->
	elk_mochinum:digits(Value);
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