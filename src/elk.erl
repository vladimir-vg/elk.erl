-module(elk).
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-export([compile/1, get_value/2, render/1, render/2, render/3]).

-define(is_falsy(V), ((V =:= undefined) or (V =:= false) or (V =:= []))).
-record(state, {top, contexts, partials, indent}).

compile(Source) ->
	Tree = elk_parser:parse(Source),
	{elk_template, Tree}.

render({elk_template, Tree}) ->
	render({elk_template, Tree}, {proplist, []}).

render({elk_template, Tree}, Context) ->
	render({elk_template, Tree}, Context, {proplist, []}).

render({elk_template, Tree}, Context, Partials) ->
	State = #state{contexts=[Context], top=Context, partials=Partials, indent=(<<>>)},
	IOList = render_iolist(Tree, State, []),
	iolist_to_binary(IOList).

%% standalone nodes
render_iolist([{nl, Nl}, indent, [{ws, Prefix}, Node, {ws, Postfix}] | Tree], State, Acc) ->
	render_iolist(Tree, State, [render_standalone(Node, Nl, Prefix, Postfix, State) | Acc]);
render_iolist([indent, [{ws, Prefix}, Node, {ws, Postfix}] | Tree], State, Acc) ->
	render_iolist(Tree, State, [render_standalone(Node, <<>>, Prefix, Postfix, State) | Acc]);
render_iolist([{nl, Nl}, indent, [Node, {ws, Postfix}] | Tree], State, Acc) ->
	render_iolist(Tree, State, [render_standalone(Node, Nl, <<>>, Postfix, State) | Acc]);
render_iolist([indent, [Node, {ws, Postfix}] | Tree], State, Acc) ->
	render_iolist(Tree, State, [render_standalone(Node, <<>>, <<>>, Postfix, State) | Acc]);
render_iolist([{nl, Nl}, indent, [{ws, Prefix}, Node] | Tree], State, Acc) ->
	render_iolist(Tree, State, [render_standalone(Node, Nl, Prefix, <<>>, State) | Acc]);
render_iolist([indent, [{ws, Prefix}, Node] | Tree], State, Acc) ->
	render_iolist(Tree, State, [render_standalone(Node, <<>>, Prefix, <<>>, State) | Acc]);
render_iolist([{nl, Nl}, indent, [Node] | Tree], State, Acc) ->
	render_iolist(Tree, State, [render_standalone(Node, Nl, <<>>, <<>>, State) | Acc]);
render_iolist([indent, [Node] | Tree], State, Acc) ->
	render_iolist(Tree, State, [render_standalone(Node, <<>>, <<>>, <<>>, State) | Acc]);

render_iolist([{nl, Nl} | Tree], State, Acc) ->
	render_iolist(Tree, State, [Nl | Acc]);
render_iolist([[comment | Line] | Tree], State, Acc) ->
	render_iolist([Line | Tree], State, Acc);
render_iolist([[{text, Text} | Line] | Tree], State, Acc) ->
	render_iolist([Line | Tree], State, [Text | Acc]);
render_iolist([[{ws, Text} | Line] | Tree], State, Acc) ->
	render_iolist([Line | Tree], State, [Text | Acc]);
	
render_iolist([[{var, Key} | Line] | Tree], State, Acc) ->
	render_iolist([Line | Tree], State, [render_var(Key, State) | Acc]);

render_iolist([[{raw_var, Key} | Line] | Tree], State, Acc) ->
	render_iolist([Line | Tree], State, [render_raw_var(Key, State) | Acc]);

render_iolist([[{inverse, Key, SubTree, EPrefix} | Line] | Tree], State, Acc) ->
	Pref = render_iolist([EPrefix], State, []),
	render_iolist([Line | Tree], State, [Pref, render_inverse(Key, State, SubTree) | Acc]);

render_iolist([[{block, Key, SubTree, EPrefix} | Line] | Tree], State, Acc) ->
	Result = render_block_parts(Key, State, [[Line], SubTree, [EPrefix]]),
	render_iolist([Line | Tree], State, [Result | Acc]);

render_iolist([indent | Tree], State, Acc) ->
	render_iolist(Tree, State, [State#state.indent | Acc]);
render_iolist([[] | Tree], State, Acc) ->
	render_iolist(Tree, State, Acc);
render_iolist([], _State, Acc) ->
	lists:reverse(Acc).

render_standalone(comment, _Nl, _Prefix, _Postfix, _State) ->
	<<>>;
render_standalone({text, Text}, Nl, Prefix, Postfix, _State) ->
	[Nl, Prefix, Text, Postfix];
render_standalone({var, Key}, Nl, Prefix, Postfix, State) ->
	case render_var(Key, State) of
		<<>> -> <<>>;
		Text -> [Nl, Prefix, Text, Postfix]
	end;
render_standalone({raw_var, Key}, Nl, Prefix, Postfix, State) ->
	case render_raw_var(Key, State) of
		<<>> -> <<>>;
		Text -> [Nl, Prefix, Text, Postfix]
	end;
render_standalone({inverse, Key, SubTree, EPrefix}, SNl, SPrefix, EPostfix, State) ->
	case {SubTree, EPrefix} of
		%% first and second tags are standalone. Just ignore them
		{[[{ws, _}] | Tree], [{ws, _}]} ->
			render_inverse(Key, State, Tree);
		
		{[[] | Tree], [{ws, _}]} ->
			render_inverse(Key, State, Tree);
		
		{[[{ws, _}] | Tree], []} ->
			render_inverse(Key, State, Tree);
		
		{[[] | Tree], []} ->
			render_inverse(Key, State, Tree);
		
		%% both aren't standalone
		{[SPostfix | Tree], EPrefix} ->
			Pref = render_iolist([EPrefix], State, []),
			Postf = render_iolist([SPostfix], State, []),
			[SNl, SPrefix, Postf, render_inverse(Key, State, Tree), Pref, EPostfix]
	end.

render_var(Key, State) ->
	Value = get(Key, State),
	stringify(Value, true).

render_raw_var(Key, State) ->
	Value = get(Key, State),
	stringify(Value, false).

render_inverse(Key, State, Tree) ->
	Value = get(Key, State),
	case ?is_falsy(Value) of
		true -> render_iolist(Tree, State, []);
		false -> <<>>
	end.

%% this differs from render_inverse, it takes list trees.
%% this was made to prevent several lookups for spostfix, subtree, and eprefix parts
render_block_parts(Key, State, Trees) ->
	Value = get(Key, State),
	case ?is_falsy(Value) of
		true -> <<>>;
		false ->
			NewContexts = [Value | State#state.contexts],
			lists:map(fun (Tree) ->
				render_iolist(Tree, State#state{contexts=NewContexts}, [])
			end, Trees)
	end.

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

get_value([], _Context) ->
	undefined;
get_value([Key | KeyChain], {Kind, Context}) ->
	case {get_value(Key, {Kind, Context}), KeyChain} of
		{undefined, _} -> undefined;
		{Value, []} -> Value;
		{Value, Chain} -> get_value(Chain, Value)
	end;
get_value(Key, {Kind, Context}) ->
	Contexts = case application:get_env(elk, contexts) of
		undefined -> [{proplist, elk_proplist_context}];
		Value -> Value
	end,
	Module = proplists:get_value(Kind, Contexts),
	Module:get(Key, Context);
get_value(_Key, _Any) ->
	undefined.

get_from_contexts(Key, [Context | ContextsList]) ->
	case get_value(Key, Context) of
		undefined -> get_from_contexts(Key, ContextsList);
		Value     -> Value
	end;
get_from_contexts(_Key, []) ->
	undefined.

stringify(true, _) -> "true";
stringify(false, _) -> <<>>;
stringify(undefined, _) -> <<>>;
stringify([], _) -> <<>>	;
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