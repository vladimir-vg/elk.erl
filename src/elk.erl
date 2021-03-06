-module(elk).
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-export([compile/1, get/3, render/1, render/2, render/3]).

-define(is_falsy(V), ((V =:= undefined) or (V =:= false) or (V =:= []))).
-record(state, {top, contexts, partials, indent}).

compile(Source) ->
    try elk_parser:parse(Source) of
        Tree -> {ok, {elk_template, Tree}}
    catch
        error:Reason -> {error, Reason}
    end.

render({elk_template, Tree}) ->
    render({elk_template, Tree}, {proplist, []}).

render({elk_template, Tree}, Context) ->
    render({elk_template, Tree}, Context, {proplist, []}).

render({elk_template, Tree}, Context, Partials) ->
    State = #state{contexts=[Context], top=Context, partials=Partials, indent=(<<>>)},
    IOList = render_iolist(Tree, State, []),
    iolist_to_binary(IOList).

%% standalone nodes
render_iolist([indent, [{ws, Prefix}, Node, {ws, Postfix}], {nl, Nl} | Tree], State, Acc) ->
    render_iolist(Tree, State, [render_standalone(Node, Prefix, Postfix, Nl, State) | Acc]);
render_iolist([indent, [{ws, Prefix}, Node], {nl, Nl} | Tree], State, Acc) ->
    render_iolist(Tree, State, [render_standalone(Node, Prefix, <<>>, Nl, State) | Acc]);
render_iolist([indent, [Node, {ws, Postfix}], {nl, Nl} | Tree], State, Acc) ->
    render_iolist(Tree, State, [render_standalone(Node, <<>>, Postfix, Nl, State) | Acc]);
render_iolist([indent, [Node], {nl, Nl} | Tree], State, Acc) ->
    render_iolist(Tree, State, [render_standalone(Node, <<>>, <<>>, Nl, State) | Acc]);

render_iolist([indent, [{ws, Prefix}, Node, {ws, Postfix}], eof | Tree], State, Acc) ->
    render_iolist(Tree, State, [render_standalone(Node, Prefix, Postfix, <<>>, State) | Acc]);
render_iolist([indent, [{ws, Prefix}, Node], eof | Tree], State, Acc) ->
    render_iolist(Tree, State, [render_standalone(Node, Prefix, <<>>, <<>>, State) | Acc]);
render_iolist([indent, [Node, {ws, Postfix}], eof | Tree], State, Acc) ->
    render_iolist(Tree, State, [render_standalone(Node, <<>>, Postfix, <<>>, State) | Acc]);
render_iolist([indent, [Node], eof | Tree], State, Acc) ->
    render_iolist(Tree, State, [render_standalone(Node, <<>>, <<>>, <<>>, State) | Acc]);

%% block tags
%% first is standalone, second is not
render_iolist([indent, [{ws, _}, {Kind, Key, [[{ws, _}], {nl, _} | SubTree], EPrefix} | Line] | Tree], State, Acc) ->
    Result = render_block_parts(Kind, Key, State, [SubTree, EPrefix]),
    render_iolist([Line | Tree], State, [Result | Acc]);
render_iolist([indent, [{Kind, Key, [[{ws, _}], {nl, _} | SubTree], EPrefix} | Line] | Tree], State, Acc) ->
    Result = render_block_parts(Kind, Key, State, [SubTree, EPrefix]),
    render_iolist([Line | Tree], State, [Result | Acc]);
render_iolist([indent, [{ws, _}, {Kind, Key, [[], {nl, _} | SubTree], EPrefix} | Line] | Tree], State, Acc) ->
    Result = render_block_parts(Kind, Key, State, [SubTree, EPrefix]),
    render_iolist([Line | Tree], State, [Result | Acc]);
render_iolist([indent, [{Kind, Key, [[], {nl, _} | SubTree], EPrefix} | Line] | Tree], State, Acc) ->
    Result = render_block_parts(Kind, Key, State, [SubTree, EPrefix]),
    render_iolist([Line | Tree], State, [Result | Acc]);

%% first is not, second is standalone
render_iolist([[{Kind, Key, SubTree, [indent, [{ws, _}]]}, {ws, _}], {nl, _} | Tree], State, Acc) ->
    Result = render_block_parts(Kind, Key, State, [SubTree]),
    render_iolist(Tree, State, [Result | Acc]);
render_iolist([[{Kind, Key, SubTree, [indent, []]}, {ws, _}], {nl, _} | Tree], State, Acc) ->
    Result = render_block_parts(Kind, Key, State, [SubTree]),
    render_iolist(Tree, State, [Result | Acc]);
render_iolist([[{Kind, Key, SubTree, [indent, [{ws, _}]]}], {nl, _} | Tree], State, Acc) ->
    Result = render_block_parts(Kind, Key, State, [SubTree]),
    render_iolist(Tree, State, [Result | Acc]);
render_iolist([[{Kind, Key, SubTree, [indent, []]}], {nl, _} | Tree], State, Acc) ->
    Result = render_block_parts(Kind, Key, State, [SubTree]),
    render_iolist(Tree, State, [Result | Acc]);
render_iolist([[{Kind, Key, SubTree, [indent, [{ws, _}]]}, {ws, _}], eof | Tree], State, Acc) ->
    Result = render_block_parts(Kind, Key, State, [SubTree]),
    render_iolist(Tree, State, [Result | Acc]);
render_iolist([[{Kind, Key, SubTree, [indent, []]}, {ws, _}], eof | Tree], State, Acc) ->
    Result = render_block_parts(Kind, Key, State, [SubTree]),
    render_iolist(Tree, State, [Result | Acc]);
render_iolist([[{Kind, Key, SubTree, [indent, [{ws, _}]]}], eof | Tree], State, Acc) ->
    Result = render_block_parts(Kind, Key, State, [SubTree]),
    render_iolist(Tree, State, [Result | Acc]);
render_iolist([[{Kind, Key, SubTree, [indent, []]}], eof | Tree], State, Acc) ->
    Result = render_block_parts(Kind, Key, State, [SubTree]),
    render_iolist(Tree, State, [Result | Acc]);

%% both aren't standalone
render_iolist([[{Kind, Key, SubTree, EPrefix} | Line] | Tree], State, Acc) ->
    Result = render_block_parts(Kind, Key, State, [SubTree, EPrefix]),
    render_iolist([Line | Tree], State, [Result | Acc]);

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

render_iolist([[self | Line] | Tree], State, Acc) ->
    Text = stringify(hd(State#state.contexts), true),
    render_iolist([Line | Tree], State, [Text | Acc]);

render_iolist([[{partial, Key} | Line] | Tree], State, Acc) ->
    Text = render_partial(Key, State),
    render_iolist([Line | Tree], State, [Text | Acc]);

render_iolist([eof | Tree], State, Acc) ->
    render_iolist(Tree, State, Acc);
render_iolist([indent | Tree], State, Acc) ->
    render_iolist(Tree, State, [State#state.indent | Acc]);
render_iolist([[] | Tree], State, Acc) ->
    render_iolist(Tree, State, Acc);
render_iolist([], _State, Acc) ->
    lists:reverse(Acc).

render_standalone(comment, _Prefix, _Postfix, _Nl, _State) ->
    <<>>;
render_standalone({text, Text}, Prefix, Postfix, Nl, State) ->
    [State#state.indent, Prefix, Text, Postfix, Nl];
render_standalone({var, Key}, Prefix, Postfix, Nl, State) ->
    case render_var(Key, State) of
        <<>> -> <<>>;
        Text -> [State#state.indent, Prefix, Text, Postfix, Nl]
    end;
render_standalone({raw_var, Key}, Prefix, Postfix, Nl, State) ->
    case render_raw_var(Key, State) of
        <<>> -> <<>>;
        Text -> [State#state.indent, Prefix, Text, Postfix, Nl]
    end;
render_standalone({partial, Key}, Prefix, Postfix, _Nl, State) ->
    Indent = State#state.indent,
    case render_partial(Key, State#state{indent=[Indent, Prefix]}) of
        <<>> -> <<>>;
        Text -> [Text, Postfix]
    end;
render_standalone({Kind, Key, SubTree, EPrefix}, SPrefix, EPostfix, ENl, State) ->
    case {SubTree, EPrefix} of
        %% first and second tags are standalone. Just ignore them
        {[[{ws, _}], {nl, _} | Tree], [indent, [{ws, _}]]} ->
            render_block_parts(Kind, Key, State, [Tree]);
        {[[], {nl, _} | Tree], [indent, [{ws, _}]]} ->
            render_block_parts(Kind, Key, State, [Tree]);
        {[[{ws, _}], {nl, _} | Tree], [indent, []]} ->
            render_block_parts(Kind, Key, State, [Tree]);
        {[[], {nl, _} | Tree], [indent, []]} ->
            render_block_parts(Kind, Key, State, [Tree]);
        
        %% first is standalone, second is not
        {[[{ws, _}], {nl, _} | Tree], EPrefix} ->
            [render_block_parts(Kind, Key, State, [Tree, EPrefix]), EPostfix, ENl];
        {[[], {nl, _} | Tree], EPrefix} ->
            [render_block_parts(Kind, Key, State, [Tree, EPrefix]), EPostfix, ENl];
        
        %% first is not, second is standalone
        {[SPostfix | Tree], [indent, [{ws, _}]]} ->
            [SPrefix, render_block_parts(Kind, Key, State, [[SPostfix], Tree])];
        {[SPostfix | Tree], [indent, []]} ->
            [SPrefix, render_block_parts(Kind, Key, State, [[SPostfix], Tree])];
        
        %% both aren't standalone
        {[SPostfix | Tree], EPrefix} ->
            [SPrefix, render_block_parts(Kind, Key, State, [[SPostfix], Tree, EPrefix]), EPostfix, ENl];
        {[], EPrefix} ->
            [SPrefix, render_block_parts(Kind, Key, State, [EPrefix]), EPostfix, ENl]
    end.

render_var(Key, State) ->
    Value = get(value, Key, State),
    stringify(Value, true).

render_raw_var(Key, State) ->
    Value = get(value, Key, State),
    stringify(Value, false).

render_partial(Key, State) ->
    Value = get(partial, Key, State),
    case Value of
        {elk_template, SubTree} ->
            render_iolist(SubTree, State, []);
        _ -> <<>>
    end.

render_block_parts(inverse, Key, State, Trees) ->
    Value = get(value, Key, State),
    case ?is_falsy(Value) of
        false -> <<>>;
        true ->
            lists:map(fun (Tree) ->
                render_iolist(Tree, State, [])
            end, Trees)
    end;
render_block_parts(block, Key, State, Trees) ->
    Value = get(value, Key, State),
    case ?is_falsy(Value) of
        true -> <<>>;
        false when is_list(Value) ->
            lists:map(fun (V) ->
                NewContexts = [V | State#state.contexts],
                lists:map(fun (Tree) ->
                    render_iolist(Tree, State#state{contexts=NewContexts}, [])
                end, Trees)
            end, Value);
        false ->
            NewContexts = [Value | State#state.contexts],
            lists:map(fun (Tree) ->
                render_iolist(Tree, State#state{contexts=NewContexts}, [])
            end, Trees)
    end.

get(value, Key, State) ->
    Contexts = State#state.contexts,
    Value = get_from_contexts(Key, Contexts),
    case Value of
        Fun when is_function(Fun, 1) ->
            Fun(State);
        Any -> Any
    end;
get(toplevel_value, Key, State) ->
    Value = get_value(Key, State#state.top),
    case Value of
        Fun when is_function(Fun, 1) ->
            Fun(State);
        Any -> Any
    end;
get(partial, Key, State) ->
    get_value(Key, State#state.partials).

get_value([], _Context) ->
    undefined;
get_value([Key | KeyChain], {Kind, Context}) ->
    case {get_value(Key, {Kind, Context}), KeyChain} of
        {undefined, _} -> undefined;
        {Value, []}    -> Value;
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
stringify([], _) -> <<>>    ;
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