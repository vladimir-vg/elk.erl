-module(elk_transform).
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-export([transform/3]).

-define(key(Node), proplists:get_value(key, Node)).

transform(self, _Node, _Index) ->
    self;
transform(block_start, Node, Index) ->
    {block_start, ?key(Node), Index};
transform(inverse_start, Node, Index) ->
    {inverse_start, ?key(Node), Index};
transform(block_end, Node, Index) ->
    {block_end, ?key(Node), Index};
transform(raw_var1, Node, _Index) ->
    {raw_var, ?key(Node)};
transform(raw_var2, Node, _Index) ->
    {raw_var, ?key(Node)};
transform(partial, Node, _Index) ->
    {partial, ?key(Node)};
transform(comment, _Node, _Index) ->
    comment;
transform(var, Node, _Index) ->
    {var, ?key(Node)};
transform(text, Node, _Index) ->
    Binary = iolist_to_binary(Node),
    String = binary_to_list(Binary),
    case string:strip(String) of
        %% check is it whitespace
        [] -> {ws, Binary};
        _  -> {text, Binary}
    end;
transform(nl, Node, _Index) ->
    {nl, iolist_to_binary(Node)};
transform(dotted_id, Node, _Index) ->
    First = iolist_to_binary(proplists:get_value(first_id, Node)),
    Rest = lists:map(fun ([<<".">>, {id, Key}]) ->
        iolist_to_binary(Key)
    end, hd(tl(Node))),
    [First | Rest];
transform(id, Node, _Index) ->
    [iolist_to_binary(Node)];
transform('template', Nodes, _Index) ->
    FirstLine = hd(Nodes),
    NewNodes = case tl(Nodes) of
        [] -> FirstLine;
        [Lines] -> FirstLine ++ lists:flatten(Lines)
    end,
    Tree1 = second_transform(NewNodes, [], []),
    Tree2 = third_transformation(Tree1, [], [], []),
    case lists:reverse(Tree2) of
        [indent | Rev] -> [indent | lists:reverse(Rev)] ++ [eof];
        _ -> [indent | Tree2] ++ [eof]
    end;
transform(_Kind, Node, _Index) ->
    Node.



%% this transformation add indent tokens and group nodes by lines
second_transform([{nl, Text} | Nodes], [], Acc) ->
    second_transform(Nodes, [], [indent, {nl, Text} | Acc]);
second_transform([{nl, Text} | Nodes], LineAcc, Acc) ->
    second_transform(Nodes, [], [indent, {nl, Text}, lists:reverse(LineAcc) | Acc]);
second_transform([Node | Nodes], LineAcc, Acc) ->
    second_transform(Nodes, [Node | LineAcc], Acc);
second_transform([], [], Acc) ->
    lists:reverse(Acc);
second_transform([], LineAcc, Acc) ->
    lists:reverse([lists:reverse(LineAcc) | Acc]).



third_transformation([indent | Nodes], LineAcc, Expected, Acc) ->
    third_transformation(Nodes, LineAcc, Expected, [indent | Acc]);
third_transformation([{nl, Text} | Nodes], LineAcc, Expected, Acc) ->
    third_transformation(Nodes, LineAcc, Expected, [{nl, Text} | Acc]);

%% no block tag found
third_transformation([[] | Nodes], LineAcc, Expected, Acc) ->
    third_transformation(Nodes, [], Expected, [lists:reverse(LineAcc) | Acc]);

%% found block tag,
%% take first tokens from line,
%% push tag to expected-list
%% recursively transform the rest until found enclosing tag
third_transformation([[{block_start, Key, Index} | LineNodes] | Nodes], LineAcc, Expected, Acc) ->
    {NewLineAcc, NewLineNodes, NewNodes, NewExpected} =
        third_transformation([LineNodes | Nodes], [], [{block_start, Key, LineAcc, Index} | Expected], []),
    third_transformation([NewLineNodes | NewNodes], NewLineAcc, NewExpected, Acc);

third_transformation([[{inverse_start, Key, Index} | LineNodes] | Nodes], LineAcc, Expected, Acc) ->
    {NewLineAcc, NewLineNodes, NewNodes, NewExpected} =
        third_transformation([LineNodes | Nodes], [], [{inverse_start, Key, LineAcc, Index} | Expected], []),
    third_transformation([NewLineNodes | NewNodes], NewLineAcc, NewExpected, Acc);

%% found matched block tag,
%% put tree acc into block tag,
%% remove tag from expected-list
third_transformation(
    [[{block_end, Key, _Index1} | LineNodes] | Nodes],
    LineAcc,
    [{block_start, Key, Prefix, _Index2} | Expected],
    Acc
) ->
    Node = case Acc of
        [indent | Rest] ->
            {block, Key, lists:reverse(Rest), [indent, lists:reverse(LineAcc)]};
        Rest ->
            {block, Key, lists:reverse(Rest), [lists:reverse(LineAcc)]}
    end,
    NewLineAcc = [Node | Prefix],
    {NewLineAcc, LineNodes, Nodes, Expected};

third_transformation(
    [[{block_end, Key, _Index1} | LineNodes] | Nodes],
    LineAcc,
    [{inverse_start, Key, Prefix, _Index2} | Expected],
    Acc
) ->
    Node = case Acc of
        [indent | Rest] ->
            {inverse, Key, lists:reverse(Rest), [indent, lists:reverse(LineAcc)]};
        Rest ->
            {inverse, Key, lists:reverse(Rest), [lists:reverse(LineAcc)]}
    end,
    NewLineAcc = [Node | Prefix],
    {NewLineAcc, LineNodes, Nodes, Expected};

%% keys doesn't match, tags unbalanced
third_transformation(
    [[{block_end, Key1, {{line, ELine}, {column, EColumn}}} | _LineNodes] | _Nodes],
    _LineAcc,
    [{inverse_start, Key2, _Prefix, {{line, SLine}, {column, SColumn}}} | _Expected],
    _Acc
) ->
    Message = lists:concat([
        "Expected end for '^", Key2, "' (", SLine, " line ", SColumn, " column)",
        " but found '/", Key1,
        "' (", ELine, " line ", EColumn, " column)"
    ]),
    erlang:error(iolist_to_binary(Message));
third_transformation(
    [[{block_end, Key1, {{line, ELine}, {column, EColumn}}} | _LineNodes] | _Nodes],
    _LineAcc,
    [{block_start, Key2, _Prefix, {{line, SLine}, {column, SColumn}}} | _Expected],
    _Acc
) ->
    Message = lists:concat([
        "Expected end for '#", Key2, "' (", SLine, " line ", SColumn, " column)",
        " but found '/", Key1,
        "' (", ELine, " line ", EColumn, " column)"
    ]),
    erlang:error(iolist_to_binary(Message));

%% end of block expected, but not found
third_transformation(
    [],
    _LineAcc,
    [{block_start, Key, _Prefix, {{line, SLine}, {column, SColumn}}} | _Expected],
    _Acc
) ->
    Message = lists:concat([
        "Expected end for '#", Key, "' (", SLine, " line ", SColumn, " column)",
        " but couldn't find '/", Key, "'"
    ]),
    erlang:error(iolist_to_binary(Message));
third_transformation(
    [],
    _LineAcc,
    [{inverse_start, Key, _Prefix, {{line, SLine}, {column, SColumn}}} | _Expected],
    _Acc
) ->
    Message = lists:concat([
        "Expected end for '^", Key, "' (", SLine, " line ", SColumn, " column)",
        " but couldn't find '/", Key, "'"
    ]),
    erlang:error(iolist_to_binary(Message));

%% found end of block, but no end expected
third_transformation(
    [[{block_end, Key, {{line, ELine}, {column, EColumn}}} | _LineNodes] | _Nodes],
    _LineAcc,
    [],
    _Acc
) ->
    Message = lists:concat([
        "Found end '/", Key, "' (", ELine, " line ", EColumn, " column)",
        " but not expected"
    ]),
    erlang:error(iolist_to_binary(Message));

%% another usual node, just skip it.
third_transformation([[LNode | LNodes] | Nodes], LineAcc, Expected, Acc) ->
    third_transformation([LNodes | Nodes], [LNode | LineAcc], Expected, Acc);

third_transformation([], [], [], Acc) ->
    lists:reverse(Acc).