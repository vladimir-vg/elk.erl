-module(elk_transform).
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-export([transform/3]).

-define(iol2b(X), iolist_to_binary(X)).
-define(get(K, O), proplists:get_value(K, O)).

prefix_key_postfix(Kind, Node) ->
	Key = ?iol2b(?get(key, Node)),
	Prefix = ?iol2b(?get(prefix, Node)),
	Postfix = ?iol2b(?get(postfix, Node)),
	{Kind, Key, Prefix, Postfix}.

block_transform([{block_start, Key, Prefix, Postfix} | Nodes], Expected, Acc) ->
	{Node, NewNodes, NewExpected} =
		block_transform(Nodes, [{block_start, Key, Prefix, Postfix} | Expected], []),
	block_transform(NewNodes, NewExpected, [Node | Acc]);
block_transform([{inverse_start, Key, Prefix, Postfix} | Nodes], Expected, Acc) ->
	{Node, NewNodes, NewExpected} =
		block_transform(Nodes, [{inverse_start, Key, Prefix, Postfix} | Expected], []),
	block_transform(NewNodes, NewExpected, [Node | Acc]);

block_transform(
	[{block_end, Key, EndPrefix, EndPostfix} | Nodes],
	[{block_start, Key, StartPrefix, StartPostfix} | Expected],
	Acc
) ->
	SubTemplate = block_transform(lists:reverse(Acc), [], []),
	Node = {block, Key, [{StartPrefix, StartPostfix}, {EndPrefix, EndPostfix}], SubTemplate},
	{Node, Nodes, Expected};

block_transform(
	[{block_end, Key, EndPrefix, EndPostfix} | Nodes],
	[{inverse_start, Key, StartPrefix, StartPostfix} | Expected],
	Acc
) ->
	SubTemplate = block_transform(lists:reverse(Acc), [], []),
	Node = {inverse, Key, [{StartPrefix, StartPostfix}, {EndPrefix, EndPostfix}], SubTemplate},
	{Node, Nodes, Expected};

block_transform([Node | Nodes], [], []) ->
	[Node | block_transform(Nodes, [], [])];
block_transform([Node | Nodes], Expected, Acc) ->
	block_transform(Nodes, Expected, [Node | Acc]);
block_transform([], [], Acc) ->
	lists:reverse(Acc).

transform(text, Node, _Index) -> {text, ?iol2b(Node)};

transform(var,           Node, _Index) -> prefix_key_postfix(var,           Node);
transform(var_raw1,      Node, _Index) -> prefix_key_postfix(raw_var,       Node);
transform(var_raw2,      Node, _Index) -> prefix_key_postfix(raw_var,       Node);
transform(partial,       Node, _Index) -> prefix_key_postfix(partial,       Node);
transform(block_start,   Node, _Index) -> prefix_key_postfix(block_start,   Node);
transform(inverse_start, Node, _Index) -> prefix_key_postfix(inverse_start, Node);
transform(block_end,     Node, _Index) -> prefix_key_postfix(block_end,     Node);

transform(comment, Node, _Index) ->
	Prefix = ?iol2b(?get(prefix, Node)),
	Postfix = ?iol2b(?get(postfix, Node)),
	{comment, Prefix, Postfix};

transform(template, Node, _Index) ->
	block_transform(Node, [], []);

transform(_Symbol, Node, _Index) ->
	Node.