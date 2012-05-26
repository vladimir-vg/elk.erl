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

%% find matching pairs for blocks
block_construct([{block_start, Key, Prefix, Postfix} | Nodes], Expected, Acc) ->
	block_construct(Nodes, [ {block_start, Key, Prefix, Postfix} | Expected], Acc);
block_construct([{inverse_start, Key, Prefix, Postfix} | Nodes], Expected, Acc) ->
	block_construct(Nodes, [ {inverse_start, Key, Prefix, Postfix} | Expected], Acc);
block_construct(
	[{block_end, Key, EndPrefix, EndPostfix} | Nodes],
	[{block_start, Key, StartPrefix, StartPostfix} | Expected],
	Acc
) ->
	SubTemplate = block_construct(lists:reverse(Acc), [], []),
	Rest = block_construct(Nodes, Expected, []),
	Node = {block, Key, [{StartPrefix, StartPostfix}, {EndPrefix, EndPostfix}], SubTemplate},
	[Node | Rest];
block_construct(
	[{block_end, Key, EndPrefix, EndPostfix} | Nodes],
	[{inverse_start, Key, StartPrefix, StartPostfix} | Expected],
	Acc
) ->
	SubTemplate = block_construct(lists:reverse(Acc), [], []),
	Rest = block_construct(Nodes, Expected, []),
	Node = {inverse, Key, [{StartPrefix, StartPostfix}, {EndPrefix, EndPostfix}], SubTemplate},
	[Node | Rest];
block_construct([Node | Nodes], Expected, Acc) ->
	block_construct(Nodes, Expected, [Node | Acc]);
block_construct([], [], Acc) ->
	lists:reverse(Acc).

transform(text, Node, _Index) -> {text, ?iol2b(Node)};

transform(var,           Node, _Index) -> prefix_key_postfix(var,           Node);
transform(var_escaped1,  Node, _Index) -> prefix_key_postfix(escaped,       Node);
transform(var_escaped2,  Node, _Index) -> prefix_key_postfix(escaped,       Node);
transform(block_start,   Node, _Index) -> prefix_key_postfix(block_start,   Node);
transform(inverse_start, Node, _Index) -> prefix_key_postfix(inverse_start, Node);
transform(block_end,     Node, _Index) -> prefix_key_postfix(block_end,     Node);

transform(template, Node, _Index) ->
	block_construct(Node, [], []);

transform(_Symbol, Node, _Index) ->
	Node.