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

transform(text, Node, _Index) -> {text, ?iol2b(Node)};

transform(var,           Node, _Index) -> prefix_key_postfix(var,           Node);
transform(var_escaped1,  Node, _Index) -> prefix_key_postfix(escaped,       Node);
transform(var_escaped2,  Node, _Index) -> prefix_key_postfix(escaped,       Node);
transform(block_start,   Node, _Index) -> prefix_key_postfix(block_start,   Node);
transform(inverse_start, Node, _Index) -> prefix_key_postfix(inverse_start, Node);
transform(block_end,     Node, _Index) -> prefix_key_postfix(block_end,     Node);

transform(_Symbol, Node, _Index) ->
	Node.