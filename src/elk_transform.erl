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

block_transform([{block_start, Key, WS} | Nodes], Expected, Acc) ->
	{Node, NewNodes, NewExpected} =
		block_transform(Nodes, [{block_start, Key, WS} | Expected], []),
	block_transform(NewNodes, NewExpected, [Node | Acc]);
block_transform([{inverse_start, Key, WS} | Nodes], Expected, Acc) ->
	{Node, NewNodes, NewExpected} =
		block_transform(Nodes, [{inverse_start, Key, WS} | Expected], []),
	block_transform(NewNodes, NewExpected, [Node | Acc]);

block_transform(
	[{block_end, Key, EWS} | Nodes],
	[{block_start, Key, SWS} | Expected],
	Acc
) ->
	SubTemplate = block_transform(lists:reverse(Acc), [], []),
	Node = {block, Key, [SWS, EWS], SubTemplate},
	{Node, Nodes, Expected};

block_transform(
	[{block_end, Key, EWS} | Nodes],
	[{inverse_start, Key, SWS} | Expected],
	Acc
) ->
	SubTemplate = block_transform(lists:reverse(Acc), [], []),
	Node = {inverse, Key, [SWS, EWS], SubTemplate},
	{Node, Nodes, Expected};

block_transform([{text, <<>>} | Nodes], Expected, Acc) ->
	block_transform(Nodes, Expected, Acc);
block_transform([Node | Nodes], [], []) ->
	[Node | block_transform(Nodes, [], [])];
block_transform([Node | Nodes], Expected, Acc) ->
	block_transform(Nodes, Expected, [Node | Acc]);
block_transform([], [], Acc) ->
	lists:reverse(Acc).

%% this transformation extracts whitespace prefixes and postfixes, or
%% keeps it if they're standalone
nl_transform([{text, Text} | Nodes]) ->
	[{text, Text} | nl_transform(Nodes)];
nl_transform([{comment, {Nl, Prefix}, Postfix} | Nodes]) when is_binary(Nl) ->
	if
		(Nl =/= <<>>) and (Postfix =/= <<>>) ->
			[{text, Nl} | nl_transform(Nodes)];
		true ->
			[{text, Nl}, {text, Prefix}, {text, Postfix} | nl_transform(Nodes)]
	end;
nl_transform([{Kind, Key, {Nl, Prefix}, Postfix} | Nodes]) when is_binary(Nl) ->
	Standalone = (Nl =/= <<>>) and (Postfix =/= <<>>),
	[{text, Nl}, {Kind, Key, {Standalone, Prefix, Postfix}} | nl_transform(Nodes)];
nl_transform([{Kind, Key, [{{Nl1, SPrefix}, SPostfix}, {{Nl2, EPrefix}, EPostfix}], Tree} | Nodes]) ->
	SStandalone = (Nl1 =/= <<>>) and (SPostfix =/= <<>>),
	EStandalone = (Nl2 =/= <<>>) and (EPostfix =/= <<>>),
	NewTree = Tree ++ [{text, Nl2}],
	NewNode = {Kind, Key, [{SStandalone, SPrefix, SPostfix}, {EStandalone, EPrefix, EPostfix}], NewTree},
	[{text, Nl1}, NewNode | nl_transform(Nodes)];
nl_transform([]) ->
	[].
	
transform(tag, Node, _Index) ->
	Nl = ?iol2b(?get(nl, Node)),
	Tag = ?get(tag, Node),
	Kind = element(1, Tag),
	case Kind of
		comment ->
			Prefix = element(2, Tag),
			setelement(2, Tag, {Nl, Prefix});
		_ ->
			Prefix = element(3, Tag),
			setelement(3, Tag, {Nl, Prefix})
		
	end;
	
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
	block_transform(nl_transform(Node), [], []);

transform(_Symbol, Node, _Index) ->
	Node.