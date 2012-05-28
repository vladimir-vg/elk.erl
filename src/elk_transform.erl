-module(elk_transform).
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-export([transform/3]).

-define(key(Node), proplists:get_value(key, Node)).

transform(self, _Node, _Index) ->
	self;
transform(block_start, Node, _Index) ->
	{block_start, ?key(Node)};
transform(inverse_start, Node, _Index) ->
	{inverse_start, ?key(Node)};
transform(block_end, Node, _Index) ->
	{block_end, ?key(Node)};
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
	[indent | second_transform(NewNodes, [], [])];
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

third_transformation([indent | Nodes]) ->
	[indent | third_transformation(Nodes)].