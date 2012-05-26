-module(elk).
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-export([compile/1, render/1]).

-record(state, {context, partials}).

compile(Source) ->
	Tree = elk_parser:parse(Source),
	{elk_template, Tree}.

render({elk_template, Tree}) ->
	IOList = render_iolist(Tree, #state{}, []),
	iolist_to_binary(IOList).

render_iolist([{text, Text} | Tree], State, Acc) ->
	render_iolist(Tree, State, [Text | Acc]);
render_iolist([], _State, Acc) ->
	lists:reverse(Acc).
