-module(render_test).
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

text_test() ->
	?assertEqual(
		<<"text">>,
		elk:render(elk:compile("text"))).

raw_var_test() ->
	Context = {proplist, [{<<"var">>, <<"world">>}]},
	?assertEqual(
		<<"hello world">>,
		elk:render(elk:compile("hello {{& var }}"), Context)),
	
	?assertEqual(
		<<"hello ">>,
		elk:render(elk:compile("hello {{& var }}"))),
	
	?assertEqual(
		<<"hello">>,
		elk:render(elk:compile("hello\n {{& var }}\n"))).