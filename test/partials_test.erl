-module(partials_test).
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

basic_test() ->
	{ok, Template1} = elk:compile(" Hello {{> world }}"),
	{ok, Template2} = elk:compile("World!"),
	Context = {proplist, []},
	Partials = {proplist, [{<<"world">>, Template2}]},
	?assertEqual(
		<<" Hello World!">>,
		elk:render(Template1, Context, Partials)).