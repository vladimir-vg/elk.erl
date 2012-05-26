-module(render_test).
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

text_test() ->
	Template = elk:compile("text"),
	?assertEqual(
		<<"text">>,
		elk:render(Template)).
