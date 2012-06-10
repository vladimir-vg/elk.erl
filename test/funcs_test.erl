-module(funcs_test).
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

simple_test() ->
	{ok, Template} = elk:compile("{{value}}"),
	Fun = fun (_State) ->
		"from function"
	end,
	Context = {proplist, [{<<"value">>, Fun}]},
	?assertEqual(
		<<"from function">>,
		elk:render(Template, Context)).

classic_example_test() ->
	Source =
		"Hello {{name}}\n"
		"You have just won {{value}} dollars!\n"
		"{{#in_ca}}\n"
		"Well, {{taxed_value}} dollars, after taxes.\n"
		"{{/in_ca}}",
	Output =
		<<"Hello Chris\n"
		"You have just won 10000 dollars!\n"
		"Well, 6000.0 dollars, after taxes.\n">>,
	{ok, Template} = elk:compile(Source),
	Fun = fun (State) -> elk:get(value, [<<"value">>], State)*0.6 end,
	Context = {proplist, [
		{<<"name">>, "Chris"},
		{<<"value">>, 10000},
		{<<"in_ca">>, true},
		{<<"taxed_value">>, Fun}]},
	?assertEqual(Output, elk:render(Template, Context)).
	