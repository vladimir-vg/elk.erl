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

escaped_var_test() ->
	Context = {proplist, [{<<"var">>, <<"\" <tag> & friends">>}]},
	?assertEqual(
		<<"hello &quot; &lt;tag&gt; &amp; friends">>,
		elk:render(elk:compile("hello {{ var }}"), Context)).

func_var_test() ->
	Fun = fun (Context) ->
		Value = elk:get_value(<<"value">>, Context),
		["(", Value, ")"]
	end,
	Context = {proplist, [{<<"value">>, "World"}, {<<"parenthesed">>, Fun}]},
	
	?assertEqual(
		<<"value is 'World' also as (World)">>,
		elk:render(elk:compile("value is '{{& value }}' also as {{& parenthesed }}"), Context)).