-module(render_test).
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

text_test() ->
	{ok, Template} = elk:compile("text"),
	?assertEqual(
		<<"text">>,
		elk:render(Template)).

raw_var_test() ->
	{ok, Template1} = elk:compile("hello {{& var }}"),
	Context = {proplist, [{<<"var">>, <<"world">>}]},
	?assertEqual(
		<<"hello world">>,
		elk:render(Template1, Context)),
	
	{ok, Template2} = elk:compile("hello {{& var }}"),
	?assertEqual(
		<<"hello ">>,
		elk:render(Template2)),
	
	{ok, Template3} = elk:compile("hello\n {{& var }}\n"),
	?assertEqual(
		<<"hello\n">>,
		elk:render(Template3)).

escaped_var_test() ->
	{ok, Template} = elk:compile("hello {{ var }}"),
	Context = {proplist, [{<<"var">>, <<"\" <tag> & friends">>}]},
	?assertEqual(
		<<"hello &quot; &lt;tag&gt; &amp; friends">>,
		elk:render(Template, Context)).

func_var_test() ->
	Fun = fun (Context) ->
		Value = elk:get_value(<<"value">>, Context),
		["(", Value, ")"]
	end,
	Context = {proplist, [{<<"value">>, "World"}, {<<"parenthesed">>, Fun}]},
	
	{ok, Template} = elk:compile("value is '{{& value }}' also as {{& parenthesed }}"),
	?assertEqual(
		<<"value is 'World' also as (World)">>,
		elk:render(Template, Context)).

bool_block_test() ->
	{ok, Template1} = elk:compile("{{#var}}true{{/var}}"),
	Context = {proplist, [{<<"var">>, true}]},
	?assertEqual(
		<<"true">>,
		elk:render(Template1, Context)),
	
	{ok, Template2} = elk:compile("{{^var}}true{{/var}}"),
	?assertEqual(
		<<>>,
		elk:render(Template2, Context)),
	
	?assertEqual(
		<<"true">>,
		elk:render(Template2)).

nested_block_test() ->
	{ok, Template} = elk:compile("{{#a}} {{b}} {{/a}}"),
	Context = {proplist, [{<<"a">>, {proplist, [{<<"b">>, "TADA!"}]}}]},
	?assertEqual(
		<<" TADA! ">>,
		elk:render(Template, Context)).