-module(test_helpers).
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-include_lib("eunit/include/eunit.hrl").

-export([read_spec/1, construct_test/1]).

read_spec(Path) ->
    {ok, Content} = file:read_file(Path),
    {ok, {List}} = json:decode(Content),
    Tests = proplists:get_value(<<"tests">>, List),
	lists:map(fun make_contexts_nested/1, Tests).

make_contexts_nested({List}) when is_list(List) ->
	NewList = lists:map(fun ({Key, Value}) ->
		{Key, make_contexts_nested(Value)}
	end, List),
	{proplist, NewList};
make_contexts_nested(Value) -> Value.

construct_test({proplist, Args}) ->
	Name = proplists:get_value(<<"name">>, Args),
	Data = proplists:get_value(<<"data">>, Args),
	Source = proplists:get_value(<<"template">>, Args),
	Expected = proplists:get_value(<<"expected">>, Args),
	Template = elk:compile(Source),
	{Name, ?_assertEqual(Expected, elk:render(Template, Data))}.