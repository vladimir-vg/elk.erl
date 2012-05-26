-module(specs_test).
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

comments_test_() ->
	Tests = test_helpers:read_spec("../spec/specs/comments.json"),
	lists:map(fun test_helpers:construct_test/1, Tests).