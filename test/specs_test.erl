-module(specs_test).
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

comments_test_() ->
	Tests = test_helpers:read_spec("../spec/specs/comments.json"),
	lists:map(fun test_helpers:construct_test/1, Tests).

interpolation_test_() ->
	Tests = test_helpers:read_spec("../spec/specs/interpolation.json"),
	lists:map(fun test_helpers:construct_test/1, Tests).

sections_test_() ->
	Tests = test_helpers:read_spec("../spec/specs/sections.json"),
	lists:map(fun test_helpers:construct_test/1, Tests).

inverted_test_() ->
	Tests = test_helpers:read_spec("../spec/specs/inverted.json"),
	lists:map(fun test_helpers:construct_test/1, Tests).

partials_test_() ->
	Tests = test_helpers:read_spec("../spec/specs/partials.json"),
	lists:map(fun test_helpers:construct_test/1, Tests).