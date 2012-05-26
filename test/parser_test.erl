-module(parser_test).
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

basic_test() ->
	?assertEqual(
		[{text, <<"aaa">>}, {var, <<"key">>, <<>>, <<>>}, {text, <<"bbb">>}],
		elk_parser:parse("aaa{{key}}bbb")).