-module(parser_test).
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

basic_test() ->
	?assertEqual(
		[{text, <<"aaa">>}, {var, <<"key">>, <<>>, <<>>}, {text, <<"bbb">>}],
		elk_parser:parse("aaa{{key}}bbb")).

block_test() ->
	?assertEqual(
		[{block, <<"key">>, [{<<>>, <<>>}, {<<>>, <<>>}], [{text, <<" text ">>}]}],
		elk_parser:parse("{{# key }} text {{/ key }}")).

inverse_test() ->
	?assertEqual(
		[{inverse, <<"key">>, [{<<>>, <<>>}, {<<>>, <<>>}], [{text, <<" text ">>}]}],
		elk_parser:parse("{{^ key }} text {{/ key }}")).

standalone_block_tag_test() ->
	?assertEqual(
		[{inverse, <<"key">>, [{<<"\r\n ">>, <<" \n">>}, {<<>>, <<" \n">>}], [{text, <<" text ">>}]}],
		elk_parser:parse("\r\n {{^ key }} \n text {{/ key }} \n")).