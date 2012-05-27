-module(parser_test).
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

basic_test() ->
	?assertEqual(
		[{text, <<"aaa">>}, {var, <<"key">>, {false, <<>>, <<>>}}, {text, <<"bbb">>}],
		elk_parser:parse("aaa{{key}}bbb")).

comment_test() ->
	?assertEqual(
		[],
		elk_parser:parse(" {{! ololo! \r\n\n huh! %^&*() }} \r\n")).

block_test() ->
	?assertEqual(
		[{block, <<"key">>, [{false, <<>>, <<>>}, {false, <<" ">>, <<>>}], [{text, <<" text">>}]}],
		elk_parser:parse("{{# key }} text {{/ key }}")).

inverse_test() ->
	?assertEqual(
		[{inverse, <<"key">>, [{false, <<>>, <<>>}, {false, <<" ">>, <<>>}], [{text, <<" text">>}]}],
		elk_parser:parse("{{^ key }} text {{/ key }}")).

standalone_block_tag_test() ->
	?assertEqual(
		[
			{text, <<"\r\n">>},
			{inverse, <<"key">>, [{true, <<" ">>, <<" \n">>}, {false, <<" ">>, <<" \n">>}],
				[{text, <<" text">>}]}],
		elk_parser:parse("\r\n {{^ key }} \n text {{/ key }} \n")).

nested_blocks_test() ->
	?assertEqual(
		[{text, <<" 1">>},
		{block, <<"first">>, [{false, <<" ">>, <<>>}, {false, <<" ">>, <<>>}], [
			{text, <<" 2">>},
			{inverse, <<"second">>, [{false, <<" ">>, <<>>}, {false, <<" ">>, <<>>}], [{text, <<" 3">>}]},
			{text, <<" 4">>}]},
		{text, <<" 5 ">>}],
		elk_parser:parse(" 1 {{#first}} 2 {{^second}} 3 {{/second}} 4 {{/first}} 5 ")).