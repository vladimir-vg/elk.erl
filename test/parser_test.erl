-module(parser_test).
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

basic_test() ->
	?assertEqual(
		[indent, [{text, <<"aaa">>}, {var, [<<"key">>]}, {text, <<"bbb">>}], eof],
		elk_parser:parse("aaa{{key}}bbb")).

comment_test() ->
	?assertEqual(
		[indent, [{ws, <<" ">>}, comment, {ws, <<" ">>}], {nl, <<"\r\n">>}, eof],
		elk_parser:parse(" {{! ololo! \r\n\n huh! %^&*() }} \r\n")).

block_test() ->
	?assertEqual(
		[indent,
			[{block, [<<"key">>],
				[[{text, <<" text1 ">>}],{nl, <<"\n">>}],
				[indent, [{text, <<" text2 ">>}]]}], eof],
		elk_parser:parse("{{# key }} text1 \n text2 {{/ key }}")).

inverse_test() ->
	?assertEqual(
		[indent, [{inverse, [<<"key">>], [], [[{text, <<" text ">>}]]}], eof],
		elk_parser:parse("{{^ key }} text {{/ key }}")).

standalone_block_tag_test() ->
	?assertEqual(
		[indent, {nl, <<"\r\n">>}, indent,
			[{ws, <<" ">>},
			{inverse, [<<"key">>],
				[[{ws, <<" ">>}], {nl, <<"\n">>}],
				[indent, [{text, <<" text ">>}]]},
			{ws, <<" ">>}],
		{nl, <<"\n">>}, eof],
		elk_parser:parse("\r\n {{^ key }} \n text {{/ key }} \n")).

nested_blocks_test() ->
	?assertEqual(
		[indent,
			[{text, <<" 1 ">>},
			{block, [<<"first">>], [],
				[[{text, <<" 2 ">>}, {inverse, [<<"second">>], [], [[{text, <<" 3 ">>}]]}, {text, <<" 4 ">>}]]},
			{text, <<" 5 ">>}], eof],
		elk_parser:parse(" 1 {{#first}} 2 {{^second}} 3 {{/second}} 4 {{/first}} 5 ")).

unmatched_block_test() ->
	?assertMatch({error, _}, elk:compile("{{#block}}")),
	?assertMatch({error, _}, elk:compile("{{/block}}")).

unbalanced_block_test() ->
	?assertMatch({error, _}, elk:compile("{{#one}}{{#two}}{{/one}}{{/two}}")).