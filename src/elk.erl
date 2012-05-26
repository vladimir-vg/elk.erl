-module(elk).
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-export([compile/1, render/1, render/2]).

-define(is_falsy(V), ((V =:= undefined) or (V =:= false))).
-record(state, {contexts, partials}).

compile(Source) ->
	Tree = elk_parser:parse(Source),
	{elk_template, Tree}.

render({elk_template, Tree}) ->
	render({elk_template, Tree}, {proplist, []}).

render({elk_template, Tree}, Context) ->
	State = #state{contexts=[Context]},
	IOList = render_iolist(Tree, State, []),
	iolist_to_binary(IOList).

render_iolist([{text, Text} | Tree], State, Acc) ->
	render_iolist(Tree, State, [Text | Acc]);
render_iolist([{raw_var, Key, Prefix, Postfix} | Tree], State, Acc) ->
	Value = get(Key, State#state.contexts),
	Standalone = not ((Prefix =:= <<>>) or (Postfix =:= <<>>)),
	case {Standalone, ?is_falsy(Value)} of
		{true,  true}  -> render_iolist(Tree, State, Acc);
		{false, true}  -> render_iolist(Tree, State, [Postfix, Prefix | Acc]);
		{_,     false} -> render_iolist(Tree, State, [Postfix, iolistify(Value), Prefix | Acc])
	end;
render_iolist([], _State, Acc) ->
	lists:reverse(Acc).

getter_func(Kind) ->
	Contexts = case application:get_env(elk, contexts) of
		undefined -> [{proplist, elk_proplist_context}];
		Value -> Value
	end,
	Module = proplists:get_value(Kind, Contexts),
	(fun Module:get/2).

get_value(Key, {Kind, Context}) ->
	Fun = getter_func(Kind),
	Fun(Key, Context).

get(Key, [Context | ContextsList]) ->
	case get_value(Key, Context) of
		undefined -> get(Key, ContextsList);
		Value     -> Value
	end;
get(_Key, []) ->
	undefined.

%% Probably in parallel universe this function will be named as 'stringify'
%% But actually result of it doesn't require to be a string, enough to be
%% iolist.
iolistify(undefined) -> "";
iolistify(Value) when is_binary(Value); is_list(Value) -> Value.
