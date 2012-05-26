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

%% this function just checks standalone-ness and inserts (or not)
%% postfix and prefix whitespace
render_tag(Value, Postfix, Prefix, Acc) ->
	Standalone = not ((Prefix =:= <<>>) or (Postfix =:= <<>>)),
	case {Standalone, ?is_falsy(Value)} of
		{true,  true}  -> Acc;
		{false, true}  -> [Postfix, Prefix | Acc];
		{_,     false} -> [Postfix, stringify(Value), Prefix | Acc]
	end.

render_iolist([{text, Text} | Tree], State, Acc) ->
	render_iolist(Tree, State, [Text | Acc]);
render_iolist([{raw_var, Key, Prefix, Postfix} | Tree], State, Acc) ->
	Value = get(Key, State),
	render_iolist(Tree, State, render_tag(Value, Prefix, Postfix, Acc));
render_iolist([], _State, Acc) ->
	lists:reverse(Acc).

get(Key, State) ->
	Contexts = State#state.contexts,
	get_from_contexts(Key, Contexts).

get_value(Key, {Kind, Context}) ->
	Contexts = case application:get_env(elk, contexts) of
		undefined -> [{proplist, elk_proplist_context}];
		Value -> Value
	end,
	Module = proplists:get_value(Kind, Contexts),
	Module:get(Key, Context).

get_from_contexts(Key, [Context | ContextsList]) ->
	case get_value(Key, Context) of
		undefined -> get_from_contexts(Key, ContextsList);
		Value     -> Value
	end;
get_from_contexts(_Key, []) ->
	undefined.

stringify(true) -> "true";
stringify(false) -> "";
stringify(undefined) -> "";
stringify(Value) when is_binary(Value); is_list(Value) -> Value.
