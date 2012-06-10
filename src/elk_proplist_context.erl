-module(elk_proplist_context).
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-export([get/2]).

get(Key, Proplist) ->
    proplists:get_value(Key, Proplist).