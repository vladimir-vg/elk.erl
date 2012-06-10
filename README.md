# Background

Initially this project was started as [walrus](https://github.com/devinus/walrus)
improvement, but then I (Gordeev Vladimir) decided to use [neotoma](https://github.com/seancribbs/neotoma).
This turned me to start project from scratch.

# Features and compatability

elk.erl almost fully responds to [mustache specs](https://github.com/mustache/spec).
It doesn't support custom delimeters (`{{=<% %>=}}`) and lambdas (as described in specs).
Everything other supported as well
(including partials, dotted keys `{{person.name}}` and dot-tag `{{.}}`).

Using elk.erl you can pass function to evaluate, instead of actual value.
But this is not actually "lambdas" as described in specs.

```erlang
Source =
    "Hello {{name}}\n"
    "You have just won {{value}} dollars!\n"
    "{{#in_ca}}\n"
    "Well, {{taxed_value}} dollars, after taxes.\n"
    "{{/in_ca}}",

{ok, Template} = elk:compile(Source),
Taxed = fun (State) -> elk:get(value, [<<"value">>], State)*0.6 end,
Context = {proplist, [
    {<<"name">>, "Chris"},
    {<<"value">>, 10000},
    {<<"in_ca">>, true},
    {<<"taxed_value">>, Taxed}]},
elk:render(Template, Context).
```

will output:

```
Hello Chris
You have just won 10000 dollars!
Well, 6000.0 dollars, after taxes.
```

# Context format

By default you can pass your mappings as proplists (`{proplist, YourPropList}`).
Also you can use other data structures. To do so you should write module that
implements specific interface and put module name into config.

For example, if you want use erlang `dict` datastructure:

```erlang
-module(elk_dict_context).

-export([get/2]).

get(Key, Dict) ->
    case dict:is_key(Key, Dict) of
        false -> undefined;
        true -> dict:fetch(Key, Dict)
    end.
```

and put in config:
```
{elk, [{contexts, [{proplist, elk_proplist_context}, {dict, elk_dict_context}]}]}
```

Now you can pass dicts as follows: `elk:render(Template, {dict, YourDict})`.

By default only one datastructure is defined:
```
{elk, [{contexts, [{proplist, elk_proplist_context}]}]}
```

Context module `get/2` function should return `undefined` if no value found for
given key and value otherwise.

# Contribution

I'm (Gordeev Vladimir) not a native english speaker, so very possible I done
a lot of grammar and lexical mistakes. Feel free to send a pull-request.

Also, if you think that some context datastructures should be available by
default, you also can send a pull-request.

# License

See `LICENSE.MIT` file.

File `src/elk_mochinum.erl` was taken from mochiweb repository.
(Licensed on MIT, same license as for whole project).