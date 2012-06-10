# Background

Initially this project was started as [walrus](https://github.com/devinus/walrus)
improvement, but then I (@vladimir-vg) decided to use [neotoma](https://github.com/seancribbs/neotoma).
This turned me to start project from scratch.

# Features and compatability

elk.erl almost fully responds to [mustache specs](https://github.com/mustache/spec).
It doesn't support custom delimeters (`{{=<% %>=}}`) and lambdas (as described in specs).

Using elk.erl you can pass function to evaluate, instead of actual value.
But this is not actually "lambdas" as described in specs.

# License

See `LICENSE.MIT` file.

File `src/elk_mochinum.erl` was taken from mochiweb repository.
(Licensed on MIT, same license as for whole project).