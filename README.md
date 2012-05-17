This project is an attempt to write mustache template engine,
compatible with specs as much as possible. First it started as [walrus](https://github.com/devinus/walrus)
improvement, but then I (@vladimir-vg) realized that it will be much easier
to follow specs with PEG parser generator, instead of classic LALR yecc generator.

File `src/elk_mochinum.erl` was taken from mochiweb repository.
(Licensed on MIT, same license as whole project).