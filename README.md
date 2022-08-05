# Reslox

ReScript implementation of lox interpreter

## Run

```sh
yarn # installs rescript compiler
node src/Main.bs.js
```

## Notes

It passes the entire jlox test suite.

The code is a balance between idiomatic rescript and the book's code examples.

There's no resolver since the environment is implemented (mostly) using immutable data structures
but there's an static analyzer which detects errors like using `this` at top level.
