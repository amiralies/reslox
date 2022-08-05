# Reslox

ReScript implementation of lox interpreter

## Run

```sh
yarn # installs rescript compiler
node src/Main.bs.js
```

## Notes

It passed the entire jlox test suite

The code is a balance between idiomatic rescript and the book's code examples

There's no resolver since the environment is implemented (mostly) with immutable data structures
but tehre'a an static resolver which detects errors like using `this` at top level
