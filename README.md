# rincewind

![Build Rincewind executables](https://github.com/giraud/rincewind/actions/workflows/main.yml/badge.svg?branch=master)

Rincewind is a CMT annotation extractor used by the [ReasonML plugin](https://github.com/reasonml-editor/reasonml-idea-plugin) for IntelliJ Idea.

### local build

prepare:
- opam switch \[create] \<ocaml version>
- opam install . --deps-only --with-test

build:
- dune build

test (any):
- dune test 
- dune build @cramtest-name
- dune build @cramtest-name --auto-promote
