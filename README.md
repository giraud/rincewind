# rincewind

![Build Rincewind executables](https://github.com/giraud/rincewind/workflows/Build%20Rincewind%20executables/badge.svg?branch=master)

Rincewind is a CMT annotation extractor used by the [ReasonML plugin](https://github.com/reasonml-editor/reasonml-idea-plugin) for IntelliJ Idea.

### local build

prepare:
- opam switch \[create] \<ocaml version>
- opam install -t . --deps-only

build:
- dune build

test:
- dune runtest 
