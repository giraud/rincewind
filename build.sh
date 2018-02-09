#!/bin/bash
ls ~/bin
PATH=~/ocaml/bin:$PATH

rm -f *.cm* *.exe *.o
ocamlopt -I +compiler-libs -o rincewind.exe ocamlcommon.cmxa str.cmxa rincewind.ml
