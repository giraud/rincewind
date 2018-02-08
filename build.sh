#!/usr/bin/env bash
rm -f *.cm* *.exe *.o
#ocamlc -g -I +compiler-libs -o a.out ocamlcommon.cma cmt_annot.ml
ocamlopt -I +compiler-libs -o rincewind.exe ocamlcommon.cmxa rincewind.ml