#!/usr/bin/env bash
rm -f *.cm* *.exe *.o
ocamlopt -I +compiler-libs -o rincewind.exe ocamlcommon.cmxa rincewind.ml