#!/bin/bash
rm -f *.cm* *.exe *.o
/usr/bin/ocamlopt -I +compiler-libs -o rincewind.exe ocamlcommon.cmxa str.cmxa rincewind.ml
