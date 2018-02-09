#!/bin/bash
ls /usr/local
rm -f *.cm* *.exe *.o
/usr/local/ocamlopt -I +compiler-libs -o rincewind.exe ocamlcommon.cmxa str.cmxa rincewind.ml
