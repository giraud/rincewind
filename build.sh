#!/bin/bash
rm -f *.cm* *.exe *.o
mkdir build
ocamlopt -I +compiler-libs -o build/rincewind.exe ocamlcommon.cmxa str.cmxa rincewind.ml
