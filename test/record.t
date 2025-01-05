Generate test file
  $ cat > input.ml <<EOF
  > type t = {
  >   pageSize: int;
  >   languages: string array
  > }
  > 
  > let x = {
  >   pageSize=20;
  >   languages=[||]
  > }
  > EOF

Compile test file with typedTree
  $ ocamlc -bin-annot input.ml

Test inferred types from cmt
  $ rincewind.exe -i input.cmt | grep --invert-match input.ml
  Va|6.4,6.5|x|t
  Rf|7.2,7.10|pageSize|int
  Rf|8.2,8.11|languages|string array
