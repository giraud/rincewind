Generate test file
  $ cat > input.ml <<EOF
  > let _ =
  >   let fn ~x = x in
  >   fn ~x:1
  > EOF

Compile test file with typedTree
  $ ocamlc -bin-annot input.ml

Test inferred types from cmt
  $ rincewind.exe -i input.cmt | grep --invert-match input.ml
  Va|2.6,2.8|fn|x:'a -> 'a
  Id|2.14,2.15|x|x|'a
  Id|3.2,3.4|fn|fn|x:'a -> 'a
  Pa|3.8,3.9|x|int
