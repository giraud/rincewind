Generate test file
  $ cat > input.ml <<EOF
  > let i = 1
  > let f = 2.
  > let st = "v"
  > EOF

Compile test file with typedTree
  $ ocamlc -bin-annot input.ml

Test inferred types from cmt
  $ rincewind.exe -i input.cmt | grep --invert-match input.ml
  Va|1.4,1.5|i|int
  Va|2.4,2.5|f|float
  Va|3.4,3.6|st|string
