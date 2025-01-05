Generate test file
  $ cat > input.ml <<EOF
  > module type X_int = sig val x : int end
  > 
  > module Make (M : X_int) : X_int = struct
  >   let x = M.x + 1
  > end
  > 
  > module Impl = Make(struct
  >   let x = 3
  > end)
  > EOF

Compile test file with typedTree
  $ ocamlc -bin-annot input.ml

Test inferred types from cmt
  $ rincewind.exe -i input.cmt | grep --invert-match input.ml
  Md|3.0,5.3|Make
  Va|4.6,4.7|x|int
  Id|4.14,4.15|+|+|int -> int -> int
  Id|4.10,4.13|M.x|M.x|int
  Md|7.0,9.4|Impl
  Va|8.6,8.7|x|int
