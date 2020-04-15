let test_val_basic () =
  let open! Let in
  let output = TestCore.run_extractor "let" in

  Alcotest.(check (list string)) "same output" [
    "Va|1.4,1.5|i|int";
    "Va|2.4,2.5|f|float";
    "Va|3.4,3.6|st|string"
  ]  output

let test_functor_let () =
  let open! FunctorLet in
  let output = TestCore.run_extractor "functorLet" in

  Alcotest.(check (list string)) "same output" [
    "Va|4.6,4.7|x|int"
  ] output

let () =
  let open Alcotest in
  run "cmt extractor" [
    "basic", [
        test_case "should get let info" `Quick test_val_basic;
        test_case "should get let info in functor" `Quick test_functor_let
    ]
  ]
