let test_val_basic () =
  let open! Let in
  let output = TestCore.(skip_first (run_extractor "let")) in

  Alcotest.(check (list string)) "same output" [
    "Va|1.4,1.5|i|int";
    "Va|2.4,2.5|f|float";
    "Va|3.4,3.6|st|string"
  ]  output

let test_functor_let () =
  let open! FunctorLet in
  let output = TestCore.(skip_first (run_extractor "functorLet")) in

  Alcotest.(check (list string)) "same output" [
    "Md|3.0,5.3|Make";
    "Va|4.6,4.7|x|int";
    "Id|4.14,4.15|+|+|int -> int -> int";
    "Id|4.10,4.13|M.x|M.x|int";
    "Md|7.0,9.4|Impl";
    "Va|8.8,8.9|x|int"
  ] output

let test_named_param () =
  let open! Param in
  let output = TestCore.(skip_first (run_extractor "param")) in

  Alcotest.(check (list string)) "same output" [
    "Va|2.6,2.8|fn|x:'a -> 'a";
    "Id|2.14,2.15|x|x|'a";
    "Id|3.2,3.4|fn|fn|x:'a -> 'a";
    "Pa|3.8,3.9|x|int";
  ]  output

let test_record () =
  let open! Record in
  let output = TestCore.(skip_first (run_extractor "record")) in

  Alcotest.(check (list string)) "same output" [
    "Va|6.4,6.5|x|t";
    "Rf|6.8,9.1|pageSize|int";
    "Rf|6.8,9.1|languages|string array";
  ]  output

let () =
  let open Alcotest in
  run "cmt extractor" [
    "basic", [
        test_case "should get let info" `Quick test_val_basic;
        test_case "should get let info in functor" `Quick test_functor_let;
        test_case "should get named param info" `Quick test_named_param;
        test_case "should get record info" `Quick test_record;
    ]
  ]
