let test_val_basic () =
  let open! Let in
  let output = TestCore.run_extractor "let" in

  Alcotest.(check (list string)) "same output" output [
    "Va|1.4,1.5|i|int";
    "Va|2.4,2.5|f|float";
    "Va|3.4,3.6|st|string"
  ]

let () =
  let open Alcotest in
  run "cmt extractor" [
    "basic", [
        test_case "should get let info" `Quick test_val_basic
    ]
  ]
