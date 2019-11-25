module TestFramework =
  Rely.Make(struct
    let config = Rely.TestFrameworkConfig.initialize {
      snapshotDir = "test/_snapshots";
      projectDir  = ""
    }
  end)

open TestFramework

let _ =
  describe "cmt extractor" (fun {test; _} ->
    test "should get let info" (fun {expect} ->
      let open! Let in
      let output = TestCore.run_extractor "Let" in

      (expect.list output).toEqual [
        "Va|1.4,1.5|i|int";
        "Va|2.4,2.5|f|float";
        "Va|3.4,3.6|st|string"
      ]
    )
  )

let _ = cli ()