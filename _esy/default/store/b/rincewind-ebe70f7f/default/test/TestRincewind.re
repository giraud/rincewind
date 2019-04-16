
module TestFramework =
  Rely.Make({
    let config =
      Rely.TestFrameworkConfig.initialize({
        snapshotDir: "test/_snapshots",
        projectDir: "",
      });
  });

open TestFramework;

open! Let;


describe("my first test suite", ({test, _}) =>
  test("1 + 1 should equal 2", ({expect}) => {
    let testFile = "Let";
    let output: list(string) = TestCore.run_extractor(testFile);
    //let x = RinceLib.Util.List.join("  ", xxx);

    expect.list(output).toEqual([
        "Va|1.4,1.5|i|int",
        "Va|2.4,2.5|f|float",
        "Va|3.4,3.6|st|string"
    ]);
  })
);


cli();
