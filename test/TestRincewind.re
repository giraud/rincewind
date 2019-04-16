module TestFramework =
  Rely.Make({
    let config =
      Rely.TestFrameworkConfig.initialize({
        snapshotDir: "test/_snapshots",
        projectDir: "",
      });
  });

open TestFramework;

describe("cmt extractor", ({test, _}) =>
  test("should get let info", ({expect}) => {
    open! Let;
    let output = TestCore.run_extractor("Let");

    expect.list(output).toEqual([
        "Va|1.4,1.5|i|int",
        "Va|2.4,2.5|f|float",
        "Va|3.4,3.6|st|string"
    ]);
  })
);


cli();
