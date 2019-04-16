
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

let print_info = (fname) => {
    let (_cmio, cmto) = Cmt_format.read(fname);
    switch (cmto) {
        | Some(cmt) => RinceLib.CmtExtractor.read_cmt(cmt)
        | _ => Printf.printf("Can't read %s", fname)
    };

};

describe("my first test suite", ({test, _}) =>
  test("1 + 1 should equal 2", ({expect}) =>
    {
        let buf = Batteries.BatBuffer.create(16);
        print_info(".TestRincewind.eobjs/byte/Let.cmt");
        expect.int(1 + 1).toBe(2);
    }
  )
);


cli();
