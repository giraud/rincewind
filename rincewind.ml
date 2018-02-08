module Driver = struct
  let usage_msg = "Usage: rincewind.exe <filename>"

  let main () =
    let args = ref [] in
    Arg.parse [] (fun s -> args := s :: !args) usage_msg;
    match !args with
          | [fname] ->
                let info = Cmt_format.read_cmt fname in
                Printf.printf "modname:%s\n" info.cmt_modname;
                Printf.printf "builddir:%s\n" info.cmt_builddir;
                Printf.printf "use_summaries:%b\n" info.cmt_use_summaries
          | _ -> failwith "Wrong number of arguments."

  let () =
    try
      main ()
    with e ->
      let s = match e with Failure s -> s | _ -> Printexc.to_string e in
      Printf.eprintf "Failure: %s\n%!" s;
      exit 1
end