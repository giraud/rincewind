(**
 Print decoded file info on a standard stream. File can be one of:

  .cmi   Compiled module interface from a corresponding .mli source file.
  .cmt   Typed abstract syntax tree for module implementations.
  .cmti  Typed abstract syntax tree for module interfaces.
*)
let print_info dump fname =
    let _cmio, cmto = Cmt_format.read fname in
    match cmto with
        | Some cmt when dump -> RinceLib.CmtDumper.print_cmt cmt
        | Some cmt -> RinceLib.CmtExtractor.read_cmt stdout cmt
        | _ -> Printf.printf "Can't read %s" fname;
    Printf.printf "\n"

module Driver = struct

    let dump = ref false

    let version = "0.5"
    let usage_message = "Usage: rincewind.exe <filename>\n" ^ version
    let spec = Arg.align [ "-d", Arg.Set dump, " Dump mode" ]

    let () =
        try
            let args = ref [] in
            Arg.parse spec (fun s -> args := s :: !args) usage_message;
            match !args with
                | [fname] -> print_info !dump fname
                | _ -> failwith ("(" ^ version ^ ") Wrong number of arguments.")
        with e ->
            let s = match e with Failure s -> s | _ -> Printexc.to_string e in
            Printf.eprintf "Failure: %s\n%!" s;
            exit 1
end