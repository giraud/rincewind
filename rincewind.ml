(**
 Print decoded file info on standard stream. File can be one of:
  .cmi   Compiled module interface from a corresponding .mli source file.
  .cmt   Typed abstract syntax tree for module implementations.
  .cmti  Typed abstract syntax tree for module interfaces.
*)
let print_info fname =
    let cmio, cmto = Cmt_format.read fname in
    match cmio, cmto with
        | _, Some cmt -> CmtExtractor.read_cmt cmt
        | Some cmi, None -> CmiExtractor.read_cmi cmi
        | None, None -> Printf.printf "Can't read %s" fname;
    Printf.printf "\n"

module Driver = struct

    let version = "0.3-dev"
    let usageMessage = "Usage: rincewind.exe <filename>\n" ^ version

    let main () =
        let args = ref [] in
        Arg.parse [] (fun s -> args := s :: !args) usageMessage;
        match !args with
            | [fname] -> print_info fname
            | _ -> failwith "Wrong number of arguments."

    let () =
        try
            main ()
        with e ->
            let s = match e with Failure s -> s | _ -> Printexc.to_string e in
            Printf.eprintf "Failure: %s\n%!" s;
            exit 1
end