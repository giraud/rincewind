module Driver = struct

    let version = "0.1"
    let usageMessage = "Usage: rincewind.exe <filename>\n" ^ version

    (**
     Print decoded file info on standard stream. File can be one of:
      .cmi   Compiled module interface from a corresponding .mli source file.
      .cmt   Typed abstract syntax tree for module implementations.
      .cmti  Typed abstract syntax tree for module interfaces.
    *)
    let print_info fname =
        let cmio, cmto = Cmt_format.read fname in
        let entries = match cmio, cmto with
            | _, Some cmt -> CmtExtractor.parse_cmt cmt
            | Some cmi, None -> CmiExtractor.parse_cmi cmi
            | None, None -> ["Can't read " ^ fname] in
        match (entries) with
            | [] -> Printf.printf "\n"
            | _ -> Printf.printf "%s" (Util.join_list "\n" entries);
        ()

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