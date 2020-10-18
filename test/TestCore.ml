let skip_first lines : string list =
  match (lines) with
    | _hd :: rest -> rest
    | other -> other

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []


let extract oc fname =
    let _cmio, cmto = Cmt_format.read fname in
    match cmto with
        | Some cmt -> RinceLib.CmtExtractor.read_cmt oc cmt
        | _ -> Printf.printf "Can't read %s" fname

let basedir = ".TestRincewind.eobjs/byte"
let out_ext = ".cmtout"

let run_extractor testFile =
    let out = open_out (basedir ^ "/" ^ testFile ^ out_ext) in
    extract out (basedir ^ "/" ^ testFile ^ ".cmt");
    close_out out;
    read_lines (basedir ^ "/" ^ testFile ^ out_ext)
