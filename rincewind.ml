open Typedtree
open Cmt_format
open Location
open Lexing

let default_to defaultValue value =
  match value with
    | Some v -> v
    | None -> defaultValue

let default_to_none = default_to "none"
let default_to_empty = default_to ""

let qname_add q v =
  match q with
    | "" -> v
    | _ -> q ^ "." ^ v

let r = Str.regexp "\n"

let clean_type str =
  Str.global_replace r "" str

let join_array separator items =
  Array.fold_left (fun acc item -> acc ^ item ^ separator) "" items

let rec join_list separator items =
  match items with
    | [] -> ""
    | hd :: [] -> hd
    | hd :: tl -> hd ^ separator ^ (join_list separator tl)

let position_to_string pos =
  (string_of_int pos.pos_lnum) ^ "|" ^ (string_of_int (pos.pos_cnum - pos.pos_bol + 1))

let location_to_string qname {loc_start; loc_end; loc_ghost} =
  (position_to_string loc_start) (*^ "," ^ (position_to_string loc_end)*) ^ "|"

let read_type env typ =
  clean_type (Format.asprintf "%a" Printtyp.type_scheme typ)

let read_pattern {pat_loc; pat_env; pat_type; pat_desc; _} =
  match pat_desc with
    | Tpat_var (ident, s) -> Some (ident.name, (read_type pat_env pat_type))
    | _ -> None

let rec read_expression_desc qname exp_loc exp_desc =
  match exp_desc with
    | Texp_let (_, vbl, e) ->
        let let_loc = (location_to_string qname exp_loc) in
        let let_binding = List.map (fun item -> default_to_empty (read_value_binding qname item)) vbl in
        let next_expr = read_expression qname e in
        Some (let_loc ^ (join_list "__EXP_LET__" let_binding) ^ "\n" ^ (default_to_empty next_expr))
    | Texp_function (_, cases, _) ->
        Some (join_list "__EXP_FUN__" (List.map (fun item -> read_case qname item) cases))
    | _ -> None

and read_case qname {c_rhs} =
  match read_expression_desc qname c_rhs.exp_loc c_rhs.exp_desc with
    | Some x -> x
    | _ -> ""

and read_expression qname {exp_loc; exp_desc; _} =
  read_expression_desc qname exp_loc exp_desc

and read_value_binding qname {vb_pat; vb_expr; vb_attributes; vb_loc} =
    let pattern = read_pattern vb_pat in
    match pattern with
      | None -> None
      | Some (pat_name, pat_type) ->
           let expressions = read_expression (qname_add qname pat_name) vb_expr in
           match expressions with
              | None -> Some (qname ^ "|" ^ pat_name ^ "|" ^ pat_type)
              | Some e -> Some (qname ^ "|" ^ pat_name ^ "|" ^ pat_type ^ "\n" ^ e)

let read_module_binding {mb_expr; _} =
  match mb_expr.mod_desc with
    | Tmod_structure s -> Some s
    | _ -> None

let rec read_structure_item qname {str_desc; str_loc; str_env } =
  match str_desc with
    | Tstr_value (_, vb) -> (location_to_string qname str_loc) ^ (join_list ";" (List.map (fun item -> default_to_empty (read_value_binding qname item)) vb))
    | Tstr_module mb ->
        (match read_module_binding mb with
          | None -> ""
          | Some mbs ->
              let qname = qname_add qname mb.mb_id.name in
              join_list "\n" (List.map (fun item -> (read_structure_item qname item)) mbs.str_items))
    | _ -> ""

let read_structure {str_items; str_type; str_final_env} =
  List.map (fun item -> (read_structure_item "" item)) str_items

let read_cmt_annots annots =
  match annots with
    | Implementation typedTree -> read_structure typedTree
    | _ -> [""]

let print_cmt_info filename =
    let info = Cmt_format.read_cmt filename in
    (*Printf.printf "modname:%s\n" info.cmt_modname;*)
    (*Printf.printf "args:%s\n" (join_array " " info.cmt_args);*)
    (*Printf.printf "sourcefile:%s\n" (default_to_none info.cmt_sourcefile);*)
    (*Printf.printf "builddir:%s\n" info.cmt_builddir;*)
    (*Printf.printf "loadpath:%s\n" (join_list "," info.cmt_loadpath);*)
    (*Printf.printf "use_summaries:%b\n" info.cmt_use_summaries;*)
    Printf.printf "%s" (join_list "\n" (read_cmt_annots info.cmt_annots));

module Driver = struct
  let spec =
    let open Command.Spec in
      empty
      +> anon ("filename" %: string)

  let command =
    Command.basic
      ~summary: "Extract cmt information"
      ~readme:  (fun () -> "rincewind.exe <filename>")
      spec (fun filename () -> print_cmt_info filename)

  let usage_msg = "Usage: rincewind.exe <filename>\nv0.2"

  let main () =
    let args = ref [] in
    Arg.parse [] (fun s -> args := s :: !args) usage_msg;
    match !args with
          | [fname] -> print_cmt_info fname
          | _ -> failwith "Wrong number of arguments."

  let () =
    try
      main ()
    with e ->
      let s = match e with Failure s -> s | _ -> Printexc.to_string e in
      Printf.eprintf "Failure: %s\n%!" s;
      exit 1
end