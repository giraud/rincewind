open Typedtree
open Cmt_format
open Location
open Lexing

type resolvedItem = {
  qname: string;
  location: Location.t;
  old: string
}

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

let deoptionalize (lst:'a option list) : 'a list =
    List.map (fun x -> match x with Some x -> x | None -> assert false) (List.filter (fun x -> x <> None) lst)

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
  (string_of_int pos.pos_lnum) ^ "|" ^ (string_of_int (pos.pos_cnum - pos.pos_bol(*begining of line*) + 1))

let location_to_string {loc_start; loc_end; loc_ghost} =
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
        let let_loc = (location_to_string exp_loc) in
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
    | Tstr_value (_, vb) -> Some {qname=qname; location=str_loc; old=(join_list ";" (List.map (fun item -> default_to_empty (read_value_binding qname item)) vb)) }
    | Tstr_module mb ->
        (match read_module_binding mb with
          | None -> None
          | Some mbs ->
              let qname = qname_add qname mb.mb_id.name in
              Some {qname=qname; location=str_loc; old = (join_list "\n" (List.map (fun item -> (match (read_structure_item qname item) with | None -> "__RSI__" | Some i -> i.old)) mbs.str_items)) })
    | _ -> None

(* map all structure items to a transformed structure *)
let read_structure {str_items(*structure_item list*); str_type(*Types.signature*); str_final_env(*Env.t*)} =
  List.map (fun item -> (read_structure_item "" item)) str_items

(*
binary_annots =
  | Packed of Types.signature * string list
  | Implementation of structure
  | Interface of signature
  | Partial_implementation of binary_part array
  | Partial_interface of binary_part array
*)
let read_cmt_annots annots =
  match annots with
    | Implementation typedTree -> Some (read_structure typedTree)
    | _ -> None

let print_cmt_info cmt =
    (*Printf.printf "modname:%s\n" info.cmt_modname;*)
    (*Printf.printf "args:%s\n" (join_array " " info.cmt_args);*)
    (*Printf.printf "sourcefile:%s\n" (default_to_none info.cmt_sourcefile);*)
    (*Printf.printf "builddir:%s\n" info.cmt_builddir;*)
    (*Printf.printf "loadpath:%s\n" (join_list "," info.cmt_loadpath);*)
    (*Printf.printf "use_summaries:%b\n" info.cmt_use_summaries;*)
    let annots = read_cmt_annots cmt.cmt_annots in
    match annots with
      | Some values -> Printf.printf "%s\n" (join_list "\n" (List.map (fun i -> i.qname ^ (location_to_string i.location) ^ i.old) (deoptionalize values)))
      | None -> Printf.printf "\n";
    ()

(*
Print decoded file info on standard stream. File can be one of:
.cmi	Compiled module interface from a corresponding .mli source file.
.cmt	Typed abstract syntax tree for module implementations.
.cmti	Typed abstract syntax tree for module interfaces.
*)
let print_info fname =
    let cmio, cmto = Cmt_format.read fname in
    match cmio, cmto with
        | Some cmi, Some cmt -> print_cmt_info cmt
        | None, Some cmt -> print_cmt_info cmt
        | Some cmi, None -> Printf.printf "cmi\n"
        | _ -> Printf.eprintf "Can't read %s\n" fname;
    ()

module Driver = struct

    let version = "0.2"
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