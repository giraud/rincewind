open Typedtree
open Cmt_format
open Location
open Lexing

let default_to defaultValue value =
  match value with
    | Some f -> f
    | None -> defaultValue

let default_to_none = default_to "none"

let join_array separator items = Array.fold_left (fun acc item -> acc ^ item ^ separator) "" items

let rec join_list separator items =
  match items with
    | [] -> ""
    | hd :: [] -> hd
    | hd :: tl -> hd ^ separator ^ (join_list separator tl)

let foption f o =
  match o with
  | None -> None
  | Some x -> f x

(* Ident.t => t = { stamp: int; name: string; mutable flags: int } *)

(* lexing.ml
type position = {
  pos_fname : string;
  pos_lnum : int;  line number
  pos_bol : int;   beginnig of line
  pos_cnum : int;  column number
}*)
let position_to_string pos = (string_of_int pos.pos_lnum) ^ ":" ^ (string_of_int (pos.pos_cnum - pos.pos_bol + 1))

(* location.ml
Location.t = { loc_start: position; loc_end: position; loc_ghost: bool }
*)
let location_to_string {loc_start; loc_end; loc_ghost} = "[" ^  (position_to_string loc_start) ^ "," ^ (position_to_string loc_end) ^ "]"

let print_type_scheme env typ =
  Printtyp.wrap_printing_env env (fun () -> Format.asprintf "%a" Printtyp.type_scheme typ)

(*
type pattern = {
    pat_desc: pattern_desc;
    pat_loc: Location.t;
    pat_extra : (pat_extra * Location.t * attribute list) list;
    pat_type: type_expr;
    mutable pat_env: Env.t;
    pat_attributes: attribute list;
}
and pattern_desc =
    Tpat_any
  | Tpat_var of Ident.t * string loc
  | Tpat_alias of pattern * Ident.t * string loc
  | Tpat_constant of constant
  | Tpat_tuple of pattern list
  | Tpat_construct of
      Longident.t loc * constructor_description * pattern list
  | Tpat_variant of label * pattern option * row_desc ref
  | Tpat_record of
      (Longident.t loc * label_description * pattern) list *
        closed_flag
  | Tpat_array of pattern list
  | Tpat_or of pattern * pattern * row_desc option
  | Tpat_lazy of pattern
*)
let read_pattern {pat_loc; pat_env; pat_type; pat_desc; _} =
  match pat_desc with
    | Tpat_var (ident, s) -> ident.name ^ ":" ^ print_type_scheme pat_env pat_type
    | _ -> ""

(* typedtree.ml
value_binding = {
    vb_pat: pattern;
    vb_expr: expression;
    vb_attributes: attributes;
    vb_loc: Location.t;
}*)
let read_value_binding {vb_pat; vb_expr; vb_attributes; vb_loc} =
    read_pattern vb_pat

(* typedtree.ml
structure_item_desc =
    Tstr_eval of expression * attributes
  | Tstr_value of rec_flag * value_binding list
  | Tstr_primitive of value_description
  | Tstr_type of type_declaration list
  | Tstr_typext of type_extension
  | Tstr_exception of extension_constructor
  | Tstr_module of module_binding
  | Tstr_recmodule of module_binding list
  | Tstr_modtype of module_type_declaration
  | Tstr_open of open_description
  | Tstr_class of (class_declaration * string list * virtual_flag) list
  | Tstr_class_type of (Ident.t * string loc * class_type_declaration) list
  | Tstr_include of include_declaration
  | Tstr_attribute of attribute
*)

(* typedtree.ml
structure_item = {
    str_desc : structure_item_desc;
    str_loc  : Location.t;
    str_env  : Env.t
} *)
let read_structure_item {str_desc; str_loc; str_env } =
  let item = match str_desc with
    | Tstr_value (Recursive, vb) -> "r value"
    | Tstr_value (Nonrecursive, vb) -> join_list ";" (List.map read_value_binding vb)
    | Tstr_module m -> "module"
    | Tstr_recmodule rm -> "rec module"
    | Tstr_class c -> "class"
    | Tstr_include i -> "include"
    | _ -> "other" in
  (location_to_string str_loc) ^ item

(* typedtree.ml
structure = {
  str_items : structure_item list;
  str_type : Types.signature;
  str_final_env : Env.t;
}
*)
let read_structure {str_items; str_type; str_final_env} =
  List.map (fun item -> (read_structure_item item)) str_items

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
    | Packed (x, y) -> ["packed"]
    | Implementation typedTree -> read_structure typedTree
    | Interface x -> ["interface"]
    | Partial_implementation x -> ["partial impl"]
    | Partial_interface x -> ["partial int"]

(*
type cmt_infos = {
  cmt_modname            : string;
  cmt_annots             : binary_annots;
  cmt_value_dependencies : (Types.value_description * Types.value_description) list;
  cmt_comments           : (string * Location.t) list;
  cmt_args               : string array;
  cmt_sourcefile         : string option;
  cmt_builddir           : string;
  cmt_loadpath           : string list;
  cmt_source_digest      : Digest.t option;
  cmt_initial_env        : Env.t;
  cmt_imports            : (string * Digest.t option) list;
  cmt_interface_digest   : Digest.t option;
  cmt_use_summaries      : bool;
}
*)
let print_cmt_info filename =
    let info = Cmt_format.read_cmt filename in
    Printf.printf "modname:%s\n" info.cmt_modname;
    Printf.printf "args:%s\n" (join_array " " info.cmt_args);
    Printf.printf "sourcefile:%s\n" (default_to_none info.cmt_sourcefile);
    Printf.printf "builddir:%s\n" info.cmt_builddir;
    Printf.printf "loadpath:%s\n" (join_list "," info.cmt_loadpath);
    Printf.printf "use_summaries:%b\n" info.cmt_use_summaries;
    Printf.printf "%s" (join_list "\n" (read_cmt_annots info.cmt_annots));

module Driver = struct
  let usage_msg = "Usage: rincewind.exe <filename>"

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