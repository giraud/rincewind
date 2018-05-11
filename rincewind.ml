open Typedtree
open Cmt_format
open Location
open Lexing
open RwTypes

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
  Str.global_replace r " " str

let join_array separator items =
  Array.fold_left (fun acc item -> acc ^ item ^ separator) "" items

let rec join_list separator items =
  match items with
    | [] -> ""
    | hd :: [] -> hd
    | hd :: tl -> hd ^ separator ^ (join_list separator tl)

let position_to_string pos =
  (string_of_int pos.pos_lnum) ^ "." ^ (string_of_int (pos.pos_cnum - pos.pos_bol(*begining of line*) + 1))

let location_to_string {loc_start; loc_end; loc_ghost} =
  (position_to_string loc_start) (*^ "," ^ (position_to_string loc_end)*) ^ "|"

(*loc_start loc_end =>
type position = {
  pos_fname : string;
  pos_lnum : int;  line number
  pos_bol : int;   beginnig of line
  pos_cnum : int;  column number
}*)
let full_location_to_string {loc_start; loc_end; loc_ghost} =
  "start: " ^ (string_of_int loc_start.pos_lnum) ^ "/" ^ (string_of_int loc_start.pos_bol) ^ "/" ^ (string_of_int loc_start.pos_cnum) ^
  " end: " ^ (string_of_int loc_end.pos_lnum) ^ "/" ^ (string_of_int loc_end.pos_bol) ^ "/" ^ (string_of_int loc_end.pos_cnum) ^
  " ghost: " ^ (string_of_bool loc_ghost)

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
type value_description =
  { val_type: type_expr;                (* Type of the value *)
    val_kind: value_kind;
    val_loc: Location.t;
    val_attributes: Parsetree.attributes;
}
*)

type resolved_item =
    | Single of resolved_item_description
    | Multiple of resolved_item_description list

let rec flat_resolved_items resolved_items =
    match resolved_items with
        | [] -> []
        | Single i :: tl -> (List.append [i] (flat_resolved_items tl))
        | Multiple l :: tl -> (List.append l (flat_resolved_items tl))

let format_resolved_item {i_kind; i_loc; i_path; i_name; i_type; i_comment} =
    i_kind ^ "|" ^ (location_to_string i_loc) ^ i_path ^ "|" ^ i_name ^ "|" ^ (clean_type i_type)(* ^ "|" ^ i_comment*)

(*
signature = signature_item list

and signature_item =
    Sig_value of Ident.t * value_description
  | Sig_type of Ident.t * type_declaration * rec_status
  | Sig_typext of Ident.t * extension_constructor * ext_status
  | Sig_module of Ident.t * module_declaration * rec_status
  | Sig_modtype of Ident.t * modtype_declaration
  | Sig_class of Ident.t * class_declaration * rec_status
  | Sig_class_type of Ident.t * class_type_declaration * rec_status
*)
let rec parse_cmi_sign path signature =
    match signature with
        | Types.Sig_value (ident, desc) -> Single {i_kind="V"; i_loc=desc.val_loc; i_path=path; i_name=ident.name; i_type=(Format.asprintf "%a" Printtyp.type_scheme desc.val_type); i_comment=""}
        | Types.Sig_type (ident, decl, status) ->  Single {i_kind="T"; i_loc=decl.type_loc; i_path=path; i_name=ident.name; i_type=""; i_comment=""}
        | Types.Sig_typext (ident, constr, status) -> Single {i_kind="E"; i_loc=constr.ext_loc; i_path=path; i_name=ident.name; i_type=""; i_comment=""}
        | Types.Sig_module (ident, decl, status) -> (
            let x = match decl.md_type with
                | Mty_signature signature -> List.map (parse_cmi_sign (qname_add path ident.name)) signature
                | _ -> []
                in
            (*Multiple (List.append [{i_kind="M"; i_loc=decl.md_loc; i_path=path; i_name=ident.name; i_type="decl.md_type"; i_comment=""}] (flat_resolved_items x))*)
            Multiple (flat_resolved_items x)
        )
        | Types.Sig_modtype (ident, decl) -> Single {i_kind="N"; i_loc=decl.mtd_loc; i_path=path; i_name=ident.name; i_type=""; i_comment=""}
        | Types.Sig_class (ident, decl, status) -> Single {i_kind="C"; i_loc=decl.cty_loc; i_path=path; i_name=ident.name; i_type=""; i_comment=""}
        | Types.Sig_class_type (ident, decl, status) -> Single {i_kind="D"; i_loc=decl.clty_loc; i_path=path; i_name=ident.name; i_type=""; i_comment=""}

(**
type cmi_infos =
    cmi_name : string; module nama
    cmi_sign : Types.signature_item list;
    cmi_crcs : (string * Digest.t option) list;
    cmi_flags : pers_flags list;
*)
let parse_cmi cmi =
    let {Cmi_format.cmi_name; cmi_sign; _} = cmi in
    let resolved_items = List.map (parse_cmi_sign "") cmi_sign in
    let to_string item = match item with
                           | Single i -> format_resolved_item i
                           | Multiple i -> join_list "\n" (List.map format_resolved_item i) in
    List.map to_string resolved_items

(*
Print decoded file info on standard stream. File can be one of:
.cmi	Compiled module interface from a corresponding .mli source file.
.cmt	Typed abstract syntax tree for module implementations.
.cmti	Typed abstract syntax tree for module interfaces.
*)
let print_info fname =
    let cmio, cmto = Cmt_format.read fname in
    let entries = match cmio, cmto with
        | Some cmi, Some cmt -> print_cmt_info cmt; []
        | None, Some cmt -> print_cmt_info cmt; []
        | Some cmi, None -> parse_cmi cmi
        | None, None -> ["Can't read " ^ fname] in
    match (entries) with
        | [] -> Printf.printf "\n"
        | _ -> Printf.printf "%s" (join_list "\n" entries);
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