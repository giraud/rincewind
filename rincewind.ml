open Typedtree
open Cmt_format
open Location
open Lexing
open RwTypes
open Util
open CmiExtractor

let default_to defaultValue value =
  match value with
    | Some v -> v
    | None -> defaultValue

let default_to_none = default_to "none"
let default_to_empty = default_to ""

let deoptionalize (lst:'a option list) : 'a list =
    List.map (fun x -> match x with Some x -> x | None -> assert false) (List.filter (fun x -> x <> None) lst)

let join_array separator items =
  Array.fold_left (fun acc item -> acc ^ item ^ separator) "" items

let read_type typ =
  Formatter.clean_type (Format.asprintf "%a" Printtyp.type_scheme typ)

(*
type pattern_desc =
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
    | Tpat_var (ident, s) -> (ident.name, (read_type pat_type))
    | Tpat_constant (c) -> ("CC", "CCT")
    | _ -> ("VV", "TT")

let rec read_expression_desc qname exp_loc exp_desc =
  match exp_desc with
    | Texp_ident (path, loc, val_desc) ->
        Single  {i_kind="_Ident_"; i_loc=exp_loc; i_path=qname; i_name=""; i_type=(read_type val_desc.val_type)}
    | Texp_constant (constant) -> Ignore
    | Texp_let (_(*flag rec/nonrec*), vbl, e) ->
        Multiple (flat_resolved_items (List.map (parse_value_binding qname) vbl))
        (*let re = read_expression (qname ^ "EE") e in*)
        (*let ve = List.map (parse_value_binding qname) vbl in*)
        (*Multiple (List.append [re] (flat_resolved_items ve))*)
        (*Single {i_kind="_L_"; i_loc=exp_loc; i_path=qname; i_name=""; i_type=""}*)
    | Texp_function (label, cases, partial) ->
        (*Single {i_kind="_F_"; i_loc=exp_loc; i_path=qname; i_name=label; i_type=""}*)
        Multiple (flat_resolved_items (List.map (read_case qname) cases))
    | Texp_apply (e, x(*(label * expression option * optional) list*)) ->
        Single {i_kind="_Apply_"; i_loc=exp_loc; i_path=qname; i_name=""; i_type=""}
    | Texp_match (e, cases, cases', partial) ->
        Single {i_kind="_Match_"; i_loc=exp_loc; i_path=qname; i_name=""; i_type=""}
    | Texp_try (e, cases) ->
        Single {i_kind="_Try_"; i_loc=exp_loc; i_path=qname; i_name=""; i_type=""}
    | Texp_tuple (es(*expression list*)) ->
        Single {i_kind="_Tuple_"; i_loc=exp_loc; i_path=qname; i_name=""; i_type=""}
    (*| Texp_construct of*)
        (*Longident.t loc * constructor_description * expression list*)
    (*| Texp_variant of label * expression option*)
    (*| Texp_record of*)
        (*(Longident.t loc * label_description * expression) list **)
          (*expression option*)
    (*| Texp_field of expression * Longident.t loc * label_description*)
    (*| Texp_setfield of*)
        (*expression * Longident.t loc * label_description * expression*)
    (*| Texp_array of expression list*)
    (*| Texp_ifthenelse of expression * expression * expression option*)
    (*| Texp_sequence of expression * expression*)
    (*| Texp_while of expression * expression*)
    (*| Texp_for of*)
        (*Ident.t * Parsetree.pattern * expression * expression * direction_flag **)
          (*expression*)
    (*| Texp_send of expression * meth * expression option*)
    (*| Texp_new of Path.t * Longident.t loc * Types.class_declaration*)
    (*| Texp_instvar of Path.t * Path.t * string loc*)
    (*| Texp_setinstvar of Path.t * Path.t * string loc * expression*)
    (*| Texp_override of Path.t * (Path.t * string loc * expression) list*)
    (*| Texp_letmodule of Ident.t * string loc * module_expr * expression*)
    (*| Texp_assert of expression*)
    (*| Texp_lazy of expression*)
    (*| Texp_object of class_structure * string list*)
    (*| Texp_pack of module_expr*)
    (*| Texp_match (e, cases, cases', partial) -> read_expression qname e*)
    | _ -> Single {i_kind="NYI"; i_loc=exp_loc; i_path=qname; i_name=""; i_type=""}

(* case = {
  c_lhs: pattern;
  c_guard: expression option;
  c_rhs: expression;
} *)
and read_case qname {c_lhs; c_guard; c_rhs} =
  (*let (pat_name, pat_type) = read_pattern c_lhs in*)
  (*Printf.printf "n:%s t:%s\n" pat_name pat_type;*)
  read_expression_desc qname c_rhs.exp_loc c_rhs.exp_desc

(* expression =
      exp_desc: expression_desc;
      exp_loc: Location.t;
      exp_extra: (exp_extra * Location.t * attribute list) list;
      exp_type: type_expr;
      exp_env: Env.t;
      exp_attributes: attribute list;
*)
and read_expression qname {exp_loc; exp_desc; _} =
  read_expression_desc qname exp_loc exp_desc

and parse_value_binding qname {vb_pat; vb_expr; vb_attributes; vb_loc} =
    let (pat_name, pat_type) = read_pattern vb_pat in
    let rootExpression = {i_kind="V"; i_loc=vb_loc; i_path=qname; i_name=pat_name; i_type=pat_type} in
    let expressions = read_expression (qname_add qname pat_name) vb_expr in
    match expressions with
        | Ignore -> Single rootExpression
        | Single e -> Multiple [rootExpression; e]
        | Multiple e -> Multiple (List.append [rootExpression] e)

let read_module_binding {mb_expr; _} =
  match mb_expr.mod_desc with
    | Tmod_structure s -> Some s
    | _ -> None

let parse_structure_item qname {str_desc; str_loc; str_env } =
    match str_desc with
        | Tstr_value (rec_flag, vbl) ->
            let items = List.map (parse_value_binding qname) vbl in
            Multiple (flat_resolved_items items)
        | _ -> Multiple []

(* binary_annots =
   | Packed of Types.signature * string list
   | Implementation of structure
   | Interface of signature
   | Partial_implementation of binary_part array
   | Partial_interface of binary_part array
*)
let parse_cmt cmt =
    (*Printf.printf "modname:%s\n" info.cmt_modname;*)
    (*Printf.printf "args:%s\n" (join_array " " info.cmt_args);*)
    (*Printf.printf "sourcefile:%s\n" (default_to_none info.cmt_sourcefile);*)
    (*Printf.printf "builddir:%s\n" info.cmt_builddir;*)
    (*Printf.printf "loadpath:%s\n" (join_list "," info.cmt_loadpath);*)
    (*Printf.printf "use_summaries:%b\n" info.cmt_use_summaries;*)
    let {Cmt_format.cmt_modname; cmt_annots; _} = cmt in
    let resolved_items = match cmt_annots with
        | Implementation t -> List.map (fun item -> (parse_structure_item cmt_modname item)) t.str_items
        | _ -> [] in
    List.map Formatter.to_string resolved_items

(*
Print decoded file info on standard stream. File can be one of:
.cmi	Compiled module interface from a corresponding .mli source file.
.cmt	Typed abstract syntax tree for module implementations.
.cmti	Typed abstract syntax tree for module interfaces.
*)
let print_info fname =
    let cmio, cmto = Cmt_format.read fname in
    let entries = match cmio, cmto with
        | Some cmi, Some cmt -> parse_cmt cmt
        | None, Some cmt -> parse_cmt cmt
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