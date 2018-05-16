open Location
open Lexing
open Typedtree
open RwTypes

let read_type typ =
  Formatter.clean_type (Format.asprintf "%a" Printtyp.type_scheme typ)

let write ~kind ~loc ~path ~name ~typ =
    Printf.printf "DIRECT: %s\n" (kind ^ "|" ^ (Formatter.format_location loc) ^ "|" ^ path ^ "|" ^ name ^ "|" ^ typ)

(**
 Extract the name of a pattern
 *)
let rec read_pattern_desc pat_desc(*pattern_desc*) =
  match pat_desc with
    | Tpat_any -> "_Any_"
    | Tpat_var (ident, s) -> ident.name
    | Tpat_alias (pattern, ident, loc) -> ident.name
    | Tpat_constant (c) -> "_Constant_"
    | Tpat_tuple (patternl) -> Util.join_list ", " (List.map (fun p -> read_pattern_desc p.pat_desc) patternl)
    | Tpat_construct (loc, constr_desc, patternl) -> "_Constr_"
    | Tpat_variant (label, pattern, row_desc) -> "_Variant_"
    | Tpat_record (rl, flag) -> "_Record_"
    | Tpat_array (patternl) -> "_Array_"
    | Tpat_or (pattern, pattern', row_desc) -> "_Or_"
    | Tpat_lazy (pattern) -> "_Lazy_"

let rec read_expression_desc qname exp_loc exp_desc =
  match exp_desc with
    | Texp_ident (path, loc, val_desc) -> Ignore
    | Texp_constant (constant) -> Ignore
    | Texp_let (_(*flag rec/nonrec*), vbl, e) ->
        Multiple (flat_resolved_items (List.map (parse_value_binding qname) vbl))
    | Texp_function (label, cases, partial) ->
        Multiple (flat_resolved_items (List.map (read_case qname) cases))
    | Texp_apply (e, x(*(label * expression option * optional) list*)) -> Ignore
    | Texp_match (e, cases, cases', partial) -> Ignore
    | Texp_try (e, cases) -> Ignore
    | Texp_tuple (es(*expression list*)) -> Ignore
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
    | _ -> Ignore

(**
 case =
   c_lhs:   pattern;
   c_guard: expression option;
   c_rhs:   expression;
*)
and read_case qname {c_lhs; c_guard; c_rhs} =
  (*let (pat_name, pat_type) = read_pattern c_lhs in Printf.printf "n:%s t:%s\n" pat_name pat_type;*)
  read_expression_desc qname c_rhs.exp_loc c_rhs.exp_desc

(**
 expression =
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
    let {pat_loc; pat_env; pat_type; pat_desc; _} = vb_pat in
    let name = (read_pattern_desc pat_desc) in
    write ~kind:"V" ~loc:vb_pat.pat_loc ~path:qname ~name:name ~typ:(read_type pat_type);
    read_expression (Util.qname_add qname name) vb_expr

(**
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

structure_item = {
    str_desc : structure_item_desc;
    str_loc  : Location.t;
    str_env  : Env.t
}

structure = {
  str_items : structure_item list;
  str_type : Types.signature;
  str_final_env : Env.t;
}
*)
(**
module_expr = {
    mod_desc: module_expr_desc;
    mod_loc: Location.t;
    mod_type: Types.module_type;
    mod_env: Env.t;
    mod_attributes: attribute list;
}

module_expr_desc =
    Tmod_ident of Path.t * Longident.t loc
  | Tmod_structure of structure
  | Tmod_functor of Ident.t * string loc * module_type option * module_expr
  | Tmod_apply of module_expr * module_expr * module_coercion
  | Tmod_constraint of
      module_expr * Types.module_type * module_type_constraint * module_coercion
  | Tmod_unpack of expression * Types.module_type
}
*)


(** structure_item_desc =
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
let rec parse_structure_item qname {str_desc; _} =
    let parse_module_expression qname {mod_desc} =
        match mod_desc with
            | Tmod_structure {str_items; _} ->
                let items = List.map (parse_structure_item qname) str_items in
                Multiple (flat_resolved_items items)
            | _ -> Ignore in

    match str_desc with
        | Tstr_value (rec_flag, vbl) ->
            let items = List.map (parse_value_binding qname) vbl in
            Multiple (flat_resolved_items items)
        | Tstr_module {mb_id; mb_expr; mb_loc} ->
            parse_module_expression (Util.qname_add qname mb_id.name) mb_expr
        | _ -> Ignore

(**
 Extract cmt information for implementation pattern
 *)
let parse_cmt cmt =
    (*Printf.printf "modname:%s\n" info.cmt_modname;*)
    (*Printf.printf "args:%s\n" (join_array " " info.cmt_args);*)
    (*Printf.printf "sourcefile:%s\n" (default_to_none info.cmt_sourcefile);*)
    (*Printf.printf "builddir:%s\n" info.cmt_builddir;*)
    (*Printf.printf "loadpath:%s\n" (join_list "," info.cmt_loadpath);*)
    (*Printf.printf "use_summaries:%b\n" info.cmt_use_summaries;*)
    let {Cmt_format.cmt_modname; cmt_annots(*binary_annots*); _} = cmt in
    let resolved_items = match cmt_annots with
        | Implementation s -> List.map (fun item -> (parse_structure_item cmt_modname item)) s.str_items
        | _ -> [] in
    List.map Formatter.to_string resolved_items
