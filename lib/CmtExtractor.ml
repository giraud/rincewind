open Typedtree
open Types
open RwTypes

let rec process_pattern_desc pat_desc =
  match pat_desc with
    | Tpat_any -> "_Any_"
    | Tpat_var (ident, {Location.txt; loc}) -> "Va|" ^ (Formatter.format_location loc) ^ "|" ^ ident.name
    | Tpat_alias (pattern, ident, loc) -> ident.name
    | Tpat_constant (c) -> "_Constant_"
    | Tpat_tuple (patternl) -> "TUPLE"
    | Tpat_construct (loc, constr_desc, patternl) -> "_Constr_"
    | Tpat_variant (label, pattern, row_desc) -> "_Variant_"
    | Tpat_record (rl, flag) -> "_Record_"
    | Tpat_array (patternl) -> "_Array_"
    | Tpat_or (pattern, pattern', row_desc) -> "_Or_"
    | Tpat_lazy (pattern) -> "_Lazy_"

let rec process_expression {exp_loc; exp_desc; exp_type; exp_env; _} =
  match exp_desc with
    | Texp_ident (path, _, {Types.val_type; val_loc; _}) ->
        let {Location.loc_ghost; _} = exp_loc in
        (match loc_ghost with
        | true -> ()
        | false -> Printf.printf "Id|%s|%s|%s\n" (Formatter.format_location exp_loc) (Formatter.format_path path) (Formatter.format_type val_type))
    | Texp_constant c -> ()
    | Texp_let (_(*flag rec/nonrec*), vbl, e) ->
        List.iter (process_value_binding exp_env) vbl;
        process_expression e
    | Texp_function (_(*label*), cases, _(*partial*)) ->
        List.iter process_case cases
    | Texp_apply (e, leol) ->
        let process_labels = fun (label, eo, o) ->
            match eo with
            | None -> ()
            | Some e -> process_expression e in
        process_expression e;
        List.iter process_labels leol
    | Texp_match (e, cl, cl', _partial) ->
        process_expression e;
        List.iter process_case cl;
        List.iter process_case cl';
    | Texp_try (e, cl) -> ()
    | Texp_tuple (el) -> ()
    | Texp_construct (cloc, cd, expression_list) ->
        List.iter process_expression expression_list
    | Texp_variant (l, eo) -> ()
    | Texp_record  (fields, expression_option) ->
        List.iter (fun (lil, {lbl_name; lbl_arg; _}, field_expression) ->
            Printf.printf "Rf|%s|%s|%s\n" (Formatter.format_location exp_loc) lbl_name (Formatter.format_type lbl_arg);
            process_expression field_expression
        ) fields ;
        (match expression_option with
           | None -> ()
           | Some e -> process_expression e)
    | Texp_field (expression, longident_loc, label_description) ->
        process_expression expression
    | Texp_setfield (e, loc, ld, e') -> ()
    | Texp_array (el) -> ()
    | Texp_ifthenelse (e, e', eo) -> ()
    | Texp_sequence (e, e') -> ()
    | Texp_while (e, e') -> ()
    | Texp_for (i, p, e, e', flag, e'') -> ()
    | Texp_send (e, m, eo) -> ()
    | Texp_new (p, loc, cd) -> ()
    | Texp_instvar (p, p', loc) -> ()
    | Texp_setinstvar (p, p', loc, e) -> ()
    | Texp_override (p, el) -> ()
    | Texp_letmodule (i, loc, me, e) -> ()
    | Texp_assert e -> process_expression e
    | Texp_lazy e -> process_expression e
    | Texp_object (cs, sl) -> ()
    | Texp_pack me -> ()

(**
 Extract information from the right handler of a case
 *)
and process_case {c_rhs(*expression*); _} =
  process_expression c_rhs

and process_value_binding_pattern {pat_desc; pat_loc; pat_extra; pat_type; pat_env; pat_attributes} =
  Printf.printf "%s|%s\n" (process_pattern_desc pat_desc) (Formatter.clean_type (RwTypes.read_type pat_type))

and process_value_binding env {vb_pat; vb_expr; vb_attributes; vb_loc} =
    process_value_binding_pattern vb_pat;
    process_expression vb_expr

and process_module_description env mod_desc =
    match mod_desc with
    | Tmod_ident (pa(*Path.t*), il(*Longident.t loc*)) -> Printf.printf "Tmod_ident"
    | Tmod_structure ({str_items; str_type; str_final_env}(*structure*)) ->
        List.iter (fun item -> process_structure_item item) str_items
    | Tmod_functor (i(*Ident.t*), sl(*string loc*), mto(*module_type option*),  me(*module_expr*)) -> Printf.printf "%s" "Tmod_functor"
    | Tmod_apply (me(*module_expr*), me'(*module_expr*), mc(*module_coercion*)) -> Printf.printf "%s" "Tmod_apply"
    | Tmod_constraint (me(*module_expr*), mt(*Types.module_type*), mtc(*module_type_constraint*), mc(*module_coercion*)) -> Printf.printf "%s" "Tmod_constraint"
    | Tmod_unpack (e(*expression*), mt(*Types.module_type*)) ->
        Printf.printf "Tmod_unpack"

and process_module_binding env {mb_id; mb_name; mb_expr; mb_attributes; mb_loc} =
    let { mod_desc; mod_loc; mod_type; mod_env; mod_attributes; } = mb_expr in
    Printf.printf "Md|%s|%s\n" (Formatter.format_location mb_loc) (Formatter.format_ident mb_id);
    process_module_description mod_env mod_desc;

(**
 Iterate on parsedtree
 *)
and process_structure_item {str_desc; str_loc; str_env} =
    match str_desc with
    | Tstr_eval (e(*expression*), a(*attributes*)) -> ()
    | Tstr_value (rf(*rec_flag*), vbl(*value_binding list*)) ->
        List.iter (process_value_binding str_env) vbl
    | Tstr_primitive (vd(*value_description*)) -> ()
    | Tstr_type (tdl(*type_declaration list*)) -> ()
    | Tstr_typext (te(*type_extension*)) -> ()
    | Tstr_exception (ec(*extension_constructor*)) -> ()
    | Tstr_module (mb(*module_binding*)) -> process_module_binding str_env mb
    | Tstr_recmodule (mbl(*module_binding list*)) -> ()
    | Tstr_modtype (mtd(*module_type_declaration*)) -> ()
    | Tstr_open (od(*open_description*)) -> ()
    | Tstr_class (cl(*(cd(*class_declaration*), sl(*string list*), vf(*virtual_flag*)) list*)) -> ()
    | Tstr_class_type (ctl(*(i(*Ident.t*), sl(*string loc*), ctd(*class_type_declaration*)) list*)) -> ()
    | Tstr_include (id(*include_declaration*)) -> ()
    | Tstr_attribute (a(*attribute*)) -> ()

(**
 Extract cmt information for implementation pattern
 *)
let read_cmt cmt =
    let {
      Cmt_format.cmt_modname (* string *);
      cmt_annots             (* binary_annots *);
      cmt_value_dependencies (* (Types.value_description * Types.value_description) list *);
      cmt_comments           (* (string * Location.t) list *);
      cmt_args               (* string array *);
      cmt_sourcefile         (* string option *);
      cmt_builddir           (* string *);
      cmt_loadpath           (* string list *);
      cmt_source_digest      (* Digest.t option *);
      cmt_initial_env        (* Env.t *);
      cmt_imports            (* (string * Digest.t option) list *);
      cmt_interface_digest   (* Digest.t option *);
      cmt_use_summaries      (* bool *);
    } = cmt in
    Printf.printf "%s|%s\n" (Util.Option.getWithDefault "<NONE>" cmt_sourcefile) cmt_builddir;

    match cmt_annots with
        | Packed (si(*Types.signature *), sl(*string list*)) -> Printf.printf "--|Packed"
        | Implementation {str_items}(*structure*) -> List.iter process_structure_item str_items
        | Interface s(*signature*) -> Printf.printf "--|Interface"
        | Partial_implementation bpa(*binary_part array*) -> Printf.printf "--|Partial_implementation"
        | Partial_interface bpa(*binary_part array*) -> Printf.printf "--|Partial_interface"

