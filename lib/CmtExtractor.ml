open Typedtree
open Types

let rec process_pattern_desc pat_desc =
  match pat_desc with
    | Tpat_var (ident, {Location.txt; loc}) ->
        let {Location.loc_ghost; _} = loc in (
        match loc_ghost with
        | true -> None
        | false -> Some("Va|" ^ (Formatter.format_location loc) ^ "|" ^ ident.name ))
    | _ -> None

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
  let pat = process_pattern_desc pat_desc in
  match pat with
  | None -> ()
  | Some(p) -> Printf.printf "%s|%s\n" p (Formatter.format_type pat_type)

and process_value_binding env {vb_pat; vb_expr; vb_attributes; vb_loc} =
    process_value_binding_pattern vb_pat;
    process_expression vb_expr

and process_module_description env mod_desc =
    match mod_desc with
    | Tmod_structure ({str_items; str_type; str_final_env}) -> List.iter (fun item -> process_structure_item item) str_items
    | _ -> ()

and extract_type_t mod_typ =
    let first l = match l with | [] -> None | hd :: tl -> hd in

    let rec process_signature_item si = match si with
      | Sig_module (_, {md_type; _}, _) -> List.flatten (process_module_type md_type)
      | Sig_type (id, td, _) -> [Some(Formatter.format_type_declaration id td)]
      | _ -> []

    and process_module_type mt = match mt with
      | Mty_signature signature -> List.map process_signature_item signature
      | _ -> []

    and process mt =
        let x = List.flatten (process_module_type mt) in
        let x' = List.filter (fun item -> match item with | None -> false | Some _ -> true) x in
        first x'
      in

    match mod_typ with
    | Mty_signature signature ->
        let x = List.map (fun signature_item -> match signature_item with | Sig_module(id, {md_type; _}, rs) -> Some(process md_type) | _ -> None) signature in
        let x' = List.filter (fun item -> match item with | None -> false | Some _ -> true) x in
        Util.Option.getWithDefault None (first x')
    | _ -> None

and process_module_binding env {mb_id; mb_name; mb_expr; mb_attributes; mb_loc} =
    let { mod_desc; mod_loc; mod_type; mod_env; mod_attributes; } = mb_expr in
    let { Asttypes.loc; _ } = mb_name in
    let { Location.loc_ghost; _ } = mod_loc in (
        match loc_ghost with
        | true ->
             (match extract_type_t mod_type with
             | Some(t) -> Printf.printf "Mg|%s|%s|%s\n" (Formatter.format_location loc) (Formatter.format_ident mb_id) t
             | None -> ())
        | false ->
            Printf.printf "Md|%s|%s\n" (Formatter.format_location mb_loc) (Formatter.format_ident mb_id)
    );
    process_module_description mod_env mod_desc;

and process_structure_item {str_desc; str_loc; str_env} =
    match str_desc with
    | Tstr_value (_, vbl) -> List.iter (process_value_binding str_env) vbl
    | Tstr_module module_binding -> process_module_binding str_env module_binding
    | _ -> ()

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
    Printf.printf "__|%s|%s\n" (Util.Option.getWithDefault "<NONE>" cmt_sourcefile) cmt_builddir;

    match cmt_annots with
        | Implementation {str_items} -> List.iter process_structure_item str_items
        | _ -> ()

