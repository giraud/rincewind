open Typedtree
open Types

let process_pattern_desc pat_desc =
  match pat_desc with
    | Tpat_var (ident, {Location.loc; _(*txt*)}) ->
        let {Location.loc_ghost; _} = loc in (
        match loc_ghost with
        | true -> None
        | false -> Some("Va|" ^ (Formatter.format_location loc) ^ "|" ^ ident.name ))
    | _ -> None

let extract_make_type mod_typ =
    let first l = match l with | [] -> None | hd :: _tl -> hd in

    match mod_typ with
    | Mty_signature signature ->
        let x = List.map (fun signature_item ->
            match signature_item with
            | Sig_value({name; _}, {val_type; _}) when name = "make" -> Some(Formatter.format_type val_type)
            | _ -> None)
            signature in
        let x' = List.filter (fun item -> match item with | None -> false | Some _ -> true) x in
        first x'
    | _ -> None

let rec process_expression oc {exp_loc; exp_desc; exp_env; _} =
  match exp_desc with
    | Texp_ident (path, {Asttypes.txt; loc}, {Types.val_type; _}) ->
        let {Location.loc_ghost; _} = exp_loc in
        (match loc_ghost with
        | true -> ()
        | false -> Printf.printf "Id|%s|%s|%s|%s\n" (Formatter.format_location loc) (Formatter.format_lident txt) (Formatter.format_path path) (Formatter.format_type val_type))
    | Texp_constant _c -> ()
    | Texp_let (_(*flag rec/nonrec*), vbl, e) ->
        List.iter (process_value_binding oc exp_env) vbl;
        process_expression oc e
    | Texp_function { cases; _ } ->
        List.iter (process_case oc) cases
    | Texp_apply (e, leol) ->
        let process_labels = fun (_arg_label, eo) ->
            match eo with
            | None -> ()
            | Some e -> process_expression oc e in
        process_expression oc e;
        List.iter process_labels leol
    | Texp_match (e, cl, cl', _partial) ->
        process_expression oc e;
        List.iter (process_case oc) cl;
        List.iter (process_case oc) cl';
    | Texp_try (_e, _cl) -> ()
    | Texp_tuple (_el) -> ()
    | Texp_construct (_cloc, _cd, expression_list) ->
        List.iter (process_expression oc) expression_list
    | Texp_variant (_l, _eo) -> ()
    | Texp_record  { fields ; extended_expression; _ } ->
        Array.iter (fun ({lbl_name; lbl_arg; _}, _field_expression) ->
            Printf.printf "Rf|%s|%s|%s\n" (Formatter.format_location exp_loc) lbl_name (Formatter.format_type lbl_arg);
            match extended_expression with
            | None -> ()
            | Some e -> process_expression oc e
        ) fields ;
        (match extended_expression with
           | None -> ()
           | Some e -> process_expression oc e)
    | Texp_field (expression, _longident_loc, _label_description) ->
        process_expression oc expression
    | Texp_setfield (_e, _loc, _ld, _e') -> ()
    | Texp_array (_el) -> ()
    | Texp_ifthenelse (_e, _e', _eo) -> ()
    | Texp_sequence (_e, _e') -> ()
    | Texp_while (_e, _e') -> ()
    | Texp_for (_i, _p, _e, _e', _flag, _e'') -> ()
    | Texp_send (_e, _m, _eo) -> ()
    | Texp_new (_p, _loc, _cd) -> ()
    | Texp_instvar (_p, _p', _loc) -> ()
    | Texp_setinstvar (_p, _p', _loc, _e) -> ()
    | Texp_override (_p, _el) -> ()
    | Texp_letmodule (_i, _loc, _me, _e) -> ()
    | Texp_assert e -> process_expression oc e
    | Texp_lazy e -> process_expression oc e
    | Texp_object (_cs, _sl) -> ()
    | Texp_pack _me -> ()
    | Texp_unreachable -> ()
    | Texp_letexception (_, _) -> ()
    | Texp_extension_constructor (_, _) -> ()

and process_case oc {c_rhs(*expression*); _} =
  process_expression oc c_rhs

and process_value_binding_pattern oc {pat_desc; pat_type; _ (*pat_loc; pat_env; pat_extra; pat_attributes*)} =
  let pat = process_pattern_desc pat_desc in
  match pat with
  | None -> ()
  | Some(p) -> Printf.fprintf oc "%s|%s\n" p (Formatter.format_type pat_type)

and process_value_binding oc _env {vb_pat; vb_expr; _(*vb_attributes; vb_loc*)} =
    process_value_binding_pattern oc vb_pat;
    process_expression oc vb_expr

and process_module_description oc _env mod_desc =
    match mod_desc with
    | Tmod_structure ({str_items; _(*str_final_env; str_type*)}) -> List.iter (fun item -> process_structure_item oc item) str_items
    | _ -> ()

and process_module_binding oc _env {mb_id; mb_name; mb_expr; mb_loc; _(*mb_attributes*)} =
    let { mod_desc; mod_loc; mod_type; mod_env; _(*mod_attributes;*) } = mb_expr in
    let { Asttypes.loc; _ } = mb_name in
    let { Location.loc_ghost; _ } = mod_loc in (
        match loc_ghost with
        | true ->
             (match extract_make_type mod_type with
             | Some(t) -> Printf.printf "Mg|%s|%s|%s\n" (Formatter.format_location loc) (Formatter.format_ident mb_id) t
             | None -> ())
        | false ->
            Printf.printf "Md|%s|%s\n" (Formatter.format_location mb_loc) (Formatter.format_ident mb_id)
    );
    process_module_description oc mod_env mod_desc;

and process_structure_item oc {str_desc; str_env; _(*str_loc;*)} =
    match str_desc with
    | Tstr_value (_, vbl) -> List.iter (process_value_binding oc str_env) vbl
    | Tstr_module module_binding -> process_module_binding oc str_env module_binding
    | Tstr_open {Typedtree.open_txt; _} ->
        let {Asttypes.loc; txt} = open_txt in
        Printf.fprintf oc "Op|%s|%s\n" (Formatter.format_location loc) (Longident.last txt)
    | _ -> ()

let read_cmt oc cmt =
    let { Cmt_format.cmt_annots; cmt_sourcefile; cmt_builddir; _ } = cmt in
    Printf.printf "__|%s|%s\n" (Util.Option.getWithDefault "<NONE>" cmt_sourcefile) cmt_builddir;

    match cmt_annots with
        | Implementation {str_items; _} -> List.iter (process_structure_item oc) str_items
        | _ -> ()
