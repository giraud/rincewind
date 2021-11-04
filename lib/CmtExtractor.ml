(*
 Md  Module
 Mg  Module ghost
 Op  Open
 Id  Identifier
 Va  Value
 Pa  Parameter
 Rf  Record field
 *)

open Typedtree
open Types

let process_pattern_desc pat_desc =
  match pat_desc with
    | Tpat_var (ident, {Location.loc; _(*txt*)}) ->
        let {Location.loc_ghost; _} = loc in (
        match loc_ghost with
        | true -> None
#if OCAML_MINOR = 6
        | false -> Some("Va|" ^ (Formatter.format_location loc) ^ "|" ^ ident.name ))
#elif OCAML_MINOR >= 7
        | false -> Some("Va|" ^ (Formatter.format_location loc) ^ "|" ^ (Ident.name ident) ))
#endif
    | _ -> None

let extract_make_type mod_typ =
    let first l = match l with | [] -> None | hd :: _tl -> hd in

    match mod_typ with
    | Mty_signature signature ->
        let x = List.map (fun signature_item ->
            match signature_item with
#if OCAML_MINOR = 6
            | Sig_value(ident, {val_type; _}) when (Ident.name ident) = "make" -> Some(Formatter.format_type val_type)
#elif OCAML_MINOR = 7
            | Sig_value(ident, {val_type; _}) when (Ident.name ident) = "make" -> Some(Formatter.format_type val_type)
#else
            | Sig_value(ident, {val_type; _}, _vis) when (Ident.name ident) = "make" -> Some(Formatter.format_type val_type)
#endif
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
        | false -> Printf.fprintf oc "Id|%s|%s|%s|%s\n" (Formatter.format_location loc) (Formatter.format_lident txt) (Formatter.format_path path) (Formatter.format_type val_type))
    | Texp_constant _c -> ()
    | Texp_let (_(*flag rec/nonrec*), value_binding_list, expression) ->
        List.iter (process_value_binding oc exp_env) value_binding_list;
        process_expression oc expression
    | Texp_function { cases; _ } ->
        List.iter (process_case oc) cases
    | Texp_apply (expression, leol) ->
        process_expression oc expression;
        List.iter (fun (arg_label, eo) ->
              match eo with
              | Some e ->
                  (match (arg_label) with
                  | Asttypes.Nolabel -> ()
                  | _ -> Printf.fprintf oc "Pa|%s|%s|%s\n" (Formatter.format_location e.exp_loc) (Formatter.format_arg arg_label) (Formatter.format_type e.exp_type)
                  );
                  process_expression oc e
              | None -> ()
        ) leol
   #if OCAML_MINOR >= 13
    | Texp_match (e, _cl, _partial) ->
        process_expression oc e;
        (* zzz List.iter (process_case oc) cl; *)
   #elif OCAML_MINOR >= 8
    | Texp_match (e, cl, _partial) ->
        process_expression oc e;
        List.iter (process_case oc) cl;
   #else
    | Texp_match (e, cl, cl', _partial) ->
        process_expression oc e;
        List.iter (process_case oc) cl;
        List.iter (process_case oc) cl';
   #endif
    | Texp_try (_e, _cl) -> ()
    | Texp_tuple (_el) -> ()
    | Texp_construct (_cloc, _cd, expression_list) ->
        List.iter (process_expression oc) expression_list
    | Texp_variant (_l, _eo) -> ()
    | Texp_record  { fields ; extended_expression; _ } ->
        Array.iter (fun ({lbl_name; lbl_arg; _}, rld) ->
            let {Location.loc_ghost; _} = exp_loc in
                (match loc_ghost with
                | true -> ()
                | false -> Printf.fprintf oc "Rf|%s|%s|%s\n" (Formatter.format_location exp_loc) lbl_name (Formatter.format_type lbl_arg));
            match rld with
            | Typedtree.Kept _e -> ()
            | Overridden (_loc, e) -> process_expression oc e
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
#if OCAML_MINOR >= 8
    | Texp_letmodule (_i, _loc, _tmp, _me, _e) -> ()
#else
    | Texp_letmodule (_i, _loc, _me, _e) -> ()
#endif
    | Texp_assert e -> process_expression oc e
    | Texp_lazy e -> process_expression oc e
    | Texp_object (_cs, _sl) -> ()
    | Texp_pack _me -> ()
    | Texp_unreachable -> ()
    | Texp_letexception (_, _) -> ()
    | Texp_extension_constructor (_, _) -> ()
#if OCAML_MINOR >= 8
    | Texp_letop _l -> ()
    | Texp_open (_od, _e) -> ()
#endif


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

and process_module_description oc env mod_desc =
    match mod_desc(*Typedtree*) with
    | Tmod_structure ({str_items; _(*str_final_env; str_type*)}) -> List.iter (fun item -> process_structure_item oc item) str_items
   #if OCAML_MINOR >= 10
    | Tmod_functor (_fp(*functor_parameter*), { Typedtree.mod_desc; _ }(*module_expr*)) ->
        process_module_description oc env mod_desc
   #else
    | Tmod_functor (_id, _loc, _module_type, { Typedtree.mod_desc; _ }(*module_expr*)) ->
        process_module_description oc env mod_desc
   #endif
    | Tmod_apply (module_expr, module_expr', _module_coercion) ->
        process_module_description oc env module_expr.Typedtree.mod_desc;
        process_module_description oc env module_expr'.Typedtree.mod_desc
    | Tmod_constraint ({ Typedtree.mod_desc; _ }(*module_expr*), (*Types.*)_module_type, _module_type_constraint, _module_coercion) ->
        process_module_description oc env mod_desc
    | _ -> ()

and process_module_binding oc _env {mb_id; mb_name; mb_expr; mb_loc; _(*mb_attributes*)} =
    let { mod_desc; mod_loc; mod_type; mod_env; _(*mod_attributes;*) } = mb_expr in
    let { Asttypes.loc; _ } = mb_name in
    let { Location.loc_ghost; _ } = mod_loc in (
        match loc_ghost with
        | true ->
             (match extract_make_type mod_type with
            #if OCAML_MINOR >= 10
             | Some(t) -> Printf.fprintf oc "Mg|%s|%s|%s\n" (Formatter.format_location loc) (Formatter.format_ident_o mb_id) t
            #else
             | Some(t) -> Printf.fprintf oc "Mg|%s|%s|%s\n" (Formatter.format_location loc) (Formatter.format_ident mb_id) t
            #endif
             | None -> ())
        | false ->
           #if OCAML_MINOR >= 10
            Printf.fprintf oc "Md|%s|%s\n" (Formatter.format_location mb_loc) (Formatter.format_ident_o mb_id)
           #else
            Printf.fprintf oc "Md|%s|%s\n" (Formatter.format_location mb_loc) (Formatter.format_ident mb_id)
           #endif
    );
    process_module_description oc mod_env mod_desc;

and process_structure_item oc {str_desc; str_env; _(*str_loc;*)} =
    match str_desc with
    | Tstr_value (_, vbl) -> List.iter (process_value_binding oc str_env) vbl
    | Tstr_module module_binding -> process_module_binding oc str_env module_binding
#if OCAML_MINOR < 8
    | Tstr_open {Typedtree.open_txt; _} ->
        let {Asttypes.loc; txt} = open_txt in
        Printf.fprintf oc "Op|%s|%s\n" (Formatter.format_location loc) (Longident.last txt)
#endif
    | _ -> ()

let read_cmt oc cmt =
    let { Cmt_format.cmt_annots; cmt_sourcefile; cmt_builddir; _ } = cmt in
    Printf.fprintf oc "__|%s|%s\n" (Util.Option.getWithDefault "<NONE>" cmt_sourcefile) cmt_builddir;




    match cmt_annots with
        | Implementation {str_items; _} -> List.iter (process_structure_item oc) str_items
        | _ -> ()
