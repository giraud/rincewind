(* deprecated *)
let stag indent name attrs children = Xml.ctag indent name attrs None children

module Dump = struct
    let constant =
    #if OCAML_MINOR >= 7
        Printpat.pretty_const
    #else
        Parmatch.pretty_const
    #endif

    let partial (p: Typedtree.partial) =
        match p with | Partial -> "Partial" | Total -> "Total"

    let dump_type = Formatter.format_type

    let rec_flag (f: Asttypes.rec_flag) =
        match f with | Nonrecursive -> "Nonrecursive" | Recursive -> "Recursive"

    let arg_name (a: Asttypes.arg_label) =
      match a with
      | Nolabel -> ""
      | Labelled(name) -> name
      | Optional(name) -> name

    let arg_opt (a: Asttypes.arg_label) =
      match a with
      | Optional(_) -> "true"
      | _ -> "false"
end

let dump_ident (ident: Ident.t) =
#if OCAML_MINOR >= 7
    Ident.unique_toplevel_name ident
#else
    let Ident.{ stamp; name; _(*flags*) } = ident in
    name ^ "/" ^ (string_of_int stamp) (* like unique_toplevel_name *)
#endif
let dump_ident_o (ident) =
  match (ident) with
  | None -> ""
  | Some(i) -> dump_ident i

let rec dump_path (p: Path.t) =
    match (p) with
    | Pident(ident) -> dump_ident ident
#if OCAML_MINOR >= 8
    | Pdot(p, name) -> (dump_path p) ^ "." ^ name
#else
    | Pdot(p, name, _flag) -> (dump_path p) ^ "." ^ name
#endif
    | Papply(p1, p2) -> (dump_path p1) ^ "-?-" ^ (dump_path p2)

let dump_pos { Lexing.pos_lnum; pos_bol; pos_cnum; _(*pos_fname*)} =
  (string_of_int pos_lnum) ^ "." ^ (string_of_int (pos_cnum - pos_bol + 1))

(* location.ml / Location.t { loc_start: position; loc_end: position; loc_ghost: bool } *)
let dump_loc { Location.loc_start; loc_end; loc_ghost } =
  (dump_pos loc_start) ^ ":" ^ (dump_pos loc_end) ^ (match loc_ghost with | false -> "" | true -> ":ghost")

let dump_longident_loc {Asttypes.txt(*LongIdent.t*); loc(*Location.t*)} =
    (Format.asprintf "%a" Printtyp.longident txt) ^ "|" ^ (dump_loc loc)

let dump_ast_loc {Asttypes.txt; loc} =
    txt ^ "|" ^ (dump_loc loc)

let dump_string_loc_o Location.{txt; loc} = (* Location.loc *)
    (Util.Option.getWithDefault "" txt) ^ "|" ^ (dump_loc loc)

let dump_string_loc Location.{txt; loc} = (* Location.loc *)
    txt ^ "|" ^ (dump_loc loc)

let dump_value_description id vd(*{ val_type; val_kind; val_loc; val_attributes }*) =
  Formatter.escape_string (Formatter.normalize_string (Format.asprintf "%a" (Printtyp.value_description id) vd))

let dump_rec_status v = match v with | Types.Trec_not -> "Trec_not" | Trec_first -> "Trec_first" | Trec_next -> "Trec_next"

let dump_value_kind (kind: Types.value_kind) =
    match kind with
    | Val_reg -> "Regular value"
    | Val_prim _(*Primitive.description*) -> "Primitive"
    | Val_ivar _(*mutable_flag * string*) -> "Instance variable (mutable ?)"
    | Val_self _(*(Ident.t * type_expr) Meths.t ref * (Ident.t * mutable_flag * virtual_flag * type_expr) Vars.t ref * string * type_expr*) -> "Self"
    | Val_anc _(*(string * Ident.t) list * string*) -> "Ancestor"
   #if OCAML_MINOR >= 10
   #elif OCAML_MINOR >= 8
    | Val_unbound _  -> "Unbound variable"
   #else
    | Val_unbound    -> "Unbound variable"
   #endif

let print_type_scheme env typ =
  Printtyp.wrap_printing_env env (fun () -> Format.asprintf "%a" Printtyp.type_scheme typ)

let rec process_module_type tab mt =
  match mt with
  | Types.Mty_ident path -> Xml.atag tab "Mty_ident" (dump_path path)
  | Mty_signature  signature ->
        stag tab "Mty_signature" [] (fun tab -> List.iter (process_signature_item tab) signature)
 #if OCAML_MINOR >= 10
  | Mty_functor (_functor_parameter, module_type) ->
        Xml.ctag tab "Mty_functor" [] (Some(["functor_parameter"])) (fun tab ->
            Xml.ctag tab "module_type" [] None (fun tab -> process_module_type tab module_type)
        )
 #else
  | Mty_functor (id(*Ident.t*), _mod_type_o (*module_type option*), mod_type (*module_type*)) ->
        Xml.ctag tab "Mty_functor" [("ident", dump_ident id)] (Some(["mod_type_option"])) (fun tab ->
            Xml.ctag tab "module_type" [] None (fun tab -> process_module_type tab mod_type)
        )
 #endif
  | Mty_alias _(*alias_presence * Path.t*) -> Xml.mtag tab "Mty_alias"

and process_value_description tab vd(*types.value_description*) =
 #if OCAML_MINOR >= 12
  let Types.{ val_type; val_kind; val_loc; val_attributes; _ } = vd in
  stag tab "value_description" [("val_type", Dump.dump_type val_type); ("val_kind",  dump_value_kind val_kind); ("val_loc", dump_loc val_loc);] (fun tab ->
      List.iter (process_attribute tab) val_attributes
  )
 #else
  let Types.{ val_type; val_kind; val_loc; val_attributes } = vd in
  stag tab "value_description" [("val_type", Dump.dump_type val_type); ("val_kind",  dump_value_kind val_kind); ("val_loc", dump_loc val_loc);] (fun tab ->
      List.iter (process_attribute tab) val_attributes
  )
 #endif

and process_modtype_declaration tab { Types.mtd_type; _(*mtd_attributes; mtd_loc;*) } =
    match mtd_type with | None -> Xml.tag tab "mtd_type" [("abstract", "true")] None | Some t -> stag tab "mtd_type" [("abstract", "false")] (fun tab -> process_module_type tab t)

and dump_module_declaration tab {Types.md_type; _(*md_attributes; md_loc*)} =
    process_module_type tab md_type

and process_signature_item tab si =
  match si with
#if OCAML_MINOR >= 8
  | Sig_value (id, vd(*value_description*), _visibility) ->
        stag tab "Sig_value" [("id", dump_ident id);] (fun tab -> process_value_description tab vd)
  | Sig_type (id, type_declaration, rec_status, _visibility) ->
        Xml.tag tab "Sig_type" [("id", dump_ident id); ("rec_status", dump_rec_status rec_status); ("type_declaration", Formatter.format_type_declaration id type_declaration)] None
  | Sig_typext (_id, _ec, _es, _visibility) -> Xml.mtag tab "Sig_typext"
  | Sig_module (_id, _mp, md, _rs, _visibility) -> stag tab "Sig_module" [] (fun tab -> dump_module_declaration tab md)
  | Sig_modtype (id, modtype_declaration, _visibility) ->
        stag tab "Sig_modtype" [("id", dump_ident id)] (fun tab -> process_modtype_declaration tab modtype_declaration)
  | Sig_class (_id, _cd, _rs, _visibility) -> Xml.mtag tab "Sig_class"
  | Sig_class_type (_id, _ctd, _rs, _visibility) -> Xml.mtag tab "Sig_class_type"
#else
  | Sig_value (id, vd(*value_description*)) ->
        stag tab "Sig_value" [("id", dump_ident id);] (fun tab -> process_value_description tab vd)
  | Sig_type (id, type_declaration, rec_status) ->
        Xml.tag tab "Sig_type" [("id", dump_ident id); ("rec_status", dump_rec_status rec_status); ("type_declaration", Formatter.format_type_declaration id type_declaration)] None
  | Sig_typext (_id, _ec, _es) -> Xml.mtag tab "Sig_typext"
  | Sig_module (_id, md, _rs) -> stag tab "Sig_module" [] (fun tab -> dump_module_declaration tab md)
  | Sig_modtype (id, modtype_declaration) ->
        stag tab "Sig_modtype" [("id", dump_ident id)] (fun tab -> process_modtype_declaration tab modtype_declaration)
  | Sig_class (_id, _cd, _rs) -> Xml.mtag tab "Sig_class"
  | Sig_class_type (_id, _ctd, _rs) -> Xml.mtag tab "Sig_class_type"
#endif

and dump_summary s = match s with
  | Env.Env_empty -> ""
  | Env_value (su, id, vd(*value_description*)) -> "<value:" ^ (dump_value_description id vd)^ "> " ^ (dump_summary su)
  | Env_type (su, id, td) -> "<" ^ (Formatter.format_type_declaration id td) ^ "> " ^ (dump_summary su)
  | Env_extension (su, id, _ec) -> "<extension:" ^ (dump_ident id) ^ "> " ^ (dump_summary su)
#if OCAML_MINOR >= 8
  | Env_module (_su, _id, _mp, _md(*module_declaration*)) -> "" (*"<module:" ^ (dump_ident id) ^ ":" ^ (dump_module_declaration md) ^ "> " ^ (dump_summary su)*)
#else
  | Env_module (_su, _id, _md(*module_declaration*)) -> "" (*"<module:" ^ (dump_ident id) ^ ":" ^ (dump_module_declaration md) ^ "> " ^ (dump_summary su)*)
#endif
  | Env_modtype _ (* summary * Ident.t * modtype_declaration *) -> "Env_modtype"
  | Env_class _ (* summary * Ident.t * class_declaration *) -> "class"
  | Env_cltype _ (* summary * Ident.t * class_type_declaration *) -> "cltype"
#if OCAML_MINOR >= 8
  | Env_open (su(*summary*), pa(*Path.t*)) -> "<open:" ^ (dump_path pa) ^ "> " ^ (dump_summary su)
#elif OCAML_MINOR >= 7
  | Env_open (su(*summary*), _mo, pa(*Path.t*)) -> "<open:" ^ (dump_path pa) ^ "> " ^ (dump_summary su)
#else
  | Env_open (su(*summary*), pa(*Path.t*)) -> "<open:" ^ (dump_path pa) ^ "> " ^ (dump_summary su)
#endif
  | Env_functor_arg _ (* summary * Ident.t *) -> "functor_arg"
  | Env_constraints (_, _) -> "constraints"
 #if OCAML_MINOR >= 10
  | Env_copy_types _(*summary*) -> "copy_types"
 #else
  | Env_copy_types (_, _) -> "copy_types"
 #endif
 #if OCAML_MINOR >= 8
  | Env_persistent (_su, _id) -> "Env_persistent"
 #endif
 #if OCAML_MINOR >= 10
  | Env_value_unbound (_(*summary*), _(*string*), _(*value_unbound_reason*)) -> "Env_value_unbound"
  | Env_module_unbound (_(*summary*), _(*string*), _(*module_unbound_reason*)) -> "Env_module_unbound"
 #endif

and dump_env env = dump_summary (Env.summary env)

and process_pattern_desc tab pat_desc =
  match pat_desc with
    | Typedtree.Tpat_any -> Xml.mtag tab "Tpat_any"
    | Tpat_var (i, sl) ->
        Xml.tag tab "Tpat_var" [("ident", dump_ident i); ("string_loc", dump_string_loc sl)] None
    | Tpat_alias (_pattern, _ident, _loc) -> Xml.mtag tab "Tpat_alias"
    | Tpat_constant (_c) ->  Xml.mtag tab "Tpat_constant"
    | Tpat_tuple (_patternl) -> Xml.mtag tab "Tpat_tuple"
   #if OCAML_MINOR >= 12
    | Tpat_construct (_loc, _constr_desc, _patternl, _) -> Xml.mtag tab "Tpat_construct"
   #else
    | Tpat_construct (_loc, _constr_desc, _patternl) -> Xml.mtag tab "Tpat_construct"
   #endif
    | Tpat_variant (_label, _pattern, _row_desc) -> Xml.mtag tab "Tpat_variant"
    | Tpat_record (_rl, _flag) -> Xml.mtag tab "Tpat_record"
    | Tpat_array (_patternl) -> Xml.mtag tab "Tpat_array"
    | Tpat_or (_pattern, _pattern', _row_desc) -> Xml.mtag tab "Tpat_or"
    | Tpat_lazy (_pattern) -> Xml.mtag tab "Tpat_lazy"
   #if OCAML_MINOR >= 13
   #elif OCAML_MINOR >= 8
    | Tpat_exception _ -> Xml.mtag tab "Tpat_exception"
   #endif

#if OCAML_MINOR >= 13
and process_case tab (c: 'a Typedtree.case) =
    let Typedtree.{c_lhs: Typedtree.pattern; c_rhs; c_guard; _} = c in
    let Typedtree.{pat_desc; pat_loc; pat_type; _ (* pat_extra; pat_env; pat_attributes *)} = c_lhs in
    Xml.ctag tab "c_lhs" [("pat_loc", dump_loc pat_loc); ("pat_type", Dump.dump_type pat_type)] (Some(["pat_env"; "pat_attributes"; "pat_extra"])) (fun tab ->
        process_pattern_desc tab pat_desc
    );
    (match (c_guard) with | None -> () | Some(e) -> process_expression tab e);
    Xml.ctag tab "c_rhs" [] None (fun tab ->
        process_expression tab c_rhs
    )
#else
and process_case tab (c: Typedtree.case) =
    let Typedtree.{c_lhs: Typedtree.pattern; c_rhs; c_guard} = c in
    let Typedtree.{pat_desc; pat_loc; pat_type; _ (* pat_extra; pat_env; pat_attributes *)} = c_lhs in
    Xml.ctag tab "c_lhs" [("pat_loc", dump_loc pat_loc); ("pat_type", Dump.dump_type pat_type)] (Some(["pat_env"; "pat_attributes"; "pat_extra"])) (fun tab ->
        process_pattern_desc tab pat_desc
    );
    (match (c_guard) with | None -> () | Some(e) -> process_expression tab e);
    Xml.ctag tab "c_rhs" [] None (fun tab ->
        process_expression tab c_rhs
    )
#endif

#if OCAML_MINOR >= 8
and process_attribute tab Parsetree.{attr_payload; attr_loc; _} =
    let loc_str = dump_pos attr_loc.loc_start in
#else
and process_attribute tab (attr_loc, attr_payload) =
    let loc_str = dump_string_loc attr_loc in
#endif
    stag tab "attribute" [("string_loc", loc_str)] (fun tab ->
        match attr_payload with
        | Parsetree.PStr _structure -> Xml.mtag tab "PStr"
        | PTyp _core_type  (* : T *) -> Xml.mtag tab "PTyp"
        | PPat (_pattern, _expression_option) (* ? P  or  ? P when E *) -> Xml.mtag tab "PPat"
        | PSig _ -> Xml.mtag tab "PSig"
    )

and process_label_description tab Types.{ (*label_description*)
                                     lbl_name  (* Short name *);
                                     lbl_arg;  (* Type of the argument *)
                                     lbl_loc;
                                     lbl_pos;
                                     _ } =
    Xml.tag tab "label_description" [("lbl_name", lbl_name); ("lbl_arg", Dump.dump_type lbl_arg); ("lbl_loc", dump_loc lbl_loc); ("lbl_pos", string_of_int lbl_pos)] (Some(["lbl_res"; "lbl_mut"; "lbl_all"; "lbl_repres"; "lbl_private"]))

and process_expression tab { exp_desc; exp_loc; exp_env; exp_type; _(*exp_attributes; exp_extra*) } =
    Xml.ctag tab "expression" [("exp_loc", dump_loc exp_loc); ("exp_type", Dump.dump_type exp_type)] (Some(["exp_attributes"; "exp_extra"])) (fun tab ->
        (match exp_desc with
        | Texp_ident (p(*Path.t*), lil(*Longident.t loc*), vd(*Types.value_description*)) ->
            stag tab "Texp_ident" [("path", dump_path p); ("longident_loc", dump_longident_loc lil)] (fun tab ->
                process_value_description tab vd
            )
        | Texp_constant constant ->
             Xml.atag tab "Texp_constant" (Formatter.unquote (Dump.constant constant))
        | Texp_let (rec_flag, value_binding_list, expression) ->
            stag tab "Texp_let" [("rec_flag", Dump.rec_flag rec_flag)] (fun tab ->
                stag tab "value_binding_list" [] (fun tab -> List.iter (process_value_binding tab exp_env) value_binding_list);
                process_expression tab expression
            )
        | Texp_function { arg_label; cases; partial; param } ->
            Xml.ctag tab "Texp_function" [("arg_label", Dump.arg_name arg_label); ("arg_optional", Dump.arg_opt arg_label); ("param", dump_ident param); ("partial", Dump.partial partial)] None (fun tab ->
                Xml.ctag tab "cases" [] None (fun tab ->
                    List.iter (process_case tab) cases
                )
            )
        | Texp_apply (expression, leol) ->
            stag tab "Texp_apply" [] (fun tab ->
                process_expression tab expression;
                stag tab "labeled_expressions" [] (fun tab ->
                    List.iter (fun (arg_label, eo) ->
                        stag tab "arg_label" [("name", Dump.arg_name arg_label); ("optional", Dump.arg_opt arg_label)] (fun tab ->
                            match eo with
                            | None -> Xml.ttag tab "expression" "none"
                            | Some e -> process_expression tab e
                        );
                        ) leol;
                )
            )
    #if OCAML_MINOR >= 13
        | Texp_match (expression, _case_list, partial) ->
            stag tab "Texp_match" [("partial", Dump.partial partial)] (fun tab ->
                process_expression tab expression;
                (* zzz List.iter (process_case tab) case_list; *)
            )
    #elif OCAML_MINOR >= 8
        | Texp_match (expression, case_list, partial) ->
            stag tab "Texp_match" [("partial", Dump.partial partial)] (fun tab ->
                process_expression tab expression;
                List.iter (process_case tab) case_list;
            )
    #else
        | Texp_match (expression, case_list, case_list', partial) ->
            Xml.ctag tab "Texp_match" [("partial", Dump.partial partial)] None (fun tab ->
                process_expression tab expression;
                List.iter (process_case tab) case_list;
                List.iter (process_case tab) case_list';
            )
    #endif
        | Texp_try _(*expression * case list*) -> Xml.mtag tab "Texp_try"
        | Texp_tuple _(*expression list*) -> Xml.mtag tab  "Texp_tuple"
        | Texp_construct (lil, _cd(*constructor_description*), el) ->
            Xml.ctag tab "Texp_construct" [("longident_loc", dump_longident_loc lil)] (Some(["constructor_description"]))
                (fun tab -> List.iter (process_expression tab) el)
        | Texp_variant (label, expression_option) ->
            (match expression_option with
            | None -> Xml.tag tab "Texp_variant" [("label", label)] None
            | Some(e) -> Xml.ctag tab "Texp_variant" [("label", label)] None (fun tab ->
                             process_expression tab e
                         ))
        | Texp_record { fields; extended_expression; _(*representation*) } ->
            stag tab "Texp_record" [] (fun tab ->
                stag tab "longident_label_description_expression" [] (fun tab ->
                    (*  (Types.label_description * Typedtree.record_label_definition) array *)
                    Array.iter (fun (ld, rld) ->
                        stag tab "record_item" [("label_loc", dump_loc ld.Types.lbl_loc)] (fun tab ->
                            process_label_description tab ld;
                            match rld with
                            | Typedtree.Kept _e(*Types.type_expr*) -> Xml.mtag tab "Kept"
                            | Overridden (_l(*Longident.t loc*), e(*expression*)) -> process_expression tab e
                        )
                    ) fields
                );
                match extended_expression with
                    | None -> ()
                    | Some e -> process_expression tab e
            )
        | Texp_field (expression, longident_loc, label_description) ->
            stag tab "Texp_field" [("longident_loc", dump_longident_loc longident_loc)] (fun tab ->
                process_label_description tab label_description;
                process_expression tab expression
            )
        | Texp_setfield _(*expression * Longident.t loc * label_description * expression*) -> Xml.mtag tab "Texp_setfield"
        | Texp_array (expression_list) ->
            stag tab "Texp_array" []
                (fun tab -> List.iter (process_expression tab) expression_list)
        | Texp_ifthenelse _(*expression * expression * expression option*) -> Xml.mtag tab "Texp_ifthenelse"
        | Texp_sequence _(*expression * expression*) -> Xml.mtag tab "Texp_sequence"
        | Texp_while _(*expression * expression*) -> Xml.mtag tab "Texp_while"
        | Texp_for _(* Ident.t * Parsetree.pattern * expression * expression * direction_flag * expression*) -> Xml.mtag tab "Texp_for"
        | Texp_send _(*expression * meth * expression option*) -> Xml.mtag tab "Texp_send"
        | Texp_new _(*Path.t * Longident.t loc * Types.class_declaration*) -> Xml.mtag tab "Texp_new"
        | Texp_instvar _(*Path.t * Path.t * string loc*) -> Xml.mtag tab "Texp_instvar"
        | Texp_setinstvar _(*Path.t * Path.t * string loc * expression*) -> Xml.mtag tab "Texp_setinstvar"
        | Texp_override _(*Path.t * (Path.t * string loc * expression) list*) -> Xml.mtag tab "Texp_override"
        | Texp_letmodule _(*Ident.t * string loc * module_expr * expression*) -> Xml.mtag tab "Texp_letmodule"
        | Texp_assert _(*expression*) -> Xml.mtag tab "Texp_assert"
        | Texp_lazy _(*expression*) -> Xml.mtag tab "Texp_lazy"
        | Texp_object _(*class_structure * string list*) -> Xml.mtag tab "Texp_object"
        | Texp_pack _(*module_expr*) -> Xml.mtag tab "Texp_pack"
        | Texp_unreachable -> Xml.mtag tab "Texp_unreachable"
        | Texp_letexception (_, _) -> Xml.mtag tab "Texp_letexception"
        | Texp_extension_constructor (_, _) -> Xml.mtag tab "Texp_extension_constructor"
    #if OCAML_MINOR >= 8
        | Texp_letop _ -> Xml.mtag tab "Texp_letop"
        | Texp_open (_, _) -> Xml.mtag tab "Texp_open"
    #endif
       )
    )

#if OCAML_MINOR = 6
and process_value_binding_pattern tab {Typedtree.pat_desc; pat_loc; pat_type; pat_env; _ (*pat_extra; pat_attributes*)} =
  Xml.ctag tab "value_binding_pattern" [("pat_loc", dump_loc pat_loc); ("pat_type", Formatter.normalize_string (print_type_scheme pat_env pat_type))] (Some(["pat_attributes";"pat_extra";"pat_env"])) (fun tab ->
#elif OCAML_MINOR >= 7
and process_value_binding_pattern tab {Typedtree.pat_desc; pat_loc; _ (*pat_type; pat_env; pat_extra; pat_attributes*)} =
  Xml.ctag tab "value_binding_pattern" [("pat_loc", dump_loc pat_loc); ("pat_type", " zzz (print_type_scheme pat_env pat_type)")] (Some(["pat_attributes"; "pat_extra"; "pat_env"])) (fun tab ->
#endif
      process_pattern_desc tab pat_desc
  )

and process_value_binding tab _parent_env {vb_pat; vb_expr; vb_loc; _(*vb_attributes*)} =
    Xml.ctag tab "value_binding" [("vb_loc", dump_loc vb_loc)] (Some(["vb_attributes"])) (fun tab ->
        process_value_binding_pattern tab vb_pat;
        process_expression tab vb_expr
    )

and process_module_expression tab { Typedtree.mod_desc; mod_loc; mod_type; mod_env; _(*mod_attributes*) } =
    Xml.atag tab "mod_env" "__";
    stag tab "mod_type" [] (fun tab -> process_module_type tab mod_type);
    Xml.atag tab "mod_loc" (dump_loc mod_loc);
    process_module_description tab mod_env mod_desc

and process_module_description tab _env mod_desc =
    match mod_desc with
    | Typedtree.Tmod_ident (path, longident_loc) ->
        Xml.tag tab "Tmod_ident" [("path", dump_path path); ("longident_loc", dump_longident_loc longident_loc)] None
    | Tmod_structure ({ str_items; str_type; _(*str_final_env*) }) ->
        stag tab "Tmod_structure" [] (fun tab ->
            stag tab "str_type" [] (fun tab -> (List.iter (fun i -> stag tab "str_item" [] (fun tab -> process_signature_item tab i)) str_type));
            stag tab "str_items" [] (fun tab -> List.iter (process_structure_item tab) str_items)
        )
   #if OCAML_MINOR >= 10
    | Tmod_functor (_fp(*functor_parameter*), _me(*module_expr*)) ->
        Xml.mtag tab "Tmod_functor"
   #else
    | Tmod_functor (id, loc, mod_type_o (*module_type option*), mod_expr) ->
        Xml.ctag tab "Tmod_functor" [("ident", dump_ident id); ("loc", dump_string_loc loc)] None (fun tab ->
            (match mod_type_o with
                | None -> ()
                | Some _mod_type -> Xml.mtag tab "Typedtree.module_type"
                (*
                { mty_desc: module_type_desc;
                    mty_type : Types.module_type;
                    mty_env : Env.t;
                    mty_loc: Location.t;
                    mty_attributes: attribute list;
                   }
                *)
            );
            Xml.ctag tab "module_expression" [] None (fun tab -> process_module_expression tab mod_expr);
        )
   #endif
    | Tmod_apply (me(*module_expr*), me'(*module_expr*), _mc(*module_coercion*)) ->
        Xml.ctag tab "Tmod_apply" [] (Some(["mod_coercion"])) (fun tab ->
            Xml.ctag tab "module_expression" [] None (fun tab -> process_module_expression tab me);
            Xml.ctag tab "module_expression2" [] None (fun tab -> process_module_expression tab me')
        )
    | Tmod_constraint (m_expr, m_type(*Types.module_type*), _mtc(*module_type_constraint*), _mc(*module_coercion*)) ->
        Xml.ctag tab "Tmod_constraint" [] (Some(["mod_type_constraint"; "mod_coercion"])) (fun tab ->
            Xml.ctag tab "module_type" [] None (fun tab -> process_module_type tab m_type);
            Xml.ctag tab "module_expression" [] None (fun tab -> process_module_expression tab m_expr)
        )
    | Tmod_unpack (expression, _module_type(*Types.module_type*)) ->
        stag tab "Tmod_unpack" [] (fun tab ->
            process_expression tab expression
        )

#if OCAML_MINOR >= 10
and process_module_binding tab _env {Typedtree.mb_id; mb_name; mb_expr; mb_attributes; mb_loc; _} =
    stag tab "module_binding" [("id", dump_ident_o mb_id); ("mb_name", dump_string_loc_o mb_name); ("mb_loc", dump_loc mb_loc)]
#elif OCAML_MINOR >= 8
and process_module_binding tab _env {Typedtree.mb_id; mb_name; mb_expr; mb_attributes; mb_loc; _} =
    stag tab "module_binding" [("id", dump_ident mb_id); ("mb_name", dump_string_loc mb_name); ("mb_loc", dump_loc mb_loc)]
#else
and process_module_binding tab _env {Typedtree.mb_id; mb_name; mb_expr; mb_attributes; mb_loc} =
    stag tab "module_binding" [("id", dump_ident mb_id); ("mb_name", dump_string_loc mb_name); ("mb_loc", dump_loc mb_loc)]
#endif
      (fun tab ->
        stag tab "mb_attributes" [] (fun tab -> List.iter (process_attribute tab) mb_attributes);
        process_module_expression tab mb_expr
      )

#if OCAML_MINOR >= 8
and process_open_description tab Typedtree.{open_expr; open_loc; _(*open_bound_items; open_override; open_env; open_attributes*)} =
    let (open_path, open_txt) = open_expr in
    Xml.tag tab "open_description" [("open_path", (dump_path open_path)); ("open_loc", dump_loc open_loc); ("open_txt", dump_longident_loc open_txt)] (Some(["open_override"; "open_attributes"]))
#else
and process_open_description tab {Typedtree.open_path; open_txt; open_loc; _(*open_override; open_attributes*)} =
    Xml.tag tab "open_description" [("open_path", (dump_path open_path)); ("open_loc", dump_loc open_loc); ("open_txt", dump_longident_loc open_txt)] (Some(["open_override"; "open_attributes"]))
#endif

and process_structure_item tab {str_desc; str_loc; str_env} =
    match str_desc with
    | Tstr_eval (_e(*expression*), _a(*attributes*)) -> Xml.mtag tab "Tstr_eval"
    | Tstr_value (rf(*rec_flag*), vbl(*value_binding list*)) ->
        Xml.ctag tab "Tstr_value" [("str_loc", dump_loc str_loc); ("rec_flag", Dump.rec_flag rf)] (Some(["str_env"])) (fun tab ->
            List.iter (process_value_binding tab str_env) vbl
        );
    | Tstr_primitive (_vd(*value_description*)) -> Xml.mtag tab "Tstr_primitive"
    | Tstr_type (_rec_flag, _decls) -> Xml.mtag tab "Tstr_type"
    | Tstr_typext (_te(*type_extension*)) -> Xml.mtag tab "Tstr_typext"
    | Tstr_exception (_ec(*extension_constructor*)) -> Xml.mtag tab "Tstr_exception"
    | Tstr_module module_binding ->
        Xml.ctag tab "Tstr_module" [("str_loc", dump_loc str_loc)] (Some(["str_env"])) (fun tab ->
            process_module_binding tab str_env module_binding
        );
    | Tstr_recmodule (_mbl(*module_binding list*)) -> Xml.mtag tab "Tstr_recmodule"
    | Tstr_modtype ({mtd_id (*Ident.t*); mtd_name(*string loc*); mtd_loc(*Location.t*); _ (*mtd_type(*module_type option*); _mtd_attributes(*attribute list*);*) }(*Typedtree.module_type_declaration*)) ->
        Xml.ctag tab "Tstr_modtype" [("mtd_id", dump_ident mtd_id); ("mtd_loc", dump_loc mtd_loc); ("mtd_name", dump_string_loc mtd_name)] (Some(["mtd_attributes"; "mtd_type"])) (fun tab -> Xml.mtag tab "zzz")
#if OCAML_MINOR >= 8
    | Tstr_open _(*open_description*) ->
        Xml.mtag tab "Tstr_open"
#else
    | Tstr_open od(*open_description*) ->
        Xml.ctag tab "Tstr_open" [("str_loc", dump_loc str_loc)] (Some(["str_env"])) (fun tab ->
            process_open_description tab od
        );
#endif
    | Tstr_class (_cl(*(cd(*class_declaration*), sl(*string list*), vf(*virtual_flag*)) list*)) -> Xml.mtag tab "Tstr_class"
    | Tstr_class_type (_ctl(*(i(*Ident.t*), sl(*string loc*), ctd(*class_type_declaration*)) list*)) -> Xml.mtag tab "Tstr_class_type"
    | Tstr_include (_id(*include_declaration*)) -> Xml.mtag tab "Tstr_include"
    | Tstr_attribute (_a(*attribute*)) -> Xml.mtag tab "Tstr_attribute"

let process_implementation tab {Typedtree.str_items; str_type; _(*str_type; str_final_env*)} =
    Xml.mtag tab "str_final_env" (* (dump_summary (Env.summary str_final_env)) *);
    stag tab "str_type" [] (fun tab ->
       List.iter (fun si -> process_signature_item tab si) str_type;
    );
    stag tab "str_items" [] (fun tab ->
        List.iter (fun item -> process_structure_item tab item) str_items;
    )

let print_meta cmt =
    let {
      Cmt_format.cmt_modname (* string *);
      (*cmt_value_dependencies (* (Types.value_description * Types.value_description) list *); *)
      (*cmt_annots             (* binary_annots *);*)
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
      _
    } = cmt in

    Printf.printf "cmt_modname|%s\n" cmt_modname;
    Printf.printf "cmt_value_dependencies|%s\n" "--not-extracted--";
    List.iteri (fun i (s, l) -> Printf.printf "cmt_comments.%s|%s\n" (string_of_int i) ("val=" ^ s ^ " / loc=" ^ (dump_loc l))) cmt_comments;
    Array.iteri (fun i a ->  Printf.printf "cmt_args.%s|%s\n" (string_of_int i) a) cmt_args;
    Printf.printf "cmt_sourcefile|%s\n" (Util.Option.getWithDefault "" cmt_sourcefile);
    Printf.printf "cmt_builddir|%s\n" cmt_builddir;
    Printf.printf "cmt_loadpath|%s\n" (Util.List.join ", " cmt_loadpath);
    Printf.printf "cmt_source_digest|%s\n" (Util.Option.mapWithDefault "" (fun d -> Digest.to_hex d) cmt_source_digest);
    List.iteri (fun i (s, d) -> Printf.printf "cmt_imports.%s|%s\n" (string_of_int i) ("'" ^ s ^ "':" ^ (Util.Option.mapWithDefault "" (fun d -> Digest.to_hex d) d))) cmt_imports;
    Printf.printf "cmt_interface_digest|%s\n" (Util.Option.mapWithDefault "" (fun d -> Digest.to_hex d) cmt_interface_digest);
    Printf.printf "cmt_use_summaries|%s\n" (string_of_bool cmt_use_summaries);
    Printf.printf "cmt_initial_env|%s\n" (dump_env cmt_initial_env);;

let print_cmt cmt =
    let { Cmt_format.cmt_annots (* binary_annots *); _ } = cmt in

    Printf.printf "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n";
    stag "" "tree" [("xmlns", "https://github.com/giraud/rincewind")] (fun tab ->
        match cmt_annots with
        | Packed (_signature, _string_list) -> Xml.mtag tab "Packed"
        | Implementation structure -> process_implementation tab structure
        | Interface _signature -> Xml.mtag tab "Interface"
        | Partial_implementation _binary_part_array -> Xml.mtag tab "Partial_implementation"
        | Partial_interface _binary_part_array -> Xml.mtag tab "Partial_interface";
    )
