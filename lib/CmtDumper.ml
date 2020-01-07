open Printf

let incr ident = ident ^ "    "

let stag indent name attrs children =
    printf "%s<%s" indent name;
    List.iter (fun (k,v) -> printf " %s=\"%s\"" k v) attrs;
    printf ">\n";
    (children (incr indent)) ;
    printf "%s</%s>\n" indent name

let tag indent name attrs =
    printf "%s<%s" indent name;
    List.iter (fun (k,v) -> printf " %s=\"%s\"" k v) attrs;
    printf "/>\n"

let atag indent name attr = printf "%s<%s val=\"%s\"/>\n" indent name attr
let ttag indent name text = printf "%s<%s>%s</%s>\n" indent name text name
let mtag indent name = tag indent "SKIPPED" [("name", name)]


let dump_partial p = match p with | Typedtree.Partial -> "Partial" | Total -> "Total"

let dump_rec_flag rf = match rf with | Asttypes.Nonrecursive -> "Nonrecursive" | Recursive -> "Recursive"

let dump_type t = Formatter.format_type t

(*
 ident.mli / Ident.t
    { stamp: int; name: string; mutable flags: int }
*)
let dump_ident i =
    Format.asprintf "%a" Printtyp.ident(*Ident.print*) i

(*
 path.mli / Path.t
   | Pident of Ident.t
   | Pdot of t * string * int
   | Papply of t * t
 *)
let dump_path p = Printtyp.string_of_path p
(*
match p with
   | Path.Pident i -> "<ident:" ^ (dump_ident i) ^ ">"
   | Pdot (t, s, i) -> "dot." ^ s ^ "_" ^ (string_of_int i)
   | Papply (t, t') -> "apply"
*)

let dump_pos { Lexing.pos_lnum; pos_bol; pos_cnum; _(*pos_fname(*source file*)*)} =
  (*"<pos:" ^ pos_fname ^ ":" ^ (string_of_int pos_lnum) ^ ":" ^ (string_of_int pos_bol) ^ ":" ^ (string_of_int pos_cnum) ^ ">"*)
  (string_of_int pos_lnum) ^ "." ^ (string_of_int (pos_cnum - pos_bol + 1))

(* location.ml / Location.t { loc_start: position; loc_end: position; loc_ghost: bool } *)
let dump_loc { Location.loc_start; loc_end; loc_ghost } =
  (dump_pos loc_start) ^ ":" ^ (dump_pos loc_end) ^ ":" ^ (string_of_bool loc_ghost)

let dump_longident_loc {Asttypes.txt(*LongIdent.t*); loc(*Location.t*)} =
    (Format.asprintf "%a" Printtyp.longident txt) ^ "|" ^ (dump_loc loc)

let dump_ast_loc {Asttypes.txt; loc} =
    txt ^ "|" ^ (dump_loc loc)

let dump_string_loc {Location.txt; loc} =
    txt ^ "|" ^ (dump_loc loc)

let dump_value_description id vd(*{ val_type; val_kind; val_loc; val_attributes }*) =
  Format.asprintf "%a" (Printtyp.value_description id) vd

let dump_rec_status v = match v with | Types.Trec_not -> "Trec_not" | Trec_first -> "Trec_first" | Trec_next -> "Trec_next"

let dump_value_kind kind =
    let k = match kind with
    | Types.Val_reg -> "Regular value"
    | Val_prim _(*Primitive.description*) -> "Primitive"
    | Val_ivar _(*mutable_flag * string*) -> "Instance variable (mutable ?)"
    | Val_self _(*(Ident.t * type_expr) Meths.t ref * (Ident.t * mutable_flag * virtual_flag * type_expr) Vars.t ref * string * type_expr*) -> "Self"
    | Val_anc _(*(string * Ident.t) list * string*) -> "Ancestor"
#if OCAML_MINOR < 8
    | Val_unbound                         -> "Unbound variable"
#else
    | Val_unbound _                       -> "Unbound variable"
#endif
    in
    "«kind:" ^ k ^ "»"

let print_type_scheme env typ =
  Printtyp.wrap_printing_env env (fun () -> Format.asprintf "%a" Printtyp.type_scheme typ)

let rec process_module_type tab mt = match mt with
  | Types.Mty_ident _pa -> mtag tab "Mty_ident"
  | Mty_signature  signature ->
        stag tab "Mty_signature" [] (fun tab -> List.iter (process_signature_item tab) signature)
  | Mty_functor _(*Ident.t * module_type option * module_type*) -> mtag tab "Mty_functor"
  | Mty_alias _(*alias_presence * Path.t*) -> mtag tab "Mty_alias"

and process_value_description tab id vd =
  let { Types.val_type; val_kind; val_loc; val_attributes } = vd in
  stag tab "value_description" [("val_type", dump_type val_type); ("val_kind",  dump_value_kind val_kind); ("val_loc", dump_loc val_loc);] (fun tab ->
      atag tab "value" (dump_value_description id vd);
      List.iter (process_attribute tab) val_attributes
  )

and process_modtype_declaration tab { Types.mtd_type; _(*mtd_attributes; mtd_loc;*) } =
    match mtd_type with | None -> tag tab "mtd_type" [("abstract", "true")] | Some t -> stag tab "mtd_type" [("abstract", "false")] (fun tab -> process_module_type tab t)

and dump_module_declaration tab {Types.md_type; _(*md_attributes; md_loc*)} =
    process_module_type tab md_type

and process_signature_item tab si = match si with
#if OCAML_MINOR >= 8
  | Sig_value (id, vd(*value_description*), _visibility) ->
        stag tab "Sig_value" [("id", dump_ident id);] (fun tab -> process_value_description tab id vd)
  | Sig_type (id, type_declaration, rec_status, _visibility) ->
        tag tab "Sig_type" [("id", dump_ident id); ("rec_status", dump_rec_status rec_status); ("type_declaration", Formatter.format_type_declaration id type_declaration)]
  | Sig_typext (_id, _ec, _es, _visibility) -> mtag tab "Sig_typext"
  | Sig_module (_id, _mp, md, _rs, _visibility) -> stag tab "Sig_module" [] (fun tab -> dump_module_declaration tab md)
  | Sig_modtype (id, modtype_declaration, _visibility) ->
        stag tab "Sig_modtype" [("id", dump_ident id)] (fun tab -> process_modtype_declaration tab modtype_declaration)
  | Sig_class (_id, _cd, _rs, _visibility) -> mtag tab "Sig_class"
  | Sig_class_type (_id, _ctd, _rs, _visibility) -> mtag tab "Sig_class_type"
#else
  | Sig_value (id, vd(*value_description*)) ->
        stag tab "Sig_value" [("id", dump_ident id);] (fun tab -> process_value_description tab id vd)
  | Sig_type (id, type_declaration, rec_status) ->
        tag tab "Sig_type" [("id", dump_ident id); ("rec_status", dump_rec_status rec_status); ("type_declaration", Formatter.format_type_declaration id type_declaration)]
  | Sig_typext (_id, _ec, _es) -> mtag tab "Sig_typext"
  | Sig_module (_id, md, _rs) -> stag tab "Sig_module" [] (fun tab -> dump_module_declaration tab md)
  | Sig_modtype (id, modtype_declaration) ->
        stag tab "Sig_modtype" [("id", dump_ident id)] (fun tab -> process_modtype_declaration tab modtype_declaration)
  | Sig_class (_id, _cd, _rs) -> mtag tab "Sig_class"
  | Sig_class_type (_id, _ctd, _rs) -> mtag tab "Sig_class_type"
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
  | Env_copy_types (_, _) -> "copy_types"
#if OCAML_MINOR >= 8
  | Env_persistent (_su, _id) -> "Env_persistent"
#endif

and dump_env env = dump_summary (Env.summary env)

and process_pattern_desc tab pat_desc =
  match pat_desc with
    | Typedtree.Tpat_any -> mtag tab "Tpat_any"
    | Tpat_var (i, sl) ->
        tag tab "Tpat_var" [("ident", dump_ident i); ("string_loc", dump_string_loc sl)]
    | Tpat_alias (_pattern, _ident, _loc) -> mtag tab "Tpat_alias"
    | Tpat_constant (_c) ->  mtag tab "Tpat_constant"
    | Tpat_tuple (_patternl) -> mtag tab "Tpat_tuple"
    | Tpat_construct (_loc, _constr_desc, _patternl) -> mtag tab "Tpat_construct"
    | Tpat_variant (_label, _pattern, _row_desc) -> mtag tab "Tpat_variant"
    | Tpat_record (_rl, _flag) -> mtag tab "Tpat_record"
    | Tpat_array (_patternl) -> mtag tab "Tpat_array"
    | Tpat_or (_pattern, _pattern', _row_desc) -> mtag tab "Tpat_or"
    | Tpat_lazy (_pattern) -> mtag tab "Tpat_lazy"
#if OCAML_MINOR >= 8
    | Tpat_exception _ -> mtag tab "Tpat_exception"
#endif

and print_case tab {Typedtree.c_lhs; c_rhs; _(*c_guard; *)} =
    let {Typedtree.pat_desc; pat_loc; _ (*pat_extra; pat_env; pat_attributes;pat_type;*)} = c_lhs in
        stag tab "c_lhs" [("pat_loc", dump_loc pat_loc); ("pat_env", "__"); ("pat_attributes", "__"); ("pat_extra", "__")] (fun tab -> process_pattern_desc tab pat_desc);
    mtag tab "guard";
    stag tab "c_rhs" [] (fun tab -> process_expression tab c_rhs)

#if OCAML_MINOR >= 8
and process_attribute tab Parsetree.{attr_payload; attr_loc; _} =
    let loc_str = dump_pos attr_loc.loc_start in
#else
and process_attribute tab (attr_loc, attr_payload) =
    let loc_str = dump_string_loc attr_loc in
#endif
    stag tab "attribute" [("string_loc", loc_str)] (fun tab ->
        match attr_payload with
        | Parsetree.PStr _structure -> mtag tab "PStr"
        | PTyp _core_type  (* : T *) -> mtag tab "PTyp"
        | PPat (_pattern, _expression_option) (* ? P  or  ? P when E *) -> mtag tab "PPat"
        | PSig _ -> mtag tab "PSig"
    )

and process_label_description tab { Types.lbl_name (* Short name *);
                                    lbl_arg;       (* Type of the argument *)
                                    lbl_loc;
                                    _ } =
    tag tab "label_description" [("lbl_name", lbl_name); ("lbl_loc", dump_loc lbl_loc); ("lbl_res", "__");("lbl_arg", dump_type lbl_arg); ("lbl_mut", "");("lbl_pos", "");("lbl_all", "");("lbl_repres", "");("lbl_private", "");]

and process_expression tab { exp_desc; exp_loc; exp_env; _(*exp_type; exp_attributes; exp_extra*) } =
(*    Printf.printf "%sexpression %s\n" indent (dump_loc exp_loc);*)
    match exp_desc with
    | Texp_ident (p(*Path.t*), lil(*Longident.t loc*), vd(*Types.value_description*)) ->
        stag tab "Texp_ident" [("path", dump_path p); ("exp_loc", dump_loc exp_loc); ("longident_loc", dump_longident_loc lil)] (fun tab ->
#if OCAML_MINOR >= 8
            process_value_description tab (Ident.create_local "xxx") vd
#else
            process_value_description tab (Ident.create "xxx") vd
#endif
        )
    | Texp_constant _constant -> mtag tab "Texp_constant"
    | Texp_let (rec_flag, value_binding_list, expression) ->
        stag tab "Texp_let" [("exp_loc", dump_loc exp_loc); ("rec_flag", dump_rec_flag rec_flag)] (fun tab ->
            stag tab "value_binding_list" [] (fun tab ->
                List.iter (process_value_binding tab exp_env) value_binding_list
            );
            process_expression tab expression
        )
    | Texp_function { arg_label; cases; partial; _(*param*) } ->
        stag tab "Texp_function" [("label", Printtyp.string_of_label arg_label); ("exp_loc", dump_loc exp_loc); ("partial", dump_partial partial)] (fun tab ->
            stag tab "case_list" [] (fun tab -> List.iter (print_case tab) cases)
        )
    | Texp_apply (expression,  leol(*(arg_label * expression option) list*)) ->
        stag tab "Texp_apply" [("exp_loc", dump_loc exp_loc)] (fun tab ->
            process_expression tab expression;
            stag tab "label_expression_option_optional" [] (fun tab ->
                List.iter (fun (_arg_label, eo) ->
                    match eo with
                    | None -> ttag tab "label" "no expression"
                    | Some e -> process_expression tab e
                ) leol
            )
        )
#if OCAML_MINOR >= 8
    | Texp_match (expression, case_list, partial) ->
        stag tab "Texp_match" [("exp_loc", dump_loc exp_loc); ("partial", dump_partial partial)] (fun tab ->
            process_expression tab expression;
            List.iter (print_case tab) case_list;
        )
#else
    | Texp_match (expression, case_list, case_list', partial) ->
        stag tab "Texp_match" [("exp_loc", dump_loc exp_loc); ("partial", dump_partial partial)] (fun tab ->
            process_expression tab expression;
            List.iter (print_case tab) case_list;
            List.iter (print_case tab) case_list';
        )
#endif
    | Texp_try _(*expression * case list*) -> mtag tab "Texp_try"
    | Texp_tuple _(*expression list*) -> mtag tab  "Texp_tuple"
    | Texp_construct (lil(*Longident.t loc*), _cd(*constructor_description*), el(*expression list*)) ->
        stag tab "Texp_construct" [("exp_loc", dump_loc exp_loc); ("longident_loc", dump_longident_loc lil)] (fun tab ->
            List.iter (process_expression tab) el
        )
    | Texp_variant _(*label * expression option*) -> mtag tab  "Texp_variant"
    | Texp_record { fields; extended_expression; _(*representation*) } ->
        stag tab "Texp_record" [("exp_loc", dump_loc exp_loc)] (fun tab ->
            stag tab "longident_label_description_expression" [] (fun tab ->
                (*  (Types.label_description * Typedtree.record_label_definition) array *)
                Array.iter (fun (ld, rld) ->
                    stag tab "record_item" [("label_loc", dump_loc ld.Types.lbl_loc)] (fun tab ->
                        process_label_description tab ld;
                        match rld with
                        | Typedtree.Kept _e(*Types.type_expr*) -> mtag tab "Kept"
                        | Overridden (_l(*Longident.t loc*), e(*expression*)) -> process_expression tab e
                    )
                ) fields
            );
            match extended_expression with
                | None -> ()
                | Some e -> process_expression tab e
        )
    | Texp_field (expression, longident_loc, label_description) ->
        stag tab "Texp_field" [("exp_loc", dump_loc exp_loc); ("longident_loc", dump_longident_loc longident_loc)] (fun tab ->
            process_label_description tab label_description;
            process_expression tab expression
        )
    | Texp_setfield _(*expression * Longident.t loc * label_description * expression*) -> mtag tab "Texp_setfield"
    | Texp_array (expression_list) ->
        stag tab "Texp_array" [("exp_loc", dump_loc exp_loc)] (fun tab -> List.iter (process_expression tab) expression_list)
    | Texp_ifthenelse _(*expression * expression * expression option*) -> mtag tab "Texp_ifthenelse"
    | Texp_sequence _(*expression * expression*) -> mtag tab "Texp_sequence"
    | Texp_while _(*expression * expression*) -> mtag tab "Texp_while"
    | Texp_for _(* Ident.t * Parsetree.pattern * expression * expression * direction_flag * expression*) -> mtag tab "Texp_for"
    | Texp_send _(*expression * meth * expression option*) -> mtag tab "Texp_send"
    | Texp_new _(*Path.t * Longident.t loc * Types.class_declaration*) -> mtag tab "Texp_new"
    | Texp_instvar _(*Path.t * Path.t * string loc*) -> mtag tab "Texp_instvar"
    | Texp_setinstvar _(*Path.t * Path.t * string loc * expression*) -> mtag tab "Texp_setinstvar"
    | Texp_override _(*Path.t * (Path.t * string loc * expression) list*) -> mtag tab "Texp_override"
    | Texp_letmodule _(*Ident.t * string loc * module_expr * expression*) -> mtag tab "Texp_letmodule"
    | Texp_assert _(*expression*) -> mtag tab "Texp_assert"
    | Texp_lazy _(*expression*) -> mtag tab "Texp_lazy"
    | Texp_object _(*class_structure * string list*) -> mtag tab "Texp_object"
    | Texp_pack _(*module_expr*) -> mtag tab "Texp_pack"
    | Texp_unreachable -> mtag tab "Texp_unreachable"
    | Texp_letexception (_, _) -> mtag tab "Texp_letexception"
    | Texp_extension_constructor (_, _) -> mtag tab "Texp_extension_constructor"
#if OCAML_MINOR >= 8
    | Texp_letop _ -> mtag tab "Texp_letop"
    | Texp_open (_, _) -> mtag tab "Texp_open"
#endif

#if OCAML_MINOR = 6
and process_value_binding_pattern tab {Typedtree.pat_desc; pat_loc; pat_type; pat_env; _ (*pat_extra; pat_attributes*)} =
  stag tab "value_binding_pattern" [("pat_loc", dump_loc pat_loc); ("pat_type", Formatter.clean_type (print_type_scheme pat_env pat_type)); ("pat_attributes", "__"); ("pat_extra", "__"); ("pat_env", "__")] (fun tab ->
#elif OCAML_MINOR >= 7
and process_value_binding_pattern tab {Typedtree.pat_desc; pat_loc; _ (*pat_type; pat_env; pat_extra; pat_attributes*)} =
  stag tab "value_binding_pattern" [("pat_loc", dump_loc pat_loc); ("pat_type", Formatter.clean_type " zzz (print_type_scheme pat_env pat_type)"); ("pat_attributes", "__"); ("pat_extra", "__"); ("pat_env", "__")] (fun tab ->
#endif
      process_pattern_desc tab pat_desc
  )

and process_value_binding tab _parent_env {vb_pat; vb_expr; vb_loc; _(*vb_attributes*)} =
    stag tab "value_binding" [("vb_loc", dump_loc vb_loc); ("vb_attributes", "__")] (fun tab ->
        process_value_binding_pattern tab vb_pat;
        process_expression tab vb_expr
    )

and process_module_description tab _env mod_desc =
    match mod_desc with
    | Typedtree.Tmod_ident (path, longident_loc) ->
        tag tab "Tmod_ident" [("path", dump_path path); ("longident_loc", dump_longident_loc longident_loc)]
    | Tmod_structure ({ str_items; str_type; _(*str_final_env*) }) ->
        stag tab "Tmod_structure" [] (fun tab ->
            stag tab "str_type" [] (fun tab -> (List.iter (fun i -> stag tab "str_item" [] (fun tab -> process_signature_item tab i)) str_type));
            stag tab "str_items" [] (fun tab -> List.iter (process_structure_item tab) str_items)
        )
    | Tmod_functor (_i(*Ident.t*), _sl(*string loc*), _mto(*module_type option*), _me(*module_expr*)) ->
        mtag tab "Tmod_functor"
    | Tmod_apply (_me(*module_expr*), _me'(*module_expr*), _mc(*module_coercion*)) ->
        mtag tab "Tmod_apply"
    | Tmod_constraint (_me(*module_expr*), _mt(*Types.module_type*), _mtc(*module_type_constraint*), _mc(*module_coercion*)) ->
        mtag tab "Tmod_constraint"
    | Tmod_unpack (expression, _module_type(*Types.module_type*)) ->
        stag tab "Tmod_unpack" [] (fun tab ->
            process_expression tab expression
        )

#if OCAML_MINOR >= 8
and process_module_binding tab _env {Typedtree.mb_id; mb_name; mb_expr; mb_attributes; mb_loc; _} =
#else
and process_module_binding tab _env {Typedtree.mb_id; mb_name; mb_expr; mb_attributes; mb_loc} =
#endif
    stag tab "module_binding" [("id", dump_ident mb_id); ("mb_name", dump_string_loc mb_name); ("mb_loc", dump_loc mb_loc)] (fun tab ->
        stag tab "mb_attributes" [] (fun tab -> List.iter (process_attribute tab) mb_attributes);
        let { Typedtree.mod_desc; mod_loc; mod_type; mod_env; _(*mod_attributes*) } = mb_expr in
            atag tab "mod_env" "__";
            stag tab "mod_type" [] (fun tab -> process_module_type tab mod_type);
            atag tab "mod_loc" (dump_loc mod_loc);
            process_module_description tab mod_env mod_desc
    )

#if OCAML_MINOR >= 8
and process_open_description tab Typedtree.{open_expr; open_loc; _(*open_bound_items; open_override; open_env; open_attributes*)} =
    let (open_path, open_txt) = open_expr in
    tag tab "open_description" [("open_path", (dump_path open_path)); ("open_loc", dump_loc open_loc); ("open_txt", dump_longident_loc open_txt); ("open_override", "__"); ("open_attributes", "__")]
#else
and process_open_description tab {Typedtree.open_path; open_txt; open_loc; _(*open_override; open_attributes*)} =
    tag tab "open_description" [("open_path", (dump_path open_path)); ("open_loc", dump_loc open_loc); ("open_txt", dump_longident_loc open_txt); ("open_override", "__"); ("open_attributes", "__")]
#endif

and process_structure_item tab {str_desc; str_loc; str_env} =
    match str_desc with
    | Tstr_eval (_e(*expression*), _a(*attributes*)) -> mtag tab "Tstr_eval"
    | Tstr_value (rf(*rec_flag*), vbl(*value_binding list*)) ->
        stag tab "Tstr_value" [("str_loc", dump_loc str_loc); ("str_env", "__"); ("rec_flag", dump_rec_flag rf)] (fun tab ->
            List.iter (process_value_binding tab str_env) vbl
        );
    | Tstr_primitive (_vd(*value_description*)) -> mtag tab "Tstr_primitive"
    | Tstr_type (_rec_flag, _decls) -> mtag tab "Tstr_type"
    | Tstr_typext (_te(*type_extension*)) -> mtag tab "Tstr_typext"
    | Tstr_exception (_ec(*extension_constructor*)) -> mtag tab "Tstr_exception"
    | Tstr_module module_binding ->
        stag tab "Tstr_module" [("str_loc", dump_loc str_loc); ("str_env", "__")] (fun tab ->
            process_module_binding tab str_env module_binding
        );
    | Tstr_recmodule (_mbl(*module_binding list*)) -> mtag tab "Tstr_recmodule"
    | Tstr_modtype (_mtd(*module_type_declaration*)) -> mtag tab "Tstr_modtype"
#if OCAML_MINOR >= 8
    | Tstr_open _(*open_description*) ->
        mtag tab "Tstr_open"
#else
    | Tstr_open od(*open_description*) ->
        stag tab "Tstr_open" [("str_loc", dump_loc str_loc); ("str_env", "__")] (fun tab ->
            process_open_description tab od
        );
#endif
    | Tstr_class (_cl(*(cd(*class_declaration*), sl(*string list*), vf(*virtual_flag*)) list*)) -> mtag tab "Tstr_class"
    | Tstr_class_type (_ctl(*(i(*Ident.t*), sl(*string loc*), ctd(*class_type_declaration*)) list*)) -> mtag tab "Tstr_class_type"
    | Tstr_include (_id(*include_declaration*)) -> mtag tab "Tstr_include"
    | Tstr_attribute (_a(*attribute*)) -> mtag tab "Tstr_attribute"

let process_implementation tab {Typedtree.str_items; _(*str_type; str_final_env*)} =
    mtag tab "str_final_env" (* (dump_summary (Env.summary str_final_env)) *);
    mtag tab "str_types" (* (Util.List.dump (fun si -> process_signature_item si) str_type) *);
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

    stag "" "tree" [("xmlns", "https://github.com/giraud/rincewind")] (fun tab ->
        match cmt_annots with
        | Packed (_signature, _string_list) -> mtag tab "Packed"
        | Implementation structure -> process_implementation tab structure
        | Interface _signature -> mtag tab "Interface"
        | Partial_implementation _binary_part_array -> mtag tab "Partial_implementation"
        | Partial_interface _binary_part_array -> mtag tab "Partial_interface";
    )

