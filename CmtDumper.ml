open Printf
open Typedtree
open Types
open RwTypes

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
let mtag indent name = atag indent "MISSING" name


let dump_partial p = match p with | Partial -> "Partial" | Total -> "Total"

let dump_rec_flag rf = match rf with | Asttypes.Nonrecursive -> "Nonrecursive" | Recursive -> "Recursive"

let dump_type t = Formatter.clean_type (RwTypes.read_type t)

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

let dump_pos { Lexing.pos_fname(*source file*); pos_lnum; pos_bol; pos_cnum; } =
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
  Formatter.clean_type (Format.asprintf "%a" (Printtyp.value_description id) vd)

let rec dump_module_type mt = match mt with
  | Mty_ident pa -> "MTY_IDENT"
  | Mty_signature sil -> "MTY_SIGNATURE"
  | Mty_functor _(*Ident.t * module_type option * module_type*) -> "MTY_FUNCTOR"
  | Mty_alias _(*alias_presence * Path.t*) -> "MTY_ALIAS"

and dump_module_declaration {md_type(*module_type*); md_attributes(*Parsetree.attributes*); md_loc(*Location.t*) } =
    dump_module_type md_type

and dump_signature_item si = match si with
  | Sig_value (id, vd(*value_description*)) -> dump_value_description id vd
  | Sig_type (id, td(*type_declaration*), rec_status(*rs*)) -> "Sig_type"
  | Sig_typext (id, ec, es) -> "Sig_typext"
  | Sig_module (id, md, rs) -> dump_module_declaration md
  | Sig_modtype (id, md) -> "Sig_modtype"
  | Sig_class (id, cd, rs) -> "Sig_class"
  | Sig_class_type (id, ctd, rs) -> "Sig_class_type"

(* *)
let rec dump_summary s = match s with
  | Env.Env_empty -> ""
  | Env_value (su, id, vd(*value_description*)) -> "<value:" ^ (dump_value_description id vd)^ "> " ^ (dump_summary su)
  | Env_type (su, id, td) -> "<" ^ (RwTypes.dump_type_declaration id td) ^ "> " ^ (dump_summary su)
  | Env_extension (su, id, ec) -> "<extension:" ^ (dump_ident id) ^ "> " ^ (dump_summary su)
  | Env_module (su, id, md(*module_declaration*)) -> "<module:" ^ (dump_ident id) ^ ":" ^ (dump_module_declaration md) ^ "> " ^ (dump_summary su)
  | Env_modtype _ (* summary * Ident.t * modtype_declaration *) -> "Env_modtype"
  | Env_class _ (* summary * Ident.t * class_declaration *) -> "class"
  | Env_cltype _ (* summary * Ident.t * class_type_declaration *) -> "cltype"
  | Env_open (su(*summary*), pa(*Path.t*)) -> "<open:" ^ (dump_path pa) ^ "> " ^ (dump_summary su)
  | Env_functor_arg _ (* summary * Ident.t *) -> "functor_arg"

let dump_env env = dump_summary (Env.summary env)

(**
 Shortcut to read a type from an expression
 *)
let read_etype {exp_type; _} =
  RwTypes.read_type exp_type

let print_type_scheme env typ =
  Printtyp.wrap_printing_env env (fun () -> Format.asprintf "%a" Printtyp.type_scheme typ)

(**
 Extract the name of a pattern
 *)
let rec process_pattern_desc tab pat_env pat_type pat_desc =
  match pat_desc with
    | Tpat_any -> mtag tab "Tpat_any"
    | Tpat_var (i(*Ident.t*), sl(*string loc*)) ->
        tag tab "Tpat_var" [("ident", dump_ident i); ("string_loc", dump_string_loc sl)]
    | Tpat_alias (pattern, ident, loc) -> mtag tab "Tpat_alias"
    | Tpat_constant (c) ->  mtag tab "Tpat_constant"
    | Tpat_tuple (patternl) -> mtag tab "Tpat_tuple"
    | Tpat_construct (loc, constr_desc, patternl) -> mtag tab "Tpat_construct"
    | Tpat_variant (label, pattern, row_desc) -> mtag tab "Tpat_variant"
    | Tpat_record (rl, flag) -> mtag tab "Tpat_record"
    | Tpat_array (patternl) -> mtag tab "Tpat_array"
    | Tpat_or (pattern, pattern', row_desc) -> mtag tab "Tpat_or"
    | Tpat_lazy (pattern) -> mtag tab "Tpat_lazy"

let dump_value_kind kind =
    let k = match kind with
    | Val_reg -> "Regular value"
    | Val_prim _(*Primitive.description*) -> "Primitive"
    | Val_ivar _(*mutable_flag * string*) -> "Instance variable (mutable ?)"
    | Val_self _(*(Ident.t * type_expr) Meths.t ref * (Ident.t * mutable_flag * virtual_flag * type_expr) Vars.t ref * string * type_expr*) -> "Self"
    | Val_anc _(*(string * Ident.t) list * string*) -> "Ancestor"
    | Val_unbound                         -> "Unbound variable"
    in
    "«kind:" ^ k ^ "»"

let process_value_description tab { val_type; val_kind; val_loc; val_attributes } =
    tag tab "value_description" [("val_kind", dump_value_kind val_kind); ("val_loc", dump_loc val_loc); ("val_type", dump_type val_type); ("val_attributes", "__")]

let rec print_case tab {c_lhs(*pattern*); c_guard(*expression option*); c_rhs(*expression*)} =
    let {pat_desc; pat_loc; pat_extra; pat_type; pat_env; pat_attributes} = c_lhs in
        stag tab "c_lhs" [("pat_loc", dump_loc pat_loc); ("pat_env", "__"); ("pat_attributes", "__"); ("pat_extra", "__")] (fun tab -> process_pattern_desc tab pat_desc);
    mtag tab "guard";
    stag tab "c_rhs" [] (fun tab -> process_expression tab c_rhs)

and process_label_description tab { lbl_name(* Short name *);
                                        lbl_res;                 (* Type of the result *)
                                        lbl_arg;                 (* Type of the argument *)
                                        lbl_mut;              (* Is this a mutable field? *)
                                        lbl_pos;                       (* Position in block *)
                                        lbl_all;   (* All the labels in this type *)
                                        lbl_repres;  (* Representation for this record *)
                                        lbl_private;          (* Read-only field? *)
                                        lbl_loc;
                                        lbl_attributes } =
    tag tab "label_description" [("lbl_name", lbl_name); ("lbl_loc", dump_loc lbl_loc); ("lbl_res", "__");("lbl_arg", dump_type lbl_arg); ("lbl_mut", "");("lbl_pos", "");("lbl_all", "");("lbl_repres", "");("lbl_private", "");]

and process_expression tab { exp_desc; exp_loc; exp_extra; exp_type; exp_env; exp_attributes } =
(*    Printf.printf "%sexpression %s\n" indent (dump_loc exp_loc);*)
    match exp_desc with
    | Texp_ident (p(*Path.t*), lil(*Longident.t loc*), vd(*Types.value_description*)) ->
        stag tab "Texp_ident" [("path", dump_path p); ("exp_loc", dump_loc exp_loc); ("longident_loc", dump_longident_loc lil)] (fun tab ->
            process_value_description tab vd
        )
    | Texp_constant constant -> mtag tab "Texp_constant"
    | Texp_let (rec_flag, value_binding_list, expression) ->
        stag tab "Texp_let" [("exp_loc", dump_loc exp_loc); ("rec_flag", dump_rec_flag rec_flag)] (fun tab ->
            stag tab "value_binding_list" [] (fun tab ->
                List.iter (process_value_binding tab exp_env) value_binding_list
            );
            process_expression tab expression
        )
    | Texp_function (label, case_list, partial) ->
        stag tab "Texp_function" [("label", label); ("exp_loc", dump_loc exp_loc); ("partial", dump_partial partial)] (fun tab ->
            stag tab "case_list" [] (fun tab -> List.iter (print_case tab) case_list)
        )
    | Texp_apply (expression,  leol(*(label * expression option * optional) list*)) ->
        stag tab "Texp_apply" [("exp_loc", dump_loc exp_loc)] (fun tab ->
            process_expression tab expression;
            stag tab "label_expression_option_optional" [] (fun tab ->
                List.iter (fun (label, eo, o) ->
                    match eo with
                    | None -> ttag tab "label" "no expression"
                    | Some e -> process_expression tab e
                ) leol
            )
        )
    | Texp_match (expression, case_list, case_list', partial) ->
        stag tab "Texp_match" [("exp_loc", dump_loc exp_loc); ("partial", dump_partial partial)] (fun tab ->
            process_expression tab expression;
            List.iter (print_case tab) case_list;
            List.iter (print_case tab) case_list';
        )
    | Texp_try _(*expression * case list*) -> mtag tab "Texp_try"
    | Texp_tuple _(*expression list*) -> mtag tab  "Texp_tuple"
    | Texp_construct (lil(*Longident.t loc*), cd(*constructor_description*), el(*expression list*)) ->
        stag tab "Texp_construct" [("exp_loc", dump_loc exp_loc); ("longident_loc", dump_longident_loc lil)] (fun tab ->
            List.iter (process_expression tab) el
        )
    | Texp_variant _(*label * expression option*) -> mtag tab  "Texp_variant"
    | Texp_record (fields(*(Longident.t loc * label_description * expression) list*), expression_option) ->
        stag tab "Texp_record" [("exp_loc", dump_loc exp_loc)] (fun tab ->
            stag tab "longident_label_description_expression" [] (fun tab ->
                List.iter (fun (lil, ld, e) ->
                    stag tab "record_item" [("longident_loc", dump_longident_loc lil)] (fun tab ->
                        process_label_description tab ld;
                        process_expression tab e
                    )
                ) fields
            );
            match expression_option with
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

and process_value_binding_pattern tab {pat_desc; pat_loc; pat_extra; pat_type; pat_env; pat_attributes} =
  stag tab "value_binding_pattern" [("pat_loc", dump_loc pat_loc); ("pat_type", Formatter.clean_type (print_type_scheme pat_env pat_type)); ("pat_attributes", "__"); ("pat_extra", "__"); ("pat_env", "__")] (fun tab ->
      process_pattern_desc tab pat_env pat_type pat_desc
  )

and process_value_binding tab parent_env {vb_pat; vb_expr; vb_attributes; vb_loc} =
    stag tab "value_binding" [("vb_loc", dump_loc vb_loc); ("vb_attributes", "__")] (fun tab ->
        process_value_binding_pattern tab vb_pat;
        process_expression tab vb_expr
    )

and process_module_description tab env mod_desc =
    match mod_desc with
    | Tmod_ident (pa(*Path.t*), il(*Longident.t loc*)) ->
        mtag tab "Tmod_ident"
    | Tmod_structure ({ str_items; str_type; str_final_env }) ->
        stag tab "Tmod_structure" [] (fun tab ->
            stag tab "str_type" [] (fun tab -> (List.iter (fun i -> atag tab "str_item" (dump_signature_item i)) str_type));
            stag tab "str_items" [] (fun tab -> List.iter (process_structure_item tab) str_items)
        )
    | Tmod_functor (i(*Ident.t*), sl(*string loc*), mto(*module_type option*),  me(*module_expr*)) ->
        mtag tab "Tmod_functor"
    | Tmod_apply (me(*module_expr*), me'(*module_expr*), mc(*module_coercion*)) ->
        mtag tab "Tmod_apply"
    | Tmod_constraint (me(*module_expr*), mt(*Types.module_type*), mtc(*module_type_constraint*), mc(*module_coercion*)) ->
        mtag tab "Tmod_constraint"
    | Tmod_unpack (e(*expression*), mt(*Types.module_type*)) ->
        mtag tab "Tmod_unpack"

and process_module_binding tab env {mb_id; mb_name; mb_expr; mb_attributes; mb_loc} =
    stag tab "module_binding" [("id", dump_ident mb_id); ("mb_name", dump_string_loc mb_name); ("mb_loc", dump_loc mb_loc); ("mb_attributes", "__")] (fun tab ->
        let { mod_desc; mod_loc; mod_type; mod_env; mod_attributes } = mb_expr in
            atag tab "mod_env" "__";
            atag tab "mod_type" (dump_module_type mod_type);
            atag tab "mod_loc" (dump_loc mod_loc);
            process_module_description tab mod_env mod_desc
    )

and process_open_description tab {open_path; open_txt; open_override; open_loc; open_attributes} =
    tag tab "open_description" [("open_path", (dump_path open_path)); ("open_loc", dump_loc open_loc); ("open_txt", "__"); ("open_override", "__"); ("open_attributes", "__")]

and process_structure_item tab {str_desc; str_loc; str_env} =
    match str_desc with
    | Tstr_eval (e(*expression*), a(*attributes*)) -> mtag tab "Tstr_eval"
    | Tstr_value (rf(*rec_flag*), vbl(*value_binding list*)) ->
        stag tab "str_item" [("m", "Tstr_value"); ("str_loc", dump_loc str_loc); ("str_env", "__"); ("rec_flag", dump_rec_flag rf)] (fun tab ->
            List.iter (process_value_binding tab str_env) vbl
        );
    | Tstr_primitive (vd(*value_description*)) -> mtag tab "Tstr_primitive"
    | Tstr_type (tdl(*type_declaration list*)) -> mtag tab "Tstr_type"
    | Tstr_typext (te(*type_extension*)) -> mtag tab "Tstr_typext"
    | Tstr_exception (ec(*extension_constructor*)) -> mtag tab "Tstr_exception"
    | Tstr_module (mb(*module_binding*)) ->
        stag tab "str_item" [("m", "Tstr_module"); ("str_loc", dump_loc str_loc); ("str_env", "__")] (fun tab ->
            process_module_binding tab str_env mb
        );
    | Tstr_recmodule (mbl(*module_binding list*)) -> mtag tab "Tstr_recmodule"
    | Tstr_modtype (mtd(*module_type_declaration*)) -> mtag tab "Tstr_modtype"
    | Tstr_open od(*open_description*) ->
        stag tab "str_item" [("m", "Tstr_open"); ("str_loc", dump_loc str_loc); ("str_env", "__")] (fun tab ->
            process_open_description tab od
        );
    | Tstr_class (cl(*(cd(*class_declaration*), sl(*string list*), vf(*virtual_flag*)) list*)) -> mtag tab "Tstr_class"
    | Tstr_class_type (ctl(*(i(*Ident.t*), sl(*string loc*), ctd(*class_type_declaration*)) list*)) -> mtag tab "Tstr_class_type"
    | Tstr_include (id(*include_declaration*)) -> mtag tab "Tstr_include"
    | Tstr_attribute (a(*attribute*)) -> mtag tab "Tstr_attribute"

let process_implementation tab {str_items; str_type; str_final_env} =
    mtag tab "str_final_env" (* (dump_summary (Env.summary str_final_env)) *);
    mtag tab "str_types" (* (Util.List.dump (fun si -> dump_signature_item si) str_type) *);
    stag tab "str_items" [] (fun tab ->
        List.iter (fun item -> process_structure_item tab item) str_items;
    )

let print_cmt cmt =
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

    stag "" "doc" [] (fun tab ->
        stag tab "meta" [] (fun tab ->
            ttag tab "cmt_modname" cmt_modname;
            atag tab "cmt_value_dependencies" "__DEPS__";
            stag tab "cmt_comments" [] (fun tab -> (List.iter (fun (s, l) -> tag tab "comment" [("val", s); ("loc", dump_loc l)]) cmt_comments));
            stag tab "cmt_args" [] (fun tab -> Array.iter (atag tab "arg") cmt_args);
            atag tab "cmt_sourcefile" (Util.Option.getWithDefault "" cmt_sourcefile);
            atag tab "cmt_builddir" cmt_builddir;
            atag tab "cmt_loadpath" (Util.List.join ", " cmt_loadpath);
            atag tab "cmt_source_digest" (Util.Option.mapWithDefault "" (fun d -> Digest.to_hex d) cmt_source_digest);
            stag tab "cmt_imports" [] (fun tab -> List.iter (fun (s, d) -> atag tab "import" ("'" ^ s ^ "':" ^ (Util.Option.mapWithDefault "" (fun d -> Digest.to_hex d) d))) cmt_imports);
            atag tab "cmt_interface_digest" (Util.Option.mapWithDefault "" (fun d -> Digest.to_hex d) cmt_interface_digest);
            atag tab "cmt_use_summaries" (string_of_bool cmt_use_summaries);
            atag tab "cmt_initial_env" (dump_env cmt_initial_env);
        );
        stag tab "tree" [] (fun tab ->
            match cmt_annots with
            | Packed (signature, string_list) -> mtag tab "Packed"
            | Implementation structure -> process_implementation tab structure
            | Interface signature -> mtag tab "Interface"
            | Partial_implementation binary_part_array -> mtag tab "Partial_implementation"
            | Partial_interface binary_part_array -> mtag tab "Partial_interface";
        );
    )

