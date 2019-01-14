open Typedtree
open Types
open RwTypes

let incr ident = ident ^ "    "

let dump_partial p = match p with | Partial -> "Partial" | Total -> "Total"

let dump_rec_flag rf = match rf with | Asttypes.Nonrecursive -> "Nonrecursive" | Recursive -> "Recursive"

let dump_type t = RwTypes.read_type t

(*
 ident.mli / Ident.t
    { stamp: int; name: string; mutable flags: int }
*)
let dump_ident i =
    Format.asprintf "%a" Printtyp.ident(*Ident.print*) i

let dump_longident_loc {Asttypes.txt(*LongIdent.t*); loc(*Location.t*)} =
    Format.asprintf "%a" Printtyp.longident txt

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
  "«loc:" ^ (dump_pos loc_start) ^ ":" ^ (dump_pos loc_end) ^ ":" ^ (string_of_bool loc_ghost) ^ "»"

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
let rec read_pattern_desc pat_env pat_type pat_desc =
  match pat_desc with
    | Tpat_any -> "«tpat_any»"
    | Tpat_var (i(*Ident.t*), sl(*string loc*)) ->
        "«tpat_var:" ^ (dump_ident i) ^ "|" ^ (dump_string_loc sl) ^ "»"
    | Tpat_alias (pattern, ident, loc) -> "«ALIAS»"
    | Tpat_constant (c) -> "«CONSTANT»"
    | Tpat_tuple (patternl) -> "«TUPLE»"
    | Tpat_construct (loc, constr_desc, patternl) -> "«CONSTR»"
    | Tpat_variant (label, pattern, row_desc) -> "«VARIANT»"
    | Tpat_record (rl, flag) -> "«RECORD»"
    | Tpat_array (patternl) -> "«ARRAY»"
    | Tpat_or (pattern, pattern', row_desc) -> "«OR»"
    | Tpat_lazy (pattern) -> "«LAZY»"

let print_value_binding_pattern indent {pat_desc; pat_loc; pat_extra; pat_type; pat_env; pat_attributes} =
  Printf.printf "%svb_pat %s " indent (dump_loc pat_loc);
  Printf.printf "%s " (read_pattern_desc pat_env pat_type pat_desc);
  Printf.printf "<type:%s|<>|%s> " (Formatter.clean_type (RwTypes.read_type pat_type)) (Formatter.clean_type (print_type_scheme pat_env pat_type));
  Printf.printf "<extra:%s> " "«EXTRA»";
  (* attributes are metadata *)
  Printf.printf "<attrs:%s> " (Util.List.dump (fun (sl, payload) -> "«ATTR»") pat_attributes);
  Printf.printf "\n";
  Printf.printf "%sENV: %s\n" (incr indent) (*(dump_env pat_env)*)"__ENV__"

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

let print_value_description indent { val_type; val_kind; val_loc; val_attributes } =
    Printf.printf "%svd:: %s loc:%s type:%s\n" indent (dump_value_kind val_kind) (dump_loc val_loc) (dump_type val_type)

let rec print_case indent {c_lhs(*pattern*); c_guard(*expression option*); c_rhs(*expression*)} =
    Printf.printf "%slhs\n" indent;
    Printf.printf "%sguard\n" indent;
    print_expression indent c_rhs

and print_expression indent { exp_desc; exp_loc; exp_extra; exp_type; exp_env; exp_attributes } =
    Printf.printf "%sexpression %s\n" indent (dump_loc exp_loc);
    let indent' = incr indent in
        (*dump_env exp_env;*)
        match exp_desc with
        | Texp_ident (p(*Path.t*), lil(*Longident.t loc*), vd(*Types.value_description*)) ->
            Printf.printf "%sTexp_ident path:%s longidentloc:%s\n" indent' (dump_path p) (dump_longident_loc lil);
            print_value_description indent' vd
        | Texp_constant c(*constant*) ->
            Printf.printf "%sTexp_constant\n" indent'
        | Texp_let _(*rec_flag * value_binding list * expression*) ->
            Printf.printf "%sTexp_let\n" indent'
        | Texp_function (lb(*label*), cl(*case list*), pa(*partial*)) ->
            Printf.printf "%sTexp_function <label:%s> <partial:%s>\n" indent' lb (dump_partial pa);
            Printf.printf "%scases:\n" indent';
            List.iter (print_case (incr indent')) cl
        | Texp_apply (e(*expression*),  leol(*(label * expression option * optional) list*)) ->
            Printf.printf "%sTexp_apply\n" indent';
            print_expression indent' e;
            List.iter (fun (label, eo, o) ->
                Printf.printf "%s«label:%s»\n" (incr indent') label;
                match eo with
                | None -> Printf.printf "%s«expression:NONE»\n" indent'
                | Some e -> print_expression (incr indent') e
            ) leol
        | Texp_match (e(*expression*), cl(*case list*), cl'(*case list*), pa(*partial*)) ->
            Printf.printf "%sTexp_match <%s>\n" indent' (dump_partial pa);
            print_expression indent' e;
            List.iter (print_case (incr indent')) cl;
            List.iter (print_case (incr indent')) cl';
        | Texp_try _(*expression * case list*) -> Printf.printf "Texp_try"
        | Texp_tuple _(*expression list*) -> Printf.printf "Texp_tuple"
        | Texp_construct (lil(*Longident.t loc*), cd(*constructor_description*), el(*expression list*)) ->
            Printf.printf "%sTexp_construct «id:%s»\n" indent' (dump_longident_loc lil);
            List.iter (print_expression (incr indent')) el
        | Texp_variant _(*label * expression option*) -> Printf.printf "Texp_variant"
        | Texp_record _(* (Longident.t loc * label_description * expression) list * expression option*) -> Printf.printf "Texp_record"
        | Texp_field _(*expression * Longident.t loc * label_description*) -> Printf.printf "Texp_field"
        | Texp_setfield _(*expression * Longident.t loc * label_description * expression*) -> Printf.printf "Texp_setfield"
        | Texp_array _(*expression list*) -> Printf.printf "Texp_array"
        | Texp_ifthenelse _(*expression * expression * expression option*) -> Printf.printf "Texp_ifthenelse"
        | Texp_sequence _(*expression * expression*) -> Printf.printf "Texp_sequence"
        | Texp_while _(*expression * expression*) -> Printf.printf "Texp_while"
        | Texp_for _(* Ident.t * Parsetree.pattern * expression * expression * direction_flag * expression*) -> Printf.printf "Texp_for"
        | Texp_send _(*expression * meth * expression option*) -> Printf.printf "Texp_send"
        | Texp_new _(*Path.t * Longident.t loc * Types.class_declaration*) -> Printf.printf "Texp_new"
        | Texp_instvar _(*Path.t * Path.t * string loc*) -> Printf.printf "Texp_instvar"
        | Texp_setinstvar _(*Path.t * Path.t * string loc * expression*) -> Printf.printf "Texp_setinstvar"
        | Texp_override _(*Path.t * (Path.t * string loc * expression) list*) -> Printf.printf "Texp_override"
        | Texp_letmodule _(*Ident.t * string loc * module_expr * expression*) ->
            Printf.printf "%sTexp_letmodule\n" indent'
        | Texp_assert _(*expression*) -> Printf.printf "Texp_assert"
        | Texp_lazy _(*expression*) -> Printf.printf "Texp_lazy"
        | Texp_object _(*class_structure * string list*) -> Printf.printf "Texp_object"
        | Texp_pack _(*module_expr*) -> Printf.printf "Texp_pack"

let print_value_binding indent env {vb_pat; vb_expr; vb_attributes; vb_loc} =
    Printf.printf "%svalue binding %s «VB_ATTRIBUTES»\n" indent (dump_loc vb_loc);
    print_value_binding_pattern (incr indent) vb_pat;
    print_expression (incr indent) vb_expr

let rec print_module_description indent env mod_desc =
    match mod_desc with
    | Tmod_ident (pa(*Path.t*), il(*Longident.t loc*)) -> Printf.printf "%s%s" indent "Tmod_ident"
    | Tmod_structure ({str_items; str_type; str_final_env}(*structure*)) ->
        Printf.printf "%sstr_types: %s\n" indent (Util.List.dump (fun si -> dump_signature_item si) str_type);
        Printf.printf "%sstr_items:\n" indent;
        List.iter (fun item -> print_structure_item (incr indent) item) str_items
    | Tmod_functor (i(*Ident.t*), sl(*string loc*), mto(*module_type option*),  me(*module_expr*)) -> Printf.printf "%s%s" indent "Tmod_functor"
    | Tmod_apply (me(*module_expr*), me'(*module_expr*), mc(*module_coercion*)) -> Printf.printf "%s%s" indent "Tmod_apply"
    | Tmod_constraint (me(*module_expr*), mt(*Types.module_type*), mtc(*module_type_constraint*), mc(*module_coercion*)) -> Printf.printf "%s%s" indent "Tmod_constraint"
    | Tmod_unpack (e(*expression*), mt(*Types.module_type*)) ->
        Printf.printf "%s%s" indent "Tmod_unpack"

and print_module_binding indent env {mb_id; mb_name; mb_expr; mb_attributes; mb_loc} =
    Printf.printf "%smodule_binding: «id:%s» «loc:%s» «attrs:%s» " indent (dump_ident mb_id) (dump_loc mb_loc) "ATTR";
    let { mod_desc; mod_loc; mod_type; mod_env; mod_attributes; } = mb_expr in
    Printf.printf "«mod_type:%s» «mod_attrs:%s»\n" (dump_module_type mod_type) "ATTRS";
    print_module_description indent mod_env mod_desc;
    Printf.printf "\n"

and print_open_description indent {open_path; open_txt; open_override; open_loc; open_attributes} =
    Printf.printf "%s%s path:%s OPEN_TXT OPEN_OVERRIDE OPEN_ATTRIBUTES \n" indent (dump_loc open_loc) (dump_path open_path)

and print_structure_item indent {str_desc; str_loc; str_env} =
    Printf.printf "%sstr_item: «ENV» %s " indent (dump_loc str_loc);

    match str_desc with
    | Tstr_eval (e(*expression*), a(*attributes*)) -> Printf.printf "Tstr_eval\n"
    | Tstr_value (rf(*rec_flag*), vbl(*value_binding list*)) ->
        Printf.printf "Tstr_value %s\n" (dump_rec_flag rf);
        List.iter (print_value_binding (incr indent) str_env) vbl
    | Tstr_primitive (vd(*value_description*)) -> Printf.printf "Tstr_primitive\n"
    | Tstr_type (tdl(*type_declaration list*)) -> Printf.printf "Tstr_type\n"
    | Tstr_typext (te(*type_extension*)) -> Printf.printf "Tstr_typext\n"
    | Tstr_exception (ec(*extension_constructor*)) -> Printf.printf "Tstr_exception\n"
    | Tstr_module (mb(*module_binding*)) ->
        Printf.printf "Tstr_module\n";
        print_module_binding (incr indent) str_env mb
    | Tstr_recmodule (mbl(*module_binding list*)) -> Printf.printf "Tstr_recmodule\n"
    | Tstr_modtype (mtd(*module_type_declaration*)) -> Printf.printf "Tstr_modtype\n"
    | Tstr_open (od(*open_description*)) ->
        Printf.printf "Tstr_open\n";
        print_open_description (incr indent) od
    | Tstr_class (cl(*(cd(*class_declaration*), sl(*string list*), vf(*virtual_flag*)) list*)) -> Printf.printf "Tstr_class\n"
    | Tstr_class_type (ctl(*(i(*Ident.t*), sl(*string loc*), ctd(*class_type_declaration*)) list*)) -> Printf.printf "Tstr_class_type\n"
    | Tstr_include (id(*include_declaration*)) -> Printf.printf "Tstr_include\n"
    | Tstr_attribute (a(*attribute*)) -> Printf.printf "Tstr_attribute\n"

let print_implementation {str_items; str_type; str_final_env} =
    Printf.printf "str_final_env: %s\n" (dump_summary (Env.summary str_final_env));
    Printf.printf "str_types: %s\n" (Util.List.dump (fun si -> dump_signature_item si) str_type);
    Printf.printf "str_items:\n";
    List.iter (fun item -> print_structure_item "  " item) str_items;
    Printf.printf "\n"

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

    Printf.printf "---META---\n\n";
    Printf.printf "cmt_modname: %s\n" cmt_modname;
    Printf.printf "cmt_value_dependencies: %s\n" "xXx";
    Printf.printf "cmt_comments: [%s]\n" (Util.List.dump (fun (s, l) -> "'" ^ s ^ "':" ^ (dump_loc l)) cmt_comments);
    Printf.printf "cmt_args: [%s]\n" (Util.Array.join ", " cmt_args);
    Printf.printf "cmt_sourcefile: %s\n" (Util.Option.getWithDefault "" cmt_sourcefile);
    Printf.printf "cmt_builddir: %s\n" cmt_builddir;
    Printf.printf "cmt_loadpath: [%s]\n" (Util.List.join ", " cmt_loadpath);
    Printf.printf "cmt_source_digest: %s\n" (Util.Option.mapWithDefault "" (fun d -> Digest.to_hex d) cmt_source_digest);
    Printf.printf "cmt_imports: %s\n" (Util.List.dump (fun (s, d) -> "'" ^ s ^ "':" ^ (Util.Option.mapWithDefault "" (fun d -> Digest.to_hex d) d)) cmt_imports);
    Printf.printf "cmt_interface_digest: %s\n" (Util.Option.mapWithDefault "" (fun d -> Digest.to_hex d) cmt_interface_digest);
    Printf.printf "cmt_use_summaries: %b\n" cmt_use_summaries;
    Printf.printf "cmt_initial_env: %s\n" (dump_env cmt_initial_env);
    Printf.printf "\n";

    Printf.printf "--TREE--\n\n";
    match cmt_annots with
    | Packed (si(*Types.signature *), sl(*string list*)) -> Printf.printf "Packed"
    | Implementation s(*structure*) -> print_implementation s
    | Interface s(*signature*) -> Printf.printf "Interface"
    | Partial_implementation bpa(*binary_part array*) -> Printf.printf "Partial_implementation"
    | Partial_interface bpa(*binary_part array*) -> Printf.printf "Partial_interface"
