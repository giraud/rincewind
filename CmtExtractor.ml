open Typedtree
open Types
open RwTypes

(**
 Shortcut to read a type from an expression
 *)
let read_etype {exp_type; _} =
  RwTypes.read_type exp_type

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

(**
 Extract interesting info from an expression
 *)
let rec read_expression qname opens {exp_loc; exp_desc; _} =
  match exp_desc with
    | Texp_ident (path, loc, vd) ->
        let head = (Path.head path) in
        let name = (Ident.name head) in
        let stamp = head.stamp in
        (try
            let found = List.find (fun o -> o.o_name == name && o.o_stamp == stamp) !opens in
            found.o_items := (Path.name path) :: !(found.o_items)
        with e ->
            Printexc.to_string e; ())
    | Texp_apply (e, labels) -> read_expression qname opens e
    | Texp_let (_(*flag rec/nonrec*), vbl, e) ->
        List.iter (read_value_binding qname opens) vbl;
        read_expression qname opens e
    | Texp_function (_(*label*), cases, _(*partial*)) ->
        List.iter (read_case qname opens) cases
    | Texp_record (fields, _(*expression option*)) ->
        List.iter (fun (_, ld, e) -> Formatter.format_resolved_item ~kind:Record ~loc:e.exp_loc ~path:qname ~name:ld.lbl_name ~typ:(read_etype e)) fields
    | _ -> ()

(**
 Extract information from the right handler of a case
 *)
and read_case qname opens {c_rhs(*expression*); _} =
  read_expression qname opens c_rhs

(**
 Read binding amd print it on standard output
 *)
and read_value_binding qname opens {vb_pat; vb_expr; vb_attributes; vb_loc} =
    let {pat_loc; pat_env; pat_type; pat_desc; _} = vb_pat in
    let name = (read_pattern_desc pat_desc) in
    Formatter.format_resolved_item ~kind:Value ~loc:vb_pat.pat_loc ~path:qname ~name:name ~typ:(RwTypes.read_type pat_type);
    read_expression (Util.path qname name) opens vb_expr

(**
 Iterate on parsedtree
 *)
let rec read_structure_item qname opens {str_desc(*structure_item_desc*); _} =
    let read_module_expression qname {mod_desc} =
        match mod_desc with
            | Tmod_structure {str_items; _} -> List.iter (read_structure_item qname opens) str_items
            | _ -> () in

    match str_desc with
        | Tstr_open {open_path; open_override; open_loc; open_attributes} -> opens := {o_name=(Path.name open_path); o_stamp=(Path.head open_path).stamp; o_loc=open_loc; o_items = ref [] } :: !opens
        | Tstr_eval (ee, ea) -> read_expression qname opens ee
        | Tstr_value (rec_flag, vbl) -> List.iter (read_value_binding qname opens) vbl
        | Tstr_module {mb_id; mb_expr; mb_loc} -> read_module_expression (Util.path qname mb_id.name) mb_expr
        | _ -> ()

(**
 Extract cmt information for implementation pattern
 *)
let read_cmt cmt =
    let opens = ref [] in
    let {Cmt_format.cmt_modname; cmt_annots(*binary_annots*); _} = cmt in
    let _ = match cmt_annots with
        | Implementation s -> List.iter (fun item -> (read_structure_item cmt_modname opens item)) s.str_items
        | _ -> () in
    List.iter Formatter.format_open !opens
