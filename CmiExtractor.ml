(**
 Cmi signature extractor
 *)
let rec parse_cmi_sign path signature =
    match signature with
        | Types.Sig_value (ident, desc) -> Formatter.format_resolved_item ~kind:Value ~loc:desc.val_loc ~path:path ~name:ident.name ~typ:(RwTypes.read_type desc.val_type)
        | Types.Sig_type (ident, decl, status) ->  Formatter.format_resolved_item ~kind:Type ~loc:decl.type_loc ~path:path ~name:ident.name ~typ:""
        | Types.Sig_typext (ident, constr, status) -> Formatter.format_resolved_item ~kind:TypeExt ~loc:constr.ext_loc ~path:path ~name:ident.name ~typ:""
        | Types.Sig_module (ident, decl, status) -> (
            match decl.md_type with
                | Mty_signature signature -> List.iter (parse_cmi_sign (Util.path path ident.name)) signature
                | _ -> ()
        )
        | Types.Sig_modtype (ident, decl) -> Formatter.format_resolved_item ~kind:ModType ~loc:decl.mtd_loc ~path:path ~name:ident.name ~typ:""
        | Types.Sig_class (ident, decl, status) -> Formatter.format_resolved_item ~kind:Class ~loc:decl.cty_loc ~path:path ~name:ident.name ~typ:""
        | Types.Sig_class_type (ident, decl, status) -> Formatter.format_resolved_item ~kind:ClassType ~loc:decl.clty_loc ~path:path ~name:ident.name ~typ:""

(**
 Very naive cmi reader
 *)
let read_cmi cmi =
    let {Cmi_format.cmi_name; cmi_sign; _} = cmi in
    List.iter (parse_cmi_sign "") cmi_sign
