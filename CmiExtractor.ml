let read_type typ =
  Formatter.clean_type (Format.asprintf "%a" Printtyp.type_scheme typ)

(*
signature = signature_item list

and signature_item =
    Sig_value of Ident.t * value_description
  | Sig_type of Ident.t * type_declaration * rec_status
  | Sig_typext of Ident.t * extension_constructor * ext_status
  | Sig_module of Ident.t * module_declaration * rec_status
  | Sig_modtype of Ident.t * modtype_declaration
  | Sig_class of Ident.t * class_declaration * rec_status
  | Sig_class_type of Ident.t * class_type_declaration * rec_status
*)
let rec parse_cmi_sign path signature =
    match signature with
        | Types.Sig_value (ident, desc) -> Formatter.format_resolved_item ~kind:Value ~loc:desc.val_loc ~path:path ~name:ident.name ~typ:(read_type desc.val_type)
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
type cmi_infos =
    cmi_name : string; module nama
    cmi_sign : Types.signature_item list;
    cmi_crcs : (string * Digest.t option) list;
    cmi_flags : pers_flags list;
*)
let read_cmi cmi =
    let {Cmi_format.cmi_name; cmi_sign; _} = cmi in
    List.iter (parse_cmi_sign "") cmi_sign
