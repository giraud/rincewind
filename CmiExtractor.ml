open RwTypes
open Formatter

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
        | Types.Sig_value (ident, desc) -> Single {i_kind="V"; i_loc=desc.val_loc; i_path=path; i_name=ident.name; i_type=(read_type desc.val_type)}
        | Types.Sig_type (ident, decl, status) ->  Single {i_kind="T"; i_loc=decl.type_loc; i_path=path; i_name=ident.name; i_type=""}
        | Types.Sig_typext (ident, constr, status) -> Single {i_kind="E"; i_loc=constr.ext_loc; i_path=path; i_name=ident.name; i_type=""}
        | Types.Sig_module (ident, decl, status) -> (
            let x = match decl.md_type with
                | Mty_signature signature -> List.map (parse_cmi_sign (Util.path path ident.name)) signature
                | _ -> []
                in
            Multiple (flat_resolved_items x)
        )
        | Types.Sig_modtype (ident, decl) -> Single {i_kind="N"; i_loc=decl.mtd_loc; i_path=path; i_name=ident.name; i_type=""}
        | Types.Sig_class (ident, decl, status) -> Single {i_kind="C"; i_loc=decl.cty_loc; i_path=path; i_name=ident.name; i_type=""}
        | Types.Sig_class_type (ident, decl, status) -> Single {i_kind="D"; i_loc=decl.clty_loc; i_path=path; i_name=ident.name; i_type=""}

(**
type cmi_infos =
    cmi_name : string; module nama
    cmi_sign : Types.signature_item list;
    cmi_crcs : (string * Digest.t option) list;
    cmi_flags : pers_flags list;
*)
let parse_cmi cmi =
    let {Cmi_format.cmi_name; cmi_sign; _} = cmi in
    let resolved_items = List.map (parse_cmi_sign "") cmi_sign in
    List.map to_string resolved_items
