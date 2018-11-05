(* find better ocaml struct !? *)
type open_expr = {
    o_name: string;
    o_stamp: int;
    o_loc: Location.t;
    o_items: string list ref
}

type entry_kind =
    Value | Record | Type | TypeExt | ModType | Class | ClassType | Ident | Field

let string_of_entry_kind k =
    match k with
    | Value -> "V"
    | Record -> "R"
    | Type -> "T"
    | TypeExt -> "TE"
    | ModType -> "MT"
    | Class -> "C"
    | ClassType -> "CT"
    | Ident -> "I"
    | Field -> "F"

let read_type typ =
    Format.asprintf "%a" Printtyp.type_scheme typ