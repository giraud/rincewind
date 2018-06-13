(* find better ocaml struct !? *)
type open_expr = {
    o_name: string;
    o_stamp: int;
    o_loc: Location.t;
    o_items: string list ref
}

type entry_kind =
    Value | Record | Type | TypeExt | ModType | Class | ClassType | Ident

let read_type typ =
    Format.asprintf "%a" Printtyp.type_scheme typ