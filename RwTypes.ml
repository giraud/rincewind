type entry_kind =
    Value | Record | Type | TypeExt | ModType | Class | ClassType

let read_type typ =
    Format.asprintf "%a" Printtyp.type_scheme typ