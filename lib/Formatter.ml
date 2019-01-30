open Location

let cr = Str.regexp "\n"
let sp = Str.regexp "[ ]+"

let clean_type str =
  Str.global_replace sp " " (Str.global_replace cr " " str)

let format_type t =
    clean_type (Format.asprintf "%a" Printtyp.type_scheme t)

let format_type_declaration id td =
    clean_type (Format.asprintf "%a" (Printtyp.type_declaration id) td)

let format_etype {Typedtree.exp_type; _} =
  format_type exp_type

let format_path p =
    Printtyp.string_of_path p

let format_ident i =
    Format.asprintf "%a" Printtyp.ident(*Ident.print*) i

let format_position pos =
  (string_of_int pos.Lexing.pos_lnum) ^ "." ^ (string_of_int (pos.pos_cnum - pos.pos_bol(*begining of line*) + 1(*idea numerotation*)))

let format_location {loc_start; loc_end; loc_ghost} =
  (format_position loc_start) ^ "," ^ (format_position loc_end)
