open Location

let quote_regexp = Str.regexp "\""
let cr_regexp = Str.regexp "\n"
let sp_regexp = Str.regexp "[ ]+"

let unquote str =
  match Str.first_chars str 1 with "\"" -> StringLabels.sub str ~pos:1 ~len:(StringLabels.length str - 2) | _ -> str

let x = 12
let escape_string str = Str.global_replace quote_regexp "'" str
let normalize_string str = Str.global_replace sp_regexp " " (Str.global_replace cr_regexp " " str)
let format_arg (a : Asttypes.arg_label) = match a with Nolabel -> "" | Labelled name -> name | Optional name -> name
let format_type t = normalize_string (Format.asprintf "%a" Printtyp.type_scheme t)
let format_type_declaration id td = normalize_string (Format.asprintf "%a" (Printtyp.type_declaration id) td)
let format_etype { Typedtree.exp_type; _ } = format_type exp_type
let format_path p = Printtyp.string_of_path p
let format_ident_o i = match i with None -> "" | Some i -> Format.asprintf "%a" Printtyp.ident (*Ident.print*) i
let format_ident i = Format.asprintf "%a" Printtyp.ident (*Ident.print*) i
let format_lident li = Format.asprintf "%a" Printtyp.longident li

let format_position pos =
  string_of_int pos.Lexing.pos_lnum ^ "." ^ string_of_int (pos.pos_cnum - pos.pos_bol (*begining of line*))

let format_location { loc_start; loc_end; _ (*loc_ghost*) } = format_position loc_start ^ "," ^ format_position loc_end