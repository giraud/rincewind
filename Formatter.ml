open Location
open Lexing
open RwTypes

let r = Str.regexp "\n"

let clean_type str =
  Str.global_replace r " " str

let format_position pos =
  (string_of_int pos.pos_lnum) ^ "." ^ (string_of_int (pos.pos_cnum - pos.pos_bol(*begining of line*) + 1))

let format_location {loc_start; loc_end; loc_ghost} =
  format_position loc_start (*^ "," ^ (position_to_string loc_end)*)

let format_resolved_item ~kind ~loc ~path ~name ~typ =
    let kind_name = match kind with | Value -> "V" | Record -> "R" | _ -> "X" in
    Printf.printf "%s|%s|%s|%s|%s\n" kind_name (format_location loc) path name typ
