open Location
open Lexing

let r = Str.regexp "\n"

let clean_type str =
  Str.global_replace r " " str

let format_position pos =
  (string_of_int pos.pos_lnum) ^ "." ^ (string_of_int (pos.pos_cnum - pos.pos_bol(*begining of line*) + 1))

let format_location {loc_start; loc_end; loc_ghost} =
  (format_position loc_start) (*^ "," ^ (position_to_string loc_end)*) ^ "|"

(*loc_start loc_end =>
type position = {
  pos_fname : string;
  pos_lnum : int;  line number
  pos_bol : int;   beginnig of line
  pos_cnum : int;  column number
}*)
let full_location_to_string {loc_start; loc_end; loc_ghost} =
  "start: " ^ (string_of_int loc_start.pos_lnum) ^ "/" ^ (string_of_int loc_start.pos_bol) ^ "/" ^ (string_of_int loc_start.pos_cnum) ^
  " end: " ^ (string_of_int loc_end.pos_lnum) ^ "/" ^ (string_of_int loc_end.pos_bol) ^ "/" ^ (string_of_int loc_end.pos_cnum) ^
  " ghost: " ^ (string_of_bool loc_ghost)

let format_resolved_item {RwTypes.i_kind; i_loc; i_path; i_name; i_type; i_comment} =
    i_kind ^ "|" ^ (format_location i_loc) ^ i_path ^ "|" ^ i_name ^ "|" ^ (clean_type i_type)(* ^ "|" ^ i_comment*)
