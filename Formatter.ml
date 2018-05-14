open Location
open Lexing
open RwTypes
open Util

let r = Str.regexp "\n"

let clean_type str =
  Str.global_replace r " " str

let format_position pos =
  (string_of_int pos.pos_lnum) ^ "." ^ (string_of_int (pos.pos_cnum - pos.pos_bol(*begining of line*) + 1))

let format_location {loc_start; loc_end; loc_ghost} =
  format_position loc_start (*^ "," ^ (position_to_string loc_end)*)

let format_resolved_item {RwTypes.i_kind; i_loc; i_path; i_name; i_type} =
    i_kind ^ "|" ^ (format_location i_loc) ^ "|" ^ i_path ^ "|" ^ i_name ^ "|" ^ (clean_type i_type)

let to_string item = match item with
                       | Ignore -> ""
                       | Single i -> format_resolved_item i
                       | Multiple i -> join_list "\n" (List.map format_resolved_item i)
