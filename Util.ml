(**
 Creates a new path by concatenating the value to the existing path
 *)
let path p v =
  match p with
    | "" -> v
    | _ -> p ^ "." ^ v

let rec join_list separator items =
  match items with
    | [] -> ""
    | hd :: [] -> hd
    | hd :: tl -> hd ^ separator ^ (join_list separator tl)

(*
let deoptionalize (lst:'a option list) : 'a list =
    List.map (fun x -> match x with Some x -> x | None -> assert false) (List.filter (fun x -> x <> None) lst)

let join_array separator items =
  Array.fold_left (fun acc item -> acc ^ item ^ separator) "" items
*)