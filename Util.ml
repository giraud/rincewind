(**
 Creates a new path by concatenating the value to the existing path
 *)
let path p v =
  match p with
    | "" -> v
    | _ -> p ^ "." ^ v

module List = struct
  let rec join separator items =
    match items with
    | [] -> ""
    | hd :: [] -> hd
    | hd :: tl -> hd ^ separator ^ (join separator tl)

  let dump f l = join ", " (List.map f l)
end

module Array = struct
  let join separator items =
    Array.fold_left (fun acc item -> acc ^ item ^ separator) "" items
end

(*
let deoptionalize (lst:'a option list) : 'a list =
    List.map (fun x -> match x with Some x -> x | None -> assert false) (List.filter (fun x -> x <> None) lst)
*)

module Option = struct
  let getWithDefault default data =
    match data with
    | None -> default
    | Some s -> s

  let mapWithDefault default f data =
    match data with
    | None -> default
    | Some d -> f d
end