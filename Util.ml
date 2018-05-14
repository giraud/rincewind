
let qname_add q v =
  match q with
    | "" -> v
    | _ -> q ^ "." ^ v

let rec join_list separator items =
  match items with
    | [] -> ""
    | hd :: [] -> hd
    | hd :: tl -> hd ^ separator ^ (join_list separator tl)
