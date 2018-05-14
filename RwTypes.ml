type resolved_item_description = {
    i_kind:    string;
    i_loc:     Location.t;
    i_path:    string;
    i_name:    string;
    i_type:    string;
}

type resolved_item =
    | Ignore
    | Single of resolved_item_description
    | Multiple of resolved_item_description list

let rec flat_resolved_items resolved_items =
    match resolved_items with
        | [] -> []
        | Ignore :: tl -> flat_resolved_items tl
        | Single i :: tl -> List.append [i] (flat_resolved_items tl)
        | Multiple l :: tl -> List.append l (flat_resolved_items tl)
