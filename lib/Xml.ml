open Printf

let incr indent = indent ^ "    "

let printf_unknown attrs =
    (match attrs with
    | None -> ()
    | Some(attr) -> List.iter (fun (k) -> printf " %s=\"-?-\"" k) attr)

(* ctag=children tag : a simple tag that has children nodes *)
let ctag indent name attrs extra children =
    printf "%s<%s" indent name;
    List.iter (fun (k,v) -> printf " %s=\"%s\"" k v) attrs;
    printf_unknown extra;
    printf ">\n";
    (children (incr indent));
    printf "%s</%s>\n" indent name

(* simple auto-closed tag *)
let tag indent name attrs extra =
    printf "%s<%s" indent name;
    List.iter (fun (k,v) -> printf " %s=\"%s\"" k v) attrs;
    printf_unknown extra;
    printf "/>\n"

(* atag=attribute tag : tag with one attribute *)
let atag indent name attr = tag indent name [("val", attr)] None

(* Xml.mtag=missing tag *)
let mtag indent name = tag indent "SKIPPED" [("name", name)] None

(* ttag=text tag : ie text node *)
let ttag indent name text = printf "%s<%s>%s</%s>\n" indent name text name
