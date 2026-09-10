module Url = Odoc_document.Url

let prefix (kind : Url.Path.kind) =
  Format.asprintf "%a" Url.Path.pp_disambiguating_prefix kind
;;

let note (url : Url.Path.t) =
  Url.Path.to_list url
  |> List.map (fun (kind, name) -> prefix kind ^ name)
  |> String.concat "/"
;;

let anchor a =
  String.map
    (function
      | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-') as c -> c
      | _ -> '-')
    a
;;

(* [Printf]'s [%S] would escape the UTF-8 an odoc anchor may carry; a djot
   attribute value needs only its quote and backslash escaped. *)
let quote s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
       if Char.equal c '"' || Char.equal c '\\' then Buffer.add_char b '\\';
       Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b
;;

let attribute a =
  let id = anchor a in
  if String.equal id a
  then Printf.sprintf "{#%s}" id
  else Printf.sprintf "{#%s odoc-anchor=%s}" id (quote a)
;;
