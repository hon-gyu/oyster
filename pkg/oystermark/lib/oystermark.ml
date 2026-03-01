module Wikilink = Wikilink
module Block_id = Block_id

(** Replace the last Text node's content in an inline tree. *)
let rec replace_last_text inline new_text =
  match inline with
  | Cmarkit.Inline.Text (_s, meta) -> Cmarkit.Inline.Text (new_text, meta)
  | Cmarkit.Inline.Inlines (inlines, meta) ->
    let rec go = function
      | [] -> []
      | [ x ] -> [ replace_last_text x new_text ]
      | x :: rest -> x :: go rest
    in
    Cmarkit.Inline.Inlines (go inlines, meta)
  | other -> other
;;

(** The mapper that transforms a cmarkit Doc, resolving wikilinks in inline
    text nodes and block identifiers at paragraph ends. *)
let mapper =
  Cmarkit.Mapper.make
    ~inline_ext_default:(fun _m i -> Some i)
    ~inline:(fun _m -> function
       | Cmarkit.Inline.Text (s, meta) ->
         (match Wikilink.scan s meta with
          | None -> Cmarkit.Mapper.default
          | Some inlines -> Cmarkit.Mapper.ret (Cmarkit.Inline.Inlines (inlines, meta)))
       | _ -> Cmarkit.Mapper.default)
    ~block: Block_id.tag_block_id_meta
    ()
;;


(** [of_string ?strict ?layout s] parses markdown string [s] into a cmarkit
    Doc with wikilinks and block IDs resolved via the mapper. *)
let of_string ?(strict = false) ?(layout = false) s =
  let doc = Cmarkit.Doc.of_string ~strict ~layout s in
  Cmarkit.Mapper.map_doc mapper doc
;;
