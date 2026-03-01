module Wikilink = Wikilink
module Block_id = Block_id

(** Extract block ID from the last text node of a paragraph's inline. *)
let extract_block_id_from_inline (inline : Cmarkit.Inline.t) =
  (* Find and modify the last Text node in the inline tree *)
  let rec last_text = function
    | Cmarkit.Inline.Text (s, _meta) -> Some s
    | Cmarkit.Inline.Inlines (inlines, _meta) ->
      let rec try_last = function
        | [] -> None
        | [ x ] -> last_text x
        | _ :: rest -> try_last rest
      in
      try_last inlines
    | _ -> None
  in
  match last_text inline with
  | None -> None
  | Some s -> Block_id.extract_trailing s
;;

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
    ~block:(fun _m -> function
       | Cmarkit.Block.Paragraph (p, meta) ->
         let inline = Cmarkit.Block.Paragraph.inline p in
         (match extract_block_id_from_inline inline with
          | None -> Cmarkit.Mapper.default
          | Some (text_before, block_id) ->
            let new_inline =
              if String.equal text_before ""
              then Cmarkit.Inline.empty
              else replace_last_text inline text_before
            in
            let new_para = Cmarkit.Block.Paragraph.make new_inline in
            let new_meta = Cmarkit.Meta.add Block_id.meta_key block_id meta in
            Cmarkit.Mapper.ret (Cmarkit.Block.Paragraph (new_para, new_meta)))
       | _ -> Cmarkit.Mapper.default)
    ()
;;


(** [of_string ?strict ?layout s] parses markdown string [s] into a cmarkit
    Doc with wikilinks and block IDs resolved via the mapper. *)
let of_string ?(strict = false) ?(layout = false) s =
  let doc = Cmarkit.Doc.of_string ~strict ~layout s in
  Cmarkit.Mapper.map_doc mapper doc
;;
