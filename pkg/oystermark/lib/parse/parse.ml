(** Pre-resolution file-level parsing  *)

open Core
module Block_id = Block_id
module Wikilink = Wikilink

(** The mapper that transforms a cmarkit Doc, parsing wikilinks in inline
    text nodes and tag block identifiers at paragraph ends to meta. *)
let mapper =
  Cmarkit.Mapper.make
    ~inline_ext_default:(fun _m i -> Some i)
    ~inline:Wikilink.parse
    ~block:Block_id.tag_block_id_meta
    ()
;;

(** [of_string ?strict ?layout s] parses markdown string [s] into a cmarkit
    Doc with wikilinks and block IDs resolved via the mapper. *)
let of_string ?(strict = false) ?(layout = false) (s : string) : Cmarkit.Doc.t =
  let doc = Cmarkit.Doc.of_string ~strict ~layout s in
  Cmarkit.Mapper.map_doc mapper doc
;;

let inline_to_plain_text (inline : Cmarkit.Inline.t) : string =
  let lines =
    Cmarkit.Inline.to_plain_text ~ext:Wikilink.to_plain_text ~break_on_soft:false inline
  in
  String.concat ~sep:"\n" (List.map lines ~f:(String.concat ~sep:""))
;;
