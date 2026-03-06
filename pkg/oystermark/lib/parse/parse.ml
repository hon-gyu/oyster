(** Pre-resolution file-level parsing  *)

open Core
module Block_id = Block_id
module Frontmatter = Frontmatter
module Wikilink = Wikilink

type doc =
  { doc : Cmarkit.Doc.t
  ; frontmatter : Yaml.value option
  ; meta : Cmarkit.Meta.t
  }

(** The mapper that transforms a cmarkit Doc, parsing wikilinks in inline
    text nodes and tag block identifiers at paragraph ends to meta. *)
let mapper =
  Cmarkit.Mapper.make
    ~inline_ext_default:(fun _m i -> Some i)
    ~inline:Wikilink.parse
    ~block:Block_id.tag_block_id_meta
    ()
;;

(** [of_string ?strict ?layout s] parses markdown string [s] into a {!doc}
    with frontmatter extracted and wikilinks/block IDs resolved. *)
let of_string ?(strict = false) ?(layout = false) (s : string) : doc =
  let { Frontmatter.yaml; body } = Frontmatter.of_string s in
  let cmarkit_doc = Cmarkit.Doc.of_string ~strict ~layout body in
  let doc = Cmarkit.Mapper.map_doc mapper cmarkit_doc in
  { doc; frontmatter = yaml; meta = Cmarkit.Meta.none }
;;

let inline_to_plain_text (inline : Cmarkit.Inline.t) : string =
  let lines =
    Cmarkit.Inline.to_plain_text ~ext:Wikilink.to_plain_text ~break_on_soft:false inline
  in
  String.concat ~sep:"\n" (List.map lines ~f:(String.concat ~sep:""))
;;
