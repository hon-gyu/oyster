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

(** Render inlines to plain text, losing their markdown syntax. Used in rendering
    heading to plain text. *)
let inline_to_plain_text (inline : Cmarkit.Inline.t) : string =
  let lines =
    Cmarkit.Inline.to_plain_text
      ~ext:(fun ~break_on_soft inline ->
        match inline with
        | Wikilink.Ext_wikilink (wl, _meta) ->
          let text = Wikilink.to_plain_text wl in
          Cmarkit.Inline.Text (text, Cmarkit.Meta.none)
        | other -> other)
      ~break_on_soft:false
      inline
  in
  String.concat ~sep:"\n" (List.map lines ~f:(String.concat ~sep:""))
;;

let commonmark_of_doc (doc : Cmarkit.Doc.t) : string =
  let custom =
    let inline (c : Cmarkit_renderer.context) = function
      | Wikilink.Ext_wikilink (wl, _) ->
        Cmarkit_renderer.Context.string c (Wikilink.to_commonmark wl);
        true
      | _ -> false
    in
    Cmarkit_renderer.make ~inline ()
  in
  let default = Cmarkit_commonmark.renderer () in
  let r = Cmarkit_renderer.compose default custom in
  Cmarkit_renderer.doc_to_string r doc
;;
