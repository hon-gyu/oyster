(** Pre-resolution file-level parsing  *)

open Core
module Block_id = Block_id
module Frontmatter = Frontmatter
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

(** [of_string ?strict ?layout s] parses markdown string [s] into a
    {!Cmarkit.Doc.t} with frontmatter embedded as a {!Frontmatter.Frontmatter}
    block and wikilinks/block IDs parsed. *)
let of_string ?(strict = false) ?(layout = false) (s : string) : Cmarkit.Doc.t =
  let open Cmarkit in
  let yaml_opt, body = Frontmatter.of_string s in
  let cmarkit_doc = Doc.of_string ~strict ~layout body in
  let body_doc = Mapper.map_doc mapper cmarkit_doc in
  match yaml_opt, Doc.block body_doc with
  | None, _ -> body_doc
  | Some yaml, Block.Blocks (blocks, meta) ->
    let blocks' = Frontmatter.Frontmatter yaml :: blocks in
    Doc.make (Block.Blocks (blocks', meta))
  | Some yaml, other ->
    Doc.make (Block.Blocks ([ Frontmatter.Frontmatter yaml; other ], Meta.none))
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
    let block (c : Cmarkit_renderer.context) = function
      | Frontmatter.Frontmatter y ->
        Cmarkit_renderer.Context.string c (Frontmatter.to_commonmark y);
        true
      | _ -> false
    in
    Cmarkit_renderer.make ~inline ~block ()
  in
  let default = Cmarkit_commonmark.renderer () in
  let r = Cmarkit_renderer.compose default custom in
  Cmarkit_renderer.doc_to_string r doc
;;
