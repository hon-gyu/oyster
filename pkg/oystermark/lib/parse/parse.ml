(** Pre-resolution file-level parsing  *)

open Core
module Block_id = Block_id
module Callout = Callout
module Frontmatter = Frontmatter
module Heading_slug = Heading_slug
module Wikilink = Wikilink

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

(** Create the single-pass mapper that:
    - parses wikilinks in inline text nodes
    - tags block identifiers at paragraph ends
    - tags callout metadata on block quotes
    - stamps deduplicated heading slugs onto heading block meta

    Returns a fresh mapper each time (heading slug dedup requires per-document state). *)
let make_mapper () : Cmarkit.Mapper.t =
  let slug_seen = Hashtbl.create (module String) in
  let map_block (mapper : Cmarkit.Mapper.t) (block : Cmarkit.Block.t)
    : Cmarkit.Block.t Cmarkit.Mapper.result
    =
    match block with
    | Cmarkit.Block.Heading (h, meta) ->
      let orig_inline = Cmarkit.Block.Heading.inline h in
      let mapped_inline =
        Cmarkit.Mapper.map_inline mapper orig_inline
        |> Option.value ~default:orig_inline
      in
      let text = inline_to_plain_text mapped_inline in
      let slug = Heading_slug.dedup_slug slug_seen text in
      let meta' = Cmarkit.Meta.add Heading_slug.meta_key slug meta in
      let h' =
        Cmarkit.Block.Heading.make
          ?id:(Cmarkit.Block.Heading.id h)
          ~layout:(Cmarkit.Block.Heading.layout h)
          ~level:(Cmarkit.Block.Heading.level h)
          mapped_inline
      in
      Cmarkit.Mapper.ret (Cmarkit.Block.Heading (h', meta'))
    | _ ->
      (match Callout.map_callout mapper block with
       | `Map _ as result -> result
       | `Default -> Block_id.tag_block_id_meta mapper block)
  in
  Cmarkit.Mapper.make
    ~inline_ext_default:(fun _m i -> Some i)
    ~inline:Wikilink.parse
    ~block:map_block
    ()
;;

(** [of_string ?strict ?layout s] parses markdown string [s] into a
    {!Cmarkit.Doc.t} with frontmatter embedded as a {!Frontmatter.Frontmatter}
    block and wikilinks/block IDs parsed. Heading slugs are stamped onto
    heading block metadata. *)
let of_string ?(strict = false) ?(layout = false) (s : string) : Cmarkit.Doc.t =
  let open Cmarkit in
  let yaml_opt, body = Frontmatter.of_string s in
  let cmarkit_doc = Doc.of_string ~strict ~layout body in
  let body_doc = Mapper.map_doc (make_mapper ()) cmarkit_doc in
  match yaml_opt, Doc.block body_doc with
  | None, _ -> body_doc
  | Some yaml, Block.Blocks (blocks, meta) ->
    let blocks' = Frontmatter.Frontmatter yaml :: blocks in
    Doc.make (Block.Blocks (blocks', meta))
  | Some yaml, other ->
    Doc.make (Block.Blocks ([ Frontmatter.Frontmatter yaml; other ], Meta.none))
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
