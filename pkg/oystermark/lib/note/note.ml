module Anchor = Anchor
module Link = Link
module Transclusion = Transclusion
module Node = Node
module Query = Query

module Private = struct
  module Address_utils = Address_utils
end

open Core

type t =
  { frontmatter : Yaml.value option
  ; body : Cmarkit.Doc.t
  }

let of_doc (doc : Cmarkit.Doc.t) : t =
  let open Cmarkit in
  let strip blocks =
    match blocks with
    | Parse.Frontmatter.Frontmatter y :: rest -> Some y, rest
    | _ -> None, blocks
  in
  let frontmatter, block =
    match Doc.block doc with
    | Block.Blocks (blocks, meta) ->
      let fm, rest = strip blocks in
      fm, Block.Blocks (rest, meta)
    | Parse.Frontmatter.Frontmatter y -> Some y, Block.empty
    | b -> None, b
  in
  { frontmatter; body = Doc.make ~defs:(Doc.defs doc) block }
;;

let to_doc (n : t) : Cmarkit.Doc.t =
  let open Cmarkit in
  match n.frontmatter with
  | None -> n.body
  | Some y ->
    let block =
      match Doc.block n.body with
      | Block.Blocks (blocks, meta) ->
        Block.Blocks (Parse.Frontmatter.Frontmatter y :: blocks, meta)
      | b -> Block.Blocks ([ Parse.Frontmatter.Frontmatter y; b ], Meta.none)
    in
    Doc.make ~defs:(Doc.defs n.body) block
;;

let select (query : Query.t) (n : t) : t = { n with body = Query.extract query n.body }

type source =
  { path : string
  ; fragment : Cmarkit.Inline.Wikilink.fragment option
  ; note : t
  }

let expand
      ?(max_depth = 5)
      ~(env : from:string -> Link.Ref.t -> source option)
      ~(path : string)
      (n : t)
  : t
  =
  let rec go ~depth ~expanding ~from (doc : Cmarkit.Doc.t) : Cmarkit.Doc.t =
    let embed (ref : Link.Ref.t) ~(depth_fallback : Cmarkit.Block.t)
      : Cmarkit.Block.t option
      =
      match env ~from ref with
      | None -> None
      | Some _ when depth >= max_depth -> Some depth_fallback
      | Some source ->
        let body =
          if List.mem expanding source.path ~equal:String.equal
          then source.note.body
          else
            go
              ~depth:(depth + 1)
              ~expanding:(source.path :: expanding)
              ~from:source.path
              source.note.body
        in
        Some
          (Transclusion.transclude
             ~depth:(depth + 1)
             ~source_path:source.path
             ~fragment:source.fragment
             (Transclusion.non_fm_blocks body))
    in
    let mapper =
      Cmarkit.Mapper.make
        ~block_ext_default:(fun _m b -> Some b)
        ~inline_ext_default:(fun _m i -> Some i)
        ~block:(fun _mapper block ->
          match block with
          | Cmarkit.Block.Paragraph (p, _) ->
            (match
               Transclusion.embed_source_of_inline (Cmarkit.Block.Paragraph.inline p)
             with
             | None -> Cmarkit.Mapper.default
             | Some embed_source ->
               let ref, depth_fallback =
                 match embed_source with
                 | Wikilink_embed (wl, meta) ->
                   Link.Ref.of_wikilink wl, Transclusion.fallback_block wl meta
                 | Image_embed ref -> ref, block
               in
               (match embed ref ~depth_fallback with
                | Some transclusion -> Cmarkit.Mapper.ret transclusion
                | None -> Cmarkit.Mapper.default))
          | _ -> Cmarkit.Mapper.default)
        ()
    in
    Cmarkit.Mapper.map_doc mapper doc
  in
  { n with body = go ~depth:0 ~expanding:[ path ] ~from:path n.body }
;;
