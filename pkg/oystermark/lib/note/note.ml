module Anchor = Anchor
module Link = Link
module Transclusion = Transclusion
module Xml = Xml

module Private = struct
  module Address_utils = Address_utils
end

open Core

type t = Parse.t =
  { frontmatter : Yaml.value option
  ; doc : Djot.Doc.t
  }

type source =
  { path : string
  ; fragment : Link.Ref.fragment option
  ; note : t
  }

let expand
      ?(max_depth = 5)
      ~(env : from:string -> Link.Ref.t -> source option)
      ~(path : string)
      (n : t)
  : t
  =
  let rec go ~depth ~expanding ~from (doc : Djot.Doc.t) : Djot.Doc.t =
    let embed (ref : Link.Ref.t) ~(depth_fallback : Transclusion.block)
      : Transclusion.block option
      =
      match env ~from ref with
      | None -> None
      | Some _ when depth >= max_depth -> Some depth_fallback
      | Some source ->
        let doc =
          if List.mem expanding source.path ~equal:String.equal
          then source.note.doc
          else
            go
              ~depth:(depth + 1)
              ~expanding:(source.path :: expanding)
              ~from:source.path
              source.note.doc
        in
        Some
          (Transclusion.transclude
             ~depth:(depth + 1)
             ~source_path:source.path
             ~fragment:source.fragment
             (Djot.Doc.blocks doc))
    in
    let mapper =
      Djot.Mapper.make
        ~block:(fun _ (Node (_, _, contents) as block) ->
          match contents with
          | Para inlines ->
            (match Transclusion.embed_source_of_inlines doc inlines with
             | None -> Djot.Mapper.default
             | Some embed_source ->
               let ref, depth_fallback =
                 match embed_source with
                 | Wikilink_embed target ->
                   Link.Ref.of_wikilink_target target, Transclusion.fallback_block target
                 | Image_embed ref -> ref, block
               in
               (match embed ref ~depth_fallback with
                | Some transclusion -> Djot.Mapper.ret transclusion
                | None -> Djot.Mapper.default))
          | _ -> Djot.Mapper.default)
        ()
    in
    Djot.Mapper.map_doc mapper doc
  in
  { n with doc = go ~depth:0 ~expanding:[ path ] ~from:path n.doc }
;;
