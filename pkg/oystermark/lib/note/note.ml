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

let footnote_def (label, blocks) : Djot.Block.t Djot.node =
  Djot.Node.make (Djot.Block.FootnoteDef (label, blocks))
;;

let part (n : t) (blocks : Djot.Block.t Djot.node list) : t =
  let referenced_labels =
    Djot.Folder.make
      ~inline:(fun _ labels (Node (_, _, inline)) ->
        match inline with
        | FootnoteReference label ->
          Djot.Folder.ret (Parse.Common.normalize_label label :: labels)
        | _ -> Djot.Folder.default)
      ()
  in
  (* Footnotes refer to footnotes in turn. *)
  let rec close seen = function
    | [] -> List.rev seen
    | label :: rest when List.mem seen label ~equal:String.equal -> close seen rest
    | label :: rest ->
      (match Djot.Doc.footnote n.doc label with
       | None -> close seen rest
       | Some body ->
         let labels =
           List.fold body ~init:[] ~f:(Djot.Folder.fold_block referenced_labels)
         in
         close (label :: seen) (rest @ List.rev labels))
  in
  let labels =
    List.fold blocks ~init:[] ~f:(Djot.Folder.fold_block referenced_labels)
    |> List.rev
    |> close []
  in
  let footnotes =
    List.filter_map labels ~f:(fun label ->
      Djot.Doc.footnote n.doc label
      |> Option.map ~f:(fun body -> footnote_def (label, body)))
  in
  { n with doc = Parse.doc_of_blocks (blocks @ footnotes) }
;;

(** [doc]'s blocks and footnote definitions, to be transcluded into another
    note: footnote labels prefixed with [prefix] so they cannot meet the host's,
    and reference links made direct, so that the host's reference definitions
    cannot capture them; the reference definitions are dropped. *)
let transcludable ~prefix (doc : Djot.Doc.t) : Djot.Block.t Djot.node list =
  let rename label = prefix ^ Parse.Common.normalize_label label in
  let direct target =
    match (target : Djot.Inline.target) with
    | Direct _ -> None
    | Reference label ->
      Djot.Doc.reference doc label
      |> Option.map ~f:(fun (destination, _) -> Djot.Inline.Direct destination)
  in
  let mapper =
    Djot.Mapper.make
      ~block:(fun _ (Node (_, _, block)) ->
        match block with
        | RefDef _ -> Djot.Mapper.delete
        | _ -> Djot.Mapper.default)
      ~inline:(fun m (Node (pos, attrs, inline)) ->
        let children ns = List.filter_map ns ~f:(Djot.Mapper.map_inline m) in
        match inline with
        | FootnoteReference label ->
          Djot.Mapper.ret
            (Djot.Node (pos, attrs, Djot.Inline.FootnoteReference (rename label)))
        | Link (ns, target) ->
          (match direct target with
           | None -> Djot.Mapper.default
           | Some target ->
             Djot.Mapper.ret
               (Djot.Node (pos, attrs, Djot.Inline.Link (children ns, target))))
        | Image (ns, target) ->
          (match direct target with
           | None -> Djot.Mapper.default
           | Some target ->
             Djot.Mapper.ret
               (Djot.Node (pos, attrs, Djot.Inline.Image (children ns, target))))
        | _ -> Djot.Mapper.default)
      ()
  in
  let map blocks = List.filter_map blocks ~f:(Djot.Mapper.map_block mapper) in
  map (Djot.Doc.blocks doc)
  @ List.map (Djot.Doc.footnotes doc) ~f:(fun (label, body) ->
    footnote_def (rename label, map body))
;;

let expand
      ?(max_depth = 5)
      ~(env : from:string -> Link.Ref.t -> source option)
      ~(path : string)
      (n : t)
  : t
  =
  let rec go ~depth ~expanding ~from (doc : Djot.Doc.t) : Djot.Doc.t =
    let transcluded = ref false in
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
        transcluded := true;
        Some
          (Transclusion.transclude
             ~depth:(depth + 1)
             ~source_path:source.path
             ~fragment:source.fragment
             (transcludable ~prefix:(source.path ^ ":") doc))
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
    let mapped = Djot.Mapper.map_doc mapper doc in
    (* The side tables are the parse's: rebuild them to take in the transcluded
       footnotes. *)
    if !transcluded
    then
      Parse.doc_of_blocks
        (Djot.Doc.blocks mapped @ List.map (Djot.Doc.footnotes mapped) ~f:footnote_def)
    else mapped
  in
  { n with doc = go ~depth:0 ~expanding:[ path ] ~from:path n.doc }
;;
