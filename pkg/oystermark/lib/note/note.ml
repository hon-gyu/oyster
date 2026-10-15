module Anchor = Anchor
module Link = Link
module Query = Query
module Transclusion = Transclusion
module Xml = Xml

module Private = struct
  module Address_utils = Address_utils
end

open Core

type t = Parse.t

type source =
  { path : string
  ; fragment : Link.Ref.fragment option
  ; note : t
  }

let footnote_def (label, blocks) : Djot.Block.t Djot.node =
  Djot.Node.make (Djot.Block.FootnoteDef (label, blocks))
;;

(** The blocks of {!Djot.Doc.source_blocks} at the places [blocks] have among
    {!Djot.Doc.blocks}, so that {!Parse.doc_of_blocks} derives heading
    identifiers again and does not keep them as written ones. *)
let as_written (doc : Djot.Doc.t) (blocks : Djot.Block.t Djot.node list)
  : Djot.Block.t Djot.node list
  =
  let preorder (blocks : Djot.Block.t Djot.node list) : Djot.Block.t Djot.node array =
    let nodes = Queue.create () in
    let folder =
      Djot.Folder.make
        ~block:(fun _ () node ->
          Queue.enqueue nodes node;
          Djot.Folder.default)
        ()
    in
    List.iter blocks ~f:(Djot.Folder.fold_block folder ());
    Queue.to_array nodes
  in
  let derived = preorder (Djot.Doc.blocks doc)
  and written = preorder (Djot.Doc.source_blocks doc) in
  List.map blocks ~f:(fun block ->
    match Array.findi derived ~f:(fun _ node -> phys_equal node block) with
    | Some (i, _) -> written.(i)
    | None -> block)
;;

let part (n : t) (blocks : Djot.Block.t Djot.node list) : t =
  (* The definitions written among [blocks] are not all referred to there. *)
  let blocks =
    let without_definitions =
      Djot.Mapper.make
        ~block:(fun _ (Node (_, _, block)) ->
          match block with
          | FootnoteDef _ -> Djot.Mapper.delete
          | _ -> Djot.Mapper.default)
        ()
    in
    as_written n blocks |> List.filter_map ~f:(Djot.Mapper.map_block without_definitions)
  in
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
      (match Djot.Doc.footnote n label with
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
      Djot.Doc.footnote n label |> Option.map ~f:(fun body -> footnote_def (label, body)))
  in
  Parse.doc_of_blocks ?frontmatter:(Djot.Doc.frontmatter n) (blocks @ footnotes)
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
      ~block:(fun m (Node (pos, attrs, block)) ->
        match block with
        | RefDef _ -> Djot.Mapper.delete
        | FootnoteDef (label, body) ->
          let body = List.filter_map body ~f:(Djot.Mapper.map_block m) in
          Djot.Mapper.ret
            (Djot.Node (pos, attrs, Djot.Block.FootnoteDef (rename label, body)))
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
  Djot.Doc.source_blocks doc |> List.filter_map ~f:(Djot.Mapper.map_block mapper)
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
          then source.note
          else
            go
              ~depth:(depth + 1)
              ~expanding:(source.path :: expanding)
              ~from:source.path
              source.note
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
    (* Transcluded blocks hold positions in another text: a document without
       locations reads none of them. *)
    if !transcluded
    then
      Parse.doc_of_blocks
        ?frontmatter:(Djot.Doc.frontmatter mapped)
        (Djot.Doc.source_blocks mapped)
    else mapped
  in
  go ~depth:0 ~expanding:[ path ] ~from:path n
;;
