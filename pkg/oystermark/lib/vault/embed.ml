open Core

(** Replace the embed paragraphs of [doc], the note at [curr_path], with
    transclusions.

    @param embed_depth Current transclusion nesting level. 0 for the root
      document, incremented by 1 each time a transcluded note is expanded.
    @param max_depth Inclusive depth limit. When [embed_depth >= max_depth],
      an embed is replaced with its depth fallback instead of expanding: a
      {!Transclusion.fallback_block} for a wikilink, the original paragraph
      for an image.
    @param docs_tbl All parsed vault documents keyed by vault-relative path
      (e.g. ["notes/foo.md"]). Shared across the entire expansion pass. *)
let rec expand_doc
          ~(embed_depth : int)
          ~(max_depth : int)
          ~(curr_path : string)
          ~(index : Index.t)
          (docs_tbl : (string, Cmarkit.Doc.t) Hashtbl.t)
          (doc : Cmarkit.Doc.t)
  : Cmarkit.Doc.t
  =
  let embed (link_ref : Link_ref.t) ~(depth_fallback : Cmarkit.Block.t)
    : Cmarkit.Block.t option
    =
    match Index.resolve index curr_path link_ref with
    | Error _ | Ok (Index.Asset _) -> None
    | Ok _ when embed_depth >= max_depth -> Some depth_fallback
    | Ok target ->
      let source_path = Index.target_path target in
      let depth = embed_depth + 1 in
      (* A self-reference reads [doc] as it stands: expanding it again would not
         terminate. *)
      let source =
        if Option.is_none link_ref.target
        then Some doc
        else
          Hashtbl.find docs_tbl source_path
          |> Option.map
               ~f:
                 (expand_doc
                    ~embed_depth:depth
                    ~max_depth
                    ~curr_path:source_path
                    ~index
                    docs_tbl)
      in
      let fragment, select =
        match target with
        | Index.Anchor { anchor = { value; _ }; _ } ->
          ( Transclusion.fragment value
          , fun blocks -> Extract.read blocks (Extract.Anchor.address value) )
        | Note _ | Asset _ -> None, Fn.id
      in
      Option.map source ~f:(fun source ->
        Transclusion.transclude
          ~depth
          ~source_path
          ~fragment
          (select (Transclusion.non_fm_blocks source)))
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
           | Some source ->
             let link_ref, depth_fallback =
               match source with
               | Wikilink_embed (wl, wl_meta) ->
                 Link_ref.of_wikilink wl, Transclusion.fallback_block wl wl_meta
               | Image_embed link_ref -> link_ref, block
             in
             (match embed link_ref ~depth_fallback with
              | Some spliced -> Cmarkit.Mapper.ret spliced
              | None -> Cmarkit.Mapper.default))
        | _ -> Cmarkit.Mapper.default)
      ()
  in
  Cmarkit.Mapper.map_doc mapper doc
;;

let expand_docs ?(max_depth = 5) ~(index : Index.t) (docs : (string * Cmarkit.Doc.t) list)
  : (string * Cmarkit.Doc.t) list
  =
  let docs_tbl = Hashtbl.of_alist_exn (module String) docs in
  List.map docs ~f:(fun (rel_path, doc) ->
    rel_path, expand_doc ~embed_depth:0 ~max_depth ~curr_path:rel_path ~index docs_tbl doc)
;;
