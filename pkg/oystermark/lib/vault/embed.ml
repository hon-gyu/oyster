(** Note embedding: expand [![[NOTE]]] wikilinks as AST transclusion.

    This is a post-resolution, pre-render transformation. Each paragraph
    containing a single embed wikilink is replaced by a [Block.Blocks] whose
    meta carries {!embed_meta} — standard cmarkit blocks, no custom extension.

    Depth limiting: embedding is allowed up to [max_depth] levels deep.
    When [embed_depth >= max_depth] the wikilink is replaced with a plain
    fallback link instead. *)

open Core

(** Metadata attached to the {!Cmarkit.Block.Blocks} node that wraps
    transcluded content. Consumers (e.g. the HTML renderer) can use this to
    style embedded blocks differently. *)
type embed_meta =
  { depth : int
    (** Transclusion depth: 1 for a direct embed, 2 for an embed within an
      embed, etc. *)
  ; source_path : string
    (** Vault-relative path of the note whose blocks were transcluded. *)
  }

let embed_meta_key : embed_meta Cmarkit.Meta.key = Cmarkit.Meta.key ()

(** Top-level content blocks of a doc, stripping leading frontmatter.
    When the doc's top block is a [Block.Blocks] carrying {!embed_meta}, the
    wrapper is preserved so that downstream consumers (rendering, further
    embedding) see the transclusion boundary. *)
let doc_blocks (doc : Cmarkit.Doc.t) : Cmarkit.Block.t list =
  match Cmarkit.Doc.block doc with
  | Cmarkit.Block.Blocks (_, meta) as b
    when Option.is_some (Cmarkit.Meta.find embed_meta_key meta) -> [ b ]
  | Cmarkit.Block.Blocks (bs, _) ->
    (match bs with
     | Parse.Frontmatter.Frontmatter _ :: rest -> rest
     | _ -> bs)
  | other -> [ other ]
;;

(** If [inline] is a single embed wikilink, return it.  cmarkit wraps a
    paragraph's inline content in [Inlines([...], _)] — we unwrap that. *)
let extract_single_embed (inline : Cmarkit.Inline.t)
  : (Parse.Wikilink.t * Cmarkit.Meta.t) option
  =
  match inline with
  | Parse.Wikilink.Ext_wikilink (w, meta) when w.embed -> Some (w, meta)
  | Cmarkit.Inline.Inlines ([ Parse.Wikilink.Ext_wikilink (w, meta) ], _) when w.embed ->
    Some (w, meta)
  | _ -> None
;;

(** A fallback paragraph that renders the embed as a plain link (used when
    [embed_depth >= max_depth] or when the target cannot be resolved to a note). *)
let fallback_block (wl : Parse.Wikilink.t) (meta : Cmarkit.Meta.t) : Cmarkit.Block.t =
  let link_inline = Parse.Wikilink.Ext_wikilink ({ wl with embed = false }, meta) in
  let p =
    Cmarkit.Block.Paragraph.make
      (Cmarkit.Inline.Inlines ([ link_inline ], Cmarkit.Meta.none))
  in
  Cmarkit.Block.Paragraph (p, Cmarkit.Meta.none)
;;

(** Collect the section starting at the heading with the given [slug],
    up to (but not including) the next heading of equal or lesser level.
    Slugs are computed on-the-fly using the same algorithm as {!Index.dedup_slug},
    so they match the index.  Returns [] when no heading matches. *)
let get_heading_section (blocks : Cmarkit.Block.t list) ~(slug : string)
  : Cmarkit.Block.t list
  =
  let seen = Hashtbl.create (module String) in
  let rec find_start : Cmarkit.Block.t list -> Cmarkit.Block.t list = function
    | [] -> []
    | (Cmarkit.Block.Heading (h, _) as b) :: rest ->
      let text =
        Parse.inline_to_plain_text (Cmarkit.Block.Heading.inline h)
      in
      let h_slug = Index.dedup_slug seen text in
      if String.equal h_slug slug
      then b :: collect_section (Cmarkit.Block.Heading.level h) rest
      else find_start rest
    | _ :: rest -> find_start rest
  and collect_section (stop_level : int) : Cmarkit.Block.t list -> Cmarkit.Block.t list
    = function
    | [] -> []
    | Cmarkit.Block.Heading (h, _) :: _ when Cmarkit.Block.Heading.level h <= stop_level
      -> []
    | b :: rest -> b :: collect_section stop_level rest
  in
  find_start blocks
;;

(** Find the (first) block in [blocks] whose {!Parse.Block_id} meta matches
    [block_id]. Only {!Cmarkit.Block.Paragraph} nodes carry block-ID metadata. *)
let get_block_by_id (blocks : Cmarkit.Block.t list) (block_id : string)
  : Cmarkit.Block.t option
  =
  List.find blocks ~f:(function
    | Cmarkit.Block.Paragraph (_, meta) ->
      (match Cmarkit.Meta.find Parse.Block_id.meta_key meta with
       | Some (bid : Parse.Block_id.t) -> String.equal bid.id block_id
       | None -> false)
    | _ -> false)
;;

(** Check depth limit, look up [path], recursively expand, then wrap in a
    [Block.Blocks] tagged with {!embed_meta}.  [extract] selects the subset of
    blocks (full note, heading section, or single block). *)
let rec embed_note
          ~(embed_depth : int)
          ~(max_depth : int)
          (docs_tbl : (string, Cmarkit.Doc.t) Hashtbl.t)
          (path : string)
          (wl : Parse.Wikilink.t)
          (meta : Cmarkit.Meta.t)
          (extract : Cmarkit.Block.t list -> Cmarkit.Block.t list)
  : Cmarkit.Block.t option
  =
  if embed_depth >= max_depth
  then Some (fallback_block wl meta)
  else (
    match Hashtbl.find docs_tbl path with
    | None -> None
    | Some target_doc ->
      let new_depth = embed_depth + 1 in
      let expanded = expand_doc ~embed_depth:new_depth ~max_depth docs_tbl target_doc in
      let blocks = extract (doc_blocks expanded) in
      let block_meta =
        Cmarkit.Meta.add
          embed_meta_key
          { depth = new_depth; source_path = path }
          Cmarkit.Meta.none
      in
      Some (Cmarkit.Block.Blocks (blocks, block_meta)))

(** Expand embed wikilinks in [doc].
    [embed_depth] is the current transclusion depth (0 for the root doc).
    [max_depth] is the inclusive limit: at [embed_depth >= max_depth] embeds
    become fallback links instead. *)
and expand_doc
      ~(embed_depth : int)
      ~(max_depth : int)
      (docs_tbl : (string, Cmarkit.Doc.t) Hashtbl.t)
      (doc : Cmarkit.Doc.t)
  : Cmarkit.Doc.t
  =
  let try_embed (wl : Parse.Wikilink.t) (meta : Cmarkit.Meta.t) : Cmarkit.Block.t option =
    let embed = embed_note ~embed_depth ~max_depth docs_tbl in
    match Cmarkit.Meta.find Resolve.resolved_key meta with
    | None | Some Resolve.Unresolved | Some (Resolve.File _) -> None
    | Some Resolve.Curr_file | Some (Resolve.Curr_heading _) | Some (Resolve.Curr_block _)
      -> embed "" wl meta (fun _ -> [])
    | Some (Resolve.Note { path }) -> embed path wl meta (fun blocks -> blocks)
    | Some (Resolve.Heading { path; slug; _ }) ->
      embed path wl meta (fun blocks -> get_heading_section blocks ~slug)
    | Some (Resolve.Block { path; block_id }) ->
      embed path wl meta (fun blocks -> Option.to_list (get_block_by_id blocks block_id))
  in
  let mapper =
    Cmarkit.Mapper.make
      ~block_ext_default:(fun _m b -> Some b)
      ~inline_ext_default:(fun _m i -> Some i)
      ~block:(fun _mapper block ->
        match block with
        | Cmarkit.Block.Paragraph (p, _) ->
          (match extract_single_embed (Cmarkit.Block.Paragraph.inline p) with
           | None -> Cmarkit.Mapper.default
           | Some (wl, meta) ->
             (match try_embed wl meta with
              | Some spliced -> Cmarkit.Mapper.ret spliced
              | None -> Cmarkit.Mapper.default))
        | _ -> Cmarkit.Mapper.default)
      ()
  in
  Cmarkit.Mapper.map_doc mapper doc
;;

(** Expand all embed wikilinks in a list of resolved docs.
    [max_depth] (default 5) controls how many transclusion levels are allowed
    before falling back to a plain link. *)
let expand_docs ?(max_depth = 5) (docs : (string * Cmarkit.Doc.t) list)
  : (string * Cmarkit.Doc.t) list
  =
  let docs_tbl = Hashtbl.of_alist_exn (module String) docs in
  List.map docs ~f:(fun (rel_path, doc) ->
    rel_path, expand_doc ~embed_depth:0 ~max_depth docs_tbl doc)
;;


module For_test = struct
  let parse_blocks (md : string) : Cmarkit.Block.t list = doc_blocks (Parse.of_string md)

  let doc_of_blocks (blocks : Cmarkit.Block.t list) : Cmarkit.Doc.t =
    Cmarkit.Doc.make (Cmarkit.Block.Blocks (blocks, Cmarkit.Meta.none))

  let print_blocks (blocks : Cmarkit.Block.t list) : unit =
    let doc = doc_of_blocks blocks in
    print_endline (Parse.commonmark_of_doc doc)
end

let%expect_test "get_heading_section: by slug" =
  let blocks =
    For_test.parse_blocks {|\
Intro.

## A

Under A.

## B

Under B.

### B.1

Deep.|}
  in
  For_test.print_blocks (get_heading_section blocks ~slug:"b");
  [%expect
    {|
    ## B

    Under B.

    ### B.1

    Deep.
    |}]
;;

let%expect_test "get_heading_section: slug not found" =
  let blocks = For_test.parse_blocks {|\
## Only heading

Content.
|} in
  For_test.print_blocks (get_heading_section blocks ~slug:"nonexistent");
  [%expect {| |}]
;;

let%expect_test "get_heading_section: stops at same-level heading" =
  let blocks = For_test.parse_blocks {|
## A

A content.

## B

B content.
|}in
  For_test.print_blocks (get_heading_section blocks ~slug:"a");
  [%expect
    {|
    ## A

    A content.
    |}]
;;

let%expect_test "find_block_by_id: found" =
  let blocks = For_test.parse_blocks {|\
First.

Target. ^myid

After.
|} in
  (match get_block_by_id blocks "myid" with
   | Some b ->
     print_endline "found: \n";
     For_test.print_blocks [b]
   | _ -> printf "not found\n");
  [%expect {|
    found:

    Target. ^myid
    |}]
;;

let%expect_test "find_block_by_id: not found" =
  let blocks = For_test.parse_blocks "No block ids here.\n\nNor here." in
  (match get_block_by_id blocks "nope" with
   | Some _ -> printf "found\n"
   | None -> printf "not found\n");
  [%expect {| not found |}]
;;
