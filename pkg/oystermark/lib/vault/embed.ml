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

(** If [inline] is a single embed wikilink — ignoring surrounding
    whitespace-only {!Cmarkit.Inline.Text} nodes — return it. *)
let extract_single_embed (inline : Cmarkit.Inline.t)
  : (Parse.Wikilink.t * Cmarkit.Meta.t) option
  =
  let is_blank : Cmarkit.Inline.t -> bool = function
    | Cmarkit.Inline.Text (s, _) -> String.is_empty (String.strip s)
    | _ -> false
  in
  let items =
    match inline with
    | Cmarkit.Inline.Inlines (items, _) -> items
    | single -> [ single ]
  in
  let significant = List.filter items ~f:(fun i -> not (is_blank i)) in
  match significant with
  | [ Parse.Wikilink.Ext_wikilink (w, meta) ] when w.embed -> Some (w, meta)
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

(** Collect the section starting at the heading with [ordinal] (0-based
    position among all headings in the document), up to (but not including) the
    next heading of equal or lesser level.  Returns [] when the heading is not
    found. *)
let get_heading_section (blocks : Cmarkit.Block.t list) ~(ordinal : int)
  : Cmarkit.Block.t list
  =
  let heading_idx = ref 0 in
  let rec find_start : Cmarkit.Block.t list -> Cmarkit.Block.t list = function
    | [] -> []
    | (Cmarkit.Block.Heading (h, _) as b) :: rest ->
      if !heading_idx = ordinal
      then b :: collect_section (Cmarkit.Block.Heading.level h) rest
      else (
        Int.incr heading_idx;
        find_start rest)
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
let find_block_by_id (blocks : Cmarkit.Block.t list) (block_id : string)
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
    | Some (Resolve.Heading { path; ordinal; _ }) ->
      embed path wl meta (fun blocks -> get_heading_section blocks ~ordinal)
    | Some (Resolve.Block { path; block_id }) ->
      embed path wl meta (fun blocks -> Option.to_list (find_block_by_id blocks block_id))
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

(* Unit tests for block extraction helpers.  Integration tests for the full
   embed pipeline live in test/expect-test/test_embed.ml. *)

let parse_blocks_ (md : string) : Cmarkit.Block.t list = doc_blocks (Parse.of_string md)

let print_block : Cmarkit.Block.t -> unit = function
  | Cmarkit.Block.Heading (h, _) ->
    printf
      "h%d: %s\n"
      (Cmarkit.Block.Heading.level h)
      (Parse.inline_to_plain_text (Cmarkit.Block.Heading.inline h))
  | Cmarkit.Block.Paragraph (p, _) ->
    printf "p: %s\n" (Parse.inline_to_plain_text (Cmarkit.Block.Paragraph.inline p))
  | _ -> ()
;;

let%expect_test "get_heading_section: by ordinal" =
  let blocks =
    parse_blocks_ "Intro.\n\n## A\n\nUnder A.\n\n## B\n\nUnder B.\n\n### B.1\n\nDeep."
  in
  List.iter (get_heading_section blocks ~ordinal:1) ~f:print_block;
  [%expect
    {|
    h2: B
    p: Under B.
    h3: B.1
    p: Deep.
    |}]
;;

let%expect_test "get_heading_section: ordinal out of range" =
  let blocks = parse_blocks_ "## Only heading\n\nContent." in
  printf "%d blocks\n" (List.length (get_heading_section blocks ~ordinal:5));
  [%expect {| 0 blocks |}]
;;

let%expect_test "get_heading_section: stops at same-level heading" =
  let blocks = parse_blocks_ "## A\n\nA content.\n\n## B\n\nB content." in
  List.iter (get_heading_section blocks ~ordinal:0) ~f:print_block;
  [%expect
    {|
    h2: A
    p: A content.
    |}]
;;

let%expect_test "find_block_by_id: found" =
  let blocks = parse_blocks_ "First.\n\nTarget. ^myid\n\nAfter." in
  (match find_block_by_id blocks "myid" with
   | Some (Cmarkit.Block.Paragraph (p, _)) ->
     printf "found: %s\n" (Parse.inline_to_plain_text (Cmarkit.Block.Paragraph.inline p))
   | _ -> printf "not found\n");
  [%expect {| found: Target. ^myid |}]
;;

let%expect_test "find_block_by_id: not found" =
  let blocks = parse_blocks_ "No block ids here.\n\nNor here." in
  (match find_block_by_id blocks "nope" with
   | Some _ -> printf "found\n"
   | None -> printf "not found\n");
  [%expect {| not found |}]
;;
