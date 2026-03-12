(** Note embedding: expand [![[NOTE]]] wikilinks as AST transclusion.

    This is a post-resolution, pre-render transformation. Each paragraph
    containing a single embed wikilink is replaced by a [Block.Blocks] whose
    meta carries {!embed_meta}.

    - rule: an embed can only be expanded if it's in a container block that has no other children or blank children only
    - future TODO: we allow embed Inline.t to violate the above rule. But at the moment we have no way to specify whether an embed is Inline.t or Block.t

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

(** Test whether a block is an embed-expandable paragraph: a paragraph
    containing a single embed wikilink, where every sibling in [siblings]
    is either a blank line or absent.  An embed can only replace a paragraph
    that is effectively the sole content of its container. *)
let is_expandable_embed_paragraph
      (block : Cmarkit.Block.t)
      ~(siblings : Cmarkit.Block.t list)
  : (Parse.Wikilink.t * Cmarkit.Meta.t) option
  =
  let all_siblings_blank : bool =
    List.for_all siblings ~f:(fun b ->
      match b with
      | Cmarkit.Block.Blank_line _ -> true
      | b' -> phys_equal b' block)
  in
  match block with
  | Cmarkit.Block.Paragraph (p, _) when all_siblings_blank ->
    extract_single_embed (Cmarkit.Block.Paragraph.inline p)
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

(** Embed a note (or part of a note) from another file in the vault.

    This is the workhorse for cross-file transclusion. It handles:

    1. {b Depth guard} — if [embed_depth >= max_depth], the embed is replaced
       with a {!fallback_block} (a plain link paragraph) to prevent infinite
       recursion in cyclic references (e.g. A embeds B which embeds A).

    2. {b Lookup} — finds the target document by [path] in [docs_tbl].
       Returns [None] if the path is missing (unresolved embed).

    3. {b Recursive expansion} — before extracting blocks from the target,
       the target document itself is expanded at [embed_depth + 1] so that
       nested embeds within the target are also resolved (up to [max_depth]).

    4. {b Extraction} — the [extract] callback selects which blocks from the
       expanded target to include:
       - Full note: [fun blocks -> blocks]
       - Heading section: [fun blocks -> Extract.get_heading_section blocks slug]
       - Single block: [fun blocks -> Option.to_list (Extract.get_block_by_caret_id ...)]

    5. {b Wrapping} — the extracted blocks are wrapped in a [Block.Blocks]
       node tagged with {!embed_meta} (depth + source path), which the HTML
       renderer uses to emit [<div class="embed" data-embed-depth="N">].

    @return [Some block] with the wrapped transclusion, or [None] if [path]
    was not found in [docs_tbl]. *)
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

(** Walk a single document's AST and replace embed paragraphs with
    transcluded content.

    This is mutually recursive with {!embed_note}: [expand_doc] finds embed
    wikilinks in the AST, and [embed_note] fetches + recursively expands the
    target document, incrementing [embed_depth] at each level.

    {b How it works:}

    A cmarkit [Mapper] traverses every block in the document. For each
    [Paragraph], we check whether its inline content is a single embed
    wikilink (via {!extract_single_embed}). If so, [try_embed] dispatches
    on the resolved target (stamped by the resolution pass):

    - {b Cross-file} targets ([Note], [Heading], [Block]) delegate to
      {!embed_note}, which looks up the target in [docs_tbl], recursively
      expands it, and extracts the requested portion.

    - {b Self-reference} targets ([Curr_file], [Curr_heading], [Curr_block])
      use [embed_self], which extracts directly from [curr_doc]'s blocks
      without recursive expansion (to avoid infinite loops — the depth guard
      still applies). Unlike cross-file embeds, self-references don't need a
      [docs_tbl] lookup since the document is already in hand.

    - {b Non-embeddable} targets ([None], [Unresolved], [File]) return
      [None], leaving the paragraph unchanged.

    Non-paragraph blocks and paragraphs without a sole embed wikilink pass
    through untouched. *)
and expand_doc
      ~(embed_depth : int)
      ~(max_depth : int)
      (docs_tbl : (string, Cmarkit.Doc.t) Hashtbl.t)
      (doc : Cmarkit.Doc.t)
  : Cmarkit.Doc.t
  =
  let try_embed
        (wl : Parse.Wikilink.t)
        (meta : Cmarkit.Meta.t)
        ~(curr_doc : Cmarkit.Doc.t)
    : Cmarkit.Block.t option
    =
    (* Self-reference embedding: extracts from the current document directly.
       Does not recursively expand the current doc (that would loop), but the
       depth guard still prevents unbounded nesting when a self-embed is
       encountered during recursive expansion of another note. *)
    let embed_self (extract : Cmarkit.Block.t list -> Cmarkit.Block.t list)
      : Cmarkit.Block.t option
      =
      if embed_depth >= max_depth
      then Some (fallback_block wl meta)
      else (
        let new_depth = embed_depth + 1 in
        let blocks = extract (doc_blocks curr_doc) in
        let block_meta =
          Cmarkit.Meta.add
            embed_meta_key
            { depth = new_depth; source_path = "" }
            Cmarkit.Meta.none
        in
        Some (Cmarkit.Block.Blocks (blocks, block_meta)))
    in
    (* Cross-file embedding: delegates to embed_note for lookup + recursion. *)
    let embed = embed_note ~embed_depth ~max_depth docs_tbl in
    match Cmarkit.Meta.find Resolve.resolved_key meta with
    (* Non-embeddable: no target, unresolved, or non-markdown file *)
    | None | Some Resolve.Unresolved | Some (Resolve.File _) -> None
    (* Self-references: extract from current doc *)
    | Some Resolve.Curr_file -> embed_self (fun blocks -> blocks)
    | Some (Resolve.Curr_heading { slug; _ }) ->
      embed_self (fun blocks -> Parse.Extract.get_heading_section blocks slug)
    | Some (Resolve.Curr_block { block_id }) ->
      embed_self (fun blocks ->
        Option.to_list (Parse.Extract.get_block_by_caret_id blocks block_id))
    (* Cross-file references: look up in vault and recursively expand *)
    | Some (Resolve.Note { path }) -> embed path wl meta (fun blocks -> blocks)
    | Some (Resolve.Heading { path; slug; _ }) ->
      embed path wl meta (fun blocks -> Parse.Extract.get_heading_section blocks slug)
    | Some (Resolve.Block { path; block_id }) ->
      embed path wl meta (fun blocks ->
        Option.to_list (Parse.Extract.get_block_by_caret_id blocks block_id))
  in
  (* The mapper only acts on Paragraph blocks — other block types cannot
     contain embed wikilinks at the top level. *)
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
             (match try_embed wl meta ~curr_doc:doc with
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
  ;;

  let print_blocks (blocks : Cmarkit.Block.t list) : unit =
    let doc = doc_of_blocks blocks in
    print_endline (Parse.commonmark_of_doc doc)
  ;;
end

let%expect_test "is_expandable_embed_paragraph: sole embed paragraph" =
  let doc = Parse.of_string "![[target]]" in
  let blocks = For_test.parse_blocks "![[target]]" in
  ignore doc;
  let block = List.hd_exn blocks in
  let result = is_expandable_embed_paragraph block ~siblings:blocks in
  printf "%b\n" (Option.is_some result);
  [%expect {| true |}]
;;

let%expect_test "is_expandable_embed_paragraph: embed mixed with text" =
  let blocks = For_test.parse_blocks "See ![[target]] here." in
  let block = List.hd_exn blocks in
  let result = is_expandable_embed_paragraph block ~siblings:blocks in
  printf "%b\n" (Option.is_some result);
  [%expect {| false |}]
;;

let%expect_test "is_expandable_embed_paragraph: embed with blank siblings only" =
  let blocks = For_test.parse_blocks "\n![[target]]\n" in
  let block =
    List.find_exn blocks ~f:(fun b ->
      match b with
      | Cmarkit.Block.Paragraph _ -> true
      | _ -> false)
  in
  let result = is_expandable_embed_paragraph block ~siblings:blocks in
  printf "%b\n" (Option.is_some result);
  [%expect {| true |}]
;;

let%expect_test "is_expandable_embed_paragraph: non-embed wikilink" =
  let blocks = For_test.parse_blocks "[[target]]" in
  let block = List.hd_exn blocks in
  let result = is_expandable_embed_paragraph block ~siblings:blocks in
  printf "%b\n" (Option.is_some result);
  [%expect {| false |}]
;;

let%expect_test "is_expandable_embed_paragraph: embed among other blocks" =
  let blocks = For_test.parse_blocks "Some text.\n\n![[target]]\n\nMore text." in
  let embed_block = List.nth_exn blocks 1 in
  let result = is_expandable_embed_paragraph embed_block ~siblings:blocks in
  printf "%b\n" (Option.is_some result);
  [%expect {| false |}]
;;
