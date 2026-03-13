(** Note embedding: expand embed wikilinks and markdown image links as AST
    transclusion.

    Supported embed sources:
    - {b Wikilink embeds}: [!\[\[NOTE\]\]] syntax (parsed as {!Parse.Wikilink.t}
      with [embed = true]).
    - {b Markdown image embeds}: [!\[alt\](note.md)] syntax. Only expanded when
      the image's resolved target is a note (i.e. a [.md] file). Non-note
      images (e.g. PNG, JPG) are left untouched for the HTML renderer.

    For media embedding (non-note images, audio, video), see {!Html}.

    This is a post-resolution, pre-render transformation. Each paragraph
    containing a single embed source is replaced by a [Block.Blocks] whose
    meta carries {!embed_meta}.

    Frontmatter is never embedded: {!doc_blocks} strips it before extraction.

    - rule: an embed can only be expanded if it's in a container block that
      has no other children or blank children only
    - future TODO: we allow embed Inline.t to violate the above rule. But at
      the moment we have no way to specify whether an embed is Inline.t or
      Block.t

    Depth limiting: embedding is allowed up to [max_depth] levels deep.
    When [embed_depth >= max_depth] the wikilink is replaced with a plain
    fallback link instead; image embeds are left as-is. *)

open Core


module type Spec = sig
  (** Frontmatter will not be embedded: {!doc_blocks} strips it before
      extraction. *)
  val frontmatter_unembeddable : unit

  (** From expanded blocks, we can restore the original embedding syntax,
      up to the difference between wikilink and commonmark inline link. *)
  val reverse_embed : unit
end

(** Metadata attached to the {!Cmarkit.Block.Blocks} node that wraps
    transcluded content. Consumers (e.g. the HTML renderer) can use this to
    style embedded blocks differently, and {!reverse_embed_doc} uses it to
    reconstruct the original embed syntax. *)
type embed_meta =
  { depth : int
    (** Transclusion depth: 1 for a direct embed, 2 for an embed within an
      embed, etc. *)
  ; source_path : string
    (** Vault-relative path of the note whose blocks were transcluded. *)
  ; fragment : Parse.Wikilink.fragment option
    (** The heading or block-ref fragment, if the embed targeted a sub-section
        rather than the full note. *)
  }

let embed_meta_key : embed_meta Cmarkit.Meta.key = Cmarkit.Meta.key ()

(** Top-level content blocks of a doc, stripping leading frontmatter.
    When the doc's top block is a [Block.Blocks] carrying {!embed_meta}, the
    wrapper is preserved so that downstream consumers (rendering, further
    embedding) see the transclusion boundary. *)
let non_fm_blocks (doc : Cmarkit.Doc.t) : Cmarkit.Block.t list =
  match Cmarkit.Doc.block doc with
  | Cmarkit.Block.Blocks (_, meta) as b
    when Option.is_some (Cmarkit.Meta.find embed_meta_key meta) -> [ b ]
  | Cmarkit.Block.Blocks (bs, _) ->
    (match bs with
     | Parse.Frontmatter.Frontmatter _ :: rest -> rest
     | _ -> bs)
  | other -> [ other ]
;;

(** The two kinds of inline that can trigger block-level transclusion. *)
type embed_source =
  | Wikilink_embed of Parse.Wikilink.t * Cmarkit.Meta.t
      (** [!\[\[NOTE\]\]] — carries the wikilink for {!fallback_block}. *)
  | Image_embed of Cmarkit.Meta.t
      (** [!\[alt\](note.md)] — only when the resolved target is a note. *)

(** If [inline] is a single embed source (embed wikilink or image link
    pointing to a note), return the classified source.  cmarkit wraps a
    paragraph's inline content in [Inlines(\[...\], _)] — we unwrap that. *)
let extract_embed_source (inline : Cmarkit.Inline.t) : embed_source option =
  let check_one (i : Cmarkit.Inline.t) : embed_source option =
    match i with
    | Parse.Wikilink.Ext_wikilink (w, meta) when w.embed ->
      Some (Wikilink_embed (w, meta))
    | Cmarkit.Inline.Image (_, meta) ->
      (match Cmarkit.Meta.find Resolve.resolved_key meta with
       | Some (Resolve.Note _ | Resolve.Heading _ | Resolve.Block _
              | Resolve.Curr_file | Resolve.Curr_heading _ | Resolve.Curr_block _) ->
         Some (Image_embed meta)
       | _ -> None)
    | _ -> None
  in
  match inline with
  | Cmarkit.Inline.Inlines ([ i ], _) -> check_one i
  | i -> check_one i
;;

(** Test whether a block is an embed-expandable paragraph: a paragraph
    containing a single embed source (wikilink or image), where every sibling
    in [siblings] is either a blank line or absent.  An embed can only replace
    a paragraph that is effectively the sole content of its container. *)
let is_expandable_embed_paragraph
      (block : Cmarkit.Block.t)
      ~(siblings : Cmarkit.Block.t list)
  : embed_source option
  =
  let all_siblings_blank : bool =
    List.for_all siblings ~f:(fun b ->
      match b with
      | Cmarkit.Block.Blank_line _ -> true
      | b' -> phys_equal b' block)
  in
  match block with
  | Cmarkit.Block.Paragraph (p, _) when all_siblings_blank ->
    extract_embed_source (Cmarkit.Block.Paragraph.inline p)
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
       with [depth_fallback] to prevent infinite recursion in cyclic
       references (e.g. A embeds B which embeds A).

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

    @param embed_depth Current transclusion nesting level. 0 for the root
      document, incremented by 1 each time we descend into an embed.
    @param max_depth Inclusive depth limit. When [embed_depth >= max_depth],
      the embed is replaced with [depth_fallback] instead of expanding.
    @param depth_fallback The block to substitute when depth is exceeded.
      For wikilink embeds this is a {!fallback_block} (plain link); for
      image embeds this is the original paragraph (keeping the image as-is).
    @param docs_tbl All parsed vault documents keyed by vault-relative path
      (e.g. ["notes/foo.md"]). Shared across the entire expansion pass.
    @param path Vault-relative path of the target note to embed.
    @param extract A selector that narrows the target's blocks to the
      desired subset. Called on the target's top-level blocks after
      recursive expansion.
    @return [Some block] with the wrapped transclusion, or [None] if [path]
    was not found in [docs_tbl]. *)
let rec embed_note
          ~(embed_depth : int)
          ~(max_depth : int)
          ~(depth_fallback : Cmarkit.Block.t)
          ~(fragment : Parse.Wikilink.fragment option)
          (docs_tbl : (string, Cmarkit.Doc.t) Hashtbl.t)
          (path : string)
          (extract : Cmarkit.Block.t list -> Cmarkit.Block.t list)
  : Cmarkit.Block.t option
  =
  if embed_depth >= max_depth
  then Some depth_fallback
  else (
    match Hashtbl.find docs_tbl path with
    | None -> None
    | Some target_doc ->
      let new_depth = embed_depth + 1 in
      let expanded =
        expand_doc ~embed_depth:new_depth ~max_depth ~curr_path:path docs_tbl target_doc
      in
      let blocks = extract (non_fm_blocks expanded) in
      let block_meta =
        Cmarkit.Meta.add
          embed_meta_key
          { depth = new_depth; source_path = path; fragment }
          Cmarkit.Meta.none
      in
      Some (Cmarkit.Block.Blocks (blocks, block_meta)))

(** Walk a single document's AST and replace embed paragraphs with
    transcluded content.

    This is mutually recursive with {!embed_note}: [expand_doc] finds embed
    wikilinks and image links in the AST, and [embed_note] fetches +
    recursively expands the target document, incrementing [embed_depth] at
    each level.

    {b How it works:}

    A cmarkit [Mapper] traverses every block in the document. For each
    [Paragraph], we check whether its inline content is a single embed
    wikilink (via {!extract_single_embed}) or a single image link pointing
    to a note (via {!extract_single_image_embed}). If so, [try_embed]
    dispatches on the resolved target (stamped by the resolution pass):

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

    Non-paragraph blocks and paragraphs without a sole embed source pass
    through untouched.

    @param embed_depth Current transclusion nesting level. 0 when called from
      {!expand_docs} on the root document; >0 when called recursively from
      {!embed_note} to expand a target document's own embeds.
    @param max_depth Inclusive depth limit, passed through to {!embed_note}.
    @param curr_path Vault-relative path of the document being expanded.
      Stored in {!embed_meta} for self-reference embeds.
    @param docs_tbl Shared vault-wide document table, passed through to
      {!embed_note} for cross-file lookups.
    @param doc The document whose embed wikilinks should be expanded. For
      self-references, this is also the source of the extracted blocks. *)
and expand_doc
      ~(embed_depth : int)
      ~(max_depth : int)
      ~(curr_path : string)
      (docs_tbl : (string, Cmarkit.Doc.t) Hashtbl.t)
      (doc : Cmarkit.Doc.t)
  : Cmarkit.Doc.t
  =
  (** Dispatch an embed based on the resolved target in [meta].
      [depth_fallback] is the block to use when [embed_depth >= max_depth]:
      for wikilinks this is a {!fallback_block}, for images it is the
      original paragraph. *)
  let try_embed
        (meta : Cmarkit.Meta.t)
        ~(depth_fallback : Cmarkit.Block.t)
        ~(curr_doc : Cmarkit.Doc.t)
    : Cmarkit.Block.t option
    =
    (* Self-reference embedding: extracts from the current document directly.
       Does not recursively expand the current doc (that would loop), but the
       depth guard still prevents unbounded nesting when a self-embed is
       encountered during recursive expansion of another note. *)
    let embed_self
          ~(fragment : Parse.Wikilink.fragment option)
          (extract : Cmarkit.Block.t list -> Cmarkit.Block.t list)
      : Cmarkit.Block.t option
      =
      if embed_depth >= max_depth
      then Some depth_fallback
      else (
        let new_depth = embed_depth + 1 in
        let blocks_to_embed = extract (non_fm_blocks curr_doc) in
        let block_meta =
          Cmarkit.Meta.add
            embed_meta_key
            { depth = new_depth; source_path = curr_path; fragment }
            Cmarkit.Meta.none
        in
        Some (Cmarkit.Block.Blocks (blocks_to_embed, block_meta)))
    in
    (* Cross-file embedding: delegates to embed_note for lookup + recursion. *)
    let embed
          ~(fragment : Parse.Wikilink.fragment option)
          (path : string)
          (extract : Cmarkit.Block.t list -> Cmarkit.Block.t list)
      : Cmarkit.Block.t option
      =
      embed_note ~embed_depth ~max_depth ~depth_fallback ~fragment docs_tbl path extract
    in
    match Cmarkit.Meta.find Resolve.resolved_key meta with
    (* Non-embeddable: no target, unresolved, or non-markdown file *)
    | None | Some Resolve.Unresolved | Some (Resolve.File _) -> None
    (* Self-references: extract from current doc *)
    | Some Resolve.Curr_file ->
      embed_self ~fragment:None (fun blocks -> blocks)
    | Some (Resolve.Curr_heading { heading; slug; _ }) ->
      embed_self
        ~fragment:(Some (Parse.Wikilink.Heading [ heading ]))
        (fun blocks -> Parse.Extract.get_heading_section blocks slug)
    | Some (Resolve.Curr_block { block_id }) ->
      embed_self
        ~fragment:(Some (Parse.Wikilink.Block_ref block_id))
        (fun blocks ->
          Option.to_list (Parse.Extract.get_block_by_caret_id blocks block_id))
    (* Cross-file references: look up in vault and recursively expand *)
    | Some (Resolve.Note { path }) ->
      embed ~fragment:None path (fun blocks -> blocks)
    | Some (Resolve.Heading { path; heading; slug; _ }) ->
      embed
        ~fragment:(Some (Parse.Wikilink.Heading [ heading ]))
        path
        (fun blocks -> Parse.Extract.get_heading_section blocks slug)
    | Some (Resolve.Block { path; block_id }) ->
      embed
        ~fragment:(Some (Parse.Wikilink.Block_ref block_id))
        path
        (fun blocks ->
          Option.to_list (Parse.Extract.get_block_by_caret_id blocks block_id))
  in
  (* The mapper acts on Paragraph blocks, checking for embed wikilinks and
     image links pointing to notes. *)
  let mapper =
    Cmarkit.Mapper.make
      ~block_ext_default:(fun _m b -> Some b)
      ~inline_ext_default:(fun _m i -> Some i)
      ~block:(fun _mapper block ->
        match block with
        | Cmarkit.Block.Paragraph (p, _) ->
          (match extract_embed_source (Cmarkit.Block.Paragraph.inline p) with
           | None -> Cmarkit.Mapper.default
           | Some source ->
             let meta, depth_fallback =
               match source with
               | Wikilink_embed (wl, wl_meta) -> wl_meta, fallback_block wl wl_meta
               | Image_embed meta -> meta, block
             in
             (match try_embed meta ~depth_fallback ~curr_doc:doc with
              | Some spliced -> Cmarkit.Mapper.ret spliced
              | None -> Cmarkit.Mapper.default))
        | _ -> Cmarkit.Mapper.default)
      ()
  in
  Cmarkit.Mapper.map_doc mapper doc
;;

(** Expand all embed wikilinks and image links in a list of resolved docs.
    [max_depth] (default 5) controls how many transclusion levels are allowed
    before falling back to a plain link (wikilinks) or keeping the original
    image (image links). *)
let expand_docs ?(max_depth = 5) (docs : (string * Cmarkit.Doc.t) list)
  : (string * Cmarkit.Doc.t) list
  =
  let docs_tbl = Hashtbl.of_alist_exn (module String) docs in
  List.map docs ~f:(fun (rel_path, doc) ->
    rel_path, expand_doc ~embed_depth:0 ~max_depth ~curr_path:rel_path docs_tbl doc)
;;

(** Reverse transclusion: replace each [Block.Blocks] carrying {!embed_meta}
    with a paragraph containing an embed wikilink [!\[\[source_path#fragment\]\]].

    This restores the original embedding syntax (up to the difference between
    wikilink and commonmark inline link, as noted in {!Spec.reverse_embed}).

    The [.md] extension is stripped from [source_path] to produce idiomatic
    wikilink targets.  Nested embeds are reversed recursively — innermost
    first, since the mapper walks depth-first. *)
let reverse_embed_doc (doc : Cmarkit.Doc.t) : Cmarkit.Doc.t =
  let strip_md (path : string) : string =
    match String.chop_suffix path ~suffix:".md" with
    | Some s -> s
    | None -> path
  in
  let mapper =
    Cmarkit.Mapper.make
      ~block_ext_default:(fun _m b -> Some b)
      ~inline_ext_default:(fun _m i -> Some i)
      ~block:(fun _mapper block ->
        match block with
        | Cmarkit.Block.Blocks (_, meta) ->
          (match Cmarkit.Meta.find embed_meta_key meta with
           | None -> Cmarkit.Mapper.default
           | Some { source_path; fragment; _ } ->
             let target =
               if String.is_empty source_path then None else Some (strip_md source_path)
             in
             let wl : Parse.Wikilink.t =
               { target; fragment; display = None; embed = true }
             in
             let inline =
               Parse.Wikilink.Ext_wikilink (wl, Cmarkit.Meta.none)
             in
             let p =
               Cmarkit.Block.Paragraph.make
                 (Cmarkit.Inline.Inlines ([ inline ], Cmarkit.Meta.none))
             in
             Cmarkit.Mapper.ret (Cmarkit.Block.Paragraph (p, Cmarkit.Meta.none)))
        | _ -> Cmarkit.Mapper.default)
      ()
  in
  Cmarkit.Mapper.map_doc mapper doc
;;

module For_test = struct
  let parse_blocks (md : string) : Cmarkit.Block.t list = non_fm_blocks (Parse.of_string md)

  let doc_of_blocks (blocks : Cmarkit.Block.t list) : Cmarkit.Doc.t =
    Cmarkit.Doc.make (Cmarkit.Block.Blocks (blocks, Cmarkit.Meta.none))
  ;;

  let print_blocks (blocks : Cmarkit.Block.t list) : unit =
    let doc = doc_of_blocks blocks in
    print_endline (Parse.commonmark_of_doc doc)
  ;;
end

let%expect_test "is_expandable_embed_paragraph: sole embed paragraph" =
  let blocks = For_test.parse_blocks "![[target]]" in
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
