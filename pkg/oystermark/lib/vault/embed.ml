(** Note embedding: expand [![[NOTE]]] wikilinks as AST transclusion.

    This is a post-resolution, pre-render transformation. Each paragraph
    containing a single embed wikilink is replaced by [Block.Blocks] containing
    the target note's expanded blocks — ordinary cmarkit nodes, no custom
    extension needed.

    Cycle detection: a note cannot transitively embed itself. When a cycle is
    detected the embed is replaced with a plain fallback link instead. *)

open Core

(** Return [true] for frontmatter blocks (skipped when transcluding). *)
let is_frontmatter : Cmarkit.Block.t -> bool = function
  | Parse.Frontmatter.Frontmatter _ -> true
  | _ -> false
;;

(** Top-level content blocks of a doc, frontmatter excluded. *)
let doc_blocks (doc : Cmarkit.Doc.t) : Cmarkit.Block.t list =
  let top =
    match Cmarkit.Doc.block doc with
    | Cmarkit.Block.Blocks (bs, _) -> bs
    | other -> [ other ]
  in
  List.filter top ~f:(fun b -> not (is_frontmatter b))
;;

(** If [inline] is a single embed wikilink — ignoring surrounding
    whitespace-only {!Cmarkit.Inline.Text} nodes — return it. *)
let extract_single_embed
      (inline : Cmarkit.Inline.t)
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

(** A fallback paragraph that renders the embed as a plain link (used on cycle
    or when the target cannot be resolved to a note). *)
let fallback_block (wl : Parse.Wikilink.t) (meta : Cmarkit.Meta.t)
  : Cmarkit.Block.t
  =
  let link_inline =
    Parse.Wikilink.Ext_wikilink ({ wl with embed = false }, meta)
  in
  let p =
    Cmarkit.Block.Paragraph.make
      (Cmarkit.Inline.Inlines ([ link_inline ], Cmarkit.Meta.none))
  in
  Cmarkit.Block.Paragraph (p, Cmarkit.Meta.none)
;;

(** Collect the section starting at the first heading whose plain-text matches
    [heading_text] at [level], up to (but not including) the next heading of
    equal or lesser level. Returns [] when the heading is not found. *)
let get_heading_section
      (blocks : Cmarkit.Block.t list)
      ~(heading_text : string)
      ~(level : int)
  : Cmarkit.Block.t list
  =
  let rec find_start : Cmarkit.Block.t list -> Cmarkit.Block.t list = function
    | [] -> []
    | (Cmarkit.Block.Heading (h, _) as b) :: rest ->
      let text =
        Parse.inline_to_plain_text (Cmarkit.Block.Heading.inline h)
      in
      if String.equal text heading_text && Cmarkit.Block.Heading.level h = level
      then b :: collect_section level rest
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
let find_block_by_id
      (blocks : Cmarkit.Block.t list)
      (block_id : string)
  : Cmarkit.Block.t option
  =
  List.find blocks ~f:(function
    | Cmarkit.Block.Paragraph (_, meta) ->
      (match Cmarkit.Meta.find Parse.Block_id.meta_key meta with
       | Some (bid : Parse.Block_id.t) -> String.equal bid.id block_id
       | None -> false)
    | _ -> false)
;;

(** Expand embed wikilinks in [doc] into spliced [Block.Blocks].
    [ancestors] is the set of note vault-relative paths currently on the
    transclusion stack; it is used for cycle detection. *)
let rec expand_doc
      (docs_tbl : (string, Cmarkit.Doc.t) Hashtbl.t)
      (ancestors : String.Set.t)
      (doc : Cmarkit.Doc.t)
  : Cmarkit.Doc.t
  =
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
             (match expand_embed docs_tbl ancestors wl meta with
              | Some spliced -> Cmarkit.Mapper.ret spliced
              | None -> Cmarkit.Mapper.default))
        | _ -> Cmarkit.Mapper.default)
      ()
  in
  Cmarkit.Mapper.map_doc mapper doc

(** Resolve one embed wikilink to a [Block.Blocks] splice (or a fallback), or
    return [None] to leave the containing paragraph unchanged (e.g. media
    embeds that the HTML renderer handles). *)
and expand_embed
      (docs_tbl : (string, Cmarkit.Doc.t) Hashtbl.t)
      (ancestors : String.Set.t)
      (wl : Parse.Wikilink.t)
      (meta : Cmarkit.Meta.t)
  : Cmarkit.Block.t option
  =
  match Cmarkit.Meta.find Resolve.resolved_key meta with
  | None | Some Resolve.Unresolved ->
    (* Unresolved: leave as-is so the renderer can show a broken-link style. *)
    None
  | Some (Resolve.File _) ->
    (* Media file (image, video, etc.): delegate to the HTML renderer. *)
    None
  | Some Resolve.Curr_file
  | Some (Resolve.Curr_heading _)
  | Some (Resolve.Curr_block _) ->
    (* Self-embed is a cycle by definition. *)
    Some (fallback_block wl meta)
  | Some (Resolve.Note { path }) ->
    embed_note docs_tbl ancestors path wl meta (fun blocks -> blocks)
  | Some (Resolve.Heading { path; heading; level }) ->
    embed_note docs_tbl ancestors path wl meta (fun blocks ->
      get_heading_section blocks ~heading_text:heading ~level)
  | Some (Resolve.Block { path; block_id }) ->
    embed_note docs_tbl ancestors path wl meta (fun blocks ->
      Option.to_list (find_block_by_id blocks block_id))

(** Shared helper: look up [path] in the docs table, guard against cycles, then
    expand and splice via [Block.Blocks]. [extract] selects the subset of
    blocks (full note, heading section, or single block). *)
and embed_note
      (docs_tbl : (string, Cmarkit.Doc.t) Hashtbl.t)
      (ancestors : String.Set.t)
      (path : string)
      (wl : Parse.Wikilink.t)
      (meta : Cmarkit.Meta.t)
      (extract : Cmarkit.Block.t list -> Cmarkit.Block.t list)
  : Cmarkit.Block.t option
  =
  if Set.mem ancestors path
  then (* Cycle detected: render as a plain link. *)
    Some (fallback_block wl meta)
  else (
    match Hashtbl.find docs_tbl path with
    | None -> None
    | Some target_doc ->
      let new_ancestors = Set.add ancestors path in
      let expanded = expand_doc docs_tbl new_ancestors target_doc in
      let blocks = extract (doc_blocks expanded) in
      Some (Cmarkit.Block.Blocks (blocks, Cmarkit.Meta.none)))
;;

(** Expand all embed wikilinks in a list of resolved docs.
    Each doc is expanded independently; cycle detection is per-expansion. *)
let expand_docs (docs : (string * Cmarkit.Doc.t) list)
  : (string * Cmarkit.Doc.t) list
  =
  let docs_tbl = Hashtbl.of_alist_exn (module String) docs in
  List.map docs ~f:(fun (rel_path, doc) ->
    let ancestors = String.Set.singleton rel_path in
    rel_path, expand_doc docs_tbl ancestors doc)
;;
