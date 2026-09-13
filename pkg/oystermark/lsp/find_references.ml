(** Find references (backlinks): find all links in the vault that target a
    given file, heading, or block.

    Spec: {!page-"feature-find-references"}.
    Walks parsed vault docs and resolves authored links against the vault index.
    Uses {!Link_collect} for link extraction at the cursor position and
    {!Oystermark.Vault.Index.resolve} for target detection and collection. *)

open Core

(** {1:implementation Implementation} *)

(** A single reference: the document it appears in and its byte range.

    [in_toc] marks a link the server wrote rather than the author: one inside a
    [::: toc] region (see {!page-"feature-toc"}).  It is a tag and not a filter
    because the callers want opposite things from it — a count that includes
    generated links is wrong, a rename that skips them leaves the TOC stale.
    See {!page-"feature-find-references".generated}. *)
type reference =
  { rel_path : string
  ; first_byte : int
  ; last_byte : int
  ; in_toc : bool
  }
[@@deriving sexp, equal, compare]

(** What references are looked for: the note or asset at [path], or the anchor
    at [address] in the note at [path]. *)
type target = Oystermark.Vault.Rename.target =
  { path : string
  ; address : Oystermark.Note.Anchor.Address.t option
  }

(** {2 Target detection}

    Determine what the cursor is on: a link, a heading, or a block ID.
    See {!page-"feature-find-references".activation}. *)

(** Determine the reference target from cursor position.

    Tries in order:
    1. Cursor on a link — resolve it to get the target.
    2. Cursor on an anchor's line — target is (current file, that anchor),
       with the anchor found by {!Anchors}, i.e. by the parser. *)
let detect_target
      ~(index : Oystermark.Vault.Index.t)
      ~(rel_path : string)
      ~(content : string)
      ~(line : int)
      ~(character : int)
  : target option
  =
  let offset = Lsp_util.byte_offset_of_position content ~line ~character in
  let doc = Lsp_util.parse_doc content in
  let links = Link_collect.collect_links ~index ~rel_path doc in
  match Link_collect.find_at_offset links offset with
  | Some link_ref ->
    let resolved = Oystermark.Vault.Index.resolve index rel_path link_ref in
    (match resolved with
     | Error _ -> None
     | Ok (Oystermark.Vault.Index.Note path | Asset path) -> Some { path; address = None }
     | Ok (Anchor { note_path; anchor }) ->
       Some
         { path = note_path
         ; address = Some (Oystermark.Note.Anchor.address anchor.value)
         })
  | None ->
    (* Not on a link — is the cursor on an anchor?  The anchors come from the
       same [doc] the links did, so a [#] or a [ ^id] inside a code block is
       not one, and a heading's identifier is the one the parser assigned
       rather than a slug re-derived here.  See {!page-"feature-index"}. *)
    Anchors.at_line (Anchors.of_doc doc) ~line
    |> Option.map ~f:(fun a -> { path = rel_path; address = Some (Anchors.address a) })
;;

(** {2 Vault scanning}

    Scan vault documents for authored links matching the target.
    See {!page-"feature-find-references".collection}. *)

(** Check whether a resolved index target matches our reference target. *)
let resolved_matches (ref_target : target) (resolved : Oystermark.Vault.Index.target)
  : bool
  =
  match ref_target, resolved with
  | { path; address = None }, resolved ->
    String.equal path (Oystermark.Vault.Index.target_path resolved)
  | { path; address = Some address }, Anchor { note_path; anchor } ->
    String.equal path note_path
    && Oystermark.Note.Anchor.Address.equal
         address
         (Oystermark.Note.Anchor.address anchor.value)
  | _ -> false
;;

(** What the fold carries: the references so far, and whether the node being
    visited sits inside a [::: toc] region. *)
type acc =
  { refs : reference list
  ; in_toc : bool
  }

(** Collect references from a single document by folding over its authored links
    and resolving each one against [index].

    A [::: toc] region is recognized by {!Toc.is_toc}, i.e. by the parser and
    by the same predicate that decides what the TOC feature owns: there is one
    definition of what a region is, so a [::: toc] written inside a code block
    is not one here either. *)
let collect_from_doc
      ~(index : Oystermark.Vault.Index.t)
      ~(source_rel_path : string)
      (ref_target : target)
      (doc : Cmarkit.Doc.t)
  : reference list
  =
  let check_link acc link_ref (meta : Cmarkit.Meta.t) =
    match Oystermark.Vault.Index.resolve index source_rel_path link_ref with
    | Error _ -> acc
    | Ok resolved ->
      if resolved_matches ref_target resolved
      then (
        let loc = Cmarkit.Meta.textloc meta in
        if Cmarkit.Textloc.is_none loc
        then acc
        else
          { acc with
            refs =
              { rel_path = source_rel_path
              ; first_byte = Cmarkit.Textloc.first_byte loc
              ; last_byte = Cmarkit.Textloc.last_byte loc
              ; in_toc = acc.in_toc
              }
              :: acc.refs
          })
      else acc
  in
  let folder =
    Cmarkit.Folder.make
      ~block:(fun f acc (b : Cmarkit.Block.t) ->
        match b with
        | Cmarkit.Block.Ext_div (d, _) when Toc.is_toc d ->
          (* Descend with the flag raised, then restore it: a region may sit
             inside anything, and anything may follow it. *)
          let inner =
            Cmarkit.Folder.fold_block
              f
              { acc with in_toc = true }
              (Cmarkit.Block.Div.block d)
          in
          Cmarkit.Folder.ret { inner with in_toc = acc.in_toc }
        | _ -> Cmarkit.Folder.default)
      ~inline:(fun _f acc i ->
        match i with
        | Cmarkit.Inline.Link (link, meta) | Cmarkit.Inline.Image (link, meta) ->
          (match
             Oystermark.Note.Link.Ref.of_cmark_reference
               (Cmarkit.Inline.Link.reference link)
           with
           | Some link_ref -> Cmarkit.Folder.ret (check_link acc link_ref meta)
           | None -> Cmarkit.Folder.default)
        | _ -> Cmarkit.Folder.default)
      ~inline_ext_default:(fun _f acc i ->
        match i with
        | Cmarkit.Inline.Ext_wikilink (w, meta) ->
          check_link acc (Oystermark.Note.Link.Ref.of_wikilink w) meta
        | _ -> acc)
      ~block_ext_default:(fun _f acc _b -> acc)
      ()
  in
  List.rev (Cmarkit.Folder.fold_doc folder { refs = []; in_toc = false } doc).refs
;;

(** Scan all vault documents for references matching [ref_target]. *)
let scan_vault
      ~(index : Oystermark.Vault.Index.t)
      ~(docs : (string * Cmarkit.Doc.t) list)
      (ref_target : target)
  : reference list
  =
  Trace_core.with_span ~__FILE__ ~__LINE__ "find_references.scan_vault"
  @@ fun _sp ->
  let refs =
    List.concat_map docs ~f:(fun (source_rel_path, doc) ->
      collect_from_doc ~index ~source_rel_path ref_target doc)
  in
  let sorted =
    List.sort refs ~compare:(fun a b ->
      let c = String.compare a.rel_path b.rel_path in
      if c <> 0 then c else Int.compare a.first_byte b.first_byte)
  in
  Trace_core.add_data_to_span _sp [ "num_refs", `Int (List.length sorted) ];
  sorted
;;

(** {2 End-to-end}

    See {!page-"feature-find-references"}. *)

(** Find all references to the target at cursor position [(line, character)]
    in file [rel_path] with [content].

    [docs] is the list of parsed vault documents.

    Returns a sorted list of {!reference} values, or an empty list if the
    cursor is not on a link, heading, or block ID. *)
let find_references
      ~(index : Oystermark.Vault.Index.t)
      ~(docs : (string * Cmarkit.Doc.t) list)
      ~(rel_path : string)
      ~(content : string)
      ~(line : int)
      ~(character : int)
      ()
  : reference list
  =
  Trace_core.with_span ~__FILE__ ~__LINE__ "find_references"
  @@ fun _sp ->
  Trace_core.add_data_to_span
    _sp
    [ "rel_path", `String rel_path; "line", `Int line; "character", `Int character ];
  match detect_target ~index ~rel_path ~content ~line ~character with
  | None ->
    Trace_core.add_data_to_span _sp [ "result", `String "no_target" ];
    []
  | Some ref_target -> scan_vault ~index ~docs ref_target
;;

(** {2 Counting}

    Used by {!Inlay_hints} for reference count computation. *)

(** Count how many links across the vault resolve to [path] (any fragment). *)
let count_file_refs ~index ~(docs : (string * Cmarkit.Doc.t) list) ~(path : string) : int =
  List.length (scan_vault ~index ~docs { path; address = None })
;;

(** Count how many links across the vault resolve to [path] with heading [slug]. *)
let count_heading_refs
      ~index
      ~(docs : (string * Cmarkit.Doc.t) list)
      ~(path : string)
      ~(slug : string)
  : int
  =
  List.length (scan_vault ~index ~docs { path; address = Some (Heading slug) })
;;

(** {1:test Test} *)

(** Helper: build an index and parsed docs for testing. *)
module For_test = struct
  let make_vault (files : (string * string) list)
    : Oystermark.Vault.Index.t * (string * Cmarkit.Doc.t) list
    =
    let md_docs =
      List.filter_map files ~f:(fun (rel_path, content) ->
        if String.is_suffix rel_path ~suffix:".md"
        then Some (rel_path, Oystermark.Parse.of_string ~locs:true content)
        else None)
    in
    let other_files =
      List.filter_map files ~f:(fun (p, _) ->
        if not (String.is_suffix p ~suffix:".md") then Some p else None)
    in
    let index = Oystermark.Vault.build_index ~md_docs ~other_files () in
    index, md_docs
  ;;
end

let%test_module "detect_target" =
  (module struct
    let files =
      [ "note-a.md", "# Alpha\n\n## Section One\n\nBody text ^block1\n"
      ; "note-b.md", "# Beta\n\nLink to [[note-a]] here.\n"
      ; "note-h.md", "# Theta\n\nSome text.\n\n{#aside}\n> An aside block.\n"
      ; "note-i.md", "# Iota\n\nRef [[note-h#aside]].\n"
      ]
    ;;

    let index, _docs = For_test.make_vault files

    let show ~rel_path ~content ~line ~character =
      match detect_target ~index ~rel_path ~content ~line ~character with
      | None -> print_endline "<none>"
      | Some { path; address = None } -> printf "Note %s\n" path
      | Some { path; address = Some (Heading slug) } -> printf "Heading %s#%s\n" path slug
      | Some { path; address = Some (Caret id) } -> printf "Caret %s#^%s\n" path id
      | Some { path; address = Some (Attr id) } -> printf "Attr %s#%s\n" path id
    ;;

    let%expect_test "cursor on wikilink" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-b.md" in
      show ~rel_path:"note-b.md" ~content ~line:2 ~character:13;
      [%expect {| Note note-a.md |}]
    ;;

    let%expect_test "cursor on heading" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-a.md" in
      show ~rel_path:"note-a.md" ~content ~line:2 ~character:3;
      [%expect {| Heading note-a.md#section-one |}]
    ;;

    let%expect_test "cursor on block id line" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-a.md" in
      show ~rel_path:"note-a.md" ~content ~line:4 ~character:5;
      [%expect {| Caret note-a.md#^block1 |}]
    ;;

    (* Cursor on a link resolving to an attribute anchor ([{#aside}] in note-h).
       See {!page-"feature-attribute-anchors"}. *)
    let%expect_test "cursor on link to attribute id" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-i.md" in
      show ~rel_path:"note-i.md" ~content ~line:2 ~character:8;
      [%expect {| Attr note-h.md#aside |}]
    ;;

    (* Cursor on a standalone block-attribute line [ {#aside} ]. *)
    let%expect_test "cursor on attribute anchor line" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-h.md" in
      show ~rel_path:"note-h.md" ~content ~line:4 ~character:2;
      [%expect {| Attr note-h.md#aside |}]
    ;;

    let%expect_test "cursor on plain text" =
      show ~rel_path:"note-a.md" ~content:"plain text" ~line:0 ~character:3;
      [%expect {| <none> |}]
    ;;
  end)
;;

let%test_module "find_references" =
  (module struct
    let files =
      [ "note-a.md", "# Alpha\n\n## Section One\n\nBody text ^block1\n"
      ; "note-b.md", "# Beta\n\nLink to [[note-a]] here.\n"
      ; ( "note-c.md"
        , "# Gamma\n\nSee [[note-a#Section One]].\n\nAlso [[note-a#^block1]].\n" )
      ; "note-d.md", "# Delta\n\nSelf ref [[#Alpha]] in note-a.\n"
      ; "note-e.md", "# Epsilon\n\nThe [key]{#the-key} span.\n"
      ; "note-f.md", "# Zeta\n\nOne [[note-e#the-key]] and two [[note-e#the-key]].\n"
      ]
    ;;

    let index, docs = For_test.make_vault files

    let show ~rel_path ~content ~line ~character =
      let refs = find_references ~index ~docs ~rel_path ~content ~line ~character () in
      List.iter refs ~f:(fun r ->
        printf "%s [%d-%d]\n" r.rel_path r.first_byte r.last_byte)
    ;;

    let%expect_test "references to note-a from link" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-b.md" in
      show ~rel_path:"note-b.md" ~content ~line:2 ~character:13;
      [%expect
        {|
        note-b.md [16-25]
        note-c.md [13-34]
        note-c.md [43-60]
        |}]
    ;;

    let%expect_test "references to heading from cursor on heading" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-a.md" in
      show ~rel_path:"note-a.md" ~content ~line:2 ~character:3;
      [%expect {| note-c.md [13-34] |}]
    ;;

    let%expect_test "references to block id from cursor on block line" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-a.md" in
      show ~rel_path:"note-a.md" ~content ~line:4 ~character:5;
      [%expect {| note-c.md [43-60] |}]
    ;;

    (* From a link targeting an attribute anchor, find all links resolving to
       the same anchor (note-f has two). See {!page-"feature-attribute-anchors"}. *)
    let%expect_test "references to attribute id from link" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-f.md" in
      show ~rel_path:"note-f.md" ~content ~line:2 ~character:8;
      [%expect
        {|
        note-f.md [12-29]
        note-f.md [39-56]
        |}]
    ;;

    let%expect_test "cursor not on anything" =
      show ~rel_path:"note-a.md" ~content:"plain text" ~line:0 ~character:3;
      [%expect {| |}]
    ;;

    let%expect_test "unresolved link returns empty" =
      show ~rel_path:"note-b.md" ~content:"See [[missing]]." ~line:0 ~character:7;
      [%expect {| |}]
    ;;
  end)
;;
