(** Find references (backlinks): find all links in the vault that target a
    given file, heading, or block.

    Spec: {!page-"feature-find-references"}.
    Walks pre-resolved vault docs and reads {!Oystermark.Vault.Resolve.resolved_key}
    metadata attached during vault building, avoiding re-parsing and re-resolving.
    Uses {!Link_collect} for link extraction at the cursor position and
    {!Oystermark.Vault.Resolve} for single-file resolution during target detection. *)

open Core

(** {1:implementation Implementation} *)

(** A single reference: the document it appears in and its byte range. *)
type reference =
  { rel_path : string
  ; first_byte : int
  ; last_byte : int
  }
[@@deriving sexp, equal, compare]

(** The kind of target we are looking for references to. *)
type target =
  | Path_only of { path : string }
  | Path_heading of
      { path : string
      ; slug : string
      }
  | Path_block of
      { path : string
      ; block_id : string
      }

(** {2 Target detection}

    Determine what the cursor is on: a link, a heading, or a block ID.
    See {!page-"feature-find-references".activation}. *)

(** Check whether [line_str] ends with a block ID marker [ ^id].
    Returns [Some id] if so. *)
let block_id_of_line (line_str : string) : string option =
  match String.lsplit2 line_str ~on:'^' with
  | Some (prefix, id) ->
    let prefix = String.rstrip prefix in
    if
      String.length prefix > 0
      && (not (String.is_empty id))
      && String.for_all id ~f:(fun c ->
        Char.is_alphanum c || Char.equal c '-' || Char.equal c '_')
    then Some id
    else None
  | None -> None
;;

(** Determine the reference target from cursor position.

    Tries in order:
    1. Cursor on a link — resolve it to get the target.
    2. Cursor on a heading — target is (current file, heading slug).
    3. Cursor on a block ID line — target is (current file, block id). *)
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
  let links = Link_collect.collect_links doc in
  match Link_collect.find_at_offset links offset with
  | Some link_ref ->
    let resolved = Oystermark.Vault.Resolve.resolve link_ref rel_path index in
    (match resolved with
     | Oystermark.Vault.Resolve.Note { path } | File { path } ->
       (match link_ref.fragment with
        | Some (Oystermark.Vault.Link_ref.Heading hs) ->
          let slug =
            String.concat ~sep:"-" (List.map hs ~f:Oystermark.Parse.Heading_slug.slugify)
          in
          Some (Path_heading { path; slug })
        | Some (Block_ref bid) -> Some (Path_block { path; block_id = bid })
        | None -> Some (Path_only { path }))
     | Heading { path; slug; _ } -> Some (Path_heading { path; slug })
     | Block { path; block_id } -> Some (Path_block { path; block_id })
     | Curr_file ->
       (match link_ref.fragment with
        | Some (Oystermark.Vault.Link_ref.Heading hs) ->
          let slug =
            String.concat ~sep:"-" (List.map hs ~f:Oystermark.Parse.Heading_slug.slugify)
          in
          Some (Path_heading { path = rel_path; slug })
        | Some (Block_ref bid) -> Some (Path_block { path = rel_path; block_id = bid })
        | None -> Some (Path_only { path = rel_path }))
     | Curr_heading { slug; _ } -> Some (Path_heading { path = rel_path; slug })
     | Curr_block { block_id } -> Some (Path_block { path = rel_path; block_id })
     | Unresolved -> None)
  | None ->
    (* Not on a link — check if cursor is on a heading or block ID line. *)
    let lines = String.split_lines content in
    (match List.nth lines line with
     | None -> None
     | Some line_str ->
       (match Hover.heading_level_of_line line_str with
        | Some _ ->
          let text =
            String.lstrip line_str ~drop:(fun c -> Char.equal c '#')
            |> String.lstrip ~drop:(fun c -> Char.equal c ' ')
          in
          let slug = Oystermark.Parse.Heading_slug.slugify text in
          Some (Path_heading { path = rel_path; slug })
        | None ->
          (match block_id_of_line line_str with
           | Some block_id -> Some (Path_block { path = rel_path; block_id })
           | None -> None)))
;;

(** {2 Vault scanning}

    Scan pre-resolved vault documents for links matching the target.
    Reads {!Oystermark.Vault.Resolve.resolved_key} from AST node metadata
    instead of re-resolving each link.
    See {!page-"feature-find-references".collection}. *)

(** Extract the path from a resolved target, substituting [source_rel_path]
    for targets that refer to the current file. *)
let path_of_resolved ~(source_rel_path : string) (t : Oystermark.Vault.Resolve.target)
  : string option
  =
  match t with
  | Oystermark.Vault.Resolve.Note { path }
  | File { path }
  | Heading { path; _ }
  | Block { path; _ } -> Some path
  | Curr_file | Curr_heading _ | Curr_block _ -> Some source_rel_path
  | Unresolved -> None
;;

(** Extract the heading slug from a resolved target, if any. *)
let slug_of_resolved (t : Oystermark.Vault.Resolve.target) : string option =
  match t with
  | Oystermark.Vault.Resolve.Heading { slug; _ } | Curr_heading { slug; _ } -> Some slug
  | _ -> None
;;

(** Extract the block id from a resolved target, if any. *)
let block_id_of_resolved (t : Oystermark.Vault.Resolve.target) : string option =
  match t with
  | Oystermark.Vault.Resolve.Block { block_id; _ } | Curr_block { block_id } ->
    Some block_id
  | _ -> None
;;

(** Check whether a pre-resolved target matches our reference target. *)
let resolved_matches
      (ref_target : target)
      (resolved : Oystermark.Vault.Resolve.target)
      ~(source_rel_path : string)
  : bool
  =
  match ref_target, path_of_resolved ~source_rel_path resolved with
  | _, None -> false
  | Path_only { path }, Some rp -> String.equal path rp
  | Path_heading { path; slug }, Some rp ->
    String.equal path rp
    &&
      (match slug_of_resolved resolved with
      | Some s -> String.equal s slug
      | None -> false)
  | Path_block { path; block_id }, Some rp ->
    String.equal path rp
    &&
      (match block_id_of_resolved resolved with
      | Some bid -> String.equal bid block_id
      | None -> false)
;;

(** Collect references from a single pre-resolved document by folding over
    its AST and reading {!Oystermark.Vault.Resolve.resolved_key} metadata. *)
let collect_from_doc
      ~(source_rel_path : string)
      (ref_target : target)
      (doc : Cmarkit.Doc.t)
  : reference list
  =
  let check_meta acc (meta : Cmarkit.Meta.t) =
    match Cmarkit.Meta.find Oystermark.Vault.Resolve.resolved_key meta with
    | None -> acc
    | Some resolved ->
      if resolved_matches ref_target resolved ~source_rel_path
      then (
        let loc = Cmarkit.Meta.textloc meta in
        if Cmarkit.Textloc.is_none loc
        then acc
        else
          { rel_path = source_rel_path
          ; first_byte = Cmarkit.Textloc.first_byte loc
          ; last_byte = Cmarkit.Textloc.last_byte loc
          }
          :: acc)
      else acc
  in
  let folder =
    Cmarkit.Folder.make
      ~inline:(fun _f acc i ->
        match i with
        | Cmarkit.Inline.Link (_, meta) | Cmarkit.Inline.Image (_, meta) ->
          Cmarkit.Folder.ret (check_meta acc meta)
        | _ -> Cmarkit.Folder.default)
      ~inline_ext_default:(fun _f acc i ->
        match i with
        | Oystermark.Parse.Wikilink.Ext_wikilink (_, meta) -> check_meta acc meta
        | _ -> acc)
      ~block_ext_default:(fun _f acc _b -> acc)
      ()
  in
  List.rev (Cmarkit.Folder.fold_doc folder [] doc)
;;

(** Scan all pre-resolved vault documents for references matching [ref_target].

    Each document's AST already has {!Oystermark.Vault.Resolve.resolved_key}
    metadata on every link node, so no re-parsing or re-resolving is needed. *)
let scan_vault ~(docs : (string * Cmarkit.Doc.t) list) (ref_target : target)
  : reference list
  =
  Trace_core.with_span ~__FILE__ ~__LINE__ "find_references.scan_vault"
  @@ fun _sp ->
  let refs =
    List.concat_map docs ~f:(fun (source_rel_path, doc) ->
      collect_from_doc ~source_rel_path ref_target doc)
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

    [docs] is the list of pre-resolved vault documents (with
    {!Oystermark.Vault.Resolve.resolved_key} metadata attached).

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
  | Some ref_target -> scan_vault ~docs ref_target
;;

(** {2 Counting}

    Used by {!Inlay_hints} for reference count computation. *)

(** Count how many links across the vault resolve to [path] (any fragment). *)
let count_file_refs ~(docs : (string * Cmarkit.Doc.t) list) ~(path : string) : int =
  List.length (scan_vault ~docs (Path_only { path }))
;;

(** Count how many links across the vault resolve to [path] with heading [slug]. *)
let count_heading_refs
      ~(docs : (string * Cmarkit.Doc.t) list)
      ~(path : string)
      ~(slug : string)
  : int
  =
  List.length (scan_vault ~docs (Path_heading { path; slug }))
;;

(** {1:test Test} *)

let%test_module "block_id_of_line" =
  (module struct
    let%expect_test "simple block id" =
      print_s [%sexp (block_id_of_line "some text ^abc123" : string option)];
      [%expect {| (abc123) |}]
    ;;

    let%expect_test "no block id" =
      print_s [%sexp (block_id_of_line "no block id here" : string option)];
      [%expect {| () |}]
    ;;

    let%expect_test "caret at start not valid" =
      print_s [%sexp (block_id_of_line "^notvalid" : string option)];
      [%expect {| () |}]
    ;;
  end)
;;

(** Helper: build an index and pre-resolved docs for testing. *)
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
    let index = Oystermark.Vault.build_index ~md_docs ~other_files ~dirs:[] in
    let resolved_docs = Oystermark.Vault.Resolve.resolve_docs md_docs index in
    index, resolved_docs
  ;;
end

let%test_module "detect_target" =
  (module struct
    let files =
      [ "note-a.md", "# Alpha\n\n## Section One\n\nBody text ^block1\n"
      ; "note-b.md", "# Beta\n\nLink to [[note-a]] here.\n"
      ]
    ;;

    let index, _docs = For_test.make_vault files

    let show ~rel_path ~content ~line ~character =
      match detect_target ~index ~rel_path ~content ~line ~character with
      | None -> print_endline "<none>"
      | Some (Path_only { path }) -> printf "Path_only %s\n" path
      | Some (Path_heading { path; slug }) -> printf "Path_heading %s#%s\n" path slug
      | Some (Path_block { path; block_id }) -> printf "Path_block %s#^%s\n" path block_id
    ;;

    let%expect_test "cursor on wikilink" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-b.md" in
      show ~rel_path:"note-b.md" ~content ~line:2 ~character:13;
      [%expect {| Path_only note-a.md |}]
    ;;

    let%expect_test "cursor on heading" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-a.md" in
      show ~rel_path:"note-a.md" ~content ~line:2 ~character:3;
      [%expect {| Path_heading note-a.md#section-one |}]
    ;;

    let%expect_test "cursor on block id line" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-a.md" in
      show ~rel_path:"note-a.md" ~content ~line:4 ~character:5;
      [%expect {| Path_block note-a.md#^block1 |}]
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
