(** Find references (backlinks): find all links in the vault that target a
    given file, heading, or block.

    Spec: {!page-"feature-find-references"}.
    Reuses {!Link_collect} for link extraction and
    {!Oystermark.Vault.Resolve} for resolution. *)

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
  | Path_heading of { path : string; slug : string }
  | Path_block of { path : string; block_id : string }

(** {2 Target detection}

    Determine what the cursor is on: a link, a heading, or a block ID.
    See {!page-"feature-find-references".activation}. *)

(** Check whether [line_str] ends with a block ID marker [ ^id].
    Returns [Some id] if so. *)
let block_id_of_line (line_str : string) : string option =
  (* Pattern: " ^<id>" at end of line, where id is alphanumeric *)
  match String.lsplit2 line_str ~on:'^' with
  | Some (prefix, id) ->
    let prefix = String.rstrip prefix in
    if String.length prefix > 0
       && not (String.is_empty id)
       && String.for_all id ~f:(fun c -> Char.is_alphanum c || Char.equal c '-' || Char.equal c '_')
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

    Scan all vault documents for links matching the target.
    See {!page-"feature-find-references".collection}. *)

(** Check whether a resolved target matches our reference target. *)
let target_matches
      (ref_target : target)
      (resolved : Oystermark.Vault.Resolve.target)
      (link_ref : Oystermark.Vault.Link_ref.t)
      ~(source_rel_path : string)
  : bool
  =
  let resolved_path =
    match resolved with
    | Oystermark.Vault.Resolve.Note { path } | File { path } -> Some path
    | Heading { path; _ } -> Some path
    | Block { path; _ } -> Some path
    | Curr_file -> Some source_rel_path
    | Curr_heading _ -> Some source_rel_path
    | Curr_block _ -> Some source_rel_path
    | Unresolved -> None
  in
  let resolved_slug =
    match resolved with
    | Oystermark.Vault.Resolve.Heading { slug; _ } -> Some slug
    | Curr_heading { slug; _ } -> Some slug
    | Note _ | File _ | Curr_file ->
      (* Resolve fell back — check the link_ref fragment directly. *)
      (match link_ref.fragment with
       | Some (Oystermark.Vault.Link_ref.Heading hs) ->
         Some
           (String.concat ~sep:"-" (List.map hs ~f:Oystermark.Parse.Heading_slug.slugify))
       | _ -> None)
    | _ -> None
  in
  let resolved_block_id =
    match resolved with
    | Oystermark.Vault.Resolve.Block { block_id; _ } -> Some block_id
    | Curr_block { block_id } -> Some block_id
    | Note _ | File _ | Curr_file ->
      (match link_ref.fragment with
       | Some (Block_ref bid) -> Some bid
       | _ -> None)
    | _ -> None
  in
  match ref_target, resolved_path with
  | _, None -> false
  | Path_only { path }, Some rp -> String.equal path rp
  | Path_heading { path; slug }, Some rp ->
    String.equal path rp
    && (match resolved_slug with
        | Some s -> String.equal s slug
        | None -> false)
  | Path_block { path; block_id }, Some rp ->
    String.equal path rp
    && (match resolved_block_id with
        | Some bid -> String.equal bid block_id
        | None -> false)
;;

(** Scan all documents in the index for references matching [ref_target].

    Each document is parsed to extract links, which are then resolved against
    the index. Links whose resolution matches [ref_target] are collected. *)
let scan_vault
      ~(index : Oystermark.Vault.Index.t)
      ~(read_file : string -> string option)
      (ref_target : target)
  : reference list
  =
  Trace_core.with_span ~__FILE__ ~__LINE__ "find_references.scan_vault"
  @@ fun _sp ->
  let refs =
    List.concat_map index.files ~f:(fun (entry : Oystermark.Vault.Index.file_entry) ->
      let source_rel_path = entry.rel_path in
      match read_file source_rel_path with
      | None -> []
      | Some content ->
        let doc = Lsp_util.parse_doc content in
        let links = Link_collect.collect_links doc in
        List.filter_map links ~f:(fun (ll : Link_collect.located_link) ->
          let resolved =
            Oystermark.Vault.Resolve.resolve ll.link_ref source_rel_path index
          in
          if target_matches ref_target resolved ll.link_ref ~source_rel_path
          then
            Some
              { rel_path = source_rel_path
              ; first_byte = ll.first_byte
              ; last_byte = ll.last_byte
              }
          else None))
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

    Returns a sorted list of {!reference} values, or an empty list if the
    cursor is not on a link, heading, or block ID. *)
let find_references
      ~(index : Oystermark.Vault.Index.t)
      ~(rel_path : string)
      ~(content : string)
      ~(line : int)
      ~(character : int)
      ~(read_file : string -> string option)
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
  | Some ref_target -> scan_vault ~index ~read_file ref_target
;;

(** {2 Counting}

    Used by {!Inlay_hints} for reference count computation. *)

(** Count how many links across the vault resolve to [path] (any fragment). *)
let count_file_refs
      ~(index : Oystermark.Vault.Index.t)
      ~(read_file : string -> string option)
      ~(path : string)
  : int
  =
  List.length (scan_vault ~index ~read_file (Path_only { path }))
;;

(** Count how many links across the vault resolve to [path] with heading [slug]. *)
let count_heading_refs
      ~(index : Oystermark.Vault.Index.t)
      ~(read_file : string -> string option)
      ~(path : string)
      ~(slug : string)
  : int
  =
  List.length (scan_vault ~index ~read_file (Path_heading { path; slug }))
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

let%test_module "detect_target" =
  (module struct
    let files =
      [ "note-a.md", "# Alpha\n\n## Section One\n\nBody text ^block1\n"
      ; "note-b.md", "# Beta\n\nLink to [[note-a]] here.\n"
      ]
    ;;

    let make_index files =
      let md_docs =
        List.filter_map files ~f:(fun (rel_path, content) ->
          if String.is_suffix rel_path ~suffix:".md"
          then Some (rel_path, Oystermark.Parse.of_string content)
          else None)
      in
      Oystermark.Vault.build_index ~md_docs ~other_files:[] ~dirs:[]
    ;;

    let index = make_index files

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

    let make_index files =
      let md_docs =
        List.filter_map files ~f:(fun (rel_path, content) ->
          if String.is_suffix rel_path ~suffix:".md"
          then Some (rel_path, Oystermark.Parse.of_string content)
          else None)
      in
      Oystermark.Vault.build_index ~md_docs ~other_files:[] ~dirs:[]
    ;;

    let index = make_index files
    let read_file rp = List.Assoc.find files ~equal:String.equal rp

    let show ~rel_path ~content ~line ~character =
      let refs = find_references ~index ~rel_path ~content ~line ~character ~read_file () in
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
