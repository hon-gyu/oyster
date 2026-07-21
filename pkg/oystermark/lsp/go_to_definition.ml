(** Go-to-definition: resolve cursor position to target path and line.

    Spec: {!page-"feature-go-to-definition"}. *)

open Core

(** {1:implementation Implementation}
    {2 Heading / block-ID line lookup}

    See {!page-"feature-go-to-definition".resolution}. *)

(** Extract a 0-based [(line, character)] position from an optional
    {!Cmarkit.Textloc.t}.  When [content] (the target file's content, whose
    positions [Textloc]s are relative to) is given, [character] is a UTF-16
    column; otherwise a byte column.  Returns [(0, 0)] if [None].
    See {!page-"feature-go-to-definition".target_position}. *)
let position_of_textloc ?content (tl : Cmarkit.Textloc.t option) : int * int =
  match tl with
  | Some tl -> Lsp_util.position_of_textloc ?content tl
  | None -> 0, 0
;;

(** {2 End-to-end}

    See {!page-"feature-go-to-definition".resolution}. *)

(** The result of a go-to-definition request: a relative path and a 0-based
    [(line, character)] position.  [None] means the link was unresolved or no
    link was found. *)
type definition_result =
  { path : string
  ; line : int
  ; character : int
  }
[@@deriving sexp, equal, compare]

(** Given file [content] at [rel_path] in a vault with [index], find the
    definition target at cursor position [(line, character)].
    [read_file] reads a target file's content by relative path (returning
    [Some content] or [None]); it is used to compute a UTF-16 target column
    for cross-file targets.  Defaults to always returning [None], in which
    case cross-file columns degrade to byte offsets. *)
let go_to_definition
      ?(config : Lsp_config.t = Lsp_config.default)
      ?(read_file : string -> string option = fun _ -> None)
      ~(index : Oystermark.Vault.Index.t)
      ~(rel_path : string)
      ~(content : string)
      ~(line : int)
      ~(character : int)
      ()
  : definition_result option
  =
  Trace_core.with_span ~__FILE__ ~__LINE__ "go_to_definition"
  @@ fun _sp ->
  Trace_core.add_data_to_span
    _sp
    [ "rel_path", `String rel_path; "line", `Int line; "character", `Int character ];
  let offset = Lsp_util.byte_offset_of_position content ~line ~character in
  let doc = Lsp_util.parse_doc content in
  let links = Link_collect.collect_links doc in
  match Link_collect.find_at_offset links offset with
  | None ->
    Trace_core.add_data_to_span _sp [ "result", `String "no_link_at_cursor" ];
    None
  | Some link_ref ->
    let target = Oystermark.Vault.Resolve.resolve link_ref rel_path index in
    let resolution_tag =
      match target with
      | Oystermark.Vault.Resolve.Note _ -> "note"
      | File _ -> "file"
      | Heading _ -> "heading"
      | Block _ -> "block"
      | Attr _ -> "attr"
      | Curr_file -> "curr_file"
      | Curr_heading _ -> "curr_heading"
      | Curr_block _ -> "curr_block"
      | Curr_attr _ -> "curr_attr"
      | Unresolved -> "unresolved"
    in
    Trace_core.add_data_to_span _sp [ "resolution", `String resolution_tag ];
    (* Cross-file: read the target so its column is UTF-16-encoded; degrade to a
       byte column if the file can't be read.  [Textloc]s are full-file-relative
       (see {!Oystermark.Parse.Frontmatter.blank_frontmatter}), so the raw file
       content is used directly. *)
    let cross ~path loc =
      let content = read_file path in
      let line, character = position_of_textloc ?content loc in
      Some { path; line; character }
    in
    (* Self-file: the current buffer is the target's content. *)
    let self loc =
      let line, character = position_of_textloc ~content loc in
      Some { path = rel_path; line; character }
    in
    (match target with
     | Oystermark.Vault.Resolve.Note { path } | File { path } ->
       (* File found but fragment (if any) wasn't resolved — resolve fell back to the note. *)
       (match config.gtd_unresolved_fragment, link_ref.fragment with
        | Strict, Some _ -> None
        | _ -> Some { path; line = 0; character = 0 })
     | Heading { path; loc; _ } -> cross ~path loc
     | Block { path; loc; _ } -> cross ~path loc
     | Attr { path; loc; _ } -> cross ~path loc
     | Curr_file ->
       (* Self-reference but fragment (if any) wasn't resolved. *)
       (match config.gtd_unresolved_fragment, link_ref.fragment with
        | Strict, Some _ -> None
        | _ -> Some { path = rel_path; line = 0; character = 0 })
     | Curr_heading { loc; _ } -> self loc
     | Curr_block { loc; _ } -> self loc
     | Curr_attr { loc; _ } -> self loc
     | Unresolved -> None)
;;

(** {1:test Test} *)

let%test_module "go_to_definition" =
  (module struct
    (** Build a vault index from a list of [(rel_path, content)] pairs. *)
    let make_index (files : (string * string) list) : Oystermark.Vault.Index.t =
      let md_docs =
        List.filter_map files ~f:(fun (rel_path, content) ->
          if String.is_suffix rel_path ~suffix:".md"
          then (
            let doc = Oystermark.Parse.of_string ~locs:true content in
            Some (rel_path, doc))
          else None)
      in
      let other_files =
        List.filter_map files ~f:(fun (p, _) ->
          if not (String.is_suffix p ~suffix:".md") then Some p else None)
      in
      Oystermark.Vault.build_index ~md_docs ~other_files ~dirs:[]
    ;;

    let files =
      [ "note-a.md", "# Alpha\n\n## Section One\n\nBody text ^block1\n"
      ; "note-b.md", "# Beta\n\nLink to [[note-a]] here.\n"
      ; ( "note-c.md"
        , "# Gamma\n\nSee [[note-a#Section One]].\n\nAlso [[note-a#^block1]].\n" )
      ; "note-d.md", "# Delta\n\nMarkdown [link](note-a)\n"
      ; "note-e.md", "# Epsilon\n\nSelf ref [[#Alpha]].\n"
      ; "note-f.md", "# Zeta\n\nThe [key term]{#key-term} is defined here.\n"
      ; ( "note-g.md"
        , "# Eta\n\nSee [[note-f#key-term]].\n\nSelf [[#local]].\n\n{#local}\n> Aside.\n"
        )
      ; "note-h.md", "# Theta\n\nééé[[note-a]]\n"
      ; "note-i.md", "# Iota\n\n日本 [key]{#jp} tail.\n"
      ; "note-j.md", "# Kappa\n\nSee [[note-i#jp]].\n"
      ; "note-k.md", "---\ntitle: K\n---\n# Kap\n\nThe [x]{#fm} here.\n"
      ; "note-l.md", "# Lambda\n\nSee [[note-k#fm]].\n"
      ]
    ;;

    let index = make_index files
    let read_file rp = List.Assoc.find files ~equal:String.equal rp

    let show ~rel_path ~content ~line ~character =
      let def_res_opt =
        go_to_definition ~read_file ~index ~rel_path ~content ~line ~character ()
      in
      print_s [%sexp (def_res_opt : definition_result option)]
    ;;

    let%expect_test "wikilink to note" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-b.md" in
      show ~rel_path:"note-b.md" ~content ~line:2 ~character:10;
      [%expect {| (((path note-a.md) (line 0) (character 0))) |}]
    ;;

    let%expect_test "wikilink to heading" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-c.md" in
      show ~rel_path:"note-c.md" ~content ~line:2 ~character:8;
      [%expect {| (((path note-a.md) (line 2) (character 0))) |}]
    ;;

    let%expect_test "wikilink to block id" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-c.md" in
      show ~rel_path:"note-c.md" ~content ~line:4 ~character:8;
      [%expect {| (((path note-a.md) (line 4) (character 0))) |}]
    ;;

    (* [[note-f#key-term]] targets an inline attribute anchor [{#key-term}]
       on line 2 of note-f. See {!page-"feature-attribute-anchors"}. *)
    let%expect_test "wikilink to attribute id (cross-file)" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-g.md" in
      show ~rel_path:"note-g.md" ~content ~line:2 ~character:8;
      [%expect {| (((path note-f.md) (line 2) (character 4))) |}]
    ;;

    (* [[#local]] targets the block attribute anchor [{#local}] on the
       blockquote at line 7 of note-g itself. *)
    let%expect_test "wikilink to attribute id (self-file)" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-g.md" in
      show ~rel_path:"note-g.md" ~content ~line:4 ~character:9;
      [%expect {| (((path note-g.md) (line 7) (character 0))) |}]
    ;;

    (* The target column is UTF-16: in note-i, two CJK chars (3 bytes each) and
       a space precede the inline anchor, so its byte column (7) resolves to
       UTF-16 column 3 — read from the target file via [read_file].
       See {!page-"feature-utf16-positions"}. *)
    let%expect_test "cross-file target column is UTF-16" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-j.md" in
      show ~rel_path:"note-j.md" ~content ~line:2 ~character:8;
      [%expect {| (((path note-i.md) (line 2) (character 3))) |}]
    ;;

    (* Frontmatter: positions are full-file-relative because the parser blanks
       rather than strips the frontmatter. note-k has 3 frontmatter lines, so
       the anchor (heading body-line 2) is full-file line 5.
       See {!Oystermark.Parse.Frontmatter.blank_frontmatter}. *)
    let%expect_test "frontmatter target: full-file line and column" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-l.md" in
      show ~rel_path:"note-l.md" ~content ~line:2 ~character:8;
      [%expect {| (((path note-k.md) (line 5) (character 4))) |}]
    ;;

    (* The [character] is a UTF-16 offset: three [é]s (2 bytes each) precede the
       link, so UTF-16 char 3 is the link's first [[[], at byte 6. A byte-based
       reading would land at byte 3 (inside the [é]s) and find no link.
       See {!page-"feature-utf16-positions"}. *)
    let%expect_test "cursor position is UTF-16 encoded" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-h.md" in
      show ~rel_path:"note-h.md" ~content ~line:2 ~character:3;
      [%expect {| (((path note-a.md) (line 0) (character 0))) |}]
    ;;

    let%expect_test "markdown link to note" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-d.md" in
      show ~rel_path:"note-d.md" ~content ~line:2 ~character:12;
      [%expect {| (((path note-a.md) (line 0) (character 0))) |}]
    ;;

    let%expect_test "cursor not on link" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-b.md" in
      show ~rel_path:"note-b.md" ~content ~line:0 ~character:0;
      [%expect {| () |}]
    ;;

    let%expect_test "unresolved wikilink" =
      let content = "See [[nonexistent]]." in
      show ~rel_path:"note-b.md" ~content ~line:0 ~character:7;
      [%expect {| () |}]
    ;;

    let%expect_test "self-reference heading" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-e.md" in
      show ~rel_path:"note-e.md" ~content ~line:2 ~character:12;
      [%expect {| (((path note-e.md) (line 0) (character 0))) |}]
    ;;
  end)
;;
