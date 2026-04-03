(** Go-to-definition: resolve cursor position to target path and line.

    Spec: {!page-"feature-go-to-definition"}. *)

open Core

(** {1:implementation Implementation}
    {2 Heading / block-ID line lookup}

    See {!page-"feature-go-to-definition".resolution}. *)

(** Find the 0-based line number of a heading with the given [slug] in [doc].
    Returns 0 if not found or if text locations are unavailable.

    Requires the document to have been parsed with [~locs:true]. *)
let find_heading_line_in_doc (doc : Cmarkit.Doc.t) (slug : string) : int =
  Trace_core.with_span ~__FILE__ ~__LINE__ "find_heading_line_in_doc"
  @@ fun _sp ->
  Trace_core.add_data_to_span _sp [ "slug", `String slug ];
  let folder =
    Cmarkit.Folder.make
      ~block:(fun _f acc block ->
        match block with
        | Cmarkit.Block.Heading (_h, meta) ->
          (match Cmarkit.Meta.find Oystermark.Parse.Heading_slug.meta_key meta with
           | Some s when String.equal s slug ->
             let loc = Cmarkit.Meta.textloc meta in
             if Cmarkit.Textloc.is_none loc
             then Cmarkit.Folder.default
             else (
               let line_num, _byte_pos = Cmarkit.Textloc.first_line loc in
               Cmarkit.Folder.ret (Some (line_num - 1)))
           | _ -> Cmarkit.Folder.default)
        | _ -> Cmarkit.Folder.default)
      ~inline_ext_default:(fun _f acc _i -> acc)
      ~block_ext_default:(fun _f acc _b -> acc)
      ()
  in
  let result = Cmarkit.Folder.fold_doc folder None doc |> Option.value ~default:0 in
  Trace_core.add_data_to_span _sp [ "result_line", `Int result ];
  result
;;

(** Find the 0-based line number of a block ID ([^id]) in [doc].
    Returns 0 if not found or if text locations are unavailable.

    Requires the document to have been parsed with [~locs:true]. *)
let find_block_id_line_in_doc (doc : Cmarkit.Doc.t) (block_id : string) : int =
  Trace_core.with_span ~__FILE__ ~__LINE__ "find_block_id_line_in_doc"
  @@ fun _sp ->
  Trace_core.add_data_to_span _sp [ "block_id", `String block_id ];
  let folder =
    Cmarkit.Folder.make
      ~block:(fun _f acc block ->
        match block with
        | Cmarkit.Block.Paragraph (_p, meta) ->
          (match Cmarkit.Meta.find Oystermark.Parse.Block_id.meta_key meta with
           | Some (bid : Oystermark.Parse.Block_id.t) when String.equal bid.id block_id ->
             let loc = Cmarkit.Meta.textloc meta in
             if Cmarkit.Textloc.is_none loc
             then Cmarkit.Folder.default
             else (
               let line_num, _byte_pos = Cmarkit.Textloc.first_line loc in
               Cmarkit.Folder.ret (Some (line_num - 1)))
           | _ -> Cmarkit.Folder.default)
        | _ -> Cmarkit.Folder.default)
      ~inline_ext_default:(fun _f acc _i -> acc)
      ~block_ext_default:(fun _f acc _b -> acc)
      ()
  in
  let result = Cmarkit.Folder.fold_doc folder None doc |> Option.value ~default:0 in
  Trace_core.add_data_to_span _sp [ "result_line", `Int result ];
  result
;;

(** {2 End-to-end}

    See {!page-"feature-go-to-definition".resolution}. *)

(** The result of a go-to-definition request: a relative path and a 0-based
    line number.  [None] means the link was unresolved or no link was found. *)
type definition_result =
  { path : string
  ; line : int
  }
[@@deriving sexp, equal]

(** Given file [content] at [rel_path] in a vault with [index], find the
    definition target at cursor position [(line, character)].
    [read_file] is called to read the target file content for heading/block
    lookup; it receives a relative path and should return [Some content] or
    [None]. *)
let go_to_definition
      ?(config : Lsp_config.t = Lsp_config.default)
      ~(index : Oystermark.Vault.Index.t)
      ~(rel_path : string)
      ~(content : string)
      ~(line : int)
      ~(character : int)
      ~(read_file : string -> string option)
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
      | Curr_file -> "curr_file"
      | Curr_heading _ -> "curr_heading"
      | Curr_block _ -> "curr_block"
      | Unresolved -> "unresolved"
    in
    Trace_core.add_data_to_span _sp [ "resolution", `String resolution_tag ];
    let parse_target c = Lsp_util.parse_doc c in
    (match target with
     | Oystermark.Vault.Resolve.Note { path } | File { path } ->
       (* File found but fragment (if any) wasn't resolved — resolve fell back to the note. *)
       (match config.gtd_unresolved_fragment, link_ref.fragment with
        | Strict, Some _ -> None
        | _ -> Some { path; line = 0 })
     | Heading { path; slug; _ } ->
       let line =
         match read_file path with
         | Some c -> find_heading_line_in_doc (parse_target c) slug
         | None -> 0
       in
       Some { path; line }
     | Block { path; block_id } ->
       let line =
         match read_file path with
         | Some c -> find_block_id_line_in_doc (parse_target c) block_id
         | None -> 0
       in
       Some { path; line }
     | Curr_file ->
       (* Self-reference but fragment (if any) wasn't resolved. *)
       (match config.gtd_unresolved_fragment, link_ref.fragment with
        | Strict, Some _ -> None
        | _ -> Some { path = rel_path; line = 0 })
     | Curr_heading { slug; _ } ->
       Some { path = rel_path; line = find_heading_line_in_doc doc slug }
     | Curr_block { block_id } ->
       Some { path = rel_path; line = find_block_id_line_in_doc doc block_id }
     | Unresolved -> None)
;;

(** {1:test Test} *)

let%test_module "find_heading_line_in_doc" =
  (module struct
    let find content slug =
      let doc = Lsp_util.parse_doc content in
      find_heading_line_in_doc doc slug
    ;;

    let%test "finds heading" =
      let content = "# Title\n\nSome text\n\n## Chapter 1\n\nBody" in
      find content "chapter-1" = 4
    ;;

    let%test "returns 0 if not found" = find "# Title\n\nBody" "missing" = 0
    let%test "first heading" = find "# Title\nBody" "title" = 0
  end)
;;

let%test_module "find_block_id_line_in_doc" =
  (module struct
    let find content block_id =
      let doc = Lsp_util.parse_doc content in
      find_block_id_line_in_doc doc block_id
    ;;

    let%test "finds block id" =
      let content = "First para\n\nSecond para ^abc123\n\nThird" in
      find content "abc123" = 2
    ;;

    let%test "returns 0 if not found" = find "no ids here" "missing" = 0
  end)
;;

let%test_module "go_to_definition" =
  (module struct
    (** Build a vault index from a list of [(rel_path, content)] pairs. *)
    let make_index (files : (string * string) list) : Oystermark.Vault.Index.t =
      let md_docs =
        List.filter_map files ~f:(fun (rel_path, content) ->
          if String.is_suffix rel_path ~suffix:".md"
          then (
            let doc = Oystermark.Parse.of_string content in
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
      ]
    ;;

    let index = make_index files
    let read_file rel_path = List.Assoc.find files ~equal:String.equal rel_path

    let show ~rel_path ~content ~line ~character =
      let def_res_opt =
        go_to_definition ~index ~rel_path ~content ~line ~character ~read_file ()
      in
      print_s [%sexp (def_res_opt : definition_result option)]
    ;;

    let%expect_test "wikilink to note" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-b.md" in
      show ~rel_path:"note-b.md" ~content ~line:2 ~character:10;
      [%expect {| (((path note-a.md) (line 0))) |}]
    ;;

    let%expect_test "wikilink to heading" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-c.md" in
      show ~rel_path:"note-c.md" ~content ~line:2 ~character:8;
      [%expect {| (((path note-a.md) (line 2))) |}]
    ;;

    let%expect_test "wikilink to block id" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-c.md" in
      show ~rel_path:"note-c.md" ~content ~line:4 ~character:8;
      [%expect {| (((path note-a.md) (line 4))) |}]
    ;;

    let%expect_test "markdown link to note" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-d.md" in
      show ~rel_path:"note-d.md" ~content ~line:2 ~character:12;
      [%expect {| (((path note-a.md) (line 0))) |}]
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
      [%expect {| (((path note-e.md) (line 0))) |}]
    ;;
  end)
;;
