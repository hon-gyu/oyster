(** Go-to-definition: resolve cursor position to target path and line.

    Spec: {!page-"feature-go-to-definition"}. *)

open Core

(** {1:implementation Implementation}
    {2 Link detection}

    See {!page-"feature-go-to-definition".link_detection}. *)

(** A link found in the AST together with its byte range.
    [first_byte] and [last_byte] are 0-based absolute byte positions. *)
type located_link =
  { link_ref : Oystermark.Vault.Link_ref.t
  ; first_byte : int
  ; last_byte : int
  }

(** Walk a parsed document's AST and collect all links (wikilinks and markdown
    links/images) together with their byte ranges from [Cmarkit.Meta.textloc].

    Requires the document to have been parsed with [~locs:true] so that
    text locations are available on AST nodes. *)
let collect_links (doc : Cmarkit.Doc.t) : located_link list =
  Trace_core.with_span ~__FILE__ ~__LINE__ "collect_links"
  @@ fun _sp ->
  let try_add_link acc link_ref loc =
    if Cmarkit.Textloc.is_none loc
    then acc
    else
      { link_ref
      ; first_byte = Cmarkit.Textloc.first_byte loc
      ; last_byte = Cmarkit.Textloc.last_byte loc
      }
      :: acc
  in
  let folder =
    Cmarkit.Folder.make
      ~inline_ext_default:(fun _f acc i ->
        match i with
        | Oystermark.Parse.Wikilink.Ext_wikilink (wl, meta) ->
          let link_ref = Oystermark.Vault.Link_ref.of_wikilink wl in
          try_add_link acc link_ref (Cmarkit.Meta.textloc meta)
        | _ -> acc)
      ~block_ext_default:(fun _f acc _b -> acc)
      ~inline:(fun _f acc i ->
        match i with
        | Cmarkit.Inline.Link (link, meta) | Cmarkit.Inline.Image (link, meta) ->
          let loc = Cmarkit.Meta.textloc meta in
          let ref_ = Cmarkit.Inline.Link.reference link in
          (match Oystermark.Vault.Link_ref.of_cmark_reference ref_ with
           | Some link_ref -> Cmarkit.Folder.ret (try_add_link acc link_ref loc)
           | None -> Cmarkit.Folder.default)
        | _ -> Cmarkit.Folder.default)
      ()
  in
  let links = List.rev (Cmarkit.Folder.fold_doc folder [] doc) in
  Trace_core.add_data_to_span _sp [ "num_links", `Int (List.length links) ];
  links
;;

(** Find the link whose byte range contains [offset].
    Returns the {!Oystermark.Vault.Link_ref.t} if found. *)
let find_link_ref_at_offset (links : located_link list) (offset : int)
  : Oystermark.Vault.Link_ref.t option
  =
  Trace_core.with_span ~__FILE__ ~__LINE__ "find_link_ref_at_offset"
  @@ fun _sp ->
  let result =
    List.find_map links ~f:(fun ll ->
      if ll.first_byte <= offset && offset <= ll.last_byte then Some ll.link_ref else None)
  in
  Trace_core.add_data_to_span
    _sp
    [ "offset", `Int offset; "found", `Bool (Option.is_some result) ];
  result
;;

(** {2 Heading / block-ID line lookup}

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
      ~(index : Oystermark.Vault.Index.t)
      ~(rel_path : string)
      ~(content : string)
      ~(line : int)
      ~(character : int)
      ~(read_file : string -> string option)
  : definition_result option
  =
  Trace_core.with_span ~__FILE__ ~__LINE__ "go_to_definition"
  @@ fun _sp ->
  Trace_core.add_data_to_span
    _sp
    [ "rel_path", `String rel_path; "line", `Int line; "character", `Int character ];
  let offset = Lsp_util.byte_offset_of_position content ~line ~character in
  let doc = Lsp_util.parse_doc content in
  let links = collect_links doc in
  match find_link_ref_at_offset links offset with
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
     | Oystermark.Vault.Resolve.Note { path } | File { path } -> Some { path; line = 0 }
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
     | Curr_file -> Some { path = rel_path; line = 0 }
     | Curr_heading { slug; _ } ->
       Some { path = rel_path; line = find_heading_line_in_doc doc slug }
     | Curr_block { block_id } ->
       Some { path = rel_path; line = find_block_id_line_in_doc doc block_id }
     | Unresolved -> None)
;;


(** {1:test Test} *)

let%test_module "collect_links" =
  (module struct
    let show text =
      let doc = Lsp_util.parse_doc text in
      let links = collect_links doc in
      List.iter links ~f:(fun ll ->
        printf
          "[%d-%d] %s\n"
          ll.first_byte
          ll.last_byte
          (Sexp.to_string (Oystermark.Vault.Link_ref.sexp_of_t ll.link_ref)))
    ;;

    let%expect_test "wikilink" =
      show "see [[Note]] here";
      [%expect {| [4-11] ((target(Note))(fragment())) |}]
    ;;

    let%expect_test "embed wikilink" =
      show "see ![[Image.png]] here";
      [%expect {| [4-17] ((target(Image.png))(fragment())) |}]
    ;;

    let%expect_test "wikilink with fragment" =
      show "go to [[Note#Heading]] now";
      [%expect {| [6-21] ((target(Note))(fragment((Heading(Heading))))) |}]
    ;;

    let%expect_test "markdown link" =
      show "see [text](other) here";
      [%expect {| [4-16] ((target(other))(fragment())) |}]
    ;;

    let%expect_test "external link ignored" =
      show "[text](https://example.com)";
      [%expect {| |}]
    ;;

    let%expect_test "two wikilinks" =
      show "[[A]] and [[B]]";
      [%expect
        {|
        [0-4] ((target(A))(fragment()))
        [10-14] ((target(B))(fragment()))
        |}]
    ;;

    let%expect_test "image link" =
      show "see ![alt](img.png) here";
      [%expect {| [4-18] ((target(img.png))(fragment())) |}]
    ;;
  end)
;;

let%test_module "find_link_ref_at_offset" =
  (module struct
    let find text offset =
      let doc = Lsp_util.parse_doc text in
      let links = collect_links doc in
      find_link_ref_at_offset links offset
    ;;

    let show text offset =
      match find text offset with
      | None -> print_endline "<none>"
      | Some lr -> print_s (Oystermark.Vault.Link_ref.sexp_of_t lr)
    ;;

    let%expect_test "cursor on wikilink target" =
      show "see [[Note]] here" 6;
      [%expect {| ((target (Note)) (fragment ())) |}]
    ;;

    let%expect_test "cursor on opening brackets" =
      show "see [[Note]] here" 4;
      [%expect {| ((target (Note)) (fragment ())) |}]
    ;;

    let%expect_test "cursor on closing brackets" =
      show "see [[Note]] here" 11;
      [%expect {| ((target (Note)) (fragment ())) |}]
    ;;

    let%expect_test "cursor outside" =
      show "see [[Note]] here" 2;
      [%expect {| <none> |}]
    ;;

    let%expect_test "cursor after link" =
      show "see [[Note]] here" 13;
      [%expect {| <none> |}]
    ;;

    let%expect_test "markdown link" =
      show "[text](other)" 8;
      [%expect {| ((target (other)) (fragment ())) |}]
    ;;

    let%expect_test "external link ignored" =
      show "[text](https://example.com)" 10;
      [%expect {| <none> |}]
    ;;
  end)
;;

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
      match go_to_definition ~index ~rel_path ~content ~line ~character ~read_file with
      | None -> print_endline "<none>"
      | Some r -> printf "%s:%d\n" r.path r.line
    ;;

    let%expect_test "wikilink to note" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-b.md" in
      show ~rel_path:"note-b.md" ~content ~line:2 ~character:10;
      [%expect {| note-a.md:0 |}]
    ;;

    let%expect_test "wikilink to heading" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-c.md" in
      show ~rel_path:"note-c.md" ~content ~line:2 ~character:8;
      [%expect {| note-a.md:2 |}]
    ;;

    let%expect_test "wikilink to block id" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-c.md" in
      show ~rel_path:"note-c.md" ~content ~line:4 ~character:8;
      [%expect {| note-a.md:4 |}]
    ;;

    let%expect_test "markdown link to note" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-d.md" in
      show ~rel_path:"note-d.md" ~content ~line:2 ~character:12;
      [%expect {| note-a.md:0 |}]
    ;;

    let%expect_test "cursor not on link" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-b.md" in
      show ~rel_path:"note-b.md" ~content ~line:0 ~character:0;
      [%expect {| <none> |}]
    ;;

    let%expect_test "unresolved wikilink" =
      let content = "See [[nonexistent]]." in
      show ~rel_path:"note-b.md" ~content ~line:0 ~character:7;
      [%expect {| <none> |}]
    ;;

    let%expect_test "self-reference heading" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-e.md" in
      show ~rel_path:"note-e.md" ~content ~line:2 ~character:12;
      [%expect {| note-e.md:0 |}]
    ;;
  end)
;;
