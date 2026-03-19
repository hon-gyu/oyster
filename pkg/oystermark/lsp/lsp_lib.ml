(** Pure logic for the oystermark LSP: link detection, position conversion,
    heading/block-ID lookup. All functions are independent of the LSP protocol.

    Uses {!Oystermark.Parse} for parsing and {!Oystermark.Vault} for link
    resolution. *)

open Core

(** {1 Position utilities} *)

(** Convert a 0-based (line, character) position to a byte offset in [content].
    [character] is treated as a byte offset within the line (correct for ASCII). *)
let byte_offset_of_position (content : string) ~(line : int) ~(character : int) : int =
  let len = String.length content in
  let cur_line = ref 0 in
  let i = ref 0 in
  while !cur_line < line && !i < len do
    if Char.equal (String.get content !i) '\n' then incr cur_line;
    incr i
  done;
  min (!i + character) len
;;

(** {1 Link detection} *)

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
  List.rev (Cmarkit.Folder.fold_doc folder [] doc)
;;

(** Find the link whose byte range contains [offset].
    Returns the {!Oystermark.Vault.Link_ref.t} if found. *)
let find_link_ref_at_offset (links : located_link list) (offset : int)
  : Oystermark.Vault.Link_ref.t option
  =
  List.find_map links ~f:(fun ll ->
    if ll.first_byte <= offset && offset <= ll.last_byte then Some ll.link_ref else None)
;;

(** {1 Heading / block-ID line lookup} *)

(** Find the 0-based line number of a heading with the given [slug] in [doc].
    Returns 0 if not found or if text locations are unavailable.

    Requires the document to have been parsed with [~locs:true]. *)
let find_heading_line_in_doc (doc : Cmarkit.Doc.t) (slug : string) : int =
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
               (* Textloc.first_line is (line_num, byte_pos) where line_num is 1-based *)
               let line_num, _byte_pos = Cmarkit.Textloc.first_line loc in
               Cmarkit.Folder.ret (Some (line_num - 1)))
           | _ -> Cmarkit.Folder.default)
        | _ -> Cmarkit.Folder.default)
      ~inline_ext_default:(fun _f acc _i -> acc)
      ~block_ext_default:(fun _f acc _b -> acc)
      ()
  in
  Cmarkit.Folder.fold_doc folder None doc |> Option.value ~default:0
;;

(** Find the 0-based line number of a block ID ([^id]) in [doc].
    Returns 0 if not found or if text locations are unavailable.

    Requires the document to have been parsed with [~locs:true]. *)
let find_block_id_line_in_doc (doc : Cmarkit.Doc.t) (block_id : string) : int =
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
  Cmarkit.Folder.fold_doc folder None doc |> Option.value ~default:0
;;

(** {1 Parsing} *)

(** Parse [content] into a [Cmarkit.Doc.t] with locations enabled.

    {b Caching opportunity}: This is called on every LSP request (e.g.
    go-to-definition). A future optimisation could cache the parsed document
    per file and invalidate on [didChange], avoiding re-parsing when the
    buffer has not changed since the last request. *)
let parse_doc (content : string) : Cmarkit.Doc.t =
  Oystermark.Parse.of_string ~locs:true content
;;

(** Parse a target file's content for heading/block-ID line lookup.

    {b Caching opportunity}: Target files are typically already parsed during
    vault index building. A future optimisation could store the parsed
    [Cmarkit.Doc.t] (with locs) in the vault index alongside each file entry,
    so that heading/block-ID lookups can reuse the already-parsed AST instead
    of re-parsing. *)
let parse_target_doc (content : string) : Cmarkit.Doc.t =
  Oystermark.Parse.of_string ~locs:true content
;;

(** {1 End-to-end: resolve cursor position to target path and line} *)

(** The result of a go-to-definition request: a relative path and a 0-based
    line number. [None] means the link was unresolved or no link was found. *)
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
  let offset = byte_offset_of_position content ~line ~character in
  let doc = parse_doc content in
  let links = collect_links doc in
  match find_link_ref_at_offset links offset with
  | None -> None
  | Some link_ref ->
    let target = Oystermark.Vault.Resolve.resolve link_ref rel_path index in
    (match target with
     | Oystermark.Vault.Resolve.Note { path } | File { path } -> Some { path; line = 0 }
     | Heading { path; slug; _ } ->
       let line =
         match read_file path with
         | Some c -> find_heading_line_in_doc (parse_target_doc c) slug
         | None -> 0
       in
       Some { path; line }
     | Block { path; block_id } ->
       let line =
         match read_file path with
         | Some c -> find_block_id_line_in_doc (parse_target_doc c) block_id
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

(* Tests
==================== *)

let%test_module "byte_offset_of_position" =
  (module struct
    let offset = byte_offset_of_position
    let%test "line 0, char 0" = offset "hello\nworld" ~line:0 ~character:0 = 0
    let%test "line 0, char 3" = offset "hello\nworld" ~line:0 ~character:3 = 3
    let%test "line 1, char 0" = offset "hello\nworld" ~line:1 ~character:0 = 6
    let%test "line 1, char 2" = offset "hello\nworld" ~line:1 ~character:2 = 8
    let%test "past end clamps" = offset "hi" ~line:0 ~character:99 = 2
    let%test "line past end" = offset "hi\n" ~line:5 ~character:0 = 3
  end)
;;

let%test_module "collect_links" =
  (module struct
    let show text =
      let doc = parse_doc text in
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
      let doc = parse_doc text in
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
      let doc = parse_doc content in
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
      let doc = parse_doc content in
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
