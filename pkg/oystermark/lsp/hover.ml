(** Hover: show a preview of the link target's content.

    Spec: {!page-"feature-hover"}. *)

open Core

(** {1:implementation Implementation} *)

(** {2 Content extraction} *)

(** Truncate [s] to at most [max_chars] bytes, snapping to the previous
    newline to avoid cutting mid-word, and appending a notice.
    Returns [s] unchanged if it is already short enough. *)
let truncate ~max_chars (s : string) : string =
  if String.length s <= max_chars
  then s
  else (
    (* Snap back to the last newline before the cut point. *)
    let cut =
      match String.rindex_exn (String.prefix s max_chars) '\n' with
      | pos -> pos
      | exception Not_found_s _ -> max_chars
    in
    String.prefix s cut ^ "\n\n*(truncated)*")
;;

(** Extract the section of [content] starting at [heading_line] (0-based)
    up to but not including the next heading of equal or higher level.

    [heading_level] is the ATX level (1–6) of the anchor heading.
    Returns the raw lines of the section joined by newlines. *)
let extract_section ~(heading_line : int) ~(heading_level : int) (content : string)
  : string
  =
  let lines = String.split_lines content in
  let lines_arr = Array.of_list lines in
  let n = Array.length lines_arr in
  (* Find where the section ends: next heading at same or higher level. *)
  let end_line =
    let rec find i =
      if i >= n
      then n
      else (
        let line = lines_arr.(i) in
        (* Count leading '#' characters. *)
        let hashes =
          String.lfindi line ~f:(fun _ c -> not (Char.equal c '#'))
          |> Option.value ~default:(String.length line)
        in
        if
          hashes >= 1
          && hashes <= heading_level
          && String.length line > hashes
          && Char.equal line.[hashes] ' '
        then i
        else find (i + 1))
    in
    find (heading_line + 1)
  in
  Array.sub lines_arr ~pos:heading_line ~len:(end_line - heading_line)
  |> Array.to_list
  |> String.concat ~sep:"\n"
;;

(** Extract the paragraph that contains [block_id] from [content].
    Returns the paragraph text (without the trailing [^id] marker)
    or [None] if not found. *)
let extract_block ~(block_id : string) (content : string) : string option =
  (* A block ID appears as " ^id" at the end of a paragraph's last line. *)
  let marker = " ^" ^ block_id in
  let lines = String.split_lines content in
  (* Walk backwards through lines to find the marker, then collect the
     paragraph (consecutive non-blank lines ending at the marker line). *)
  let lines_arr = Array.of_list lines in
  let n = Array.length lines_arr in
  let find_marker () =
    let rec loop i =
      if i >= n
      then None
      else if String.is_suffix lines_arr.(i) ~suffix:marker
      then Some i
      else loop (i + 1)
    in
    loop 0
  in
  match find_marker () with
  | None -> None
  | Some marker_line ->
    (* Walk backwards to find the start of the paragraph. *)
    let start =
      let rec loop i =
        if i < 0 || String.is_empty (String.strip lines_arr.(i))
        then i + 1
        else loop (i - 1)
      in
      loop (marker_line - 1)
    in
    let para_lines =
      Array.sub lines_arr ~pos:start ~len:(marker_line - start + 1) |> Array.to_list
    in
    Some (String.concat ~sep:"\n" para_lines)
;;

(** {2 Heading-level parsing} *)

(** Return the ATX heading level (1–6) of [line], or [None]. *)
let heading_level_of_line (line : string) : int option =
  let hashes =
    String.lfindi line ~f:(fun _ c -> not (Char.equal c '#'))
    |> Option.value ~default:(String.length line)
  in
  if
    hashes >= 1
    && hashes <= 6
    && String.length line > hashes
    && Char.equal line.[hashes] ' '
  then Some hashes
  else None
;;

(** Find the 0-based line number and ATX level of the heading whose slug
    matches [slug] in [content].  Returns [None] if not found. *)
let find_heading_in_content ~(slug : string) (content : string) : (int * int) option =
  let lines = String.split_lines content in
  List.findi lines ~f:(fun _i line ->
    match heading_level_of_line line with
    | None -> false
    | Some _ ->
      (* Strip leading '#'s and space, then slugify. *)
      let text =
        String.lstrip line ~drop:(fun c -> Char.equal c '#')
        |> String.lstrip ~drop:(fun c -> Char.equal c ' ')
      in
      String.equal (Oystermark.Parse.Heading_slug.slugify text) slug)
  |> Option.map ~f:(fun (i, line) ->
    let level = heading_level_of_line line |> Option.value_exn in
    i, level)
;;

(** {2 Formatting} *)

(** Build the hover string: path header, separator, then body.
    If [body] is empty, shows [*(empty)*] instead. *)
let format_hover ~(path : string) (body : string) : string =
  let header = "*Path*:" ^ path in
  if String.is_empty (String.strip body)
  then header ^ "\n\n*(empty)*"
  else header ^ "\n\n" ^ body
;;

(** {2 Main computation} *)

(** Compute hover content for the link at the given position.

    Returns [(markdown_string, first_byte, last_byte)] or [None] if
    there is no recognisable, readable link at the cursor.

    See {!page-"feature-hover"}. *)
let hover
      ?(config : Lsp_config.t = Lsp_config.default)
      ~(index : Oystermark.Vault.Index.t)
      ~(rel_path : string)
      ~(content : string)
      ~(line : int)
      ~(character : int)
      ~(read_file : string -> string option)
      ()
  : (string * int * int) option
  =
  Trace_core.with_span ~__FILE__ ~__LINE__ "hover"
  @@ fun _sp ->
  Trace_core.add_data_to_span
    _sp
    [ "rel_path", `String rel_path; "line", `Int line; "character", `Int character ];
  let offset = Lsp_util.byte_offset_of_position content ~line ~character in
  let doc = Lsp_util.parse_doc content in
  let links = Link_collect.collect_links doc in
  match Link_collect.find_at_offset links offset with
  | None -> None
  | Some link_ref ->
    let ll =
      List.find_exn links ~f:(fun ll -> ll.first_byte <= offset && offset <= ll.last_byte)
    in
    let target = Oystermark.Vault.Resolve.resolve link_ref rel_path index in
    (* Determine which file to read and which portion to extract. *)
    let result_opt =
      match target with
      | Oystermark.Vault.Resolve.Unresolved -> None
      | Note { path } | File { path } ->
        (match read_file path with
         | None -> None
         | Some file_content ->
           let body =
             match link_ref.fragment with
             | Some (Oystermark.Vault.Link_ref.Heading hs) ->
               (* Fragment present but resolve fell back — try to find section. *)
               let slug =
                 String.concat
                   ~sep:"-"
                   (List.map hs ~f:Oystermark.Parse.Heading_slug.slugify)
               in
               (match find_heading_in_content ~slug file_content with
                | Some (hline, hlevel) ->
                  extract_section ~heading_line:hline ~heading_level:hlevel file_content
                | None -> file_content)
             | Some (Block_ref bid) ->
               (match extract_block ~block_id:bid file_content with
                | Some p -> p
                | None -> file_content)
             | None -> file_content
           in
           Some (format_hover ~path body))
      | Heading { path; slug; _ } ->
        (match read_file path with
         | None -> None
         | Some file_content ->
           let body =
             match find_heading_in_content ~slug file_content with
             | Some (hline, hlevel) ->
               extract_section ~heading_line:hline ~heading_level:hlevel file_content
             | None -> file_content
           in
           Some (format_hover ~path body))
      | Block { path; block_id } ->
        (match read_file path with
         | None -> None
         | Some file_content ->
           let body =
             match extract_block ~block_id file_content with
             | Some p -> p
             | None -> file_content
           in
           Some (format_hover ~path body))
      | Curr_file ->
        let body =
          match link_ref.fragment with
          | Some (Oystermark.Vault.Link_ref.Heading hs) ->
            let slug =
              String.concat ~sep:"-" (List.map hs ~f:Oystermark.Parse.Heading_slug.slugify)
            in
            (match find_heading_in_content ~slug content with
             | Some (hline, hlevel) ->
               extract_section ~heading_line:hline ~heading_level:hlevel content
             | None -> content)
          | Some (Block_ref bid) ->
            (match extract_block ~block_id:bid content with
             | Some p -> p
             | None -> content)
          | None -> content
        in
        Some (format_hover ~path:rel_path body)
      | Curr_heading { slug; _ } ->
        let body =
          match find_heading_in_content ~slug content with
          | Some (hline, hlevel) ->
            extract_section ~heading_line:hline ~heading_level:hlevel content
          | None -> content
        in
        Some (format_hover ~path:rel_path body)
      | Curr_block { block_id } ->
        let body =
          match extract_block ~block_id content with
          | Some p -> p
          | None -> content
        in
        Some (format_hover ~path:rel_path body)
    in
    Option.map result_opt ~f:(fun raw ->
      let text = truncate ~max_chars:config.hover_max_chars raw in
      Trace_core.add_data_to_span _sp [ "content_bytes", `Int (String.length text) ];
      text, ll.first_byte, ll.last_byte)
;;

(** {1:test Test} *)

let%test_module "truncate" =
  (module struct
    let%expect_test "short string unchanged" =
      print_string (truncate ~max_chars:100 "hello\nworld");
      [%expect
        {|
        hello
        world
        |}]
    ;;

    let%expect_test "truncates at newline" =
      let s = "line one\nline two\nline three" in
      print_string (truncate ~max_chars:15 s);
      [%expect
        {|
        line one

        *(truncated)* |}]
    ;;
  end)
;;

let%test_module "extract_section" =
  (module struct
    let content =
      "# Title\n\nIntro.\n\n## Section One\n\nBody one.\n\n## Section Two\n\nBody two.\n"
    ;;

    let%expect_test "extracts first section" =
      print_string (extract_section ~heading_line:4 ~heading_level:2 content);
      [%expect
        {|
        ## Section One

        Body one.
         |}]
    ;;

    let%expect_test "top-level heading stops at next h1" =
      print_string (extract_section ~heading_line:0 ~heading_level:1 content);
      [%expect
        {|
        # Title

        Intro.

        ## Section One

        Body one.

        ## Section Two

        Body two.
        |}]
    ;;
  end)
;;

let%test_module "extract_block" =
  (module struct
    let content = "First para.\n\nSecond para ^abc\n\nThird para.\n"

    let%expect_test "finds block" =
      print_s [%sexp (extract_block ~block_id:"abc" content : string option)];
      [%expect {| ("Second para ^abc") |}]
    ;;

    let%expect_test "missing block returns None" =
      print_s [%sexp (extract_block ~block_id:"nope" content : string option)];
      [%expect {| () |}]
    ;;
  end)
;;

let%test_module "format_hover" =
  (module struct
    let%expect_test "with content" =
      print_string (format_hover ~path:"dir/note.md" "# Hello\n\nWorld.\n");
      [%expect
        {|
        *Path*:dir/note.md

        # Hello

        World.
        |}]
    ;;

    let%expect_test "empty content" =
      print_string (format_hover ~path:"dir/empty.md" "");
      [%expect {|
        *Path*:dir/empty.md

        *(empty)*
        |}]
    ;;

    let%expect_test "whitespace-only content" =
      print_string (format_hover ~path:"dir/blank.md" "  \n\n  ");
      [%expect {|
        *Path*:dir/blank.md

        *(empty)*
        |}]
    ;;
  end)
;;

let%test_module "hover" =
  (module struct
    let files =
      [ ( "note-a.md"
        , "# Alpha\n\n## Section One\n\nBody text. ^block1\n\n## Section Two\n\nMore.\n" )
      ; "note-b.md", "# Beta\n\nSee [[note-a]].\n"
      ; "note-c.md", "# Gamma\n\nSee [[note-a#Section One]].\n"
      ; "note-d.md", "# Delta\n\nSee [[note-a#^block1]].\n"
      ; "note-e.md", "# Epsilon\n\nSelf [[#Epsilon]].\n"
      ; "empty.md", ""
      ; "note-f.md", "# Zeta\n\nSee [[empty]].\n"
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
      match hover ~index ~rel_path ~content ~line ~character ~read_file () with
      | None -> print_endline "<none>"
      | Some (text, fb, lb) -> printf "[%d-%d]\n%s\n" fb lb text
    ;;

    let%expect_test "plain note link" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-b.md" in
      show ~rel_path:"note-b.md" ~content ~line:2 ~character:8;
      [%expect
        {|
        [12-21]
        *Path*:note-a.md

        # Alpha

        ## Section One

        Body text. ^block1

        ## Section Two

        More.
        |}]
    ;;

    let%expect_test "heading fragment" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-c.md" in
      show ~rel_path:"note-c.md" ~content ~line:2 ~character:8;
      [%expect
        {|
        [13-34]
        *Path*:note-a.md

        ## Section One

        Body text. ^block1
        |}]
    ;;

    let%expect_test "block fragment" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-d.md" in
      show ~rel_path:"note-d.md" ~content ~line:2 ~character:8;
      [%expect
        {|
        [13-30]
        *Path*:note-a.md

        Body text. ^block1
        |}]
    ;;

    let%expect_test "self-referencing heading" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-e.md" in
      show ~rel_path:"note-e.md" ~content ~line:2 ~character:8;
      [%expect
        {|
        [16-27]
        *Path*:note-e.md

        # Epsilon

        Self [[#Epsilon]].
        |}]
    ;;

    let%expect_test "empty target note" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-f.md" in
      show ~rel_path:"note-f.md" ~content ~line:2 ~character:8;
      [%expect
        {|
        [12-20]
        *Path*:empty.md

        *(empty)*
        |}]
    ;;

    let%expect_test "unresolved link returns none" =
      let content = "See [[missing]]." in
      show ~rel_path:"note-b.md" ~content ~line:0 ~character:6;
      [%expect {| <none> |}]
    ;;

    let%expect_test "cursor outside link returns none" =
      let content = "See [[note-a]]." in
      show ~rel_path:"note-b.md" ~content ~line:0 ~character:0;
      [%expect {| <none> |}]
    ;;

    let%expect_test "truncation" =
      let config = { Lsp_config.default with hover_max_chars = 30 } in
      let content = List.Assoc.find_exn files ~equal:String.equal "note-b.md" in
      (match
         hover
           ~config
           ~index
           ~rel_path:"note-b.md"
           ~content
           ~line:2
           ~character:8
           ~read_file
           ()
       with
       | None -> print_endline "<none>"
       | Some (text, _, _) -> print_string text);
      [%expect
        {|
        *Path*:note-a.md

        # Alpha


        *(truncated)*
        |}]
    ;;
  end)
;;
