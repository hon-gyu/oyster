(** Pure logic for the oystermark LSP: link detection, position conversion,
    heading/block-ID lookup. All functions are independent of the LSP protocol. *)

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

(** {1 Substring search} *)

(** Find substring [needle] in [haystack] starting at [from]. *)
let find_substring haystack ~needle ~from =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if from + nlen > hlen
  then None
  else (
    let rec loop i =
      if i + nlen > hlen
      then None
      else if String.is_substring_at haystack ~pos:i ~substring:needle
      then Some i
      else loop (i + 1)
    in
    loop from)
;;

(** {1 Link detection in raw text} *)

(** Find the wikilink ([[…]]) enclosing byte [offset] in [text].
    Returns the parsed {!Oystermark.Parse.Wikilink.t}. *)
let find_wikilink_at_offset (text : string) (offset : int)
  : Oystermark.Parse.Wikilink.t option
  =
  let len = String.length text in
  let rec scan i =
    if i > len - 4
    then None
    else (
      match find_substring text ~needle:"[[" ~from:i with
      | None -> None
      | Some open_pos ->
        let embed = open_pos > 0 && Char.equal (String.get text (open_pos - 1)) '!' in
        let span_start = if embed then open_pos - 1 else open_pos in
        let content_start = open_pos + 2 in
        (match find_substring text ~needle:"]]" ~from:content_start with
         | None -> None
         | Some close_pos ->
           let span_end = close_pos + 1 in
           if span_start <= offset && offset <= span_end
           then (
             let content =
               String.sub text ~pos:content_start ~len:(close_pos - content_start)
             in
             Some (Oystermark.Parse.Wikilink.make ~embed content))
           else scan (close_pos + 2)))
  in
  scan 0
;;

(** Find a markdown link destination where [offset] falls inside a
    [\[text\](url)] or [!\[alt\](url)] span. Returns the raw URL string. *)
let find_mdlink_dest_at_offset (text : string) (offset : int) : string option =
  let len = String.length text in
  let rec scan i =
    if i >= len - 2
    then None
    else (
      match find_substring text ~needle:"](" ~from:i with
      | None -> None
      | Some bracket_pos ->
        let url_start = bracket_pos + 2 in
        let rec find_close j depth =
          if j >= len
          then None
          else if Char.equal (String.get text j) '('
          then find_close (j + 1) (depth + 1)
          else if Char.equal (String.get text j) ')'
          then if depth = 0 then Some j else find_close (j + 1) (depth - 1)
          else find_close (j + 1) depth
        in
        (match find_close url_start 0 with
         | None -> scan (bracket_pos + 2)
         | Some close_paren ->
           let rec find_open j depth =
             if j < 0
             then None
             else if Char.equal (String.get text j) ']'
             then find_open (j - 1) (depth + 1)
             else if Char.equal (String.get text j) '['
             then if depth = 0 then Some j else find_open (j - 1) (depth - 1)
             else find_open (j - 1) depth
           in
           (match find_open (bracket_pos - 1) 0 with
            | None -> scan (bracket_pos + 2)
            | Some open_bracket ->
              let span_start =
                if open_bracket > 0 && Char.equal (String.get text (open_bracket - 1)) '!'
                then open_bracket - 1
                else open_bracket
              in
              if span_start <= offset && offset <= close_paren
              then (
                let url = String.sub text ~pos:url_start ~len:(close_paren - url_start) in
                Some url)
              else scan (bracket_pos + 2))))
  in
  scan 0
;;

(** Try to extract a {!Oystermark.Vault.Link_ref.t} from the raw text at
    [offset]. Checks wikilinks first, then markdown links. *)
let find_link_ref_at_offset (text : string) (offset : int)
  : Oystermark.Vault.Link_ref.t option
  =
  match find_wikilink_at_offset text offset with
  | Some wl -> Some (Oystermark.Vault.Link_ref.of_wikilink wl)
  | None ->
    (match find_mdlink_dest_at_offset text offset with
     | Some url -> Oystermark.Vault.Link_ref.of_cmark_dest url
     | None -> None)
;;

(** {1 Heading / block-ID line lookup} *)

(** Find the 0-based line number of a heading in [content] whose text matches
    [heading_text]. Returns 0 if not found. *)
let find_heading_line (content : string) (heading_text : string) : int =
  let lines = String.split_lines content in
  let rec loop i = function
    | [] -> 0
    | line :: rest ->
      let trimmed = String.lstrip line in
      if String.length trimmed > 0 && Char.equal (String.get trimmed 0) '#'
      then (
        let j = ref 0 in
        while !j < String.length trimmed && Char.equal (String.get trimmed !j) '#' do
          incr j
        done;
        while !j < String.length trimmed && Char.equal (String.get trimmed !j) ' ' do
          incr j
        done;
        let text = String.drop_prefix trimmed !j in
        if String.equal text heading_text then i else loop (i + 1) rest)
      else loop (i + 1) rest
  in
  loop 0 lines
;;

(** Find the 0-based line number of a block ID ([^id]) in [content].
    Returns 0 if not found. *)
let find_block_id_line (content : string) (block_id : string) : int =
  let pattern = "^" ^ block_id in
  let lines = String.split_lines content in
  let rec loop i = function
    | [] -> 0
    | line :: rest ->
      if String.is_substring line ~substring:pattern then i else loop (i + 1) rest
  in
  loop 0 lines
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
  match find_link_ref_at_offset content offset with
  | None -> None
  | Some link_ref ->
    let target = Oystermark.Vault.Resolve.resolve link_ref rel_path index in
    (match target with
     | Oystermark.Vault.Resolve.Note { path } | File { path } -> Some { path; line = 0 }
     | Heading { path; heading; _ } ->
       let line =
         match read_file path with
         | Some c -> find_heading_line c heading
         | None -> 0
       in
       Some { path; line }
     | Block { path; block_id } ->
       let line =
         match read_file path with
         | Some c -> find_block_id_line c block_id
         | None -> 0
       in
       Some { path; line }
     | Curr_file -> Some { path = rel_path; line = 0 }
     | Curr_heading { heading; _ } ->
       Some { path = rel_path; line = find_heading_line content heading }
     | Curr_block { block_id } ->
       Some { path = rel_path; line = find_block_id_line content block_id }
     | Unresolved -> None)
;;

(** {1 Tests} *)

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

let%test_module "find_wikilink_at_offset" =
  (module struct
    let find = find_wikilink_at_offset

    let show text offset =
      match find text offset with
      | None -> print_endline "<none>"
      | Some wl -> print_endline (Oystermark.Parse.Wikilink.to_commonmark wl)
    ;;

    let%expect_test "cursor on target" =
      show "see [[Note]] here" 6;
      [%expect {| [[Note]] |}]
    ;;

    let%expect_test "cursor on opening brackets" =
      show "see [[Note]] here" 4;
      [%expect {| [[Note]] |}]
    ;;

    let%expect_test "cursor on closing brackets" =
      show "see [[Note]] here" 11;
      [%expect {| [[Note]] |}]
    ;;

    let%expect_test "cursor outside" =
      show "see [[Note]] here" 2;
      [%expect {| <none> |}]
    ;;

    let%expect_test "cursor after link" =
      show "see [[Note]] here" 13;
      [%expect {| <none> |}]
    ;;

    let%expect_test "embed wikilink" =
      show "see ![[Image.png]] here" 8;
      [%expect {| ![[Image.png]] |}]
    ;;

    let%expect_test "wikilink with fragment" =
      show "go to [[Note#Heading]] now" 10;
      [%expect {| [[Note#Heading]] |}]
    ;;

    let%expect_test "wikilink with display" =
      show "see [[Note|label]] ok" 8;
      [%expect {| [[Note|label]] |}]
    ;;

    let%expect_test "second of two links" =
      show "[[A]] and [[B]]" 12;
      [%expect {| [[B]] |}]
    ;;

    let%expect_test "first of two links" =
      show "[[A]] and [[B]]" 3;
      [%expect {| [[A]] |}]
    ;;

    let%expect_test "between two links" =
      show "[[A]] and [[B]]" 7;
      [%expect {| <none> |}]
    ;;
  end)
;;

let%test_module "find_mdlink_dest_at_offset" =
  (module struct
    let find = find_mdlink_dest_at_offset

    let show text offset =
      match find text offset with
      | None -> print_endline "<none>"
      | Some url -> print_endline url
    ;;

    let%expect_test "cursor on url" =
      show "see [text](url) here" 12;
      [%expect {| url |}]
    ;;

    let%expect_test "cursor on text" =
      show "see [text](url) here" 5;
      [%expect {| url |}]
    ;;

    let%expect_test "cursor on opening bracket" =
      show "see [text](url) here" 4;
      [%expect {| url |}]
    ;;

    let%expect_test "cursor on closing paren" =
      show "see [text](url) here" 14;
      [%expect {| url |}]
    ;;

    let%expect_test "cursor outside" =
      show "see [text](url) here" 2;
      [%expect {| <none> |}]
    ;;

    let%expect_test "cursor after link" =
      show "see [text](url) here" 16;
      [%expect {| <none> |}]
    ;;

    let%expect_test "image link" =
      show "see ![alt](img.png) here" 6;
      [%expect {| img.png |}]
    ;;

    let%expect_test "url with fragment" =
      show "[go](Note#Heading)" 6;
      [%expect {| Note#Heading |}]
    ;;
  end)
;;

let%test_module "find_link_ref_at_offset" =
  (module struct
    let find = find_link_ref_at_offset

    let show text offset =
      match find text offset with
      | None -> print_endline "<none>"
      | Some lr -> print_s (Oystermark.Vault.Link_ref.sexp_of_t lr)
    ;;

    let%expect_test "wikilink takes priority" =
      show "[[Note]]" 3;
      [%expect {| ((target (Note)) (fragment ())) |}]
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

let%test_module "find_heading_line" =
  (module struct
    let%test "finds heading" =
      let content = "# Title\n\nSome text\n\n## Chapter 1\n\nBody" in
      find_heading_line content "Chapter 1" = 4
    ;;

    let%test "returns 0 if not found" = find_heading_line "# Title\n\nBody" "Missing" = 0
    let%test "first heading" = find_heading_line "# Title\nBody" "Title" = 0
  end)
;;

let%test_module "find_block_id_line" =
  (module struct
    let%test "finds block id" =
      let content = "First para\n\nSecond para ^abc123\n\nThird" in
      find_block_id_line content "abc123" = 2
    ;;

    let%test "returns 0 if not found" = find_block_id_line "no ids here" "missing" = 0
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
