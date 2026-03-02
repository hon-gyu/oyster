open! Core
open Oystermark

(* Link_ref tests
   ==================================================================== *)

let pp_link_ref_fragment : Link_ref.fragment option -> string = function
  | None -> "-"
  | Some (Link_ref.Heading hs) -> "Heading [" ^ String.concat ~sep:"; " hs ^ "]"
  | Some (Link_ref.Block_ref s) -> "Block_ref " ^ s
;;

let pp_link_ref (r : Link_ref.t) =
  Printf.sprintf
    "target=%s frag=%s"
    (Option.value ~default:"-" r.target)
    (pp_link_ref_fragment r.fragment)
;;

let%expect_test "percent_decode" =
  let cases =
    [ "no encoding", "hello"
    ; "space", "hello%20world"
    ; "multiple", "a%20b%20c"
    ; "hash", "Note%23Heading"
    ; "invalid hex", "hello%ZZ"
    ; "truncated", "hello%2"
    ; "empty", ""
    ; "percent at end", "hello%"
    ]
  in
  let cols =
    [ Ascii_table.Column.create "name" (fun (n, _, _) -> n)
    ; Ascii_table.Column.create "input" (fun (_, i, _) -> i)
    ; Ascii_table.Column.create "output" (fun (_, _, o) -> o)
    ]
  in
  let rows =
    List.map cases ~f:(fun (name, input) -> name, input, Link_ref.percent_decode input)
  in
  print_string (Ascii_table.to_string_noattr cols rows);
  [%expect
    {|
    ┌────────────────┬────────────────┬──────────────┐
    │ name           │ input          │ output       │
    ├────────────────┼────────────────┼──────────────┤
    │ no encoding    │ hello          │ hello        │
    │ space          │ hello%20world  │ hello world  │
    │ multiple       │ a%20b%20c      │ a b c        │
    │ hash           │ Note%23Heading │ Note#Heading │
    │ invalid hex    │ hello%ZZ       │ hello%ZZ     │
    │ truncated      │ hello%2        │ hello%2      │
    │ empty          │                │              │
    │ percent at end │ hello%         │ hello%       │
    └────────────────┴────────────────┴──────────────┘
    |}]
;;

let%expect_test "is_external" =
  let cases =
    [ "https", "https://example.com", true
    ; "http", "http://example.com", true
    ; "mailto", "mailto:user@example.com", true
    ; "ftp", "ftp://files.example.com", true
    ; "note", "Note", false
    ; "path", "dir/Note.md", false
    ; "fragment only", "#heading", false
    ]
  in
  List.iter cases ~f:(fun (name, input, expected) ->
    let result = Link_ref.is_external input in
    if Bool.equal result expected
    then Printf.printf "%s: OK\n" name
    else Printf.printf "%s: FAIL (got %b, expected %b)\n" name result expected);
  [%expect
    {|
    https: OK
    http: OK
    mailto: OK
    ftp: OK
    note: OK
    path: OK
    fragment only: OK
    |}]
;;

let%expect_test "of_wikilink" =
  let cases =
    [ "basic", Wikilink.make ~embed:false "Note"
    ; "with heading", Wikilink.make ~embed:false "Note#H1#H2"
    ; "with block", Wikilink.make ~embed:false "Note#^blockid"
    ; "fragment only", Wikilink.make ~embed:false "#Heading"
    ; "empty", Wikilink.make ~embed:false ""
    ]
  in
  let cols =
    [ Ascii_table.Column.create "name" (fun (n, _) -> n)
    ; Ascii_table.Column.create "link_ref" (fun (_, r) -> pp_link_ref r)
    ]
  in
  let rows = List.map cases ~f:(fun (name, w) -> name, Link_ref.of_wikilink w) in
  print_string (Ascii_table.to_string_noattr cols rows);
  [%expect
    {|
    ┌───────────────┬────────────────────────────────────┐
    │ name          │ link_ref                           │
    ├───────────────┼────────────────────────────────────┤
    │ basic         │ target=Note frag=-                 │
    │ with heading  │ target=Note frag=Heading [H1; H2]  │
    │ with block    │ target=Note frag=Block_ref blockid │
    │ fragment only │ target=- frag=Heading [Heading]    │
    │ empty         │ target=- frag=-                    │
    └───────────────┴────────────────────────────────────┘
    |}]
;;

let%expect_test "of_markdown_dest" =
  let cases =
    [ "simple note", "Note"
    ; "with ext", "Note.md"
    ; "encoded space", "Note%202"
    ; "heading", "Note#Heading"
    ; "nested heading", "Note#H1#H2"
    ; "block ref", "Note#^blockid"
    ; "fragment only", "#Heading"
    ; "external https", "https://example.com"
    ; "external mailto", "mailto:x@y.com"
    ; "empty", ""
    ; "encoded hash", "Note%23Heading"
    ]
  in
  let cols =
    [ Ascii_table.Column.create "name" (fun (n, _, _) -> n)
    ; Ascii_table.Column.create "input" (fun (_, i, _) -> i)
    ; Ascii_table.Column.create "result" (fun (_, _, r) -> r)
    ]
  in
  let rows =
    List.map cases ~f:(fun (name, input) ->
      let result =
        match Link_ref.of_markdown_dest input with
        | None -> "None (external)"
        | Some r -> pp_link_ref r
      in
      name, input, result)
  in
  print_string (Ascii_table.to_string_noattr cols rows);
  [%expect
    {|
    ┌─────────────────┬─────────────────────┬────────────────────────────────────┐
    │ name            │ input               │ result                             │
    ├─────────────────┼─────────────────────┼────────────────────────────────────┤
    │ simple note     │ Note                │ target=Note frag=-                 │
    │ with ext        │ Note.md             │ target=Note.md frag=-              │
    │ encoded space   │ Note%202            │ target=Note 2 frag=-               │
    │ heading         │ Note#Heading        │ target=Note frag=Heading [Heading] │
    │ nested heading  │ Note#H1#H2          │ target=Note frag=Heading [H1; H2]  │
    │ block ref       │ Note#^blockid       │ target=Note frag=Block_ref blockid │
    │ fragment only   │ #Heading            │ target=- frag=Heading [Heading]    │
    │ external https  │ https://example.com │ None (external)                    │
    │ external mailto │ mailto:x@y.com      │ None (external)                    │
    │ empty           │                     │ target=- frag=-                    │
    │ encoded hash    │ Note%23Heading      │ target=Note frag=Heading [Heading] │
    └─────────────────┴─────────────────────┴────────────────────────────────────┘
    |}]
;;

(* Resolve tests
   ==================================================================== *)

(** Helper to build a synthetic vault index for testing. *)
let test_index : Index.t =
  { files =
      [ { rel_path = "Note1.md"
        ; headings =
            [ { text = "L2"; level = 2 }
            ; { text = "L3"; level = 3 }
            ; { text = "L4"; level = 4 }
            ; { text = "Another L3"; level = 3 }
            ]
        ; block_ids = [ "para1"; "block-2" ]
        }
      ; { rel_path = "dir/Note2.md"
        ; headings = [ { text = "Intro"; level = 1 } ]
        ; block_ids = []
        }
      ; { rel_path = "dir/inner_dir/deep.md"; headings = []; block_ids = [ "deep1" ] }
      ; { rel_path = "image.png"; headings = []; block_ids = [] }
      ; { rel_path = "indir_same_name.md"; headings = []; block_ids = [] }
      ; { rel_path = "dir/indir_same_name.md"; headings = []; block_ids = [] }
      ]
  }
;;

let pp_target : Resolve.target -> string = function
  | Resolve.File { path } -> Printf.sprintf "File(%s)" path
  | Resolve.Heading { path; heading; level } ->
    Printf.sprintf "Heading(%s, %s, H%d)" path heading level
  | Resolve.Block { path; block_id } -> Printf.sprintf "Block(%s, %s)" path block_id
  | Resolve.Current_file -> "Current_file"
  | Resolve.Current_heading { heading; level } ->
    Printf.sprintf "Current_heading(%s, H%d)" heading level
  | Resolve.Current_block { block_id } -> Printf.sprintf "Current_block(%s)" block_id
  | Resolve.Unresolved -> "Unresolved"
;;

let resolve_case name link_ref =
  let result = Resolve.resolve ~index:test_index ~current_file:"Note1.md" link_ref in
  name, pp_target result
;;

let%expect_test "resolve_file" =
  let cases =
    [ resolve_case "exact match" { target = Some "Note1"; fragment = None }
    ; resolve_case "exact with ext" { target = Some "Note1.md"; fragment = None }
    ; resolve_case "exact path" { target = Some "dir/Note2"; fragment = None }
    ; resolve_case "subsequence" { target = Some "Note2"; fragment = None }
    ; resolve_case "deep subsequence" { target = Some "inner_dir/deep"; fragment = None }
    ; resolve_case "partial subseq" { target = Some "dir/deep"; fragment = None }
    ; resolve_case "asset" { target = Some "image.png"; fragment = None }
    ; resolve_case "unresolved" { target = Some "nonexistent"; fragment = None }
    ; resolve_case "bad path" { target = Some "random/Note1"; fragment = None }
    ; resolve_case
        "exact root same name"
        { target = Some "indir_same_name"; fragment = None }
    ; resolve_case
        "exact dir same name"
        { target = Some "dir/indir_same_name"; fragment = None }
    ]
  in
  let cols =
    [ Ascii_table.Column.create "name" fst; Ascii_table.Column.create "result" snd ]
  in
  print_string (Ascii_table.to_string_noattr cols cases);
  [%expect
    {|
    ┌──────────────────────┬──────────────────────────────┐
    │ name                 │ result                       │
    ├──────────────────────┼──────────────────────────────┤
    │ exact match          │ File(Note1.md)               │
    │ exact with ext       │ File(Note1.md)               │
    │ exact path           │ File(dir/Note2.md)           │
    │ subsequence          │ File(dir/Note2.md)           │
    │ deep subsequence     │ File(dir/inner_dir/deep.md)  │
    │ partial subseq       │ File(dir/inner_dir/deep.md)  │
    │ asset                │ File(image.png)              │
    │ unresolved           │ Unresolved                   │
    │ bad path             │ Unresolved                   │
    │ exact root same name │ File(indir_same_name.md)     │
    │ exact dir same name  │ File(dir/indir_same_name.md) │
    └──────────────────────┴──────────────────────────────┘
    |}]
;;

let%expect_test "resolve_headings" =
  let cases =
    [ resolve_case
        "single heading"
        { target = Some "Note1"; fragment = Some (Heading [ "L2" ]) }
    ; resolve_case
        "nested valid"
        { target = Some "Note1"; fragment = Some (Heading [ "L2"; "L3" ]) }
    ; resolve_case
        "skip level"
        { target = Some "Note1"; fragment = Some (Heading [ "L2"; "L4" ]) }
    ; resolve_case
        "three levels"
        { target = Some "Note1"; fragment = Some (Heading [ "L2"; "L3"; "L4" ]) }
    ; resolve_case
        "invalid hierarchy"
        { target = Some "Note1"; fragment = Some (Heading [ "L2"; "L4"; "L3" ]) }
    ; resolve_case
        "invalid hierarchy 2"
        { target = Some "Note1"; fragment = Some (Heading [ "L2"; "L4"; "Another L3" ]) }
    ; resolve_case
        "nonexistent heading"
        { target = Some "Note1"; fragment = Some (Heading [ "NoSuch" ]) }
    ; resolve_case
        "heading in other file"
        { target = Some "Note2"; fragment = Some (Heading [ "Intro" ]) }
    ]
  in
  let cols =
    [ Ascii_table.Column.create "name" fst; Ascii_table.Column.create "result" snd ]
  in
  print_string (Ascii_table.to_string_noattr cols cases);
  [%expect
    {|
    ┌───────────────────────┬──────────────────────────────────┐
    │ name                  │ result                           │
    ├───────────────────────┼──────────────────────────────────┤
    │ single heading        │ Heading(Note1.md, L2, H2)        │
    │ nested valid          │ Heading(Note1.md, L3, H3)        │
    │ skip level            │ Heading(Note1.md, L4, H4)        │
    │ three levels          │ Heading(Note1.md, L4, H4)        │
    │ invalid hierarchy     │ File(Note1.md)                   │
    │ invalid hierarchy 2   │ File(Note1.md)                   │
    │ nonexistent heading   │ File(Note1.md)                   │
    │ heading in other file │ Heading(dir/Note2.md, Intro, H1) │
    └───────────────────────┴──────────────────────────────────┘
    |}]
;;

let%expect_test "resolve_blocks" =
  let cases =
    [ resolve_case
        "block found"
        { target = Some "Note1"; fragment = Some (Block_ref "para1") }
    ; resolve_case
        "block with hyphen"
        { target = Some "Note1"; fragment = Some (Block_ref "block-2") }
    ; resolve_case
        "block not found"
        { target = Some "Note1"; fragment = Some (Block_ref "nope") }
    ; resolve_case
        "block in deep file"
        { target = Some "deep"; fragment = Some (Block_ref "deep1") }
    ]
  in
  let cols =
    [ Ascii_table.Column.create "name" fst; Ascii_table.Column.create "result" snd ]
  in
  print_string (Ascii_table.to_string_noattr cols cases);
  [%expect
    {|
    ┌────────────────────┬─────────────────────────────────────┐
    │ name               │ result                              │
    ├────────────────────┼─────────────────────────────────────┤
    │ block found        │ Block(Note1.md, para1)              │
    │ block with hyphen  │ Block(Note1.md, block-2)            │
    │ block not found    │ File(Note1.md)                      │
    │ block in deep file │ Block(dir/inner_dir/deep.md, deep1) │
    └────────────────────┴─────────────────────────────────────┘
    |}]
;;

let%expect_test "resolve_self_references" =
  let cases =
    [ resolve_case "empty = current file" { target = None; fragment = None }
    ; resolve_case "current heading" { target = None; fragment = Some (Heading [ "L2" ]) }
    ; resolve_case
        "current nested heading"
        { target = None; fragment = Some (Heading [ "L2"; "L3" ]) }
    ; resolve_case "current block" { target = None; fragment = Some (Block_ref "para1") }
    ; resolve_case
        "current invalid heading"
        { target = None; fragment = Some (Heading [ "NoSuch" ]) }
    ; resolve_case
        "current invalid block"
        { target = None; fragment = Some (Block_ref "nope") }
    ]
  in
  let cols =
    [ Ascii_table.Column.create "name" fst; Ascii_table.Column.create "result" snd ]
  in
  print_string (Ascii_table.to_string_noattr cols cases);
  [%expect
    {|
    ┌─────────────────────────┬─────────────────────────┐
    │ name                    │ result                  │
    ├─────────────────────────┼─────────────────────────┤
    │ empty = current file    │ Current_file            │
    │ current heading         │ Current_heading(L2, H2) │
    │ current nested heading  │ Current_heading(L3, H3) │
    │ current block           │ Current_block(para1)    │
    │ current invalid heading │ Current_file            │
    │ current invalid block   │ Current_file            │
    └─────────────────────────┴─────────────────────────┘
    |}]
;;

let%expect_test "normalize_target" =
  let cases =
    [ "Note", "Note.md"
    ; "Note.md", "Note.md"
    ; "image.png", "image.png"
    ; "dir/Note", "dir/Note.md"
    ; "Figure.jpg.md", "Figure.jpg.md"
    ]
  in
  List.iter cases ~f:(fun (input, expected) ->
    let result = Resolve.normalize_target input in
    if String.equal result expected
    then Printf.printf "%s -> %s: OK\n" input result
    else Printf.printf "%s -> %s: FAIL (expected %s)\n" input result expected);
  [%expect
    {|
    Note -> Note.md: OK
    Note.md -> Note.md: OK
    image.png -> image.png: OK
    dir/Note -> dir/Note.md: OK
    Figure.jpg.md -> Figure.jpg.md: OK
    |}]
;;

let%expect_test "is_path_subsequence" =
  let cases =
    [ [ "dir"; "note.md" ], [ "dir"; "note.md" ], true
    ; [ "dir"; "inner_dir"; "note.md" ], [ "dir"; "note.md" ], true
    ; [ "dir"; "inner_dir"; "note.md" ], [ "inner_dir"; "note.md" ], true
    ; [ "dir"; "inner_dir"; "note.md" ], [ "random"; "note.md" ], false
    ; [ "a"; "b"; "c" ], [ "a"; "c" ], true
    ; [ "a"; "b"; "c" ], [ "c"; "a" ], false
    ]
  in
  List.iter cases ~f:(fun (haystack, needle, expected) ->
    let result = Resolve.is_path_subsequence ~haystack ~needle in
    let status = if Bool.equal result expected then "OK" else "FAIL" in
    Printf.printf
      "[%s] in [%s] = %b: %s\n"
      (String.concat ~sep:"/" needle)
      (String.concat ~sep:"/" haystack)
      result
      status);
  [%expect
    {|
    [dir/note.md] in [dir/note.md] = true: OK
    [dir/note.md] in [dir/inner_dir/note.md] = true: OK
    [inner_dir/note.md] in [dir/inner_dir/note.md] = true: OK
    [random/note.md] in [dir/inner_dir/note.md] = false: OK
    [a/c] in [a/b/c] = true: OK
    [c/a] in [a/b/c] = false: OK
    |}]
;;

(* Index extraction tests
   ==================================================================== *)

let%expect_test "extract_headings" =
  let md =
    {|
# Title

## Chapter 1

### Section 1.1

## Chapter 2

#### Deep
|}
  in
  let doc = Cmarkit.Doc.of_string ~strict:false md in
  let headings = Index.extract_headings doc in
  List.iter headings ~f:(fun (h : Index.heading_entry) ->
    Printf.printf "H%d: %s\n" h.level h.text);
  [%expect
    {|
    H1: Title
    H2: Chapter 1
    H3: Section 1.1
    H2: Chapter 2
    H4: Deep
    |}]
;;

let%expect_test "extract_block_ids" =
  let md =
    {|
First paragraph ^para1

Second paragraph without block id

Third paragraph ^block-2
|}
  in
  let doc = Cmarkit.Doc.of_string ~strict:false md in
  (* Need to apply block_id mapper first *)
  let mapper =
    Cmarkit.Mapper.make
      ~inline_ext_default:(fun _m i -> Some i)
      ~block:Block_id.tag_block_id_meta
      ()
  in
  let doc = Cmarkit.Mapper.map_doc mapper doc in
  let block_ids = Index.extract_block_ids doc in
  List.iter block_ids ~f:(fun id -> Printf.printf "%s\n" id);
  [%expect
    {|
    para1
    block-2
    |}]
;;
