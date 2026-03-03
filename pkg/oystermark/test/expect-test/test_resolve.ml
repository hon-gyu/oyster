open! Core
open Oystermark

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
  | Resolve.Curr_file -> "Current_file"
  | Resolve.Curr_heading { heading; level } ->
    Printf.sprintf "Current_heading(%s, H%d)" heading level
  | Resolve.Curr_block { block_id } -> Printf.sprintf "Current_block(%s)" block_id
  | Resolve.Unresolved -> "Unresolved"
;;

let resolve_case name link_ref =
  let result = Resolve.resolve link_ref "Note1.md" test_index in
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
