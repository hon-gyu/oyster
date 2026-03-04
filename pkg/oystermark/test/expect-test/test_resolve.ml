open! Core
open Oystermark

(* Resolve tests
   ==================================================================== *)

(** Vault index modelled after the tt vault to cover behaviors documented in Note 1.md. *)
let test_index : Index.t =
  { files =
      [ { rel_path = "Note 1.md"
        ; headings =
            [ { text = "Level 3 title"; level = 3 }
            ; { text = "Level 4 title"; level = 4 }
            ; { text = "Example (level 3)"; level = 3 }
            ; { text = "L2"; level = 2 }
            ; { text = "L3"; level = 3 }
            ; { text = "L4"; level = 4 }
            ; { text = "Another L3"; level = 3 }
            ]
        ; block_ids = [ "para1"; "block-2" ]
        }
      ; { rel_path = "Note 2.md"
        ; headings =
            [ { text = "Some level 2 title"; level = 2 }
            ; { text = "L4"; level = 4 }
            ; { text = "Level 3 title"; level = 3 }
            ; { text = "Another level 2 title"; level = 2 }
            ]
        ; block_ids = []
        }
      ; { rel_path = "Three laws of motion.md"; headings = []; block_ids = [] }
      ; { rel_path = "().md"; headings = []; block_ids = [] }
      ; { rel_path = "ww.md"; headings = []; block_ids = [] }
      ; { rel_path = "Figure1.jpg"; headings = []; block_ids = [] }
      ; { rel_path = "Figure1.jpg.md"; headings = []; block_ids = [] }
      ; { rel_path = "Figure1.jpg.md.md"; headings = []; block_ids = [] }
      ; { rel_path = "Figure1.md"; headings = []; block_ids = [] }
      ; { rel_path = "Figure1^2.jpg"; headings = []; block_ids = [] }
      ; { rel_path = "image.png"; headings = []; block_ids = [] }
      ; { rel_path = "empty_video.mp4"; headings = []; block_ids = [] }
      ; { rel_path = "unsupported_text_file.txt"; headings = []; block_ids = [] }
      ; { rel_path = "a.joiwduvqneoi"; headings = []; block_ids = [] }
      ; { rel_path = "Something"; headings = []; block_ids = [] }
      ; { rel_path = "Something.md"; headings = []; block_ids = [] }
      ; { rel_path = "Note 1"; headings = []; block_ids = [] }
      ; { rel_path = "indir_same_name.md"; headings = []; block_ids = [] }
      ; { rel_path = "dir/indir_same_name.md"; headings = []; block_ids = [] }
      ; { rel_path = "dir/indir2.md"; headings = []; block_ids = [] }
      ; { rel_path = "dir/inner_dir/note_in_inner_dir.md"; headings = []; block_ids = [] }
      ; { rel_path = "dir/inner_dir/deep.md"; headings = []; block_ids = [ "deep1" ] }
      ]
  }
;;

let curr_file = "Note 1.md"

let target_to_string (t : Resolve.target) : string =
  match t with
  | File { path } -> sprintf "File(%s)" path
  | Heading { path; heading; level } -> sprintf "Heading(%s, %s, %d)" path heading level
  | Block { path; block_id } -> sprintf "Block(%s, %s)" path block_id
  | Curr_file -> "Curr_file"
  | Curr_heading { heading; level } -> sprintf "Curr_heading(%s, %d)" heading level
  | Curr_block { block_id } -> sprintf "Curr_block(%s)" block_id
  | Unresolved -> "Unresolved"
;;

let resolve_and_print cases =
  let cols =
    [ Ascii_table.Column.create "name" (fun (name, _) -> name)
    ; Ascii_table.Column.create "result" (fun (_, link_ref) ->
        Resolve.resolve link_ref curr_file test_index |> target_to_string)
    ]
  in
  print_string (Ascii_table.to_string_noattr cols cases)
;;

let%expect_test "resolve_file" =
  let cases : (string * Link_ref.t) list =
    [ "exact match", { target = Some "Note 1"; fragment = None }
    ; "exact with ext", { target = Some "Note 1.md"; fragment = None }
    ; "spaces in name", { target = Some "Three laws of motion"; fragment = None }
    ; "spaces with ext", { target = Some "Three laws of motion.md"; fragment = None }
    ; "exact path", { target = Some "dir/indir_same_name"; fragment = None }
    ; "subsequence", { target = Some "Note 2"; fragment = None }
    ; "deep subseq", { target = Some "inner_dir/note_in_inner_dir"; fragment = None }
    ; "partial subseq", { target = Some "dir/note_in_inner_dir"; fragment = None }
    ; "full path subseq", { target = Some "dir/inner_dir/note_in_inner_dir"; fragment = None }
    ; "subseq from non-root", { target = Some "indir2"; fragment = None }
    ; "asset png", { target = Some "image.png"; fragment = None }
    ; "asset txt", { target = Some "unsupported_text_file.txt"; fragment = None }
    ; "asset unknown ext", { target = Some "a.joiwduvqneoi"; fragment = None }
    ; "asset video", { target = Some "empty_video.mp4"; fragment = None }
    ; "unresolved", { target = Some "nonexistent"; fragment = None }
    ; "bad path", { target = Some "random/Note 1"; fragment = None }
    ; "root same name wins", { target = Some "indir_same_name"; fragment = None }
    ; "dir same name exact", { target = Some "dir/indir_same_name"; fragment = None }
    ; "random dir unresolved", { target = Some "random/note_in_inner_dir"; fragment = None }
    ; "().md", { target = Some "().md"; fragment = None }
    ; "ww", { target = Some "ww"; fragment = None }
    ]
  in
  resolve_and_print cases;
  [%expect {|
    ┌───────────────────────┬──────────────────────────────────────────┐
    │ name                  │ result                                   │
    ├───────────────────────┼──────────────────────────────────────────┤
    │ exact match           │ File(Note 1.md)                          │
    │ exact with ext        │ File(Note 1.md)                          │
    │ spaces in name        │ File(Three laws of motion.md)            │
    │ spaces with ext       │ File(Three laws of motion.md)            │
    │ exact path            │ File(dir/indir_same_name.md)             │
    │ subsequence           │ File(Note 2.md)                          │
    │ deep subseq           │ File(dir/inner_dir/note_in_inner_dir.md) │
    │ partial subseq        │ File(dir/inner_dir/note_in_inner_dir.md) │
    │ full path subseq      │ File(dir/inner_dir/note_in_inner_dir.md) │
    │ subseq from non-root  │ File(dir/indir2.md)                      │
    │ asset png             │ File(image.png)                          │
    │ asset txt             │ File(unsupported_text_file.txt)          │
    │ asset unknown ext     │ File(a.joiwduvqneoi)                     │
    │ asset video           │ File(empty_video.mp4)                    │
    │ unresolved            │ Unresolved                               │
    │ bad path              │ Unresolved                               │
    │ root same name wins   │ File(indir_same_name.md)                 │
    │ dir same name exact   │ File(dir/indir_same_name.md)             │
    │ random dir unresolved │ Unresolved                               │
    │ ().md                 │ File(().md)                              │
    │ ww                    │ File(ww.md)                              │
    └───────────────────────┴──────────────────────────────────────────┘
    |}]
;;

let%expect_test "resolve_note_vs_asset_priority" =
  let cases : (string * Link_ref.t) list =
    [ (* Figure1.jpg exists as asset AND Figure1.jpg.md exists as note.
         resolve_file with "Figure1.jpg" has a dot so no .md appended -> exact match on asset *)
      "Figure1.jpg -> asset", { target = Some "Figure1.jpg"; fragment = None }
    ; (* With explicit .md, searches for note "Figure1.jpg.md" *)
      "Figure1.jpg.md -> note", { target = Some "Figure1.jpg.md"; fragment = None }
    ; "Figure1.jpg.md.md", { target = Some "Figure1.jpg.md.md"; fragment = None }
    ; (* "Figure1^2.jpg" has a dot, no .md appended, exact match *)
      "Figure1^2.jpg -> asset", { target = Some "Figure1^2.jpg"; fragment = None }
    ; (* "Something" has no dot -> .md appended -> "Something.md" matches note *)
      "Something -> note", { target = Some "Something"; fragment = None }
    ; (* "Note 1" has no dot -> .md appended -> "Note 1.md" matches note *)
      "Note 1 -> note", { target = Some "Note 1"; fragment = None }
    ; (* "Figure1" has no dot -> .md appended -> "Figure1.md" matches note *)
      "Figure1 -> note", { target = Some "Figure1"; fragment = None }
    ]
  in
  resolve_and_print cases;
  [%expect {|
    ┌────────────────────────┬─────────────────────────┐
    │ name                   │ result                  │
    ├────────────────────────┼─────────────────────────┤
    │ Figure1.jpg -> asset   │ File(Figure1.jpg)       │
    │ Figure1.jpg.md -> note │ File(Figure1.jpg.md)    │
    │ Figure1.jpg.md.md      │ File(Figure1.jpg.md.md) │
    │ Figure1^2.jpg -> asset │ File(Figure1^2.jpg)     │
    │ Something -> note      │ File(Something.md)      │
    │ Note 1 -> note         │ File(Note 1.md)         │
    │ Figure1 -> note        │ File(Figure1.md)        │
    └────────────────────────┴─────────────────────────┘
    |}]
;;

let%expect_test "resolve_headings_note2" =
  (* Note 2.md headings: "Some level 2 title" L2, "L4" L4, "Level 3 title" L3,
     "Another level 2 title" L2 *)
  let cases : (string * Link_ref.t) list =
    [ ( "Note 2#Some level 2 title"
      , { target = Some "Note 2"; fragment = Some (Heading [ "Some level 2 title" ]) } )
    ; ( "Note 2#Some level 2 title#Level 3 title"
      , { target = Some "Note 2"
        ; fragment = Some (Heading [ "Some level 2 title"; "Level 3 title" ])
        } )
    ; ( "Note 2#Some level 2 title#L4"
      , { target = Some "Note 2"
        ; fragment = Some (Heading [ "Some level 2 title"; "L4" ])
        } )
    ; ( "Note 2#Level 3 title"
      , { target = Some "Note 2"; fragment = Some (Heading [ "Level 3 title" ]) } )
    ; ( "Note 2#L4"
      , { target = Some "Note 2"; fragment = Some (Heading [ "L4" ]) } )
    ; ( "Note 2#random -> fallback"
      , { target = Some "Note 2"; fragment = Some (Heading [ "random" ]) } )
    ; ( "Note 2#random#Level 3 title -> fallback"
      , { target = Some "Note 2"
        ; fragment = Some (Heading [ "random"; "Level 3 title" ])
        } )
    ]
  in
  resolve_and_print cases;
  [%expect {|
    ┌─────────────────────────────────────────┬───────────────────────────────────────────┐
    │ name                                    │ result                                    │
    ├─────────────────────────────────────────┼───────────────────────────────────────────┤
    │ Note 2#Some level 2 title               │ Heading(Note 2.md, Some level 2 title, 2) │
    │ Note 2#Some level 2 title#Level 3 title │ Heading(Note 2.md, Level 3 title, 3)      │
    │ Note 2#Some level 2 title#L4            │ Heading(Note 2.md, L4, 4)                 │
    │ Note 2#Level 3 title                    │ Heading(Note 2.md, Level 3 title, 3)      │
    │ Note 2#L4                               │ Heading(Note 2.md, L4, 4)                 │
    │ Note 2#random -> fallback               │ File(Note 2.md)                           │
    │ Note 2#random#Level 3 title -> fallback │ File(Note 2.md)                           │
    └─────────────────────────────────────────┴───────────────────────────────────────────┘
    |}]
;;

let%expect_test "resolve_headings_note1" =
  (* Note 1.md headings: "Level 3 title" L3, "Level 4 title" L4, "Example (level 3)" L3,
     "L2" L2, "L3" L3, "L4" L4, "Another L3" L3 *)
  let cases : (string * Link_ref.t) list =
    [ "L2", { target = Some "Note 1"; fragment = Some (Heading [ "L2" ]) }
    ; "L2 L3", { target = Some "Note 1"; fragment = Some (Heading [ "L2"; "L3" ]) }
    ; "L2 L4", { target = Some "Note 1"; fragment = Some (Heading [ "L2"; "L4" ]) }
    ; "L2 L3 L4", { target = Some "Note 1"; fragment = Some (Heading [ "L2"; "L3"; "L4" ]) }
    ; ( "L2 L4 L3 -> fallback (decreasing)"
      , { target = Some "Note 1"; fragment = Some (Heading [ "L2"; "L4"; "L3" ]) } )
    ; ( "L2 L4 Another L3 -> fallback"
      , { target = Some "Note 1"
        ; fragment = Some (Heading [ "L2"; "L4"; "Another L3" ])
        } )
    ; ( "NoSuch -> fallback"
      , { target = Some "Note 1"; fragment = Some (Heading [ "NoSuch" ]) } )
    ; ( "heading in unresolved file"
      , { target = Some "nonexistent"; fragment = Some (Heading [ "L2" ]) } )
    ]
  in
  resolve_and_print cases;
  [%expect {|
    ┌───────────────────────────────────┬───────────────────────────┐
    │ name                              │ result                    │
    ├───────────────────────────────────┼───────────────────────────┤
    │ L2                                │ Heading(Note 1.md, L2, 2) │
    │ L2 L3                             │ Heading(Note 1.md, L3, 3) │
    │ L2 L4                             │ Heading(Note 1.md, L4, 4) │
    │ L2 L3 L4                          │ Heading(Note 1.md, L4, 4) │
    │ L2 L4 L3 -> fallback (decreasing) │ File(Note 1.md)           │
    │ L2 L4 Another L3 -> fallback      │ File(Note 1.md)           │
    │ NoSuch -> fallback                │ File(Note 1.md)           │
    │ heading in unresolved file        │ Unresolved                │
    └───────────────────────────────────┴───────────────────────────┘
    |}]
;;

let%expect_test "resolve_blocks" =
  let cases : (string * Link_ref.t) list =
    [ "block found", { target = Some "Note 1"; fragment = Some (Block_ref "para1") }
    ; ( "block with hyphen"
      , { target = Some "Note 1"; fragment = Some (Block_ref "block-2") } )
    ; ( "block not found -> fallback"
      , { target = Some "Note 1"; fragment = Some (Block_ref "nope") } )
    ; "block in deep file", { target = Some "deep"; fragment = Some (Block_ref "deep1") }
    ; ( "block in unresolved file"
      , { target = Some "nonexistent"; fragment = Some (Block_ref "x") } )
    ]
  in
  resolve_and_print cases;
  [%expect {|
    ┌─────────────────────────────┬─────────────────────────────────────┐
    │ name                        │ result                              │
    ├─────────────────────────────┼─────────────────────────────────────┤
    │ block found                 │ Block(Note 1.md, para1)             │
    │ block with hyphen           │ Block(Note 1.md, block-2)           │
    │ block not found -> fallback │ File(Note 1.md)                     │
    │ block in deep file          │ Block(dir/inner_dir/deep.md, deep1) │
    │ block in unresolved file    │ Unresolved                          │
    └─────────────────────────────┴─────────────────────────────────────┘
    |}]
;;

let%expect_test "resolve_self_references" =
  let cases : (string * Link_ref.t) list =
    [ "[[]] -> curr file", { target = None; fragment = None }
    ; "[[#L2]]", { target = None; fragment = Some (Heading [ "L2" ]) }
    ; "[[#L2#L3]]", { target = None; fragment = Some (Heading [ "L2"; "L3" ]) }
    ; "[[#^para1]]", { target = None; fragment = Some (Block_ref "para1") }
    ; "[[#NoSuch]] -> fallback", { target = None; fragment = Some (Heading [ "NoSuch" ]) }
    ; "[[#^nope]] -> fallback", { target = None; fragment = Some (Block_ref "nope") }
    ; ( "[[#]] empty heading -> curr"
      , { target = None; fragment = None } )
    ]
  in
  resolve_and_print cases;
  [%expect {|
    ┌─────────────────────────────┬─────────────────────┐
    │ name                        │ result              │
    ├─────────────────────────────┼─────────────────────┤
    │ [[]] -> curr file           │ Curr_file           │
    │ [[#L2]]                     │ Curr_heading(L2, 2) │
    │ [[#L2#L3]]                  │ Curr_heading(L3, 3) │
    │ [[#^para1]]                 │ Curr_block(para1)   │
    │ [[#NoSuch]] -> fallback     │ Curr_file           │
    │ [[#^nope]] -> fallback      │ Curr_file           │
    │ [[#]] empty heading -> curr │ Curr_file           │
    └─────────────────────────────┴─────────────────────┘
    |}]
;;

let%expect_test "resolve_self_ref_unknown_curr_file" =
  let resolve_with_unknown link_ref =
    Resolve.resolve link_ref "unknown_file.md" test_index |> target_to_string
  in
  let cases =
    [ "self ref", { Link_ref.target = None; fragment = None }
    ; "self heading", { target = None; fragment = Some (Heading [ "L2" ]) }
    ; "self block", { target = None; fragment = Some (Block_ref "para1") }
    ]
  in
  let cols =
    [ Ascii_table.Column.create "name" fst
    ; Ascii_table.Column.create "result" (fun (_, lr) -> resolve_with_unknown lr)
    ]
  in
  print_string (Ascii_table.to_string_noattr cols cases);
  [%expect {|
    ┌──────────────┬───────────┐
    │ name         │ result    │
    ├──────────────┼───────────┤
    │ self ref     │ Curr_file │
    │ self heading │ Curr_file │
    │ self block   │ Curr_file │
    └──────────────┴───────────┘
    |}]
;;

let%expect_test "resolve_asset_with_fragment" =
  (* [[Figure1.jpg#2]] — the "#2" is parsed as heading fragment, but Figure1.jpg has no
     headings, so it falls back to File *)
  let cases : (string * Link_ref.t) list =
    [ ( "Figure1.jpg#2 -> fallback to asset"
      , { target = Some "Figure1.jpg"; fragment = Some (Heading [ "2" ]) } )
    ; ( "Note 2## -> empty heading fallback"
      , { target = Some "Note 2"; fragment = None } )
    ]
  in
  resolve_and_print cases;
  [%expect {|
    ┌────────────────────────────────────┬───────────────────┐
    │ name                               │ result            │
    ├────────────────────────────────────┼───────────────────┤
    │ Figure1.jpg#2 -> fallback to asset │ File(Figure1.jpg) │
    │ Note 2## -> empty heading fallback │ File(Note 2.md)   │
    └────────────────────────────────────┴───────────────────┘
    |}]
;;
