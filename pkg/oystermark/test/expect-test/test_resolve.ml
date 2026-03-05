open! Core
open Oystermark

(* Resolve tests
   ==================================================================== *)

(* Vault index modelled after the tt vault to cover behaviors documented in Note 1.md. *)
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

let make_link_ref
      (target : string option)
      (fragment : (string list * [ `Heading | `Block_ref ]) option)
  : Link_ref.t
  =
  { target
  ; fragment =
      (match fragment with
       | None -> None
       | Some (headings, `Heading) -> Some (Heading headings)
       | Some (block_id, `Block_ref) ->
         (match block_id with
          | [ block_id ] -> Some (Block_ref block_id)
          | _ -> invalid_arg "should only exists one block id"))
  }
;;

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

let resolve_and_print
      ?(curr_file : string = "Note 1.md")
      (cases : (string * Link_ref.t) list)
  =
  let cols =
    [ Ascii_table.Column.create "name" (fun (name, _) -> name)
    ; Ascii_table.Column.create "input/target" (fun (_, (lr : Link_ref.t)) ->
        Option.value ~default:"-" lr.target)
    ; Ascii_table.Column.create "input/fragment" (fun (_, (lr : Link_ref.t)) ->
        match lr.fragment with
        | None -> "-"
        | Some (Heading hs) -> "H[" ^ String.concat ~sep:"; " hs ^ "]"
        | Some (Block_ref s) -> "B[" ^ s ^ "]")
    ; Ascii_table.Column.create "result" (fun (_, lr) ->
        Resolve.resolve lr curr_file test_index |> target_to_string)
    ]
  in
  print_string (Ascii_table.to_string_noattr cols cases ~limit_width_to:150)
;;

(* Shorthand aliases *)
let t s = Some s
let h hs = Some (hs, `Heading)
let b id = Some ([ id ], `Block_ref)

let%expect_test "resolve_file" =
  let cases =
    [ "exact match", make_link_ref (t "Note 1") None
    ; "exact with ext", make_link_ref (t "Note 1.md") None
    ; "spaces in name", make_link_ref (t "Three laws of motion") None
    ; "spaces with ext", make_link_ref (t "Three laws of motion.md") None
    ; "exact path", make_link_ref (t "dir/indir_same_name") None
    ; "subsequence", make_link_ref (t "Note 2") None
    ; "deep subseq", make_link_ref (t "inner_dir/note_in_inner_dir") None
    ; "partial subseq", make_link_ref (t "dir/note_in_inner_dir") None
    ; "full path subseq", make_link_ref (t "dir/inner_dir/note_in_inner_dir") None
    ; "subseq from non-root", make_link_ref (t "indir2") None
    ; "asset png", make_link_ref (t "image.png") None
    ; "asset txt", make_link_ref (t "unsupported_text_file.txt") None
    ; "asset unknown ext", make_link_ref (t "a.joiwduvqneoi") None
    ; "asset video", make_link_ref (t "empty_video.mp4") None
    ; "unresolved", make_link_ref (t "nonexistent") None
    ; "bad path", make_link_ref (t "random/Note 1") None
    ; "root same name wins", make_link_ref (t "indir_same_name") None
    ; "dir same name exact", make_link_ref (t "dir/indir_same_name") None
    ; "random dir unresolved", make_link_ref (t "random/note_in_inner_dir") None
    ; "().md", make_link_ref (t "().md") None
    ; "ww", make_link_ref (t "ww") None
    ]
  in
  resolve_and_print cases;
  [%expect
    {|
    ┌───────────────────────┬─────────────────────────────────┬────────────────┬──────────────────────────────────────────┐
    │ name                  │ input/target                    │ input/fragment │ result                                   │
    ├───────────────────────┼─────────────────────────────────┼────────────────┼──────────────────────────────────────────┤
    │ exact match           │ Note 1                          │ -              │ File(Note 1.md)                          │
    │ exact with ext        │ Note 1.md                       │ -              │ File(Note 1.md)                          │
    │ spaces in name        │ Three laws of motion            │ -              │ File(Three laws of motion.md)            │
    │ spaces with ext       │ Three laws of motion.md         │ -              │ File(Three laws of motion.md)            │
    │ exact path            │ dir/indir_same_name             │ -              │ File(dir/indir_same_name.md)             │
    │ subsequence           │ Note 2                          │ -              │ File(Note 2.md)                          │
    │ deep subseq           │ inner_dir/note_in_inner_dir     │ -              │ File(dir/inner_dir/note_in_inner_dir.md) │
    │ partial subseq        │ dir/note_in_inner_dir           │ -              │ File(dir/inner_dir/note_in_inner_dir.md) │
    │ full path subseq      │ dir/inner_dir/note_in_inner_dir │ -              │ File(dir/inner_dir/note_in_inner_dir.md) │
    │ subseq from non-root  │ indir2                          │ -              │ File(dir/indir2.md)                      │
    │ asset png             │ image.png                       │ -              │ File(image.png)                          │
    │ asset txt             │ unsupported_text_file.txt       │ -              │ File(unsupported_text_file.txt)          │
    │ asset unknown ext     │ a.joiwduvqneoi                  │ -              │ File(a.joiwduvqneoi)                     │
    │ asset video           │ empty_video.mp4                 │ -              │ File(empty_video.mp4)                    │
    │ unresolved            │ nonexistent                     │ -              │ Unresolved                               │
    │ bad path              │ random/Note 1                   │ -              │ Unresolved                               │
    │ root same name wins   │ indir_same_name                 │ -              │ File(indir_same_name.md)                 │
    │ dir same name exact   │ dir/indir_same_name             │ -              │ File(dir/indir_same_name.md)             │
    │ random dir unresolved │ random/note_in_inner_dir        │ -              │ Unresolved                               │
    │ ().md                 │ ().md                           │ -              │ File(().md)                              │
    │ ww                    │ ww                              │ -              │ File(ww.md)                              │
    └───────────────────────┴─────────────────────────────────┴────────────────┴──────────────────────────────────────────┘
    |}]
;;

let%expect_test "resolve_note_vs_asset_priority" =
  let cases =
    [ "Figure1.jpg -> asset", make_link_ref (t "Figure1.jpg") None
    ; "Figure1.jpg.md -> note", make_link_ref (t "Figure1.jpg.md") None
    ; "Figure1.jpg.md.md", make_link_ref (t "Figure1.jpg.md.md") None
    ; "Figure1^2.jpg -> asset", make_link_ref (t "Figure1^2.jpg") None
    ; "Something -> note", make_link_ref (t "Something") None
    ; "Note 1 -> note", make_link_ref (t "Note 1") None
    ; "Figure1 -> note", make_link_ref (t "Figure1") None
    ]
  in
  resolve_and_print cases;
  [%expect
    {|
    ┌────────────────────────┬───────────────────┬────────────────┬─────────────────────────┐
    │ name                   │ input/target      │ input/fragment │ result                  │
    ├────────────────────────┼───────────────────┼────────────────┼─────────────────────────┤
    │ Figure1.jpg -> asset   │ Figure1.jpg       │ -              │ File(Figure1.jpg)       │
    │ Figure1.jpg.md -> note │ Figure1.jpg.md    │ -              │ File(Figure1.jpg.md)    │
    │ Figure1.jpg.md.md      │ Figure1.jpg.md.md │ -              │ File(Figure1.jpg.md.md) │
    │ Figure1^2.jpg -> asset │ Figure1^2.jpg     │ -              │ File(Figure1^2.jpg)     │
    │ Something -> note      │ Something         │ -              │ File(Something.md)      │
    │ Note 1 -> note         │ Note 1            │ -              │ File(Note 1.md)         │
    │ Figure1 -> note        │ Figure1           │ -              │ File(Figure1.md)        │
    └────────────────────────┴───────────────────┴────────────────┴─────────────────────────┘
    |}]
;;

let%expect_test "resolve_headings_note2" =
  let cases =
    [ "single heading", make_link_ref (t "Note 2") (h [ "Some level 2 title" ])
    ; ( "nested heading"
      , make_link_ref (t "Note 2") (h [ "Some level 2 title"; "Level 3 title" ]) )
    ; "nested skip level", make_link_ref (t "Note 2") (h [ "Some level 2 title"; "L4" ])
    ; "L3 directly", make_link_ref (t "Note 2") (h [ "Level 3 title" ])
    ; "L4 directly", make_link_ref (t "Note 2") (h [ "L4" ])
    ; "random -> fallback", make_link_ref (t "Note 2") (h [ "random" ])
    ; ( "random#L3 -> fallback"
      , make_link_ref (t "Note 2") (h [ "random"; "Level 3 title" ]) )
    ]
  in
  resolve_and_print cases;
  [%expect
    {|
    ┌───────────────────────┬──────────────┬──────────────────────────────────────┬───────────────────────────────────────────┐
    │ name                  │ input/target │ input/fragment                       │ result                                    │
    ├───────────────────────┼──────────────┼──────────────────────────────────────┼───────────────────────────────────────────┤
    │ single heading        │ Note 2       │ H[Some level 2 title]                │ Heading(Note 2.md, Some level 2 title, 2) │
    │ nested heading        │ Note 2       │ H[Some level 2 title; Level 3 title] │ Heading(Note 2.md, Level 3 title, 3)      │
    │ nested skip level     │ Note 2       │ H[Some level 2 title; L4]            │ Heading(Note 2.md, L4, 4)                 │
    │ L3 directly           │ Note 2       │ H[Level 3 title]                     │ Heading(Note 2.md, Level 3 title, 3)      │
    │ L4 directly           │ Note 2       │ H[L4]                                │ Heading(Note 2.md, L4, 4)                 │
    │ random -> fallback    │ Note 2       │ H[random]                            │ File(Note 2.md)                           │
    │ random#L3 -> fallback │ Note 2       │ H[random; Level 3 title]             │ File(Note 2.md)                           │
    └───────────────────────┴──────────────┴──────────────────────────────────────┴───────────────────────────────────────────┘
    |}]
;;

let%expect_test "resolve_headings_note1" =
  let cases =
    [ "L2", make_link_ref (t "Note 1") (h [ "L2" ])
    ; "L2 L3", make_link_ref (t "Note 1") (h [ "L2"; "L3" ])
    ; "L2 L4", make_link_ref (t "Note 1") (h [ "L2"; "L4" ])
    ; "L2 L3 L4", make_link_ref (t "Note 1") (h [ "L2"; "L3"; "L4" ])
    ; "L2 L4 L3 -> fallback", make_link_ref (t "Note 1") (h [ "L2"; "L4"; "L3" ])
    ; ( "L2 L4 Another L3 -> fallback"
      , make_link_ref (t "Note 1") (h [ "L2"; "L4"; "Another L3" ]) )
    ; "NoSuch -> fallback", make_link_ref (t "Note 1") (h [ "NoSuch" ])
    ; "heading unresolved file", make_link_ref (t "nonexistent") (h [ "L2" ])
    ]
  in
  resolve_and_print cases;
  [%expect
    {|
    ┌──────────────────────────────┬──────────────┬───────────────────────┬───────────────────────────┐
    │ name                         │ input/target │ input/fragment        │ result                    │
    ├──────────────────────────────┼──────────────┼───────────────────────┼───────────────────────────┤
    │ L2                           │ Note 1       │ H[L2]                 │ Heading(Note 1.md, L2, 2) │
    │ L2 L3                        │ Note 1       │ H[L2; L3]             │ Heading(Note 1.md, L3, 3) │
    │ L2 L4                        │ Note 1       │ H[L2; L4]             │ Heading(Note 1.md, L4, 4) │
    │ L2 L3 L4                     │ Note 1       │ H[L2; L3; L4]         │ Heading(Note 1.md, L4, 4) │
    │ L2 L4 L3 -> fallback         │ Note 1       │ H[L2; L4; L3]         │ File(Note 1.md)           │
    │ L2 L4 Another L3 -> fallback │ Note 1       │ H[L2; L4; Another L3] │ File(Note 1.md)           │
    │ NoSuch -> fallback           │ Note 1       │ H[NoSuch]             │ File(Note 1.md)           │
    │ heading unresolved file      │ nonexistent  │ H[L2]                 │ Unresolved                │
    └──────────────────────────────┴──────────────┴───────────────────────┴───────────────────────────┘
    |}]
;;

let%expect_test "resolve_blocks" =
  let cases =
    [ "block found", make_link_ref (t "Note 1") (b "para1")
    ; "block with hyphen", make_link_ref (t "Note 1") (b "block-2")
    ; "block not found -> fallback", make_link_ref (t "Note 1") (b "nope")
    ; "block in deep file", make_link_ref (t "deep") (b "deep1")
    ; "block in unresolved file", make_link_ref (t "nonexistent") (b "x")
    ]
  in
  resolve_and_print cases;
  [%expect
    {|
    ┌─────────────────────────────┬──────────────┬────────────────┬─────────────────────────────────────┐
    │ name                        │ input/target │ input/fragment │ result                              │
    ├─────────────────────────────┼──────────────┼────────────────┼─────────────────────────────────────┤
    │ block found                 │ Note 1       │ B[para1]       │ Block(Note 1.md, para1)             │
    │ block with hyphen           │ Note 1       │ B[block-2]     │ Block(Note 1.md, block-2)           │
    │ block not found -> fallback │ Note 1       │ B[nope]        │ File(Note 1.md)                     │
    │ block in deep file          │ deep         │ B[deep1]       │ Block(dir/inner_dir/deep.md, deep1) │
    │ block in unresolved file    │ nonexistent  │ B[x]           │ Unresolved                          │
    └─────────────────────────────┴──────────────┴────────────────┴─────────────────────────────────────┘
    |}]
;;

let%expect_test "resolve_self_references" =
  let cases =
    [ "[[]] -> curr file", make_link_ref None None
    ; "[[#L2]]", make_link_ref None (h [ "L2" ])
    ; "[[#L2#L3]]", make_link_ref None (h [ "L2"; "L3" ])
    ; "[[#^para1]]", make_link_ref None (b "para1")
    ; "[[#NoSuch]] -> fallback", make_link_ref None (h [ "NoSuch" ])
    ; "[[#^nope]] -> fallback", make_link_ref None (b "nope")
    ; "[[#]] empty heading", make_link_ref None None
    ]
  in
  resolve_and_print cases;
  [%expect
    {|
    ┌─────────────────────────┬──────────────┬────────────────┬─────────────────────┐
    │ name                    │ input/target │ input/fragment │ result              │
    ├─────────────────────────┼──────────────┼────────────────┼─────────────────────┤
    │ [[]] -> curr file       │ -            │ -              │ Curr_file           │
    │ [[#L2]]                 │ -            │ H[L2]          │ Curr_heading(L2, 2) │
    │ [[#L2#L3]]              │ -            │ H[L2; L3]      │ Curr_heading(L3, 3) │
    │ [[#^para1]]             │ -            │ B[para1]       │ Curr_block(para1)   │
    │ [[#NoSuch]] -> fallback │ -            │ H[NoSuch]      │ Curr_file           │
    │ [[#^nope]] -> fallback  │ -            │ B[nope]        │ Curr_file           │
    │ [[#]] empty heading     │ -            │ -              │ Curr_file           │
    └─────────────────────────┴──────────────┴────────────────┴─────────────────────┘
    |}]
;;

let%expect_test "resolve_asset_with_fragment" =
  let cases =
    [ "Figure1.jpg#2 -> fallback", make_link_ref (t "Figure1.jpg") (h [ "2" ])
    ; "Note 2## -> empty heading", make_link_ref (t "Note 2") None
    ]
  in
  resolve_and_print cases;
  [%expect
    {|
    ┌───────────────────────────┬──────────────┬────────────────┬───────────────────┐
    │ name                      │ input/target │ input/fragment │ result            │
    ├───────────────────────────┼──────────────┼────────────────┼───────────────────┤
    │ Figure1.jpg#2 -> fallback │ Figure1.jpg  │ H[2]           │ File(Figure1.jpg) │
    │ Note 2## -> empty heading │ Note 2       │ -              │ File(Note 2.md)   │
    └───────────────────────────┴──────────────┴────────────────┴───────────────────┘
    |}]
;;
