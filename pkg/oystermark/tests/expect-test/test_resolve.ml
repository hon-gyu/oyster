open! Core
open Oystermark
module Index = Vault.Index
module Link_ref = Note.Link_ref

(* Vault index modelled after the tt vault to cover behaviors documented in Note 1.md. *)
let test_index : Index.t =
  Vault.build_index
    ~md_docs:
      [ ( "Note 1.md"
        , Parse.of_string
            ~locs:true
            "### Level 3 title\n\
             #### Level 4 title\n\
             ### Example (level 3)\n\
             ## L2\n\
             ### L3\n\
             #### L4\n\
             ### Another L3\n\n\
             para ^para1\n\n\
             block ^block-2\n" )
      ; ( "Note 2.md"
        , Parse.of_string
            ~locs:true
            "## Some level 2 title\n\
             #### L4\n\
             ### Level 3 title\n\
             ## Another level 2 title\n" )
      ; "Three laws of motion.md", Parse.of_string ~locs:true ""
      ; "().md", Parse.of_string ~locs:true ""
      ; "ww.md", Parse.of_string ~locs:true ""
      ; "Figure1.jpg.md", Parse.of_string ~locs:true ""
      ; "Figure1.jpg.md.md", Parse.of_string ~locs:true ""
      ; "Figure1.md", Parse.of_string ~locs:true ""
      ; "Something.md", Parse.of_string ~locs:true ""
      ; "indir_same_name.md", Parse.of_string ~locs:true ""
      ; "dir/indir_same_name.md", Parse.of_string ~locs:true ""
      ; "dir/indir2.md", Parse.of_string ~locs:true ""
      ; "dir/inner_dir/note_in_inner_dir.md", Parse.of_string ~locs:true ""
      ; "dir/inner_dir/deep.md", Parse.of_string ~locs:true "deep ^deep1\n"
      ]
    ~other_files:
      [ "Figure1.jpg"
      ; "Figure1^2.jpg"
      ; "image.png"
      ; "empty_video.mp4"
      ; "unsupported_text_file.txt"
      ; "a.joiwduvqneoi"
      ; "Something"
      ; "Note 1"
      ]
    ()
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
       | Some (headings, `Heading) -> Some (Hash_path headings)
       | Some (block_id, `Block_ref) ->
         (match block_id with
          | [ block_id ] -> Some (Caret_id block_id)
          | _ -> invalid_arg "should only exists one block id"))
  }
;;

let resolve_and_print
      ?(curr_file : string = "Note 1.md")
      (cases : (string * Link_ref.t) list)
  =
  let cols =
    [ Ascii_table.Column.create "name" (fun (name, _) -> name)
    ; Ascii_table.Column.create "input" (fun (_, lr) ->
        Link_ref.sexp_of_t lr |> Sexp.to_string_hum)
    ; Ascii_table.Column.create "result" (fun (_, lr) ->
        match Index.resolve test_index curr_file lr with
        | Ok target -> Index.sexp_of_target target |> Sexp.to_string_hum
        | Error Missing_path -> "Missing_path"
        | Error (Missing_anchor path) -> sprintf "(Missing_anchor %s)" path)
    ]
  in
  print_string (Ascii_table.to_string_noattr cols cases ~limit_width_to:200)
;;

(* Shorthand aliases *)
let t s = Some s
let h hs = Some (hs, `Heading)
let b id = Some ([ id ], `Block_ref)

let%expect_test "path resolution" =
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
    ┌───────────────────────┬────────────────────────────────────────────────────────────┬───────────────────────────────────────────┐
    │ name                  │ input                                                      │ result                                    │
    ├───────────────────────┼────────────────────────────────────────────────────────────┼───────────────────────────────────────────┤
    │ exact match           │ ((target ("Note 1")) (fragment ()))                        │ (Note "Note 1.md")                        │
    │ exact with ext        │ ((target ("Note 1.md")) (fragment ()))                     │ (Note "Note 1.md")                        │
    │ spaces in name        │ ((target ("Three laws of motion")) (fragment ()))          │ (Note "Three laws of motion.md")          │
    │ spaces with ext       │ ((target ("Three laws of motion.md")) (fragment ()))       │ (Note "Three laws of motion.md")          │
    │ exact path            │ ((target (dir/indir_same_name)) (fragment ()))             │ (Note dir/indir_same_name.md)             │
    │ subsequence           │ ((target ("Note 2")) (fragment ()))                        │ (Note "Note 2.md")                        │
    │ deep subseq           │ ((target (inner_dir/note_in_inner_dir)) (fragment ()))     │ (Note dir/inner_dir/note_in_inner_dir.md) │
    │ partial subseq        │ ((target (dir/note_in_inner_dir)) (fragment ()))           │ (Note dir/inner_dir/note_in_inner_dir.md) │
    │ full path subseq      │ ((target (dir/inner_dir/note_in_inner_dir)) (fragment ())) │ (Note dir/inner_dir/note_in_inner_dir.md) │
    │ subseq from non-root  │ ((target (indir2)) (fragment ()))                          │ (Note dir/indir2.md)                      │
    │ asset png             │ ((target (image.png)) (fragment ()))                       │ (Asset image.png)                         │
    │ asset txt             │ ((target (unsupported_text_file.txt)) (fragment ()))       │ (Asset unsupported_text_file.txt)         │
    │ asset unknown ext     │ ((target (a.joiwduvqneoi)) (fragment ()))                  │ (Asset a.joiwduvqneoi)                    │
    │ asset video           │ ((target (empty_video.mp4)) (fragment ()))                 │ (Asset empty_video.mp4)                   │
    │ unresolved            │ ((target (nonexistent)) (fragment ()))                     │ Missing_path                              │
    │ bad path              │ ((target ("random/Note 1")) (fragment ()))                 │ Missing_path                              │
    │ root same name wins   │ ((target (indir_same_name)) (fragment ()))                 │ (Note indir_same_name.md)                 │
    │ dir same name exact   │ ((target (dir/indir_same_name)) (fragment ()))             │ (Note dir/indir_same_name.md)             │
    │ random dir unresolved │ ((target (random/note_in_inner_dir)) (fragment ()))        │ Missing_path                              │
    │ ().md                 │ ((target ("().md")) (fragment ()))                         │ (Note "().md")                            │
    │ ww                    │ ((target (ww)) (fragment ()))                              │ (Note ww.md)                              │
    └───────────────────────┴────────────────────────────────────────────────────────────┴───────────────────────────────────────────┘
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
    ┌────────────────────────┬──────────────────────────────────────────────┬──────────────────────────┐
    │ name                   │ input                                        │ result                   │
    ├────────────────────────┼──────────────────────────────────────────────┼──────────────────────────┤
    │ Figure1.jpg -> asset   │ ((target (Figure1.jpg)) (fragment ()))       │ (Asset Figure1.jpg)      │
    │ Figure1.jpg.md -> note │ ((target (Figure1.jpg.md)) (fragment ()))    │ (Note Figure1.jpg.md)    │
    │ Figure1.jpg.md.md      │ ((target (Figure1.jpg.md.md)) (fragment ())) │ (Note Figure1.jpg.md.md) │
    │ Figure1^2.jpg -> asset │ ((target (Figure1^2.jpg)) (fragment ()))     │ (Asset Figure1^2.jpg)    │
    │ Something -> note      │ ((target (Something)) (fragment ()))         │ (Note Something.md)      │
    │ Note 1 -> note         │ ((target ("Note 1")) (fragment ()))          │ (Note "Note 1.md")       │
    │ Figure1 -> note        │ ((target (Figure1)) (fragment ()))           │ (Note Figure1.md)        │
    └────────────────────────┴──────────────────────────────────────────────┴──────────────────────────┘
    |}]
;;

let%expect_test "heading resolution in note 2" =
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
    ┌───────────────────────┬──────────────────────────────────────────────────────────────────────────┬──────────────────────────────────────────────────────────────────────────────────┐
    │ name                  │ input                                                                    │ result                                                                           │
    ├───────────────────────┼──────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────┤
    │ single heading        │ ((target ("Note 2")) (fragment ((Hash_path ("Some level 2 title")))))    │ (Anchor (note_path "Note 2.md")                                                  │
    │                       │                                                                          │  (anchor                                                                         │
    │                       │                                                                          │   ((value                                                                        │
    │                       │                                                                          │     (Heading                                                                     │
    │                       │                                                                          │      ((text "Some level 2 title") (level 2) (slug some-level-2-title))))         │
    │                       │                                                                          │    (loc ((first_byte 0) (last_byte 20) (first_line (1 0)) (last_line (1 0))))))) │
    │ nested heading        │ ((target ("Note 2"))                                                     │ (Anchor (note_path "Note 2.md")                                                  │
    │                       │  (fragment ((Hash_path ("Some level 2 title" "Level 3 title")))))        │  (anchor                                                                         │
    │                       │                                                                          │   ((value (Heading ((text "Level 3 title") (level 3) (slug level-3-title))))     │
    │                       │                                                                          │    (loc                                                                          │
    │                       │                                                                          │     ((first_byte 30) (last_byte 46) (first_line (3 30)) (last_line (3 30)))))))  │
    │ nested skip level     │ ((target ("Note 2")) (fragment ((Hash_path ("Some level 2 title" L4))))) │ (Anchor (note_path "Note 2.md")                                                  │
    │                       │                                                                          │  (anchor                                                                         │
    │                       │                                                                          │   ((value (Heading ((text L4) (level 4) (slug l4))))                             │
    │                       │                                                                          │    (loc                                                                          │
    │                       │                                                                          │     ((first_byte 22) (last_byte 28) (first_line (2 22)) (last_line (2 22)))))))  │
    │ L3 directly           │ ((target ("Note 2")) (fragment ((Hash_path ("Level 3 title")))))         │ (Anchor (note_path "Note 2.md")                                                  │
    │                       │                                                                          │  (anchor                                                                         │
    │                       │                                                                          │   ((value (Heading ((text "Level 3 title") (level 3) (slug level-3-title))))     │
    │                       │                                                                          │    (loc                                                                          │
    │                       │                                                                          │     ((first_byte 30) (last_byte 46) (first_line (3 30)) (last_line (3 30)))))))  │
    │ L4 directly           │ ((target ("Note 2")) (fragment ((Hash_path (L4)))))                      │ (Anchor (note_path "Note 2.md")                                                  │
    │                       │                                                                          │  (anchor                                                                         │
    │                       │                                                                          │   ((value (Heading ((text L4) (level 4) (slug l4))))                             │
    │                       │                                                                          │    (loc                                                                          │
    │                       │                                                                          │     ((first_byte 22) (last_byte 28) (first_line (2 22)) (last_line (2 22)))))))  │
    │ random -> fallback    │ ((target ("Note 2")) (fragment ((Hash_path (random)))))                  │ (Missing_anchor Note 2.md)                                                       │
    │ random#L3 -> fallback │ ((target ("Note 2")) (fragment ((Hash_path (random "Level 3 title")))))  │ (Missing_anchor Note 2.md)                                                       │
    └───────────────────────┴──────────────────────────────────────────────────────────────────────────┴──────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "heading resolution in note 1" =
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
    ┌──────────────────────────────┬─────────────────────────────────────────────────────────────────────┬─────────────────────────────────────────────────────────────────────────────────┐
    │ name                         │ input                                                               │ result                                                                          │
    ├──────────────────────────────┼─────────────────────────────────────────────────────────────────────┼─────────────────────────────────────────────────────────────────────────────────┤
    │ L2                           │ ((target ("Note 1")) (fragment ((Hash_path (L2)))))                 │ (Anchor (note_path "Note 1.md")                                                 │
    │                              │                                                                     │  (anchor                                                                        │
    │                              │                                                                     │   ((value (Heading ((text L2) (level 2) (slug l2))))                            │
    │                              │                                                                     │    (loc                                                                         │
    │                              │                                                                     │     ((first_byte 59) (last_byte 63) (first_line (4 59)) (last_line (4 59))))))) │
    │ L2 L3                        │ ((target ("Note 1")) (fragment ((Hash_path (L2 L3)))))              │ (Anchor (note_path "Note 1.md")                                                 │
    │                              │                                                                     │  (anchor                                                                        │
    │                              │                                                                     │   ((value (Heading ((text L3) (level 3) (slug l3))))                            │
    │                              │                                                                     │    (loc                                                                         │
    │                              │                                                                     │     ((first_byte 65) (last_byte 70) (first_line (5 65)) (last_line (5 65))))))) │
    │ L2 L4                        │ ((target ("Note 1")) (fragment ((Hash_path (L2 L4)))))              │ (Anchor (note_path "Note 1.md")                                                 │
    │                              │                                                                     │  (anchor                                                                        │
    │                              │                                                                     │   ((value (Heading ((text L4) (level 4) (slug l4))))                            │
    │                              │                                                                     │    (loc                                                                         │
    │                              │                                                                     │     ((first_byte 72) (last_byte 78) (first_line (6 72)) (last_line (6 72))))))) │
    │ L2 L3 L4                     │ ((target ("Note 1")) (fragment ((Hash_path (L2 L3 L4)))))           │ (Anchor (note_path "Note 1.md")                                                 │
    │                              │                                                                     │  (anchor                                                                        │
    │                              │                                                                     │   ((value (Heading ((text L4) (level 4) (slug l4))))                            │
    │                              │                                                                     │    (loc                                                                         │
    │                              │                                                                     │     ((first_byte 72) (last_byte 78) (first_line (6 72)) (last_line (6 72))))))) │
    │ L2 L4 L3 -> fallback         │ ((target ("Note 1")) (fragment ((Hash_path (L2 L4 L3)))))           │ (Missing_anchor Note 1.md)                                                      │
    │ L2 L4 Another L3 -> fallback │ ((target ("Note 1")) (fragment ((Hash_path (L2 L4 "Another L3"))))) │ (Missing_anchor Note 1.md)                                                      │
    │ NoSuch -> fallback           │ ((target ("Note 1")) (fragment ((Hash_path (NoSuch)))))             │ (Missing_anchor Note 1.md)                                                      │
    │ heading unresolved file      │ ((target (nonexistent)) (fragment ((Hash_path (L2)))))              │ Missing_path                                                                    │
    └──────────────────────────────┴─────────────────────────────────────────────────────────────────────┴─────────────────────────────────────────────────────────────────────────────────┘
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
    ┌─────────────────────────────┬───────────────────────────────────────────────────────┬──────────────────────────────────────────────────────────────────────────────────┐
    │ name                        │ input                                                 │ result                                                                           │
    ├─────────────────────────────┼───────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────┤
    │ block found                 │ ((target ("Note 1")) (fragment ((Caret_id para1))))   │ (Anchor (note_path "Note 1.md")                                                  │
    │                             │                                                       │  (anchor                                                                         │
    │                             │                                                       │   ((value (Caret para1))                                                         │
    │                             │                                                       │    (loc                                                                          │
    │                             │                                                       │     ((first_byte 96) (last_byte 106) (first_line (9 96)) (last_line (9 96))))))) │
    │ block with hyphen           │ ((target ("Note 1")) (fragment ((Caret_id block-2)))) │ (Anchor (note_path "Note 1.md")                                                  │
    │                             │                                                       │  (anchor                                                                         │
    │                             │                                                       │   ((value (Caret block-2))                                                       │
    │                             │                                                       │    (loc                                                                          │
    │                             │                                                       │     ((first_byte 109) (last_byte 122) (first_line (11 109))                      │
    │                             │                                                       │      (last_line (11 109)))))))                                                   │
    │ block not found -> fallback │ ((target ("Note 1")) (fragment ((Caret_id nope))))    │ (Missing_anchor Note 1.md)                                                       │
    │ block in deep file          │ ((target (deep)) (fragment ((Caret_id deep1))))       │ (Anchor (note_path dir/inner_dir/deep.md)                                        │
    │                             │                                                       │  (anchor                                                                         │
    │                             │                                                       │   ((value (Caret deep1))                                                         │
    │                             │                                                       │    (loc ((first_byte 0) (last_byte 10) (first_line (1 0)) (last_line (1 0))))))) │
    │ block in unresolved file    │ ((target (nonexistent)) (fragment ((Caret_id x))))    │ Missing_path                                                                     │
    └─────────────────────────────┴───────────────────────────────────────────────────────┴──────────────────────────────────────────────────────────────────────────────────┘
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
    ┌─────────────────────────┬─────────────────────────────────────────────────┬──────────────────────────────────────────────────────────────────────────────────┐
    │ name                    │ input                                           │ result                                                                           │
    ├─────────────────────────┼─────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────┤
    │ [[]] -> curr file       │ ((target ()) (fragment ()))                     │ (Note "Note 1.md")                                                               │
    │ [[#L2]]                 │ ((target ()) (fragment ((Hash_path (L2)))))     │ (Anchor (note_path "Note 1.md")                                                  │
    │                         │                                                 │  (anchor                                                                         │
    │                         │                                                 │   ((value (Heading ((text L2) (level 2) (slug l2))))                             │
    │                         │                                                 │    (loc                                                                          │
    │                         │                                                 │     ((first_byte 59) (last_byte 63) (first_line (4 59)) (last_line (4 59)))))))  │
    │ [[#L2#L3]]              │ ((target ()) (fragment ((Hash_path (L2 L3)))))  │ (Anchor (note_path "Note 1.md")                                                  │
    │                         │                                                 │  (anchor                                                                         │
    │                         │                                                 │   ((value (Heading ((text L3) (level 3) (slug l3))))                             │
    │                         │                                                 │    (loc                                                                          │
    │                         │                                                 │     ((first_byte 65) (last_byte 70) (first_line (5 65)) (last_line (5 65)))))))  │
    │ [[#^para1]]             │ ((target ()) (fragment ((Caret_id para1))))     │ (Anchor (note_path "Note 1.md")                                                  │
    │                         │                                                 │  (anchor                                                                         │
    │                         │                                                 │   ((value (Caret para1))                                                         │
    │                         │                                                 │    (loc                                                                          │
    │                         │                                                 │     ((first_byte 96) (last_byte 106) (first_line (9 96)) (last_line (9 96))))))) │
    │ [[#NoSuch]] -> fallback │ ((target ()) (fragment ((Hash_path (NoSuch))))) │ (Missing_anchor Note 1.md)                                                       │
    │ [[#^nope]] -> fallback  │ ((target ()) (fragment ((Caret_id nope))))      │ (Missing_anchor Note 1.md)                                                       │
    │ [[#]] empty heading     │ ((target ()) (fragment ()))                     │ (Note "Note 1.md")                                                               │
    └─────────────────────────┴─────────────────────────────────────────────────┴──────────────────────────────────────────────────────────────────────────────────┘
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
    ┌───────────────────────────┬───────────────────────────────────────────────────────┬──────────────────────────────┐
    │ name                      │ input                                                 │ result                       │
    ├───────────────────────────┼───────────────────────────────────────────────────────┼──────────────────────────────┤
    │ Figure1.jpg#2 -> fallback │ ((target (Figure1.jpg)) (fragment ((Hash_path (2))))) │ (Missing_anchor Figure1.jpg) │
    │ Note 2## -> empty heading │ ((target ("Note 2")) (fragment ()))                   │ (Note "Note 2.md")           │
    └───────────────────────────┴───────────────────────────────────────────────────────┴──────────────────────────────┘
    |}]
;;
