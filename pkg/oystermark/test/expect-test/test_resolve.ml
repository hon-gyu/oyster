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

let%expect_test "resolve_file" =
  let cases : (string * Link_ref.t) list =
    [ "exact match", { target = Some "Note1"; fragment = None }
    ; "exact with ext", { target = Some "Note1.md"; fragment = None }
    ; "exact path", { target = Some "dir/Note2"; fragment = None }
    ; "subsequence", { target = Some "Note2"; fragment = None }
    ; "deep subsequence", { target = Some "inner_dir/deep"; fragment = None }
    ; "partial subseq", { target = Some "dir/deep"; fragment = None }
    ; "asset", { target = Some "image.png"; fragment = None }
    ; "unresolved", { target = Some "nonexistent"; fragment = None }
    ; "bad path", { target = Some "random/Note1"; fragment = None }
    ; "exact root same name", { target = Some "indir_same_name"; fragment = None }
    ; "exact dir same name", { target = Some "dir/indir_same_name"; fragment = None }
    ]
  in
  let cols =
    [ Ascii_table.Column.create "name" fst
    ; Ascii_table.Column.create "result" (fun (name, link_ref) ->
        Link_ref.sexp_of_t link_ref |> Sexp.to_string_hum)
    ]
  in
  print_string (Ascii_table.to_string_noattr cols cases);
  [%expect
    {|
    ┌──────────────────────┬────────────────────────────────────────────────┐
    │ name                 │ result                                         │
    ├──────────────────────┼────────────────────────────────────────────────┤
    │ exact match          │ ((target (Note1)) (fragment ()))               │
    │ exact with ext       │ ((target (Note1.md)) (fragment ()))            │
    │ exact path           │ ((target (dir/Note2)) (fragment ()))           │
    │ subsequence          │ ((target (Note2)) (fragment ()))               │
    │ deep subsequence     │ ((target (inner_dir/deep)) (fragment ()))      │
    │ partial subseq       │ ((target (dir/deep)) (fragment ()))            │
    │ asset                │ ((target (image.png)) (fragment ()))           │
    │ unresolved           │ ((target (nonexistent)) (fragment ()))         │
    │ bad path             │ ((target (random/Note1)) (fragment ()))        │
    │ exact root same name │ ((target (indir_same_name)) (fragment ()))     │
    │ exact dir same name  │ ((target (dir/indir_same_name)) (fragment ())) │
    └──────────────────────┴────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "resolve_headings" =
  let cases : (string * Link_ref.t) list =
    [ "single heading", { target = Some "Note1"; fragment = Some (Heading [ "L2" ]) }
    ; "nested valid", { target = Some "Note1"; fragment = Some (Heading [ "L2"; "L3" ]) }
    ; "skip level", { target = Some "Note1"; fragment = Some (Heading [ "L2"; "L4" ]) }
    ; ( "three levels"
      , { target = Some "Note1"; fragment = Some (Heading [ "L2"; "L3"; "L4" ]) } )
    ; ( "invalid hierarchy"
      , { target = Some "Note1"; fragment = Some (Heading [ "L2"; "L4"; "L3" ]) } )
    ; ( "invalid hierarchy 2"
      , { target = Some "Note1"; fragment = Some (Heading [ "L2"; "L4"; "Another L3" ]) }
      )
    ; ( "nonexistent heading"
      , { target = Some "Note1"; fragment = Some (Heading [ "NoSuch" ]) } )
    ; ( "heading in other file"
      , { target = Some "Note2"; fragment = Some (Heading [ "Intro" ]) } )
    ]
  in
  let cols =
    [ Ascii_table.Column.create "name" fst
    ; Ascii_table.Column.create "result" (fun (name, link_ref) ->
        Link_ref.sexp_of_t link_ref |> Sexp.to_string_hum)
    ]
  in
  print_string (Ascii_table.to_string_noattr cols cases);
  [%expect
    {|
    ┌───────────────────────┬────────────────────────────────────────────────────────────────┐
    │ name                  │ result                                                         │
    ├───────────────────────┼────────────────────────────────────────────────────────────────┤
    │ single heading        │ ((target (Note1)) (fragment ((Heading (L2)))))                 │
    │ nested valid          │ ((target (Note1)) (fragment ((Heading (L2 L3)))))              │
    │ skip level            │ ((target (Note1)) (fragment ((Heading (L2 L4)))))              │
    │ three levels          │ ((target (Note1)) (fragment ((Heading (L2 L3 L4)))))           │
    │ invalid hierarchy     │ ((target (Note1)) (fragment ((Heading (L2 L4 L3)))))           │
    │ invalid hierarchy 2   │ ((target (Note1)) (fragment ((Heading (L2 L4 "Another L3"))))) │
    │ nonexistent heading   │ ((target (Note1)) (fragment ((Heading (NoSuch)))))             │
    │ heading in other file │ ((target (Note2)) (fragment ((Heading (Intro)))))              │
    └───────────────────────┴────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "resolve_blocks" =
  let cases : (string * Link_ref.t) list =
    [ "block found", { target = Some "Note1"; fragment = Some (Block_ref "para1") }
    ; ( "block with hyphen"
      , { target = Some "Note1"; fragment = Some (Block_ref "block-2") } )
    ; "block not found", { target = Some "Note1"; fragment = Some (Block_ref "nope") }
    ; "block in deep file", { target = Some "deep"; fragment = Some (Block_ref "deep1") }
    ]
  in
  let cols =
    [ Ascii_table.Column.create "name" fst
    ; Ascii_table.Column.create "result" (fun (name, link_ref) ->
        Link_ref.sexp_of_t link_ref |> Sexp.to_string_hum)
    ]
  in
  print_string (Ascii_table.to_string_noattr cols cases);
  [%expect
    {|
    ┌────────────────────┬─────────────────────────────────────────────────────┐
    │ name               │ result                                              │
    ├────────────────────┼─────────────────────────────────────────────────────┤
    │ block found        │ ((target (Note1)) (fragment ((Block_ref para1))))   │
    │ block with hyphen  │ ((target (Note1)) (fragment ((Block_ref block-2)))) │
    │ block not found    │ ((target (Note1)) (fragment ((Block_ref nope))))    │
    │ block in deep file │ ((target (deep)) (fragment ((Block_ref deep1))))    │
    └────────────────────┴─────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "resolve_self_references" =
  let cases : (string * Link_ref.t) list =
    [ "empty = current file", { target = None; fragment = None }
    ; "current heading", { target = None; fragment = Some (Heading [ "L2" ]) }
    ; ( "current nested heading"
      , { target = None; fragment = Some (Heading [ "L2"; "L3" ]) } )
    ; "current block", { target = None; fragment = Some (Block_ref "para1") }
    ; "current invalid heading", { target = None; fragment = Some (Heading [ "NoSuch" ]) }
    ; "current invalid block", { target = None; fragment = Some (Block_ref "nope") }
    ]
  in
  let cols =
    [ Ascii_table.Column.create "name" fst
    ; Ascii_table.Column.create "result" (fun (name, link_ref) ->
        Link_ref.sexp_of_t link_ref |> Sexp.to_string_hum)
    ]
  in
  print_string (Ascii_table.to_string_noattr cols cases);
  [%expect
    {|
    ┌─────────────────────────┬───────────────────────────────────────────────┐
    │ name                    │ result                                        │
    ├─────────────────────────┼───────────────────────────────────────────────┤
    │ empty = current file    │ ((target ()) (fragment ()))                   │
    │ current heading         │ ((target ()) (fragment ((Heading (L2)))))     │
    │ current nested heading  │ ((target ()) (fragment ((Heading (L2 L3)))))  │
    │ current block           │ ((target ()) (fragment ((Block_ref para1))))  │
    │ current invalid heading │ ((target ()) (fragment ((Heading (NoSuch))))) │
    │ current invalid block   │ ((target ()) (fragment ((Block_ref nope))))   │
    └─────────────────────────┴───────────────────────────────────────────────┘
    |}]
;;
