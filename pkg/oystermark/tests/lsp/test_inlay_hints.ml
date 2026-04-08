(** Spec: {!page-"feature-inlay-hints"}.
    Impl: {!Lsp_lib.Inlay_hints}. *)

open Core
open Lsp_helper

let vault_root =
  let cwd = Core_unix.getcwd () in
  Filename.concat cwd "data"
;;

let files =
  [ ( "note-a.md"
    , "# Alpha\n\n\
       ## Section One\n\n\
       Body text ^block1\n\n\
       ## Section Two\n\n\
       More content.\n" )
  ; ( "note-b.md"
    , "# Beta\n\n\
       Link to [[note-a]] here.\n\n\
       See [[note-a#Section One]].\n\n\
       Also [[note-a#^block1]].\n\n\
       Markdown [link](note-a).\n\n\
       Unresolved [[missing-note]].\n" )
  ; "subdir/nested.md", "# Nested\n\nLink to [[note-a]] from subdirectory.\n"
  ; "empty.md", ""
  ; "note-c.md", "# Gamma\n\nSee [[empty]].\n"
  ]
;;

let _index, docs = Lsp_lib.Find_references.For_test.make_vault files

(* Unit tests
   ========== *)

let%expect_test "unit: hints for note-a (has incoming refs)" =
  let content = List.Assoc.find_exn files ~equal:String.equal "note-a.md" in
  let hints =
    Lsp_lib.Inlay_hints.inlay_hints
      ~docs
      ~rel_path:"note-a.md"
      ~content
      ~range_start_line:0
      ~range_end_line:20
      ()
  in
  List.iter hints ~f:(fun h ->
    printf "(%d,%d) %s\n" h.line h.character h.label);
  [%expect
    {|
    (0,0) 5 refs
    (2,14) 1 ref
    |}]
;;

let%expect_test "unit: hints for note-b (no incoming refs)" =
  let content = List.Assoc.find_exn files ~equal:String.equal "note-b.md" in
  let hints =
    Lsp_lib.Inlay_hints.inlay_hints
      ~docs
      ~rel_path:"note-b.md"
      ~content
      ~range_start_line:0
      ~range_end_line:20
      ()
  in
  printf "%d hints\n" (List.length hints);
  [%expect {| 0 hints |}]
;;

let%expect_test "unit: partial range" =
  let content = List.Assoc.find_exn files ~equal:String.equal "note-a.md" in
  let hints =
    Lsp_lib.Inlay_hints.inlay_hints
      ~docs
      ~rel_path:"note-a.md"
      ~content
      ~range_start_line:2
      ~range_end_line:5
      ()
  in
  List.iter hints ~f:(fun h ->
    printf "(%d,%d) %s\n" h.line h.character h.label);
  [%expect {| (2,14) 1 ref |}]
;;

(* E2E tests
   ========= *)

let%expect_test "e2e: inlay hints for note-a" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-a.md";
  let response = inlay_hint s ~rel_path:"note-a.md" ~start_line:0 ~end_line:20 in
  let result = parse_inlay_hint_result response in
  List.iter result ~f:(fun (line, char, label) -> printf "(%d,%d) %s\n" line char label);
  shutdown s;
  [%expect
    {|
    (0,0) 5 refs
    (2,14) 1 ref
    |}]
;;

let%expect_test "e2e: inlay hints for file with no refs" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  let response = inlay_hint s ~rel_path:"note-b.md" ~start_line:0 ~end_line:20 in
  let result = parse_inlay_hint_result response in
  printf "%d hints\n" (List.length result);
  shutdown s;
  [%expect {| 0 hints |}]
;;
