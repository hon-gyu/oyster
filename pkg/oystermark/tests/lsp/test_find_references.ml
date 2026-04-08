(** Spec: {!page-"feature-find-references"}.
    Impl: {!Lsp_lib.Find_references}. *)

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

let index = Vault_helper.make_index files
let read_file rel_path = List.Assoc.find files ~equal:String.equal rel_path

(* Unit tests
   ========== *)

let%expect_test "unit: references to note-a from wikilink in note-b" =
  let content = List.Assoc.find_exn files ~equal:String.equal "note-b.md" in
  let refs =
    Lsp_lib.Find_references.find_references
      ~index
      ~rel_path:"note-b.md"
      ~content
      ~line:2
      ~character:13
      ~read_file
      ()
  in
  List.iter refs ~f:(fun r ->
    printf "%s [%d-%d]\n" r.rel_path r.first_byte r.last_byte);
  [%expect
    {|
    note-b.md [16-25]
    note-b.md [38-59]
    note-b.md [68-85]
    note-b.md [98-111]
    subdir/nested.md [18-27]
    |}]
;;

let%expect_test "unit: references to heading" =
  let content = List.Assoc.find_exn files ~equal:String.equal "note-a.md" in
  let refs =
    Lsp_lib.Find_references.find_references
      ~index
      ~rel_path:"note-a.md"
      ~content
      ~line:2
      ~character:3
      ~read_file
      ()
  in
  List.iter refs ~f:(fun r ->
    printf "%s [%d-%d]\n" r.rel_path r.first_byte r.last_byte);
  [%expect {| note-b.md [38-59] |}]
;;

let%expect_test "unit: references to block id" =
  let content = List.Assoc.find_exn files ~equal:String.equal "note-a.md" in
  let refs =
    Lsp_lib.Find_references.find_references
      ~index
      ~rel_path:"note-a.md"
      ~content
      ~line:4
      ~character:5
      ~read_file
      ()
  in
  List.iter refs ~f:(fun r ->
    printf "%s [%d-%d]\n" r.rel_path r.first_byte r.last_byte);
  [%expect {| note-b.md [68-85] |}]
;;

let%expect_test "unit: cursor not on link, heading or block" =
  let refs =
    Lsp_lib.Find_references.find_references
      ~index
      ~rel_path:"note-a.md"
      ~content:"plain text"
      ~line:0
      ~character:3
      ~read_file
      ()
  in
  printf "%d refs\n" (List.length refs);
  [%expect {| 0 refs |}]
;;

let%expect_test "unit: unresolved link returns empty" =
  let content = List.Assoc.find_exn files ~equal:String.equal "note-b.md" in
  let refs =
    Lsp_lib.Find_references.find_references
      ~index
      ~rel_path:"note-b.md"
      ~content
      ~line:10
      ~character:15
      ~read_file
      ()
  in
  printf "%d refs\n" (List.length refs);
  [%expect {| 0 refs |}]
;;

(* E2E tests
   ========= *)

let%expect_test "e2e: references to note-a" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  let response = references s ~rel_path:"note-b.md" ~line:2 ~character:13 in
  let result = parse_references_result s.vault_root response in
  List.iter result ~f:(fun (path, line, char) -> printf "%s (%d,%d)\n" path line char);
  shutdown s;
  [%expect
    {|
    note-b.md (2,8)
    note-b.md (4,4)
    note-b.md (6,5)
    note-b.md (8,9)
    subdir/nested.md (2,8)
    |}]
;;

let%expect_test "e2e: no references for cursor on plain text" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-a.md";
  let response = references s ~rel_path:"note-a.md" ~line:0 ~character:0 in
  let result = parse_references_result s.vault_root response in
  printf "%d refs\n" (List.length result);
  shutdown s;
  [%expect {| 0 refs |}]
;;
