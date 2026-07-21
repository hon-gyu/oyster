(** Spec: {!page-"feature-rename"}.
    Impl: {!Lsp_lib.Rename}. *)

open Core
open Lsp_helper

let files =
  [ "target.md", "# Old Heading\n\n{#old-id}\n> Paragraph\n"
  ; ( "links.md"
    , "[[target#Old Heading|alias]]\n\
       ![[target#Old Heading]]\n\
       [label](target#Old%20Heading)\n\
       [[target#old-id|anchor]]\n\
       [[target|note]]\n" )
  ]
;;

let index, docs = Lsp_lib.Find_references.For_test.make_vault files
let read_file path = List.Assoc.find files ~equal:String.equal path

let show edits =
  List.iter edits ~f:(fun (e : Lsp_lib.Rename.edit) ->
    printf "%s [%d-%d] -> %s\n" e.rel_path e.first_byte e.last_byte e.new_text)
;;

let%expect_test "rename heading from its definition updates wikilinks and markdown link" =
  let content = Option.value_exn (read_file "target.md") in
  Lsp_lib.Rename.For_test.rename
    ~index
    ~docs
    ~read_file
    ~rel_path:"target.md"
    ~content
    ~line:0
    ~character:3
    ~new_name:"New Heading"
    ()
  |> show;
  [%expect
    {|
    links.md [9-22] -> New Heading
    links.md [39-53] -> New Heading
    links.md [68-89] -> New%20Heading
    target.md [2-13] -> New Heading
    |}]
;;

let%expect_test "rename explicit anchor from a referring link" =
  let content = Option.value_exn (read_file "links.md") in
  Lsp_lib.Rename.For_test.rename
    ~index
    ~docs
    ~read_file
    ~rel_path:"links.md"
    ~content
    ~line:3
    ~character:12
    ~new_name:"new-id"
    ()
  |> show;
  [%expect
    {|
    links.md [92-100] -> new-id
    target.md [17-23] -> new-id
    |}]
;;

let%expect_test "path-only targets and invalid IDs are rejected" =
  let content = Option.value_exn (read_file "links.md") in
  let edits =
    Lsp_lib.Rename.For_test.rename
      ~index
      ~docs
      ~read_file
      ~rel_path:"links.md"
      ~content
      ~line:3
      ~character:12
      ~new_name:"not valid"
      ()
  in
  printf "%d edits\n" (List.length edits);
  [%expect {| 0 edits |}]
;;

let%expect_test "rename note preserves aliases and fragments" =
  let content = Option.value_exn (read_file "links.md") in
  Lsp_lib.Rename.For_test.rename
    ~index
    ~docs
    ~read_file
    ~rel_path:"links.md"
    ~content
    ~line:4
    ~character:3
    ~new_name:"renamed note"
    ()
  |> show;
  [%expect
    {|
    links.md [2-8] -> renamed note
    links.md [32-38] -> renamed note
    links.md [61-67] -> renamed%20note
    links.md [85-91] -> renamed note
    links.md [110-116] -> renamed note
    |}]
;;

let%expect_test "server: note rename includes text edits and a file operation" =
  let vault_root = Filename.concat (Core_unix.getcwd ()) "data" in
  let s = start_server ~vault_root in
  did_open s ~rel_path:"note-b.md";
  Server.rename s ~rel_path:"note-b.md" ~line:2 ~character:13 ~new_name:"renamed"
  |> document_change_kinds
  |> List.iter ~f:print_endline;
  [%expect
    {|
    text-edits
    text-edits
    rename
    |}]
;;
