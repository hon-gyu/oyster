(** Spec: {!page-"feature-codeaction-create-unresolved-link"}.
    Impl: {!Lsp_lib.Create_unresolved_note}. *)

open Core
open Linol_lsp.Lsp.Types
open Lsp_helper

let index, _docs =
  Lsp_lib.Find_references.For_test.make_vault [ "existing.md", "# Existing\n" ]
;;

let show content first_byte last_byte =
  let action =
    Lsp_lib.Create_unresolved_note.For_test.action_at_range
      ~index
      ~rel_path:"source.md"
      ~content
      ~first_byte
      ~last_byte
  in
  print_s [%sexp (action : Lsp_lib.Create_unresolved_note.action option)]
;;

let%expect_test "unresolved wikilink creates a Markdown note" =
  show "See [[new-note]]." 4 15;
  [%expect {| (((rel_path new-note.md) (title new-note))) |}]
;;

let%expect_test "nested Markdown target preserves its path" =
  show "See [new](folder/New%20Note)." 4 28;
  [%expect {| (((rel_path "folder/New Note.md") (title "New Note"))) |}]
;;

let%expect_test "resolved note has no action" =
  show "See [[existing]]." 4 15;
  [%expect {| () |}]
;;

let%expect_test "fragments, embeds, images, and traversal have no action" =
  List.iter
    [ "[[missing#Heading]]"; "![[missing]]"; "![alt](missing.png)"; "[[../outside]]" ]
    ~f:(fun content -> show content 0 (String.length content));
  [%expect
    {|
    ()
    ()
    ()
    ()
    |}]
;;

let%expect_test "server: quick fix creates and initializes the note" =
  let vault_root = Filename.concat (Core_unix.getcwd ()) "data" in
  let s = start_server ~vault_root in
  did_open s ~rel_path:"note-b.md";
  Server.code_action
    s
    ~rel_path:"note-b.md"
    ~start_line:10
    ~start_character:11
    ~end_line:10
    ~end_character:27
    ()
  |> List.iter ~f:(fun (action : CodeAction.t) ->
    print_endline action.title;
    let edit = Option.value_exn action.edit in
    List.iter (document_change_kinds edit) ~f:print_endline;
    List.iter (inserted_texts edit) ~f:(fun text -> printf "text: %S\n" text));
  [%expect
    {|
    Create note "missing-note.md"
    create
    text-edits
    text: "# missing-note\n"
    |}]
;;
