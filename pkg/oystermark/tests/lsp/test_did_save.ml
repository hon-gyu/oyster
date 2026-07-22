(** Spec: {!page-"feature-document-sync"}.
    Impl: {!Lsp_lib.Server} ([did_save]).

    Tests the save-triggered refresh contract: when a sibling file
    appears on disk and a [didSave] fires, every open document's
    diagnostics are recomputed against the freshly rebuilt vault.
    This is what makes a previously-unresolved [[[brand-new]]] link in
    an {i already-open} buffer lose its warning after the user creates
    [brand-new.md] in a different tab and saves it. *)

open Core
open Lsp_helper

let%expect_test "didSave refreshes diagnostics in other open docs" =
  with_tmp_vault
    ~files:[ "a.md", "# A\n\nLink: [[brand-new]]\n" ]
    (fun vault_root ->
       let s = start_server ~vault_root in
       (* Initial diagnostics: one unresolved-link warning in a.md. *)
       let initial = open_doc s ~rel_path:"a.md" in
       printf "initial: %d diagnostic(s)\n" (List.length initial);
       (* Create [brand-new.md] on disk (simulates the user creating the
          file in another tab), then fire didSave to trigger refresh. *)
       Out_channel.write_all
         (Filename.concat vault_root "brand-new.md")
         ~data:"# Brand new\n";
       Server.did_save s
       |> List.iter ~f:(fun (rel_path, diags) ->
         printf "after save: %s has %d diagnostic(s)\n" rel_path (List.length diags)));
  [%expect
    {|
    initial: 1 diagnostic(s)
    after save: a.md has 0 diagnostic(s)
    |}]
;;

(* Every open buffer is refreshed, not just the saved one — that is the
   whole point of tracking [open_docs]. *)
let%expect_test "didSave refreshes every open document" =
  with_tmp_vault
    ~files:
      [ "a.md", "# A\n\nLink: [[brand-new]]\n"
      ; "b.md", "# B\n\nAlso: [[brand-new]] and [[still-missing]]\n"
      ]
    (fun vault_root ->
       let s = start_server ~vault_root in
       did_open s ~rel_path:"a.md";
       did_open s ~rel_path:"b.md";
       Out_channel.write_all
         (Filename.concat vault_root "brand-new.md")
         ~data:"# Brand new\n";
       Server.did_save s
       |> List.iter ~f:(fun (rel_path, diags) ->
         printf "%s:\n" rel_path;
         diagnostic_positions diags
         |> List.iter ~f:(fun (msg, line, char) -> printf "  %d:%d %s\n" line char msg)));
  [%expect
    {|
    a.md:
    b.md:
      2:24 unresolved link: still-missing
    |}]
;;

(* A closed document drops out of the refresh set. *)
let%expect_test "didClose stops refreshing a document" =
  with_tmp_vault
    ~files:[ "a.md", "# A\n\nLink: [[missing]]\n"; "b.md", "# B\n" ]
    (fun vault_root ->
       let s = start_server ~vault_root in
       did_open s ~rel_path:"a.md";
       did_open s ~rel_path:"b.md";
       Server.did_close s ~rel_path:"a.md";
       Server.did_save s
       |> List.iter ~f:(fun (rel_path, _) -> printf "refreshed: %s\n" rel_path));
  [%expect {| refreshed: b.md |}]
;;
