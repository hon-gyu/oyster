(** Spec: {!page-"feature-document-sync"}.
    Impl: [pkg/oystermark/lsp/main.ml] ([on_notif_doc_did_save]).

    Tests the save-triggered refresh contract: when a sibling file
    appears on disk and a [didSave] fires, every open document's
    diagnostics are republished against the freshly rebuilt vault.
    This is what makes a previously-unresolved [[[brand-new]]] link in
    an {i already-open} buffer lose its warning after the user creates
    [brand-new.md] in a different tab and saves it. *)

open Core
open Lsp_helper

(* Per-test fresh vault
   ===================== *)

let with_tmp_vault ~(files : (string * string) list) (f : string -> unit) : unit =
  let dir = Core_unix.mkdtemp "/tmp/oystermark-lsp-test-" in
  List.iter files ~f:(fun (rel, content) ->
    let full = Filename.concat dir rel in
    Core_unix.mkdir_p (Filename.dirname full);
    Out_channel.write_all full ~data:content);
  Exn.protect
    ~f:(fun () -> f dir)
    ~finally:(fun () ->
      let (_ : Core_unix.Exit_or_signal.t) =
        Core_unix.system (sprintf "rm -rf %s" (Filename.quote dir))
      in
      ())
;;

let%expect_test "didSave refreshes diagnostics in other open docs" =
  with_tmp_vault
    ~files:[ "a.md", "# A\n\nLink: [[brand-new]]\n" ]
    (fun vault_root ->
       let s = start_server ~vault_root in
       initialize s;
       did_open s ~rel_path:"a.md";
       (* Initial diagnostics: one unresolved-link warning in a.md. *)
       let initial = read_notification s.ic ~method_:"textDocument/publishDiagnostics" in
       let initial_diags = parse_diagnostics_notification initial in
       printf "initial: %d diagnostic(s)\n" (List.length initial_diags);
       (* Create [brand-new.md] on disk (simulates the user creating the
          file in another tab), then fire didSave to trigger refresh. *)
       Out_channel.write_all
         (Filename.concat vault_root "brand-new.md")
         ~data:"# Brand new\n";
       did_save s ~rel_path:"a.md";
       (* Expect a fresh publishDiagnostics for a.md with zero entries. *)
       (match
          try_read_notification
            s.ic
            ~method_:"textDocument/publishDiagnostics"
            ~timeout_ms:1000
        with
        | None -> printf "after save: NO notification received\n"
        | Some notif ->
          let diags = parse_diagnostics_notification notif in
          printf "after save: %d diagnostic(s)\n" (List.length diags));
       shutdown s);
  [%expect
    {|
    initial: 1 diagnostic(s)
    after save: 0 diagnostic(s)
    |}]
;;
