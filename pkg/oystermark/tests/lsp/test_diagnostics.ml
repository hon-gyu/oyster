(** Spec: {!page-"feature-diagnostics"}.
    Impl: {!Lsp_lib.Diagnostics}. *)

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
  ]
;;

let index = Vault_helper.make_index files

let show ~(rel_path : string) ~(content : string) : unit =
  let diags = Lsp_lib.Diagnostics.compute ~index ~rel_path ~content () in
  List.iter diags ~f:(fun d -> print_s [%sexp (d : Lsp_lib.Diagnostics.diagnostic)])
;;

(* In-process result
------------------ *)

let%expect_test "resolved link: no diagnostic" =
  show ~rel_path:"note-b.md" ~content:"Link to [[note-a]] here.";
  [%expect {| |}]
;;

let%expect_test "unresolved link: diagnostic" =
  show ~rel_path:"note-b.md" ~content:"See [[nonexistent]] here.";
  [%expect {| ((first_byte 4) (last_byte 18) (message "unresolved link: nonexistent")) |}]
;;

let%expect_test "mixed resolved and unresolved" =
  show
    ~rel_path:"note-b.md"
    ~content:"[[note-a]] and [[missing]] and [[note-a#Section One]]";
  [%expect {| ((first_byte 15) (last_byte 25) (message "unresolved link: missing")) |}]
;;

let%expect_test "empty document" =
  show ~rel_path:"note-b.md" ~content:"";
  [%expect {| |}]
;;

let%expect_test "markdown link unresolved" =
  show ~rel_path:"note-b.md" ~content:"see [text](nowhere) here";
  [%expect {| ((first_byte 4) (last_byte 18) (message "unresolved link: nowhere")) |}]
;;

let%expect_test "external link skipped" =
  show ~rel_path:"note-b.md" ~content:"see [text](https://example.com) here";
  [%expect {| |}]
;;

let%expect_test "embed wikilink unresolved" =
  show ~rel_path:"note-b.md" ~content:"see ![[missing.png]] here";
  [%expect {| ((first_byte 4) (last_byte 19) (message "unresolved link: missing.png")) |}]
;;

(* Trace
------------ *)

let%expect_test "trace: diagnostics spans" =
  let t = Trace_collect.create () in
  Trace_collect.with_collect t (fun () ->
    let _result =
      Lsp_lib.Diagnostics.compute
        ~index
        ~rel_path:"note-b.md"
        ~content:"[[note-a]] and [[missing]]"
        ()
    in
    ());
  let open Trace_collect in
  let spans =
    Trace_collect.spans t
    |> Span_pipeline.normalize_duration
    |> Span_pipeline.scrub_attributes ~scrub:[ [ "line" ]; [ "character" ] ]
  in
  print_endline (Trace_collect.format spans);
  [%expect
    {|
    diagnostics.compute 3us num_diagnostics=1 rel_path=note-b.md
    ├── parse_doc 1us content_len=26
    └── collect_links 2us num_links=2
    |}]
;;

(* E2E
------------ *)

let%expect_test "e2e: unresolved link produces diagnostic on didOpen" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  (* The server publishes diagnostics as a notification after didOpen. *)
  let notif = read_notification s.ic ~method_:"textDocument/publishDiagnostics" in
  let diags = parse_diagnostics_notification notif in
  List.iter diags ~f:(fun (msg, line, char) -> printf "%d:%d %s\n" line char msg);
  shutdown s;
  [%expect {| 10:11 unresolved link: missing-note |}]
;;

let%expect_test "e2e: didChange republishes diagnostics" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"subdir/nested.md";
  (* didOpen: no unresolved links *)
  let notif = read_notification s.ic ~method_:"textDocument/publishDiagnostics" in
  let diags = parse_diagnostics_notification notif in
  printf "after open: %d diagnostics\n" (List.length diags);
  (* didChange: add an unresolved link *)
  did_change
    s
    ~rel_path:"subdir/nested.md"
    ~version:2
    ~text:"# Nested\n\nLink to [[non-exist]] now.\n";
  let notif = read_notification s.ic ~method_:"textDocument/publishDiagnostics" in
  let diags = parse_diagnostics_notification notif in
  List.iter diags ~f:(fun (msg, line, char) ->
    printf "after change: %d:%d %s\n" line char msg);
  shutdown s;
  [%expect
    {|
    after open: 0 diagnostics
    after change: 2:8 unresolved link: non-exist
    |}]
;;

let%expect_test "e2e: resolved links produce no diagnostics" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"subdir/nested.md";
  let notif = read_notification s.ic ~method_:"textDocument/publishDiagnostics" in
  let diags = parse_diagnostics_notification notif in
  printf "%d diagnostics\n" (List.length diags);
  shutdown s;
  [%expect {| 0 diagnostics |}]
;;
