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
let read_file rel_path = List.Assoc.find files ~equal:String.equal rel_path

(* Diagnostics are computed in-process via [Lsp_lib.Diagnostics.compute].
   The server publishes them as [textDocument/publishDiagnostics]
   notifications, which are push-based — no request/response E2E test
   is applicable here. *)
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
  print_s [%sexp (Trace_collect.span_names t : string list)];
  [%expect {| (parse_doc collect_links diagnostics.compute) |}]
;;

let%expect_test "trace: num_diagnostics attribute" =
  let t = Trace_collect.create () in
  Trace_collect.with_collect t (fun () ->
    let _result =
      Lsp_lib.Diagnostics.compute
        ~index
        ~rel_path:"note-b.md"
        ~content:"[[missing-a]] and [[missing-b]]"
        ()
    in
    ());
  let sp = Trace_collect.find_span t "diagnostics.compute" in
  let n = Option.bind sp ~f:(fun s -> Trace_collect.span_attr s "num_diagnostics") in
  print_s [%sexp (n : string option)];
  [%expect {| (2) |}]
;;
