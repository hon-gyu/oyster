(** Spec: {!page-"feature-go-to-definition"}.
    Impl: {!Lsp_lib.Go_to_definition}. *)

open Core
open Lsp_helper

(** Vault root for integration (E2E) tests — points at the on-disk [data/]
       directory. *)
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

let%expect_test "e2e: wikilink to note" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  let response = definition s ~rel_path:"note-b.md" ~line:2 ~character:13 in
  let result = parse_definition_result vault_root response in
  print_s [%sexp (result : Lsp_lib.Go_to_definition.definition_result option)];
  shutdown s;
  [%expect {| (((path note-a.md) (line 0))) |}]
;;

let%expect_test "e2e: wikilink to heading" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  let response = definition s ~rel_path:"note-b.md" ~line:4 ~character:10 in
  let result = parse_definition_result vault_root response in
  print_s [%sexp (result : Lsp_lib.Go_to_definition.definition_result option)];
  shutdown s;
  [%expect {| (((path note-a.md) (line 2))) |}]
;;

let%expect_test "e2e: wikilink to block id" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  let response = definition s ~rel_path:"note-b.md" ~line:6 ~character:10 in
  let result = parse_definition_result vault_root response in
  print_s [%sexp (result : Lsp_lib.Go_to_definition.definition_result option)];
  shutdown s;
  [%expect {| (((path note-a.md) (line 4))) |}]
;;

let%expect_test "e2e: markdown link" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  let response = definition s ~rel_path:"note-b.md" ~line:8 ~character:18 in
  let result = parse_definition_result vault_root response in
  print_s [%sexp (result : Lsp_lib.Go_to_definition.definition_result option)];
  shutdown s;
  [%expect {| (((path note-a.md) (line 0))) |}]
;;

let%expect_test "e2e: unresolved link" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  let response = definition s ~rel_path:"note-b.md" ~line:10 ~character:16 in
  let result = parse_definition_result vault_root response in
  print_s [%sexp (result : Lsp_lib.Go_to_definition.definition_result option)];
  shutdown s;
  [%expect {| () |}]
;;

let%expect_test "e2e: cursor not on link" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  let response = definition s ~rel_path:"note-b.md" ~line:0 ~character:2 in
  let result = parse_definition_result vault_root response in
  print_s [%sexp (result : Lsp_lib.Go_to_definition.definition_result option)];
  shutdown s;
  [%expect {| () |}]
;;

let%expect_test "e2e: cross-directory link" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"subdir/nested.md";
  let response = definition s ~rel_path:"subdir/nested.md" ~line:2 ~character:13 in
  let result = parse_definition_result vault_root response in
  print_s [%sexp (result : Lsp_lib.Go_to_definition.definition_result option)];
  shutdown s;
  [%expect {| (((path note-a.md) (line 0))) |}]
;;

(* Trace
------------ *)

let%expect_test "trace: heading resolution spans" =
  let t = Trace_collect.create () in
  Trace_collect.with_collect t (fun () ->
    let content = List.Assoc.find_exn files ~equal:String.equal "note-b.md" in
    let _result =
      Lsp_lib.Go_to_definition.go_to_definition
        ~index
        ~rel_path:"note-b.md"
        ~content
        ~line:4
        ~character:8
        ~read_file
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
        go_to_definition 7us resolution=heading rel_path=note-b.md line=- character=-
        ├── byte_offset_of_position 1us line=- character=- offset=42
        ├── parse_doc 2us content_len=144
        ├── collect_links 3us num_links=5
        ├── find_link_ref_at_offset 4us offset=42 found=true
        ├── parse_doc 5us content_len=74
        └── find_heading_line_in_doc 6us result_line=2 slug=section-one
        |}]
;;
