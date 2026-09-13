(** Trace coverage for {!Test_lsp.Test_go_to_definition}. *)

open Core

let%expect_test "trace: heading resolution spans" =
  let t = Trace_collect.create () in
  Trace_collect.with_collect t (fun () ->
    let content =
      List.Assoc.find_exn
        Test_lsp.Test_go_to_definition.files
        ~equal:String.equal
        "note-b.md"
    in
    let _result =
      Lsp_lib.Go_to_definition.go_to_definition
        ~index:Test_lsp.Test_go_to_definition.index
        ~rel_path:"note-b.md"
        ~content
        ~line:4
        ~character:8
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
    go_to_definition 5us resolution=anchor rel_path=note-b.md line=- character=-
    ├── byte_offset_of_position 1us line=- character=- offset=42
    ├── parse_doc 2us content_len=144
    ├── collect_links 3us num_links=5
    └── find_link_ref_at_offset 4us offset=42 found=true
    |}]
;;
