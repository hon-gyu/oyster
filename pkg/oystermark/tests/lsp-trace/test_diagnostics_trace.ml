(** Trace coverage for {!Test_lsp.Test_diagnostics}. *)

let%expect_test "trace: diagnostics spans" =
  let t = Trace_collect.create () in
  Trace_collect.with_collect t (fun () ->
    let _result =
      Lsp_lib.Diagnostics.compute
        ~index:Test_lsp.Test_diagnostics.index
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
