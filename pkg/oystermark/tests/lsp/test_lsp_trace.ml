(** Trace-based tests for LSP functions.

    Uses {!Trace_collect} to capture OTEL spans emitted by [lsp_lib]
    and assert on the span names and attributes. *)

open Core

let%test_module "go_to_definition trace" =
  (module struct
    let make_index (files : (string * string) list) : Oystermark.Vault.Index.t =
      let md_docs =
        List.filter_map files ~f:(fun (rel_path, content) ->
          if String.is_suffix rel_path ~suffix:".md"
          then (
            let doc = Oystermark.Parse.of_string content in
            Some (rel_path, doc))
          else None)
      in
      let other_files =
        List.filter_map files ~f:(fun (p, _) ->
          if not (String.is_suffix p ~suffix:".md") then Some p else None)
      in
      Oystermark.Vault.build_index ~md_docs ~other_files ~dirs:[]
    ;;

    let files =
      [ "note-a.md", "# Alpha\n\n## Section One\n\nBody text ^block1\n"
      ; "note-b.md", "# Beta\n\nLink to [[note-a]] here.\n"
      ; ( "note-c.md"
        , "# Gamma\n\nSee [[note-a#Section One]].\n\nAlso [[note-a#^block1]].\n" )
      ]
    ;;

    let index = make_index files
    let read_file rel_path = List.Assoc.find files ~equal:String.equal rel_path

    let%expect_test "heading resolution spans" =
      let t = Trace_collect.create () in
      Trace_collect.with_collect t (fun () ->
        let content = List.Assoc.find_exn files ~equal:String.equal "note-c.md" in
        let _result =
          Lsp_lib.Go_to_definition.go_to_definition
            ~index
            ~rel_path:"note-c.md"
            ~content
            ~line:2
            ~character:8
            ~read_file
        in
        ());
      print_s [%sexp (Trace_collect.span_names t : string list)];
      [%expect
        {|
        (byte_offset_of_position parse_doc collect_links find_link_ref_at_offset
         parse_doc find_heading_line_in_doc go_to_definition)
        |}];
      let open Trace_collect in
      let spans =
        Trace_collect.spans t
        |> Span_pipeline.normalize_duration
        |> Span_pipeline.scrub_attributes ~scrub:[ [ "line" ]; [ "character" ] ]
      in
      print_endline (Trace_collect.format spans);
      [%expect
        {|
        go_to_definition 7us resolution=heading rel_path=note-c.md line=- character=-
        ├── byte_offset_of_position 1us line=- character=- offset=17
        ├── parse_doc 2us content_len=63
        ├── collect_links 3us num_links=2
        ├── find_link_ref_at_offset 4us offset=17 found=true
        ├── parse_doc 5us content_len=43
        └── find_heading_line_in_doc 6us result_line=2 slug=section-one
        |}]
    ;;
  end)
;;
