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
          Lsp_lib.go_to_definition
            ~index
            ~rel_path:"note-c.md"
            ~content
            ~line:2
            ~character:8
            ~read_file
        in
        ());
      print_s [%sexp (Trace_collect.span_names t : string list)];
      [%expect {|
        (byte_offset_of_position parse_doc collect_links find_link_ref_at_offset
         parse_target_doc find_heading_line_in_doc go_to_definition)
        |}];
      let go_span = Trace_collect.find_span t "go_to_definition" in
      (match go_span with
       | None -> print_endline "<no span>"
       | Some sp ->
         List.iter (Trace_collect.span_attrs sp) ~f:(fun (k, v) -> printf "%s=%s\n" k v));
      [%expect {|
        resolution=heading
        rel_path=note-c.md
        line=2
        character=8
        code.filepath=pkg/oystermark/lsp/lsp_lib.ml
        code.lineno=220
        |}]
    ;;
  end)
;;
