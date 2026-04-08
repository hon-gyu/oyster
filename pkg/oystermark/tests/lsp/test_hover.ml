(** Spec: {!page-"feature-hover"}.
    Impl: {!Lsp_lib.Hover}. *)

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
  ; "empty.md", ""
  ; "note-c.md", "# Gamma\n\nSee [[empty]].\n"
  ]
;;

let index = Vault_helper.make_index files
let read_file rel_path = List.Assoc.find files ~equal:String.equal rel_path

let%expect_test "e2e: hover on wikilink to note" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  (* Line 2: "Link to [[note-a]] here." — cursor on "note-a" *)
  let response = hover s ~rel_path:"note-b.md" ~line:2 ~character:13 in
  let result = parse_hover_result response in
  print_s [%sexp (result : string option)];
  shutdown s;
  [%expect
    {|
    ( "*Path*:note-a.md\
     \n\
     \n# Alpha\
     \n\
     \n## Section One\
     \n\
     \nBody text ^block1\
     \n\
     \n## Section Two\
     \n\
     \nMore content.\
     \n")
    |}]
;;

let%expect_test "e2e: hover on heading fragment" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  (* Line 4: "See [[note-a#Section One]]." *)
  let response = hover s ~rel_path:"note-b.md" ~line:4 ~character:10 in
  let result = parse_hover_result response in
  print_s [%sexp (result : string option)];
  shutdown s;
  [%expect
    {|
    ( "*Path*:note-a.md\
     \n\
     \n## Section One\
     \n\
     \nBody text ^block1\
     \n")
    |}]
;;

let%expect_test "e2e: hover on block fragment" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  (* Line 6: "Also [[note-a#^block1]]." *)
  let response = hover s ~rel_path:"note-b.md" ~line:6 ~character:10 in
  let result = parse_hover_result response in
  print_s [%sexp (result : string option)];
  shutdown s;
  [%expect
    {|
    ( "*Path*:note-a.md\
     \n\
     \nBody text ^block1")
    |}]
;;

let%expect_test "e2e: hover on empty note" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-c.md";
  (* Line 2: "See [[empty]]." *)
  let response = hover s ~rel_path:"note-c.md" ~line:2 ~character:8 in
  let result = parse_hover_result response in
  print_s [%sexp (result : string option)];
  shutdown s;
  [%expect
    {|
    ( "*Path*:empty.md\
     \n\
     \n*(empty)*")
    |}]
;;

let%expect_test "e2e: hover on unresolved link" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  (* Line 10: "Unresolved [[missing-note]]." *)
  let response = hover s ~rel_path:"note-b.md" ~line:10 ~character:16 in
  let result = parse_hover_result response in
  print_s [%sexp (result : string option)];
  shutdown s;
  [%expect {| () |}]
;;

let%expect_test "e2e: hover cursor not on link" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  let response = hover s ~rel_path:"note-b.md" ~line:0 ~character:2 in
  let result = parse_hover_result response in
  print_s [%sexp (result : string option)];
  shutdown s;
  [%expect {| () |}]
;;

(* Trace
------------ *)

let%expect_test "trace: hover spans for heading fragment" =
  let t = Trace_collect.create () in
  Trace_collect.with_collect t (fun () ->
    let content = List.Assoc.find_exn files ~equal:String.equal "note-b.md" in
    let _result =
      Lsp_lib.Hover.hover
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
    hover 5us content_bytes=52 rel_path=note-b.md line=- character=-
    ├── byte_offset_of_position 1us line=- character=- offset=42
    ├── parse_doc 2us content_len=144
    ├── collect_links 3us num_links=5
    └── find_link_ref_at_offset 4us offset=42 found=true
    |}]
;;
