open Core
open Lsp_helper

(* Fixtures
       ================ *)

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
(* Integration (E2E)
       ------------------ *)

let%expect_test "e2e: hover on wikilink to note" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  (* Line 2: "Link to [[note-a]] here." — cursor on "note-a" *)
  let result = hover s ~rel_path:"note-b.md" ~line:2 ~character:13 in
  print_endline (pp_hover_result result);
  shutdown s;
  [%expect
    {|
        # Alpha

        ## Section One

        Body text ^block1

        ## Section Two

        More content.
        |}]
;;

let%expect_test "e2e: hover on heading fragment" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  (* Line 4: "See [[note-a#Section One]]." *)
  let result = hover s ~rel_path:"note-b.md" ~line:4 ~character:10 in
  print_endline (pp_hover_result result);
  shutdown s;
  [%expect
    {|
        ## Section One

        Body text ^block1
        |}]
;;

let%expect_test "e2e: hover on block fragment" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  (* Line 6: "Also [[note-a#^block1]]." *)
  let result = hover s ~rel_path:"note-b.md" ~line:6 ~character:10 in
  print_endline (pp_hover_result result);
  shutdown s;
  [%expect {| Body text ^block1 |}]
;;

let%expect_test "e2e: hover on unresolved link" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  (* Line 10: "Unresolved [[missing-note]]." *)
  let result = hover s ~rel_path:"note-b.md" ~line:10 ~character:16 in
  print_endline (pp_hover_result result);
  shutdown s;
  [%expect {| null |}]
;;

let%expect_test "e2e: hover cursor not on link" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  let result = hover s ~rel_path:"note-b.md" ~line:0 ~character:2 in
  print_endline (pp_hover_result result);
  shutdown s;
  [%expect {| null |}]
;;

(* Trace-based
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
  print_s [%sexp (Trace_collect.span_names t : string list)];
  [%expect
    {|
        (byte_offset_of_position parse_doc collect_links find_link_ref_at_offset
         hover)
        |}]
;;

(* In-process result
       ------------------ *)

let%expect_test "in-process: hover on note link returns full content" =
  let content = List.Assoc.find_exn files ~equal:String.equal "note-b.md" in
  match
    Lsp_lib.Hover.hover
      ~index
      ~rel_path:"note-b.md"
      ~content
      ~line:2
      ~character:13
      ~read_file
      ()
  with
  | None -> print_endline "<none>"
  | Some (text, _, _) ->
    print_string text;
    [%expect
      {|
        # Alpha

        ## Section One

        Body text ^block1

        ## Section Two

        More content.
        |}]
;;

let%expect_test "in-process: hover on heading fragment returns section" =
  let content = List.Assoc.find_exn files ~equal:String.equal "note-b.md" in
  match
    Lsp_lib.Hover.hover
      ~index
      ~rel_path:"note-b.md"
      ~content
      ~line:4
      ~character:8
      ~read_file
      ()
  with
  | None -> print_endline "<none>"
  | Some (text, _, _) ->
    print_string text;
    [%expect
      {|
        ## Section One

        Body text ^block1
        |}]
;;

let%expect_test "in-process: hover on block fragment returns paragraph" =
  let content = List.Assoc.find_exn files ~equal:String.equal "note-b.md" in
  match
    Lsp_lib.Hover.hover
      ~index
      ~rel_path:"note-b.md"
      ~content
      ~line:6
      ~character:8
      ~read_file
      ()
  with
  | None -> print_endline "<none>"
  | Some (text, _, _) ->
    print_string text;
    [%expect {| Body text ^block1 |}]
;;

let%expect_test "in-process: hover on unresolved returns none" =
  let content = List.Assoc.find_exn files ~equal:String.equal "note-b.md" in
  match
    Lsp_lib.Hover.hover
      ~index
      ~rel_path:"note-b.md"
      ~content
      ~line:10
      ~character:16
      ~read_file
      ()
  with
  | None -> print_endline "<none>"
  | Some (text, _, _) ->
    print_string text;
    [%expect {| <none> |}]
;;
