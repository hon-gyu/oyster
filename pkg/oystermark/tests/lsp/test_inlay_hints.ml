(** Spec: {!page-"feature-inlay-hints-link-direction"}.
    Impl: {!Lsp_lib.Inlay_hints} (the response), {!Lsp_lib.Link_direction}
    (the arrows).

    Reference counts used to be part of this response and are now lenses; see
    {!module-Test_lsp.Test_reference_lens}. *)

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
       [Body text]{#block1}\n\n\
       ## Section Two\n\n\
       More content.\n" )
  ; ( "note-b.md"
    , "# Beta\n\n\
       Link to [[note-a]] here.\n\n\
       See [[note-a#Section One]].\n\n\
       Also [[note-a#block1]].\n\n\
       Markdown [link](note-a).\n\n\
       Unresolved [[missing-note]].\n" )
  ; "subdir/nested.md", "# Nested\n\nLink to [[note-a]] from subdirectory.\n"
  ; "empty.md", ""
  ; "note-c.md", "# Gamma\n\nSee [[empty]].\n"
    (* A note that links into itself: the case the direction arrows exist for.
       See {!page-"feature-inlay-hints-link-direction"}. *)
  ; ( "moc.md"
    , "- [[#alpha-two]]\n\
       - [[#baz]]\n\n\
       # Alpha two\n\n\
       [Body]{#para}\n\n\
       # Baz\n\n\
       Back to [[#alpha-two]] and [[#para]].\n" )
  ]
;;

let index, docs = Lsp_lib.Find_references.For_test.make_vault files

(* Unit tests
   ========== *)

let show ?config ~rel_path ~range_start_line ~range_end_line () =
  let content = List.Assoc.find_exn files ~equal:String.equal rel_path in
  Lsp_lib.Inlay_hints.inlay_hints
    ?config
    ~index
    ~rel_path
    ~content
    ~range_start_line
    ~range_end_line
    ()
  |> List.iter ~f:(fun (h : Lsp_lib.Inlay_hints.hint) ->
    printf "(%d,%d) %s\n" h.line h.character h.label)
;;

let%expect_test "unit: arrows on a note that links into itself" =
  show ~rel_path:"moc.md" ~range_start_line:0 ~range_end_line:20 ();
  [%expect
    {|
    (0,16) ↓3
    (1,10) ↓6
    (9,22) ↑6
    (9,36) ↑4
    |}]
;;

(* Counts are lenses now, so a note pointed at from all over the vault but
   linking nowhere inside itself has no hints at all.
   See {!page-"feature-codelens-reference-counts"}. *)
let%expect_test "unit: inbound references produce no hints" =
  show ~rel_path:"note-a.md" ~range_start_line:0 ~range_end_line:20 ();
  [%expect {| |}]
;;

let%expect_test "unit: partial range" =
  show ~rel_path:"moc.md" ~range_start_line:0 ~range_end_line:1 ();
  [%expect {| (0,16) ↓3 |}]
;;

let%expect_test "unit: linkDirection off" =
  show
    ~config:{ Lsp_lib.Config.default with inlay_link_direction = false }
    ~rel_path:"moc.md"
    ~range_start_line:0
    ~range_end_line:20
    ();
  [%expect {| |}]
;;

(* Server tests
   ============ *)

(* A note nothing links inward gets an empty response, however many links
   point at it from elsewhere. *)
let%expect_test "server: a note with no intra-note links" =
  let s = start_server ~vault_root () in
  did_open s ~rel_path:"note-a.md";
  let result =
    Server.inlay_hint s ~rel_path:"note-a.md" ~start_line:0 ~end_line:20
    |> inlay_hint_positions
  in
  printf "%d hints\n" (List.length result);
  [%expect {| 0 hints |}]
;;

let%expect_test "server: arrows on an intra-note link list" =
  let s = start_server ~vault_root () in
  did_open s ~rel_path:"moc.md";
  Server.inlay_hint s ~rel_path:"moc.md" ~start_line:0 ~end_line:20
  |> inlay_hint_positions
  |> List.iter ~f:(fun (line, char, label) -> printf "(%d,%d) %s\n" line char label);
  [%expect
    {|
    (0,16) ↓3
    (1,10) ↓6
    (9,22) ↑6
    (9,36) ↑4
    |}]
;;

(* The switch reaches the server through the configuration, like every other
   setting.  See {!page-"feature-configuration"}. *)
let%expect_test "server: linkDirection off" =
  let s =
    start_server
      ~vault_root
      ~init_options:(`Assoc [ "inlayHints", `Assoc [ "linkDirection", `Bool false ] ])
      ()
  in
  did_open s ~rel_path:"moc.md";
  Server.inlay_hint s ~rel_path:"moc.md" ~start_line:0 ~end_line:20
  |> inlay_hint_positions
  |> List.iter ~f:(fun (line, char, label) -> printf "(%d,%d) %s\n" line char label);
  [%expect {| |}]
;;
