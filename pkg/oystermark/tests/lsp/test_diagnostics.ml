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
  [%expect
    {| ((first_byte 4) (last_byte 19) (message "unresolved image: missing.png")) |}]
;;

let%expect_test "note embed unresolved" =
  show ~rel_path:"note-b.md" ~content:"see ![[missing-note]] here";
  [%expect
    {| ((first_byte 4) (last_byte 20) (message "unresolved embed: missing-note")) |}]
;;

let%expect_test "markdown image unresolved" =
  show ~rel_path:"note-b.md" ~content:"see ![alt](missing.png) here";
  [%expect
    {| ((first_byte 4) (last_byte 22) (message "unresolved image: missing.png")) |}]
;;

(* Server
------------ *)

let%expect_test "server: unresolved link produces diagnostic on didOpen" =
  let s = start_server ~vault_root () in
  open_doc s ~rel_path:"note-b.md"
  |> diagnostic_positions
  |> List.iter ~f:(fun (msg, line, char) -> printf "%d:%d %s\n" line char msg);
  [%expect {| 10:11 unresolved link: missing-note |}]
;;

let%expect_test "server: didChange republishes diagnostics" =
  let s = start_server ~vault_root () in
  (* didOpen: no unresolved links *)
  let on_open = open_doc s ~rel_path:"subdir/nested.md" in
  printf "after open: %d diagnostics\n" (List.length on_open);
  (* didChange: add an unresolved link *)
  did_change
    s
    ~rel_path:"subdir/nested.md"
    ~text:"# Nested\n\nLink to [[non-exist]] now.\n"
  |> diagnostic_positions
  |> List.iter ~f:(fun (msg, line, char) ->
    printf "after change: %d:%d %s\n" line char msg);
  [%expect
    {|
    after open: 0 diagnostics
    after change: 2:8 unresolved link: non-exist
    |}]
;;

let%expect_test "server: resolved links produce no diagnostics" =
  let s = start_server ~vault_root () in
  let diags = open_doc s ~rel_path:"subdir/nested.md" in
  printf "%d diagnostics\n" (List.length diags);
  [%expect {| 0 diagnostics |}]
;;

(* Duplicate anchor ids
---------------------- *)

(* An id written twice is ambiguous, and every occurrence is reported so both
   sites are visible.  See {!page-"feature-diagnostics".duplicate_ids}. *)
let%expect_test "an id written twice is reported at both sites" =
  show
    ~rel_path:"note-b.md"
    ~content:"{#twice}\nFirst block.\n\n{#twice}\nSecond block.\n";
  [%expect
    {|
    ((first_byte 0) (last_byte 20) (message "duplicate anchor id: twice"))
    ((first_byte 23) (last_byte 44) (message "duplicate anchor id: twice"))
    |}]
;;

(* A heading whose id is written rather than derived is one anchor, reachable
   two ways.  It reaches the collection twice — as the heading's slug, which
   the parser resolves from the attribute, and as the attribute line — and
   that is not a collision. *)
let%expect_test "an explicit id on a heading is not a duplicate of itself" =
  show ~rel_path:"note-b.md" ~content:"{#intro}\n## Overview\n\nBody.\n";
  [%expect {| |}]
;;

(* The cross-kind collision the check exists for still fires: a derived slug
   and an unrelated hand-written id that happen to be the same string. *)
let%expect_test "a derived slug colliding with a hand-written id is reported" =
  show ~rel_path:"note-b.md" ~content:"## Overview\n\n{#overview}\nAn unrelated block.\n";
  [%expect
    {|
    ((first_byte 0) (last_byte 10) (message "duplicate anchor id: overview"))
    ((first_byte 13) (last_byte 43) (message "duplicate anchor id: overview"))
    |}]
;;
