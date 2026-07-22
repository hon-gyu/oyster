(** Spec: {!page-"feature-rename"}.
    Impl: {!Lsp_lib.Rename}. *)

open Core
open Lsp_helper

let files =
  [ "target.md", "# Old Heading\n\n{#old-id}\n> Paragraph\n"
  ; ( "links.md"
    , "[[target#Old Heading|alias]]\n\
       ![[target#Old Heading]]\n\
       [label](target#Old%20Heading)\n\
       [[target#old-id|anchor]]\n\
       [[target|note]]\n" )
  ]
;;

let index, docs = Lsp_lib.Find_references.For_test.make_vault files
let read_file path = List.Assoc.find files ~equal:String.equal path

let show edits =
  List.iter edits ~f:(fun (e : Lsp_lib.Rename.edit) ->
    printf "%s [%d-%d] -> %s\n" e.rel_path e.first_byte e.last_byte e.new_text)
;;

(** The edits' {e effect}, per file. An edit can carry the right [new_text] and
    still span the wrong range, so the ranges are only really pinned down by
    applying them — see {!Lsp_helper.apply_edits}. *)
let show_applied edits =
  List.map edits ~f:(fun (e : Lsp_lib.Rename.edit) -> e.rel_path)
  |> List.dedup_and_sort ~compare:String.compare
  |> List.iter ~f:(fun rel_path ->
    let edits =
      List.filter edits ~f:(fun (e : Lsp_lib.Rename.edit) ->
        String.equal e.rel_path rel_path)
    in
    printf "--- %s\n%s" rel_path (apply_edits (Option.value_exn (read_file rel_path)) edits))
;;

let%expect_test "rename heading from its definition updates wikilinks and markdown link" =
  let content = Option.value_exn (read_file "target.md") in
  Lsp_lib.Rename.For_test.rename
    ~index
    ~docs
    ~read_file
    ~rel_path:"target.md"
    ~content
    ~line:0
    ~character:3
    ~new_name:"New Heading"
    ()
  |> fun edits ->
  show edits;
  show_applied edits;
  [%expect
    {|
    links.md [9-20] -> New Heading
    links.md [39-50] -> New Heading
    links.md [68-81] -> New%20Heading
    target.md [2-13] -> New Heading
    --- links.md
    [[target#New Heading|alias]]
    ![[target#New Heading]]
    [label](target#New%20Heading)
    [[target#old-id|anchor]]
    [[target|note]]
    --- target.md
    # New Heading

    {#old-id}
    > Paragraph
    |}]
;;

let%expect_test "rename explicit anchor from a referring link" =
  let content = Option.value_exn (read_file "links.md") in
  Lsp_lib.Rename.For_test.rename
    ~index
    ~docs
    ~read_file
    ~rel_path:"links.md"
    ~content
    ~line:3
    ~character:12
    ~new_name:"new-id"
    ()
  |> fun edits ->
  show edits;
  show_applied edits;
  [%expect
    {|
    links.md [92-98] -> new-id
    target.md [17-23] -> new-id
    --- links.md
    [[target#Old Heading|alias]]
    ![[target#Old Heading]]
    [label](target#Old%20Heading)
    [[target#new-id|anchor]]
    [[target|note]]
    --- target.md
    # Old Heading

    {#new-id}
    > Paragraph
    |}]
;;

let%expect_test "path-only targets and invalid IDs are rejected" =
  let content = Option.value_exn (read_file "links.md") in
  let edits =
    Lsp_lib.Rename.For_test.rename
      ~index
      ~docs
      ~read_file
      ~rel_path:"links.md"
      ~content
      ~line:3
      ~character:12
      ~new_name:"not valid"
      ()
  in
  printf "%d edits\n" (List.length edits);
  [%expect {| 0 edits |}]
;;

let%expect_test "rename note preserves aliases and fragments" =
  let content = Option.value_exn (read_file "links.md") in
  Lsp_lib.Rename.For_test.rename
    ~index
    ~docs
    ~read_file
    ~rel_path:"links.md"
    ~content
    ~line:4
    ~character:3
    ~new_name:"renamed note"
    ()
  |> fun edits ->
  show edits;
  show_applied edits;
  [%expect
    {|
    links.md [2-8] -> renamed note
    links.md [32-38] -> renamed note
    links.md [61-67] -> renamed%20note
    links.md [85-91] -> renamed note
    links.md [110-116] -> renamed note
    --- links.md
    [[renamed note#Old Heading|alias]]
    ![[renamed note#Old Heading]]
    [label](renamed%20note#Old%20Heading)
    [[renamed note#old-id|anchor]]
    [[renamed note|note]]
    |}]
;;

(* An id that is a prefix of another id, and a link fragment sitting in the
   definition file, are both things a bare ["#" ^ id] search would mistake for
   the definition. See {!Lsp_lib.Rename.attr_id_offset}. *)
let collide_files =
  [ ( "collide.md"
    , "# Doc\n\nSee [[collide#note]] first.\n\n{#note-extended}\n> Longer.\n\n{#note}\n> Short.\n"
    )
  ]
;;

let collide_index, collide_docs =
  Lsp_lib.Find_references.For_test.make_vault collide_files
;;

let collide_read_file path = List.Assoc.find collide_files ~equal:String.equal path

let%expect_test "attribute rename is not confused by prefixes or link fragments" =
  let content = Option.value_exn (collide_read_file "collide.md") in
  let edits =
    Lsp_lib.Rename.For_test.rename
      ~index:collide_index
      ~docs:collide_docs
      ~read_file:collide_read_file
      ~rel_path:"collide.md"
      ~content
      ~line:7
      ~character:2
      ~new_name:"renamed"
      ()
  in
  show edits;
  printf "---\n%s" (apply_edits content edits);
  [%expect {|
    collide.md [21-25] -> renamed
    collide.md [66-70] -> renamed
    ---
    # Doc

    See [[collide#renamed]] first.

    {#note-extended}
    > Longer.

    {#renamed}
    > Short.
    |}]
;;

let%expect_test "server: note rename includes text edits and a file operation" =
  let vault_root = Filename.concat (Core_unix.getcwd ()) "data" in
  let s = start_server ~vault_root in
  did_open s ~rel_path:"note-b.md";
  Server.rename s ~rel_path:"note-b.md" ~line:2 ~character:13 ~new_name:"renamed"
  |> document_change_kinds
  |> List.iter ~f:print_endline;
  [%expect
    {|
    text-edits
    text-edits
    rename
    |}]
;;
