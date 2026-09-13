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

let%expect_test "server: hover on wikilink to note" =
  let s = start_server ~vault_root () in
  did_open s ~rel_path:"note-b.md";
  (* Line 2: "Link to [[note-a]] here." — cursor on "note-a" *)
  let result = Server.hover s ~rel_path:"note-b.md" ~line:2 ~character:13 |> hover_text in
  print_s [%sexp (result : string option)];
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

let%expect_test "server: hover on heading fragment" =
  let s = start_server ~vault_root () in
  did_open s ~rel_path:"note-b.md";
  (* Line 4: "See [[note-a#Section One]]." *)
  let result = Server.hover s ~rel_path:"note-b.md" ~line:4 ~character:10 |> hover_text in
  print_s [%sexp (result : string option)];
  [%expect
    {|
    ( "*Path*:note-a.md\
     \n\
     \n## Section One\
     \n\
     \nBody text ^block1")
    |}]
;;

let%expect_test "server: hover on block fragment" =
  let s = start_server ~vault_root () in
  did_open s ~rel_path:"note-b.md";
  (* Line 6: "Also [[note-a#^block1]]." *)
  let result = Server.hover s ~rel_path:"note-b.md" ~line:6 ~character:10 |> hover_text in
  print_s [%sexp (result : string option)];
  [%expect
    {|
    ( "*Path*:note-a.md\
     \n\
     \nBody text ^block1")
    |}]
;;

let%expect_test "server: hover on empty note" =
  let s = start_server ~vault_root () in
  did_open s ~rel_path:"note-c.md";
  (* Line 2: "See [[empty]]." *)
  let result = Server.hover s ~rel_path:"note-c.md" ~line:2 ~character:8 |> hover_text in
  print_s [%sexp (result : string option)];
  [%expect
    {|
    ( "*Path*:empty.md\
     \n\
     \n*(empty)*")
    |}]
;;

let%expect_test "server: hover on unresolved link" =
  let s = start_server ~vault_root () in
  did_open s ~rel_path:"note-b.md";
  (* Line 10: "Unresolved [[missing-note]]." *)
  let result =
    Server.hover s ~rel_path:"note-b.md" ~line:10 ~character:16 |> hover_text
  in
  print_s [%sexp (result : string option)];
  [%expect {| () |}]
;;

let%expect_test "server: hover cursor not on link" =
  let s = start_server ~vault_root () in
  did_open s ~rel_path:"note-b.md";
  let result = Server.hover s ~rel_path:"note-b.md" ~line:0 ~character:2 |> hover_text in
  print_s [%sexp (result : string option)];
  [%expect {| () |}]
;;

(* When the index is older than the file (for example, with unsaved edits),
   hover resolves the fragment against the file content, using the same rules
   as the index. *)
let%expect_test "fragment missing from a stale index is read from the file" =
  let stale =
    [ "target.md", "# Target\n"
    ; "source.md", "[[target#Parent#Child]] [[target#Solo]] [[target#^fresh]]\n"
    ]
  in
  let fresh =
    "# Target\n\n\
     ## Parent\n\n\
     intro\n\n\
     ### Child\n\n\
     child body\n\n\
     ## Solo\n\n\
     solo body\n\n\
     new para ^fresh\n"
  in
  let index = Vault_helper.make_index stale in
  let read_file = function
    | "target.md" -> Some fresh
    | path -> List.Assoc.find stale ~equal:String.equal path
  in
  let content = List.Assoc.find_exn stale ~equal:String.equal "source.md" in
  List.iter [ "Parent#Child"; "Solo"; "^fresh" ] ~f:(fun needle ->
    let offset = Option.value_exn (String.substr_index content ~pattern:needle) in
    let line, character = Lsp_lib.Util.position_of_byte_offset content offset in
    Lsp_lib.Hover.hover
      ~index
      ~rel_path:"source.md"
      ~content
      ~line
      ~character
      ~read_file
      ()
    |> Option.value_map ~default:"<none>" ~f:(fun (text, _, _) -> text)
    |> printf "== %s\n%s\n" needle);
  [%expect
    {|
    == Parent#Child
    *Path*:target.md

    ### Child

    child body
    == Solo
    *Path*:target.md

    ## Solo

    solo body

    new para ^fresh
    == ^fresh
    *Path*:target.md

    new para ^fresh
    |}]
;;
