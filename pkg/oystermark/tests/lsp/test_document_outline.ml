(** Spec: {!page-"feature-document-outline"}.
    Impl: {!Lsp_lib.Document_outline}. *)

open Core
open Lsp_helper

let content =
  "{#preamble}\n\
   Before.\n\n\
   # Title\n\n\
   Root text ^root\n\n\
   ## Child\n\n\
   The [key]{#key} term.\n\n\
   #### Deep\n\n\
   ### Sibling\n"
;;

let index, _docs =
  Lsp_lib.Find_references.For_test.make_vault [ "outline.md", content ]
;;

let rec show depth (symbol : Lsp_lib.Document_outline.symbol) =
  printf
    "%s%s [%d-%d] select[%d-%d]\n"
    (String.make (depth * 2) ' ')
    symbol.name
    symbol.first_byte
    symbol.last_byte
    symbol.selection_first_byte
    symbol.selection_last_byte;
  List.iter symbol.children ~f:(show (depth + 1))
;;

let%expect_test "heading hierarchy with block and attribute anchors" =
  Lsp_lib.Document_outline.For_test.document_outline
    ~index
    ~rel_path:"outline.md"
    ~content
  |> List.iter ~f:(show 0);
  [%expect
    {|
    #preamble [12-19] select[12-19]
    Title [21-103] select[21-28]
      ^root [30-45] select[30-45]
      Child [47-103] select[47-55]
        #key [61-72] select[61-72]
        Deep [80-91] select[80-89]
        Sibling [91-103] select[91-102]
    |}]
;;

let%expect_test "unknown file has no outline" =
  let symbols =
    Lsp_lib.Document_outline.For_test.document_outline
      ~index
      ~rel_path:"missing.md"
      ~content:""
  in
  printf "%d symbols\n" (List.length symbols);
  [%expect {| 0 symbols |}]
;;

let%expect_test "e2e documentSymbol returns a hierarchical result" =
  let vault_root = Filename.concat (Core_unix.getcwd ()) "data" in
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-a.md";
  let result = document_symbols s ~rel_path:"note-a.md" in
  let rec print_symbols depth = function
    | `List symbols ->
      List.iter symbols ~f:(fun symbol ->
        let name = Yojson.Safe.Util.(member "name" symbol |> to_string) in
        printf "%s%s\n" (String.make (depth * 2) ' ') name;
        print_symbols (depth + 1) (Yojson.Safe.Util.member "children" symbol))
    | `Null -> ()
    | _ -> ()
  in
  print_symbols 0 result;
  shutdown s;
  [%expect
    {|
    Alpha
      Section One
        ^block1
      Section Two
    |}]
;;

