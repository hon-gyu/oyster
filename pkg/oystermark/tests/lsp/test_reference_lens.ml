(** Spec: {!page-"feature-codelens-reference-counts"}.
    Impl: {!Lsp_lib.Reference_counts} and [Server.reference_lenses].

    The counts themselves are {!Lsp_lib.Reference_counts}' own tests; what is
    checked here is the lens they become — its title, the line it sits above,
    and the fact that it carries nothing to run. *)

open Core
open Lsp_helper
open Linol_lsp.Lsp.Types

let vault_root =
  let cwd = Core_unix.getcwd () in
  Filename.concat cwd "data"
;;

(** Every lens in [rel_path]: the line it is drawn above, its title, and
    whether it has a command to run. *)
let show_lenses ?init_options ~vault_root rel_path =
  let s = start_server ?init_options ~vault_root () in
  did_open s ~rel_path;
  match Server.code_lens s ~rel_path with
  | None -> print_endline "no vault"
  | Some lenses ->
    List.iter lenses ~f:(fun (l : CodeLens.t) ->
      printf
        "%d: %s%s\n"
        l.range.start.line
        (Option.value_map l.command ~default:"(no title)" ~f:(fun c -> c.title))
        (match l.command with
         | Some c when not (String.is_empty c.command) -> sprintf " -> %s" c.command
         | _ -> ""))
;;

(* note-a is pointed at by four links in note-b, one from a subdirectory, and
   one of them names [## Section One]. *)
let%expect_test "counts above the note and the heading they are about" =
  show_lenses ~vault_root "note-a.md";
  [%expect
    {|
    0: 5 backlinks -> editor.action.showReferences
    2: 1 reference -> editor.action.showReferences
    |}]
;;

(* A note's own links to its own headings are counted apart: nothing points at
   this map of contents from outside, so its in-note links are all its lenses
   have to say.  See {!page-"feature-codelens-reference-counts".self}. *)
let%expect_test "self-references count" =
  show_lenses ~vault_root "moc.md";
  [%expect
    {|
    0: 4 in-note links -> editor.action.showReferences
    3: 2 in-note references -> editor.action.showReferences
    7: 1 in-note reference -> editor.action.showReferences
    |}]
;;

(* The [::: toc] region names both headings and counts for neither: [# Alpha]
   has no lens at all, and [## Method] is at the count note-c gives it plus the
   one link the prose makes.  See
   {!page-"feature-codelens-reference-counts".self}. *)
let%expect_test "a table of contents is not a reference" =
  show_lenses ~vault_root "toc-note.md";
  [%expect
    {|
    0: 1 backlink, 1 in-note link -> editor.action.showReferences
    7: 1 reference, 1 in-note reference -> editor.action.showReferences
    |}]
;;

(* Turned on, the TOC entries are counted as what they are — links the note
   makes to itself — and [# Alpha], which only the TOC names, gets a lens.
   See {!page-"feature-codelens-reference-counts".self}. *)
let%expect_test "codeLens.countTocLinks" =
  show_lenses
    ~vault_root
    ~init_options:(`Assoc [ "codeLens", `Assoc [ "countTocLinks", `Bool true ] ])
    "toc-note.md";
  [%expect
    {|
    0: 1 backlink, 3 in-note links -> editor.action.showReferences
    5: 1 in-note reference -> editor.action.showReferences
    7: 1 reference, 2 in-note references -> editor.action.showReferences
    |}]
;;

(* Nothing points at note-b, and a zero is not worth a line of the note. *)
let%expect_test "no lens where the count is zero" =
  show_lenses ~vault_root "note-b.md";
  [%expect {| |}]
;;

(* The lens hands the client the locations it counted, under the command name
   the configuration says this client answers to.
   See {!page-"feature-codelens-reference-counts".click}. *)
let%expect_test "the lens carries the references" =
  let s = start_server ~vault_root () in
  did_open s ~rel_path:"note-a.md";
  Server.code_lens s ~rel_path:"note-a.md"
  |> Option.value ~default:[]
  |> List.iter ~f:(fun (l : CodeLens.t) ->
    match l.command with
    | None -> printf "no command\n"
    | Some c ->
      printf
        "title %S, command %S, args %b\n"
        c.title
        c.command
        (Option.is_some c.arguments));
  [%expect
    {|
    title "5 backlinks", command "editor.action.showReferences", args true
    title "1 reference", command "editor.action.showReferences", args true
    |}]
;;

(* The command name is the client's, so it is configurable — and [""] asks for
   a lens that is a label, which is what an unaware client makes of it anyway.
   See {!page-"feature-codelens-reference-counts".click}. *)
let%expect_test "showReferencesCommand: a client's own name, or none" =
  let show command =
    show_lenses
      ~vault_root
      ~init_options:
        (`Assoc [ "codeLens", `Assoc [ "showReferencesCommand", `String command ] ])
      "note-a.md"
  in
  show "my-editor.showRefs";
  show "";
  [%expect
    {|
    0: 5 backlinks -> my-editor.showRefs
    2: 1 reference -> my-editor.showRefs
    0: 5 backlinks
    2: 1 reference
    |}]
;;

let%expect_test "arguments: uri, position, then the locations counted" =
  let s = start_server ~vault_root () in
  did_open s ~rel_path:"note-a.md";
  Server.code_lens s ~rel_path:"note-a.md"
  |> Option.value ~default:[]
  |> List.iter ~f:(fun (l : CodeLens.t) ->
    match l.command with
    | None | Some { arguments = None; _ } -> printf "no arguments\n"
    | Some ({ arguments = Some args; _ } as c) ->
      let locations =
        match List.nth args 2 with
        | Some (`List l) -> List.length l
        | _ -> -1
      in
      printf
        "%s: %d arguments, %d locations, uri ends %S\n"
        c.title
        (List.length args)
        locations
        (match List.hd args with
         | Some (`String u) -> String.suffix u 20
         | _ -> "?"));
  [%expect
    {|
    5 backlinks: 3 arguments, 5 locations, uri ends "s/lsp/data/note-a.md"
    1 reference: 3 arguments, 1 locations, uri ends "s/lsp/data/note-a.md"
    |}]
;;

let%expect_test "codeLens.references off" =
  show_lenses
    ~vault_root
    ~init_options:(`Assoc [ "codeLens", `Assoc [ "references", `Bool false ] ])
    "note-a.md";
  [%expect {| |}]
;;

(* A note with a command panel gets both kinds of lens, the commands first.
   See {!page-"feature-codelens-reference-counts"} and
   {!page-"feature-command-block".surfaces}. *)
let%expect_test "command lenses come before counts" =
  let panel = "```oysterlsp\ndaily/today\n```\n" in
  with_tmp_vault
    ~files:
      [ "hub.md", "# Hub\n\n" ^ panel ^ "\n## Section\n"
      ; "other.md", "# Other\n\nSee [[hub]] and [[hub#Section]].\n"
      ]
    (fun vault_root ->
       show_lenses
         ~vault_root
         ~init_options:
           (`Assoc [ "dailyNotes", `Assoc [ "format", `String "YYYY-MM-DD" ] ])
         "hub.md");
  [%expect
    {|
    3: Create today's daily note -> oystermark.dailyNote.openDay
    0: 2 backlinks -> editor.action.showReferences
    6: 1 reference -> editor.action.showReferences
    |}]
;;
