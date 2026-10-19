(** Spec: {!page-"feature-command-block"}.
    Impl: {!Lsp_lib.Command_block} (finding the lines) and {!Lsp_lib.Server}
    (the surfaces built on them).

    {!Lsp_lib.Command_block}'s own tests cover recognition; these check what a
    server answers with, which is where the block meets the vault, the clock
    and the daily-note settings. *)

open Core
open Linol_lsp.Lsp.Types
open Lsp_helper

(* Fixed so the expectations mean the same thing tomorrow. *)
let today = Date.create_exn ~y:2026 ~m:Month.Jul ~d:26

let panel =
  {|```oysterlsp
daily/today
daily/yesterday
daily/prev
daily/next
```
|}
;;

let files =
  [ "journal/2026-07-03.md", "# Early\n"
  ; "journal/2026-07-10.md", "# Middle\n"
  ; "journal/2026-07-26.md", "# Today\n\n" ^ panel
  ; "idea.md", "# An idea\n\n" ^ panel
  ]
;;

let options : Yojson.Safe.t =
  `Assoc
    [ "dailyNotes", `Assoc [ "format", `String "YYYY-MM-DD"; "folder", `String "journal" ]
    ]
;;

let start ~vault_root = start_server ~init_options:options ~today ~vault_root ()

(** Every lens in [rel_path]: its line, its title, and the command it carries
    (or nothing, when the command cannot run here). *)
let show_lenses ?(files = files) rel_path =
  with_tmp_vault ~files (fun vault_root ->
    let s = start ~vault_root in
    match Server.code_lens s ~rel_path with
    | None -> print_endline "no vault"
    | Some lenses ->
      List.iter lenses ~f:(fun (l : CodeLens.t) ->
        printf
          "%d: %s\n"
          l.range.start.line
          (match l.command with
           | Some c ->
             sprintf
               "%s -> %s"
               c.title
               (String.concat
                  ~sep:" "
                  (List.map
                     (Option.value c.arguments ~default:[])
                     ~f:Yojson.Safe.to_string))
           | None -> "(nothing to run)")))
;;

(** {1 Lenses}

    See {!page-"feature-command-block".surfaces}. *)

(* From a daily note, every line of the panel resolves: the calendar three
   against the clock, previous/next against *this* note's date. *)
let%expect_test "a panel inside a daily note" =
  show_lenses "journal/2026-07-26.md";
  [%expect
    {|
    3: Open today's daily note -> "journal/2026-07-26.md" 0
    4: Create yesterday's daily note -> "journal/2026-07-26.md" -1
    5: Open previous daily note (journal/2026-07-10.md) -> "journal/2026-07-10.md" false
    6: (nothing to run)
    |}]
;;

(* The same block in an ordinary note: the calendar commands are unchanged,
   but previous/next have no date to move from.  They keep their lens and say
   so — a lens that vanished would look like a server that failed. *)
let%expect_test "the same panel in an ordinary note" =
  show_lenses "idea.md";
  [%expect
    {|
    3: Open today's daily note -> "idea.md" 0
    4: Create yesterday's daily note -> "idea.md" -1
    5: (nothing to run)
    6: (nothing to run)
    |}]
;;

(* The point of putting the panel in a note: the identical block means
   different things in two of them. *)
let%expect_test "previous and next follow the host note" =
  (* Replace, rather than prepend: {!with_tmp_vault} writes in order, so a
     second entry for the same path would overwrite the panel. *)
  let files =
    List.map files ~f:(fun (path, content) ->
      if String.equal path "journal/2026-07-10.md"
      then path, "# Middle\n\n" ^ panel
      else path, content)
  in
  show_lenses ~files "journal/2026-07-10.md";
  [%expect
    {|
    3: Open today's daily note -> "journal/2026-07-10.md" 0
    4: Create yesterday's daily note -> "journal/2026-07-10.md" -1
    5: Open previous daily note (journal/2026-07-03.md) -> "journal/2026-07-03.md" false
    6: Open next daily note (journal/2026-07-26.md) -> "journal/2026-07-26.md" false
    |}]
;;

let%expect_test "a note with no block has no lenses" =
  show_lenses ~files:[ "plain.md", "# Nothing here\n" ] "plain.md";
  [%expect {| |}]
;;

(* An unsupported daily-note format disables the commands; the lines still
   report why rather than going quiet. *)
let%expect_test "daily notes disabled" =
  with_tmp_vault ~files (fun vault_root ->
    let s =
      start_server
        ~init_options:(`Assoc [ "dailyNotes", `Assoc [ "format", `String "YYYY-ww" ] ])
        ~today
        ~vault_root
        ()
    in
    Option.iter (Server.code_lens s ~rel_path:"idea.md") ~f:(fun lenses ->
      List.iter lenses ~f:(fun (l : CodeLens.t) ->
        printf "%d: %b\n" l.range.start.line (Option.is_some l.command))));
  [%expect
    {|
    3: false
    4: false
    5: false
    6: false
    |}]
;;

(** {1 Code action}

    The keyboard route to the same command, for clients that render no
    lenses. *)

let show_action ?(rel_path = "journal/2026-07-26.md") line =
  with_tmp_vault ~files (fun vault_root ->
    let s = start ~vault_root in
    Server.code_action
      s
      ~rel_path
      ~start_line:line
      ~start_character:0
      ~end_line:line
      ~end_character:0
      ()
    |> List.iter ~f:(fun (a : CodeAction.t) -> printf "%s\n" a.title))
;;

(* On a panel line, exactly one action: the line's own.  The daily-note menu
   would otherwise bury it under five. *)
let%expect_test "one action on a command line" =
  show_action 4;
  [%expect {| Create yesterday's daily note |}]
;;

(* Off the panel, the ordinary menu is unchanged. *)
let%expect_test "the usual actions elsewhere in the note" =
  show_action 0;
  [%expect
    {|
    Open today's daily note
    Create yesterday's daily note
    Create tomorrow's daily note
    Open previous daily note
    Insert link to today's daily note
    Insert table of contents
    |}]
;;

(* A line whose command cannot run offers no action — there is nothing to
   apply.  The lens still explains it. *)
let%expect_test "no action for an inapplicable command" =
  show_action ~rel_path:"idea.md" 5;
  [%expect
    {|
    Open today's daily note
    Create yesterday's daily note
    Create tomorrow's daily note
    Insert link to today's daily note
    Insert table of contents
    |}]
;;

(** {1 Creating a block}

    See {!page-"feature-command-block".insert}: which notes are offered a
    panel, and which lines it comes with. *)

(** The line a workspace edit's first text edit starts at, [-1] if it has
    none.  The block goes in at the cursor, and an edit with the right text at
    the wrong place is still wrong. *)
let insert_line (edit : WorkspaceEdit.t) : int =
  Option.value edit.documentChanges ~default:[]
  |> List.find_map ~f:(function
    | `TextDocumentEdit (e : TextDocumentEdit.t) ->
      List.hd e.edits
      |> Option.map ~f:(function
        | `TextEdit (t : TextEdit.t) -> t.range.start.line
        | `AnnotatedTextEdit (t : AnnotatedTextEdit.t) -> t.range.start.line)
    | `CreateFile _ | `RenameFile _ | `DeleteFile _ -> None)
  |> Option.value ~default:(-1)
;;

(** The insert action offered at [line] of [rel_path], with where it would
    write and what. *)
let show_insert ?(files = files) ?(line = 0) rel_path =
  with_tmp_vault ~files (fun vault_root ->
    let s = start ~vault_root in
    Server.code_action
      s
      ~rel_path
      ~start_line:line
      ~start_character:0
      ~end_line:line
      ~end_character:0
      ()
    (* Other actions carry edits too — the daily-note link, for one — so
       select this one by name rather than by having an edit at all. *)
    |> List.filter ~f:(fun (a : CodeAction.t) ->
      String.is_substring a.title ~substring:"command block")
    |> List.iter ~f:(fun (a : CodeAction.t) ->
      Option.iter a.edit ~f:(fun edit ->
        printf "%s, at line %d\n" a.title (insert_line edit);
        List.iter (inserted_texts edit) ~f:print_string)))
;;

(* An ordinary note gets the calendar three; previous and next would be dead
   the moment they were written. *)
let%expect_test "an ordinary note is offered the calendar commands" =
  show_insert ~files:[ "plain.md", "# Nothing here\n" ] "plain.md";
  [%expect
    {|
    Insert oysterlsp command block, at line 0
    ```oysterlsp
    daily/today
    daily/yesterday
    daily/tomorrow
    ```
    |}]
;;

(* The panel goes in where the cursor is, not at the top of the note. *)
let%expect_test "the block is written at the cursor's line" =
  show_insert ~files:[ "plain.md", "# Nothing here\n\nprose.\n" ] ~line:2 "plain.md";
  [%expect
    {|
    Insert oysterlsp command block, at line 2
    ```oysterlsp
    daily/today
    daily/yesterday
    daily/tomorrow
    ```
    |}]
;;

(* A daily note in the middle of the journal can use all five. *)
let%expect_test "a daily note with neighbours is offered all five" =
  let files =
    List.map files ~f:(fun (path, content) ->
      if String.equal path "journal/2026-07-10.md"
      then path, "# Middle\n"
      else path, content)
  in
  show_insert ~files "journal/2026-07-10.md";
  [%expect
    {|
    Insert oysterlsp command block, at line 0
    ```oysterlsp
    daily/today
    daily/yesterday
    daily/tomorrow
    daily/prev
    daily/next
    ```
    |}]
;;

(* The earliest daily note has nothing behind it, so [daily/prev] is left out
   of the block rather than written and immediately reported dead. *)
let%expect_test "the earliest daily note gets no daily/prev" =
  show_insert "journal/2026-07-03.md";
  [%expect
    {|
    Insert oysterlsp command block, at line 0
    ```oysterlsp
    daily/today
    daily/yesterday
    daily/tomorrow
    daily/next
    ```
    |}]
;;

(* Once a note has a panel the action stops: adding a line to one is typing,
   which completion already helps with. *)
let%expect_test "a note that already has a block is offered nothing" =
  show_insert "idea.md";
  [%expect {| |}]
;;

(* With daily notes disabled nothing resolves, and a block of dead lines is
   not worth writing. *)
let%expect_test "no commands, no block" =
  with_tmp_vault
    ~files:[ "plain.md", "# Nothing here\n" ]
    (fun vault_root ->
       let s =
         start_server
           ~init_options:(`Assoc [ "dailyNotes", `Assoc [ "format", `String "YYYY-ww" ] ])
           ~today
           ~vault_root
           ()
       in
       Server.code_action
         s
         ~rel_path:"plain.md"
         ~start_line:0
         ~start_character:0
         ~end_line:0
         ~end_character:0
         ()
       |> List.iter ~f:(fun (a : CodeAction.t) -> printf "%s\n" a.title));
  [%expect {| Insert table of contents |}]
;;

(** {1 Go to definition}

    See {!page-"feature-command-block".surfaces} and
    {!page-"feature-go-to-definition".command_block}: the navigation key
    reaches the note a line names, without running the line. *)

(** Where go-to-definition lands for each line of [rel_path], panel lines
    included. *)
let show_definitions
      ?(files = files)
      ?(character = 0)
      ?(lines = [ 0; 2; 3; 4; 5; 6 ])
      rel_path
  =
  with_tmp_vault ~files (fun vault_root ->
    let s = start ~vault_root in
    did_open s ~rel_path;
    List.iter lines ~f:(fun line ->
      let target =
        match Server.definition s ~rel_path ~line ~character |> definition_result s with
        | None -> "-"
        | Some { path; line; character } -> sprintf "%s:%d:%d" path line character
      in
      printf "%d: %s\n" line target))
;;

(* From the daily note holding the panel: today is this note, previous is the
   note before it.  Yesterday's does not exist — its lens offers to create it,
   which a jump may not do — and there is nothing after today. *)
let%expect_test "each panel line jumps to the note it names" =
  show_definitions "journal/2026-07-26.md";
  [%expect
    {|
    0: -
    2: -
    3: journal/2026-07-26.md:0:0
    4: -
    5: journal/2026-07-10.md:0:0
    6: -
    |}]
;;

(* The same panel in an ordinary note: the calendar line still points at the
   journal, and previous/next have no date to move from. *)
let%expect_test "the same panel in an ordinary note" =
  show_definitions "idea.md";
  [%expect
    {|
    0: -
    2: -
    3: journal/2026-07-26.md:0:0
    4: -
    5: -
    6: -
    |}]
;;

(* A misspelling names no command, so it names no note either — the
   diagnostic below is what it gets. *)
let%expect_test "an unknown command line has no definition" =
  show_definitions
    ~files:[ "typo.md", "```oysterlsp\ndaily/yesterdya\n```\n" ]
    ~lines:[ 1 ]
    "typo.md";
  [%expect {| 1: - |}]
;;

(* Links elsewhere in a note that happens to hold a panel are unaffected. *)
let%expect_test "links outside the block still resolve" =
  show_definitions
    ~files:(("linker.md", "# Linker\n\nSee [[idea]].\n\n" ^ panel) :: files)
    ~character:9
    ~lines:[ 2; 5 ]
    "linker.md";
  [%expect
    {|
    2: idea.md:0:0
    5: journal/2026-07-26.md:0:0
    |}]
;;

(** {1 Completion and diagnostics} *)

let%expect_test "completion inside the block offers the catalogue" =
  with_tmp_vault ~files (fun vault_root ->
    let s = start ~vault_root in
    match Server.completion s ~rel_path:"idea.md" ~line:4 ~character:0 with
    | None -> print_endline "none"
    | Some list ->
      List.iter list.items ~f:(fun (i : CompletionItem.t) ->
        printf "%-16s %s\n" i.label (Option.value i.detail ~default:"")));
  [%expect
    {|
    daily/today      Open today's daily note, creating it when missing
    daily/yesterday  Open yesterday's daily note, creating it when missing
    daily/tomorrow   Open tomorrow's daily note, creating it when missing
    daily/prev       Open the previous existing daily note, relative to this note
    daily/next       Open the next existing daily note, relative to this note
    |}]
;;

(* A misspelling is inert, and inert looks exactly like unimplemented — so it
   is diagnosed. *)
let%expect_test "an unknown command name is diagnosed" =
  let content =
    {|# Note

```oysterlsp
daily/today
daily/yesterdya
```
|}
  in
  with_tmp_vault
    ~files:[ "typo.md", content ]
    (fun vault_root ->
       let s = start ~vault_root in
       Server.did_open s ~rel_path:"typo.md" ~content
       |> List.iter ~f:(fun (d : Diagnostic.t) ->
         printf
           "%d:%d %s\n"
           d.range.start.line
           d.range.start.character
           (match d.message with
            | `String m -> m
            | _ -> "")));
  [%expect
    {|
    4:0 unknown oysterlsp command "daily/yesterdya"; expected one of daily/today, daily/yesterday, daily/tomorrow, daily/prev, daily/next
    |}]
;;
