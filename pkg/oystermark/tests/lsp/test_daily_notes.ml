(** Spec: {!page-"feature-daily-notes"}.
    Impl: {!Lsp_lib.Daily_notes} (pure layer) and {!Lsp_lib.Server} (actions and
    the command).

    The pure layer's own tests live beside it; these drive the server, where
    settings, the clock, and the disk meet. *)

open Core
open Linol_lsp.Lsp.Types
open Lsp_helper

(* "Today" is fixed so these expectations mean the same thing tomorrow; the
   fixture dates are chosen around it, with gaps to exercise previous/next. *)
let today = Date.create_exn ~y:2026 ~m:Month.Jul ~d:26

let files =
  [ "journal/2026-07-03.md", "# Friday\n"
  ; "journal/2026-07-10.md", "# Friday again\n"
  ; "journal/2026-07-26.md", "# Today\n"
  ; "idea.md", "# Not a daily note\n"
  ]
;;

let options ?(folder = "journal") (format : string) : Yojson.Safe.t =
  `Assoc [ "dailyNotes", `Assoc [ "format", `String format; "folder", `String folder ] ]
;;

(** Every code action offered at [rel_path], as [title -> edit, command
    arguments].  The edit is what creates a missing note and — being a text
    edit against it — what opens it on a client with no [window/showDocument];
    the command that follows only opens.  See
    {!page-"feature-daily-notes".focus}. *)
let show_actions ?(init_options = options "YYYY-MM-DD") ~(vault_root : string) rel_path =
  let s = start_server ~init_options ~today ~vault_root () in
  did_open s ~rel_path;
  Server.code_action
    s
    ~rel_path
    ~start_line:0
    ~start_character:0
    ~end_line:0
    ~end_character:0
    ()
  |> List.iter ~f:(fun (a : CodeAction.t) ->
    let args =
      match a.command with
      | None -> "no command"
      | Some c ->
        List.map (Option.value c.arguments ~default:[]) ~f:Yojson.Safe.to_string
        |> String.concat ~sep:" "
    in
    let edit =
      match a.edit with
      | None -> "no edit"
      | Some edit -> String.concat ~sep:"," (document_change_kinds edit)
    in
    printf "%-32s %-22s %s\n" a.title edit args)
;;

let%expect_test "actions from a daily note: calendar plus previous/next" =
  with_tmp_vault ~files (fun vault_root ->
    show_actions ~vault_root "journal/2026-07-26.md");
  [%expect
    {|
    Open today's daily note          no edit                "journal/2026-07-26.md" false false
    Create yesterday's daily note    create,text-edits      "journal/2026-07-25.md" false true
    Create tomorrow's daily note     create,text-edits      "journal/2026-07-27.md" false true
    Open previous daily note         no edit                "journal/2026-07-10.md" false
    Insert link to today's daily note text-edits             no command
    Insert oysterlsp command block   text-edits             no command
    Insert table of contents         text-edits             no command
    |}]
;;

(* Previous/next skip the gap between the 3rd and the 10th, and stop at the
   ends of what exists — they never create. *)
let%expect_test "previous and next from the middle of the range" =
  with_tmp_vault ~files (fun vault_root ->
    show_actions ~vault_root "journal/2026-07-10.md");
  [%expect
    {|
    Open today's daily note          no edit                "journal/2026-07-26.md" false false
    Create yesterday's daily note    create,text-edits      "journal/2026-07-25.md" false true
    Create tomorrow's daily note     create,text-edits      "journal/2026-07-27.md" false true
    Open previous daily note         no edit                "journal/2026-07-03.md" false
    Open next daily note             no edit                "journal/2026-07-26.md" false
    Insert link to today's daily note text-edits             no command
    Insert oysterlsp command block   text-edits             no command
    Insert table of contents         text-edits             no command
    |}]
;;

(* Off a daily note the calendar family is still offered; the existing family
   is not, because there is no current date to move from. *)
let%expect_test "actions from an ordinary note" =
  with_tmp_vault ~files (fun vault_root -> show_actions ~vault_root "idea.md");
  [%expect
    {|
    Open today's daily note          no edit                "journal/2026-07-26.md" false false
    Create yesterday's daily note    create,text-edits      "journal/2026-07-25.md" false true
    Create tomorrow's daily note     create,text-edits      "journal/2026-07-27.md" false true
    Insert link to today's daily note text-edits             no command
    Insert oysterlsp command block   text-edits             no command
    Insert table of contents         text-edits             no command
    |}]
;;

(* A format with [/] nests the note in folders; the paths follow. *)
let%expect_test "nested format" =
  with_tmp_vault ~files (fun vault_root ->
    show_actions ~init_options:(options "YYYY/MM/YYYY-MM-DD") ~vault_root "idea.md");
  [%expect
    {|
    Create today's daily note        create,text-edits      "journal/2026/07/2026-07-26.md" false true
    Create yesterday's daily note    create,text-edits      "journal/2026/07/2026-07-25.md" false true
    Create tomorrow's daily note     create,text-edits      "journal/2026/07/2026-07-27.md" false true
    Insert link to today's daily note create,text-edits      no command
    Insert oysterlsp command block   text-edits             no command
    Insert table of contents         text-edits             no command
    |}]
;;

(* The create edit's shape is the whole point of it: a [CreateFile] for the
   note and a text edit against it that writes {e nothing}.  The empty edit is
   what makes a client open the new note — a text edit cannot be applied to a
   document that is not open — while leaving the note as empty as creation
   through the command left it.  See {!page-"feature-daily-notes".focus}. *)
let%expect_test "the create edit opens the note without writing to it" =
  with_tmp_vault ~files (fun vault_root ->
    let s = start_server ~init_options:(options "YYYY-MM-DD") ~today ~vault_root () in
    did_open s ~rel_path:"idea.md";
    Server.code_action
      s
      ~rel_path:"idea.md"
      ~start_line:0
      ~start_character:0
      ~end_line:0
      ~end_character:0
      ()
    |> List.filter ~f:(fun (a : CodeAction.t) ->
      String.equal a.title "Create tomorrow's daily note")
    |> List.iter ~f:(fun (a : CodeAction.t) ->
      Option.iter a.edit ~f:(fun edit ->
        printf
          "%s: [%s] wrote %s\n"
          a.title
          (String.concat ~sep:"; " (document_change_kinds edit))
          (String.concat ~sep:"|" (inserted_texts edit) |> sprintf "%S"))));
  [%expect {| Create tomorrow's daily note: [create; text-edits] wrote "" |}]
;;

(* An unsupported format disables the feature rather than naming a wrong file.
   The unrelated quick-fix actions are unaffected — the one line below belongs
   to {!page-"feature-toc"}, not to this family, and its presence is what
   shows the menu itself still works. *)
let%expect_test "unsupported format offers nothing of its own" =
  with_tmp_vault ~files (fun vault_root ->
    show_actions ~init_options:(options "YYYY-ww") ~vault_root "idea.md");
  [%expect {| Insert table of contents         text-edits             no command |}]
;;

(* ...and says why.  Offering nothing is also what a correctly configured
   server does when it has nothing to offer, so the rejection has to be
   reported for the two to be distinguishable.  It joins the configuration
   warnings, which the adapter turns into [window/showMessage] at initialize —
   see {!page-"feature-configuration".tolerance}. *)
let%expect_test "a rejected format is reported" =
  let show ?init_options () =
    with_tmp_vault ~files (fun vault_root ->
      let s = start_server ?init_options ~today ~vault_root () in
      print_s [%sexp (Server.config_warnings s : string list)])
  in
  show ~init_options:(options "YYYY-ww") ();
  show ~init_options:(options "/YYYY-MM-DD") ();
  show ~init_options:(options "YYYY-MM-DD") ();
  (* Unconfigured: the default format is usable, so there is nothing to say. *)
  show ();
  [%expect
    {|
    ("daily notes disabled: unsupported format token 'w'")
    ("daily notes disabled: format must not start with /")
    ()
    ()
    |}]
;;

(** {1 The command}

    [workspace/executeCommand] decides {i what} should happen; the adapter
    performs it.  See {!page-"feature-daily-notes"}. *)

let show_command ?(arguments : Yojson.Safe.t list option) ~(vault_root : string) command =
  let s = start_server ~init_options:(options "YYYY-MM-DD") ~today ~vault_root () in
  match Server.execute_command s ~command ~arguments with
  | None -> print_endline "no intent"
  | Some { uri; create; report_unfocused } ->
    printf
      "%s create=%s report_unfocused=%b\n"
      (Server.rel_path_of_uri s uri)
      (match create with
       | None -> "none"
       | Some edit -> String.concat ~sep:"," (document_change_kinds edit))
      report_unfocused
;;

let%expect_test "existing note is opened without an edit" =
  with_tmp_vault ~files (fun vault_root ->
    show_command
      ~vault_root
      ~arguments:[ `String "journal/2026-07-26.md"; `Bool true ]
      Server.daily_note_command);
  [%expect {| journal/2026-07-26.md create=none report_unfocused=true |}]
;;

let%expect_test "missing note is created, then opened" =
  with_tmp_vault ~files (fun vault_root ->
    show_command
      ~vault_root
      ~arguments:[ `String "journal/2026-07-27.md"; `Bool true ]
      Server.daily_note_command);
  [%expect {| journal/2026-07-27.md create=create report_unfocused=true |}]
;;

(* The optional third argument is the code action saying "I already handed the
   client an edit that opens this note".  A failure to focus is then not worth
   a message — the reader is looking at the note.  Absent, as a lens or a
   scripting client sends it, the report stands.  See
   {!page-"feature-daily-notes".focus}. *)
let%expect_test "an action that opened the note by its edit asks for no report" =
  with_tmp_vault ~files (fun vault_root ->
    show_command
      ~vault_root
      ~arguments:[ `String "journal/2026-07-27.md"; `Bool false; `Bool true ]
      Server.daily_note_command;
    show_command
      ~vault_root
      ~arguments:[ `String "journal/2026-07-27.md"; `Bool false; `Bool false ]
      Server.daily_note_command;
    (* A fourth argument is not a shape this server knows. *)
    show_command
      ~vault_root
      ~arguments:[ `String "journal/2026-07-27.md"; `Bool false; `Bool true; `Bool true ]
      Server.daily_note_command);
  [%expect
    {|
    journal/2026-07-27.md create=none report_unfocused=false
    journal/2026-07-27.md create=none report_unfocused=true
    no intent
    |}]
;;

(* An editor may send anything; neither an unknown command nor unusable
   arguments should reach the disk. *)
let%expect_test "unknown command and bad arguments yield no intent" =
  with_tmp_vault ~files (fun vault_root ->
    show_command
      ~vault_root
      ~arguments:[ `String "x.md"; `Bool true ]
      "some.other.command";
    show_command ~vault_root Server.daily_note_command;
    show_command ~vault_root ~arguments:[ `Int 3 ] Server.daily_note_command);
  [%expect
    {|
    no intent
    no intent
    no intent
    |}]
;;

(** {1 Linking instead of opening}

    See {!page-"feature-daily-notes".link}.  The one action in the family that
    needs no [window/showDocument]: it writes a link, and go-to-definition
    follows it. *)

(** The link action offered at [rel_path], as the operations its edit performs
    and the text it writes. *)
let show_link ?(init_options = options "YYYY-MM-DD") ~(vault_root : string) rel_path =
  let s = start_server ~init_options ~today ~vault_root () in
  did_open s ~rel_path;
  Server.code_action
    s
    ~rel_path
    ~start_line:0
    ~start_character:6
    ~end_line:0
    ~end_character:6
    ()
  |> List.filter ~f:(fun (a : CodeAction.t) ->
    String.is_prefix a.title ~prefix:"Insert link")
  |> List.iter ~f:(fun (a : CodeAction.t) ->
    Option.iter a.edit ~f:(fun edit ->
      printf
        "%s: [%s] %s\n"
        a.title
        (String.concat ~sep:"; " (document_change_kinds edit))
        (String.concat ~sep:" " (inserted_texts edit))))
;;

(* Today's note exists: nothing to create, just the link. *)
let%expect_test "links to an existing daily note" =
  with_tmp_vault ~files (fun vault_root -> show_link ~vault_root "idea.md");
  [%expect {| Insert link to today's daily note: [text-edits] [[2026-07-26]] |}]
;;

(* When today's note is missing the same action creates it, so the link it
   writes resolves immediately rather than landing unresolved. *)
let%expect_test "creates today's note when it is missing" =
  let files =
    List.filter files ~f:(fun (p, _) -> not (String.equal p "journal/2026-07-26.md"))
  in
  with_tmp_vault ~files (fun vault_root -> show_link ~vault_root "idea.md");
  [%expect {| Insert link to today's daily note: [create; text-edits] [[2026-07-26]] |}]
;;

(* The link is the base name, not the path: resolution matches a path
   subsequence, so the folders need not be spelled out. *)
let%expect_test "a nested format still links by base name" =
  with_tmp_vault ~files (fun vault_root ->
    show_link ~init_options:(options "YYYY/MM/YYYY-MM-DD") ~vault_root "idea.md");
  [%expect {| Insert link to today's daily note: [create; text-edits] [[2026-07-26]] |}]
;;

(* Nothing resolves with the feature disabled, so there is nothing to link. *)
let%expect_test "no link action when daily notes are disabled" =
  with_tmp_vault ~files (fun vault_root ->
    show_link ~init_options:(options "YYYY-ww") ~vault_root "idea.md");
  [%expect {| |}]
;;

(* The action appears in every menu in the vault, which is not always wanted;
   [linkAction] withdraws it without disabling daily notes.  See
   {!page-"feature-configuration"}. *)
let%expect_test "linkAction: false withdraws the action, and nothing else" =
  let init_options : Yojson.Safe.t =
    `Assoc
      [ ( "dailyNotes"
        , `Assoc
            [ "format", `String "YYYY-MM-DD"
            ; "folder", `String "journal"
            ; "linkAction", `Bool false
            ] )
      ]
  in
  with_tmp_vault ~files (fun vault_root ->
    show_link ~init_options ~vault_root "idea.md";
    print_endline "-- the rest of the menu:";
    show_actions ~init_options ~vault_root "idea.md");
  [%expect
    {|
    -- the rest of the menu:
    Open today's daily note          no edit                "journal/2026-07-26.md" false false
    Create yesterday's daily note    create,text-edits      "journal/2026-07-25.md" false true
    Create tomorrow's daily note     create,text-edits      "journal/2026-07-27.md" false true
    Insert oysterlsp command block   text-edits             no command
    Insert table of contents         text-edits             no command
    |}]
;;

(** {1 Midnight}

    What is offered before midnight and acted on after must act on the new day.
    See {!page-"feature-daily-notes".midnight}. *)

(** A server whose clock reads [clock], so a test can move it between offering
    and acting. *)
let start_server_with_clock ~(clock : Date.t ref) ~(vault_root : string) =
  let s = Server.create ~now:(fun () -> !clock) () in
  Server.initialize s ~root:vault_root ~init_options:(options "YYYY-MM-DD") ();
  s
;;

let show_action (a : CodeAction.t) =
  let args =
    match a.command with
    | None -> "no command"
    | Some c ->
      List.map (Option.value c.arguments ~default:[]) ~f:Yojson.Safe.to_string
      |> String.concat ~sep:" "
  in
  let edit =
    match a.edit with
    | None -> "no edit"
    | Some edit ->
      sprintf
        "%s %S"
        (String.concat ~sep:"," (document_change_kinds edit))
        (String.concat ~sep:"|" (inserted_texts edit))
  in
  printf "%-34s %-24s %s\n" a.title edit args
;;

(* Offered on the 26th, when today's note exists; picked on the 27th, when it
   does not.  The title is a day old, but the edit creates the 27th's note and
   the link names it. *)
let%expect_test "a deferred action is resolved for the day it is picked" =
  with_tmp_vault ~files (fun vault_root ->
    let clock = ref today in
    let s = start_server_with_clock ~clock ~vault_root in
    did_open s ~rel_path:"idea.md";
    let offered =
      Server.code_action
        s
        ~defer:true
        ~rel_path:"idea.md"
        ~start_line:0
        ~start_character:0
        ~end_line:0
        ~end_character:0
        ()
      |> List.filter ~f:(fun (a : CodeAction.t) -> Option.is_some a.data)
    in
    print_endline "-- offered on the 26th:";
    List.iter offered ~f:show_action;
    clock := Date.add_days today 1;
    print_endline "-- resolved on the 27th:";
    List.iter offered ~f:(fun a -> show_action (Server.resolve_code_action s a)));
  [%expect
    {|
    -- offered on the 26th:
    Open today's daily note            no edit                  no command
    Create yesterday's daily note      no edit                  no command
    Create tomorrow's daily note       no edit                  no command
    Insert link to today's daily note  no edit                  no command
    -- resolved on the 27th:
    Open today's daily note            create,text-edits ""     "journal/2026-07-27.md" false true
    Create yesterday's daily note      no edit                  "journal/2026-07-26.md" false false
    Create tomorrow's daily note       create,text-edits ""     "journal/2026-07-28.md" false true
    Insert link to today's daily note  create,text-edits "[[2026-07-27]]" no command
    |}]
;;

(* The keyboard route to a command-block line is deferred the same way. *)
let%expect_test "a command-block line's action is resolved for the day it is picked" =
  let files = ("panel.md", "# Panel\n\n```oysterlsp\ndaily/today\n```\n") :: files in
  with_tmp_vault ~files (fun vault_root ->
    let clock = ref today in
    let s = start_server_with_clock ~clock ~vault_root in
    did_open s ~rel_path:"panel.md";
    let offered =
      Server.code_action
        s
        ~defer:true
        ~rel_path:"panel.md"
        ~start_line:3
        ~start_character:0
        ~end_line:3
        ~end_character:0
        ()
    in
    List.iter offered ~f:show_action;
    clock := Date.add_days today 1;
    List.iter offered ~f:(fun a -> show_action (Server.resolve_code_action s a)));
  [%expect
    {|
    Open today's daily note            no edit                  no command
    Open today's daily note            create,text-edits ""     "journal/2026-07-27.md" false true
    |}]
;;

(* A lens cannot be resolved into anything but its command, so the command
   itself is dated when it runs: the path argument only routes it. *)
let%expect_test "the openDay command is dated when it runs" =
  with_tmp_vault ~files (fun vault_root ->
    let clock = ref (Date.add_days today 1) in
    let s = start_server_with_clock ~clock ~vault_root in
    let run arguments =
      match
        Server.execute_command s ~command:Server.daily_note_day_command ~arguments
      with
      | None -> print_endline "no intent"
      | Some { uri; create; report_unfocused } ->
        printf
          "%s create=%s report_unfocused=%b\n"
          (Server.rel_path_of_uri s uri)
          (match create with
           | None -> "none"
           | Some edit -> String.concat ~sep:"," (document_change_kinds edit))
          report_unfocused
    in
    run (Some [ `String "idea.md"; `Int 0 ]);
    run (Some [ `String "idea.md"; `Int (-1) ]);
    (* A path where the days belong is not this command's shape. *)
    run (Some [ `String "idea.md"; `String "journal/2026-07-27.md" ]));
  [%expect
    {|
    journal/2026-07-27.md create=create report_unfocused=true
    journal/2026-07-26.md create=none report_unfocused=true
    no intent
    |}]
;;

(* Anything that is not a deferred daily-note action comes back as it went. *)
let%expect_test "resolving any other action changes nothing" =
  with_tmp_vault ~files (fun vault_root ->
    let s = start_server_with_clock ~clock:(ref today) ~vault_root in
    let action =
      CodeAction.create ~title:"Something else" ~data:(`Assoc [ "x", `Int 1 ]) ()
    in
    print_s [%sexp (Poly.equal (Server.resolve_code_action s action) action : bool)]);
  [%expect {| true |}]
;;
