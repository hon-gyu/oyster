(** Spec: {!page-"feature-toc"}.  Impl: {!Lsp_lib.Toc}, surfaced by
    {!Lsp_lib.Server.code_action} and the diagnostics the sync handlers
    return.

    The unit-level behaviour — scanning, generation, staleness — is pinned by
    the inline tests in {!Lsp_lib.Toc}.  What is checked here is the part only
    the server does: that the diagnostic lands on the marker line, that the
    quick fix's {e range} is right (applying it yields the generated TOC and
    keeps the fences), and that the insert action appears outside a region
    and not inside one. *)

open Core
open Linol_lsp.Lsp.Types
open Lsp_helper

let start = "::: toc"
let stop = ":::"

(** A vault of one note, opened with [content] in its buffer, so that
    everything below answers against the text the test wrote.  See
    {!page-"feature-toc".frame}. *)
let with_note (content : string) (f : Server.t -> unit) : unit =
  with_tmp_vault
    ~files:[ "note.md", content ]
    (fun root ->
       let s = start_server ~vault_root:root () in
       ignore (Server.did_open s ~rel_path:"note.md" ~content : Diagnostic.t list);
       f s)
;;

let show_diagnostics (content : string) : unit =
  with_note content (fun s ->
    Server.did_change s ~rel_path:"note.md" ~content
    |> diagnostic_positions
    |> List.iter ~f:(fun (message, line, character) ->
      printf "%d:%d %s\n" line character message))
;;

(** The table-of-contents actions offered over the whole of [line], with the
    buffer each one's edit produces when applied.

    Titles are filtered because the [Refactor] menu always carries the
    daily-note family (see {!page-"feature-daily-notes"}), which has nothing
    to do with this spec and would drift with the clock. *)
let show_actions ?only (content : string) ~(line : int) : unit =
  with_note content (fun s ->
    let end_character =
      match List.nth (String.split_lines content) line with
      | Some l -> String.length l
      | None -> 0
    in
    Server.code_action
      s
      ?only
      ~rel_path:"note.md"
      ~start_line:line
      ~start_character:0
      ~end_line:line
      ~end_character
      ()
    |> List.filter ~f:(fun (a : CodeAction.t) ->
      String.is_substring a.title ~substring:"table of contents")
    |> List.iter ~f:(fun (action : CodeAction.t) ->
      print_endline action.title;
      match action.edit with
      | None -> ()
      | Some edit ->
        List.iter (text_edits edit) ~f:(fun ((range : Range.t), new_text) ->
          let byte (p : Position.t) =
            Lsp_lib.Util.byte_offset_of_position
              content
              ~line:p.line
              ~character:p.character
          in
          print_string
            (apply_edits
               content
               [ { rel_path = "note.md"
                 ; first_byte = byte range.start
                 ; last_byte = byte range.end_
                 ; new_text
                 }
               ]))))
;;

let%expect_test "a stale region is reported on its opening fence line" =
  show_diagnostics (String.concat_lines [ "# Alpha"; ""; start; stop; ""; "## Beta" ]);
  [%expect {| 2:0 table of contents is out of date |}]
;;

let%expect_test "an up-to-date region is quiet" =
  show_diagnostics
    (String.concat_lines
       [ "# Alpha"
       ; ""
       ; start
       ; "- [Alpha](#Alpha)"
       ; "  - [Beta](#Beta)"
       ; stop
       ; ""
       ; "## Beta"
       ]);
  [%expect {| |}]
;;

let%expect_test "a note without a toc div is quiet" =
  show_diagnostics (String.concat_lines [ "# Alpha"; ""; "## Beta" ]);
  [%expect {| |}]
;;

let%expect_test "an unterminated opening fence is reported" =
  show_diagnostics (String.concat_lines [ start; ""; "# Alpha" ]);
  [%expect {| 0:0 unterminated table of contents: missing a closing ::: fence |}]
;;

let%expect_test "quick fix rewrites the body and keeps the fences" =
  show_actions
    ~only:[ CodeActionKind.QuickFix ]
    (String.concat_lines [ "# Alpha"; ""; start; "stale"; stop; ""; "## Beta" ])
    ~line:2;
  [%expect
    {|
    Update table of contents
    # Alpha

    ::: toc
    - [Alpha](#Alpha)
      - [Beta](#Beta)
    :::

    ## Beta
    |}]
;;

let%expect_test "no quick fix over an up-to-date region" =
  show_actions
    ~only:[ CodeActionKind.QuickFix ]
    (String.concat_lines [ "# Alpha"; ""; start; "- [Alpha](#Alpha)"; stop ])
    ~line:2;
  [%expect {| |}]
;;

let%expect_test "insert action at the cursor's line" =
  show_actions
    ~only:[ CodeActionKind.Refactor ]
    (String.concat_lines [ "# Alpha"; ""; "prose"; ""; "## Beta" ])
    ~line:2;
  [%expect
    {|
    Insert table of contents
    # Alpha

    ::: toc
    - [Alpha](#Alpha)
      - [Beta](#Beta)
    :::
    prose

    ## Beta
    |}]
;;

let%expect_test "no insert action inside a region" =
  show_actions
    ~only:[ CodeActionKind.Refactor ]
    (String.concat_lines [ start; "- [Alpha](#Alpha)"; stop; ""; "# Alpha" ])
    ~line:1;
  [%expect {| |}]
;;
