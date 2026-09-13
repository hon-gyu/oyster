(** Hover and go-to-definition must name the same anchor.

    Spec: {!page-"feature-hover"}, {!page-"feature-go-to-definition"}.
    Impl: {!Lsp_lib.Hover}, {!Lsp_lib.Go_to_definition}.

    The two use different code: go-to-definition uses
    {!Oystermark.Vault.Index.resolve}, and hover reads the note with
    {!Oystermark.Note.read}, also when resolution stops at the note. If they
    disagree, a link previews one section and jumps to another.

    Checked invariant: the first line of the hover body is the line
    go-to-definition lands on, in the same file. Lines are compared on letters
    and digits only. *)

open Core
open Lsp_helper

(** The line of [path] the definition landed on, and the first non-empty line
    of the hover body, for the link at [needle] in [rel_path]. *)
let check (s : Server.t) ~(vault_root : string) ~(rel_path : string) (needle : string) =
  let content = In_channel.read_all (Filename.concat vault_root rel_path) in
  let offset = Option.value_exn (String.substr_index content ~pattern:needle) + 2 in
  let line, character = Lsp_lib.Util.position_of_byte_offset content offset in
  let definition =
    Server.definition s ~rel_path ~line ~character |> definition_result s
  in
  let hover = Server.hover s ~rel_path ~line ~character |> hover_text in
  match definition, hover with
  | None, None -> printf "%-24s both none\n" needle
  | None, Some _ | Some _, None ->
    printf
      "%-24s DISAGREE: definition %s, hover %s\n"
      needle
      (if Option.is_some definition then "some" else "none")
      (if Option.is_some hover then "some" else "none")
  | Some { path; line; character = _ }, Some hover ->
    let hover_path, hover_body =
      let rest = Option.value_exn (String.chop_prefix hover ~prefix:"*Path*:") in
      String.lsplit2_exn rest ~on:'\n'
    in
    let hover_first_line =
      String.split_lines hover_body
      |> List.find ~f:(fun l -> not (String.is_empty (String.strip l)))
      |> Option.value ~default:""
    in
    let definition_line =
      List.nth
        (String.split_lines (In_channel.read_all (Filename.concat vault_root path)))
        line
      |> Option.value ~default:"<past end of file>"
    in
    let letters s = String.filter s ~f:Char.is_alphanum |> String.lowercase in
    let agree =
      String.equal path hover_path
      && String.equal (letters definition_line) (letters hover_first_line)
    in
    printf
      "%-24s %s %s:%d %S | hover %s %S\n"
      needle
      (if agree then "agree   " else "DISAGREE")
      path
      line
      definition_line
      hover_path
      hover_first_line
;;

let files =
  [ (* [## Section Two] is followed immediately by a list, with no blank line
       between: the shape that made the parser's heading identity and hover's
       line scan disagree. *)
    ( "note-a.md"
    , "# Alpha\n\
       - a [markdown link](#Section-Two) to a heading below.\n\n\
       ## Section Two\n\
       - a list butted against the heading.\n\n\
       Cross-file [[note-b#Section One]] and [[note-b#^block1]] and [[note-b#anchor]].\n\n\
       Whole note [[note-b]].\n\n\
       Self [[#Alpha]].\n" )
  ; ( "note-b.md"
    , "# Beta\n\n\
       ## Section One\n\n\
       Body text ^block1\n\n\
       The [key term]{#anchor} is defined here.\n" )
  ]
;;

let%expect_test "hover and definition name the same anchor" =
  with_tmp_vault ~files (fun vault_root ->
    let s = start_server ~vault_root () in
    did_open s ~rel_path:"note-a.md";
    let check = check s ~vault_root ~rel_path:"note-a.md" in
    check "(#Section-Two)";
    check "[[#Alpha]]";
    check "[[note-b#Section One]]";
    check "[[note-b#^block1]]";
    check "[[note-b#anchor]]";
    check "[[note-b]]");
  [%expect
    {|
    (#Section-Two)           agree    note-a.md:3 "## Section Two" | hover note-a.md "## Section Two"
    [[#Alpha]]               agree    note-a.md:0 "# Alpha" | hover note-a.md "# Alpha"
    [[note-b#Section One]]   agree    note-b.md:2 "## Section One" | hover note-b.md "## Section One"
    [[note-b#^block1]]       agree    note-b.md:4 "Body text ^block1" | hover note-b.md "Body text ^block1"
    [[note-b#anchor]]        agree    note-b.md:6 "The [key term]{#anchor} is defined here." | hover note-b.md "The [key term]{#anchor} is defined here."
    [[note-b]]               agree    note-b.md:0 "# Beta" | hover note-b.md "# Beta"
    |}]
;;
