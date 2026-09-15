(** The flags of [oyster block] as a syntax for a query. Impl:
    {!Oystermark.Note.Query_flags}.

    The queries themselves are tested in {!Test_query}; here only the
    translation is, printed with {!Oystermark.Note.Query.to_string}. *)

open Core
module Query = Oystermark.Note.Query
module Query_flags = Oystermark.Note.Query_flags

let none : Query_flags.t =
  { under = None
  ; direct = false
  ; kind = None
  ; lang = None
  ; attr_id = None
  ; caret_id = None
  ; key = None
  ; nth = None
  }
;;

let flags (flags : Query_flags.t) =
  match Query_flags.to_query flags with
  | Ok query -> printf "%s\n" (Query.to_string query)
  | Error message -> printf "<%s>\n" message
;;

let%expect_test "with no flag, the query is every block of the note" =
  flags none;
  [%expect {| each(self; recurse(children; emit true; descend true)) |}]
;;

let%expect_test "each flag adds a filter, and -nth keeps one of what they leave" =
  flags { none with kind = Some "code_block"; lang = Some "sh"; nth = Some 2 };
  flags { none with attr_id = Some "x"; caret_id = Some "q1"; key = Some "deps" };
  [%expect
    {|
    each(self; recurse(children; emit true; descend true)) | filter(kind = "code_block") | filter(info = "sh") | nth(2)
    each(self; recurse(children; emit true; descend true)) | filter(named {#x}) | filter(named ^q1) | filter(key = "deps")
    |}]
;;

let%expect_test "-under scopes the query to a section, -direct stops at a subheading" =
  flags { none with under = Some "Setup" };
  flags { none with under = Some "Setup"; direct = true };
  [%expect
    {|
    each(self; recurse(children; emit true; descend true)) | filter(named #setup) | section | each(self; recurse(children; emit true; descend true))
    each(self; recurse(children; emit true; descend true)) | filter(named #setup) | section(direct) | each(self; recurse(children; emit true; descend true))
    |}]
;;

let%expect_test "a kind that is not a node's kind is rejected, with the kinds that are" =
  flags { none with kind = Some "tabel" };
  [%expect
    {| <unknown kind tabel; kinds: heading, paragraph, code_block, math_block, html_block, raw_block, callout, block_quote, list, list_item, keyed, div, footnote_definition, table, definition_list, thematic_break> |}]
;;
