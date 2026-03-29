open! Core
open Oystermark
module Wikilink = Parse.Wikilink
module Block_id = Parse.Block_id

(* Pretty-printing helpers
-------------------- *)

let pp_textloc (meta : Cmarkit.Meta.t) =
  let loc = Cmarkit.Meta.textloc meta in
  if Cmarkit.Textloc.is_none loc
  then "none"
  else
    Printf.sprintf
      "%d..%d"
      (Cmarkit.Textloc.first_byte loc)
      (Cmarkit.Textloc.last_byte loc)
;;

let rec pp_inline = function
  | Cmarkit.Inline.Text (s, m) -> Printf.sprintf "Text(%s @%s)" s (pp_textloc m)
  | Cmarkit.Inline.Inlines (is, m) ->
    Printf.sprintf
      "Inlines(@%s)[%s]"
      (pp_textloc m)
      (List.map is ~f:pp_inline |> String.concat ~sep:", ")
  | Wikilink.Ext_wikilink (w, m) ->
    Printf.sprintf
      "Wikilink(%s @%s)"
      (Wikilink.sexp_of_t w |> Sexp.to_string_hum)
      (pp_textloc m)
  | _ -> "?"
;;

(* Expect tests
==================== *)

let wikilink_cases =
  [ "basic note", "Note"
  ; "note with ext", "Note.md"
  ; "dir path", "dir/Note"
  ; "display text", "Note|custom text"
  ; "heading", "Note#Heading"
  ; "nested heading", "Note#H1#H2"
  ; "current note heading", "#Heading"
  ; "block ref", "Note#^blockid"
  ; "block ref hyphen", "Note#^block-id"
  ; "invalid block_id _", "Note#^block_id"
  ; "block ref current", "#^blockid"
  ; "heading + display", "#H1#H2|text"
  ; "embed", "Note"
  ; "hash collapse", "##A###B"
  ; "heading then ^block", "Note#H1#^blockid"
  ; "empty target #", "#"
  ; "empty target", ""
  ]
;;

let%expect_test "parse_content" =
  let cols =
    [ Ascii_table.Column.create "name" (fun (n, _, _) -> n)
    ; Ascii_table.Column.create "input" (fun (_, i, _) -> i)
    ; Ascii_table.Column.create "result" (fun (_, _, w) ->
        Wikilink.sexp_of_t w |> Sexp.to_string_hum)
    ]
  in
  let rows =
    List.map wikilink_cases ~f:(fun (name, input) ->
      let w = Wikilink.make ~embed:false input in
      name, input, w)
  in
  print_string (Ascii_table.to_string_noattr cols rows ~limit_width_to:150);
  [%expect
    {|
    ┌──────────────────────┬──────────────────┬───────────────────────────────────────────────────────────────────────────────┐
    │ name                 │ input            │ result                                                                        │
    ├──────────────────────┼──────────────────┼───────────────────────────────────────────────────────────────────────────────┤
    │ basic note           │ Note             │ ((target (Note)) (fragment ()) (display ()) (embed false))                    │
    │ note with ext        │ Note.md          │ ((target (Note.md)) (fragment ()) (display ()) (embed false))                 │
    │ dir path             │ dir/Note         │ ((target (dir/Note)) (fragment ()) (display ()) (embed false))                │
    │ display text         │ Note|custom text │ ((target (Note)) (fragment ()) (display ("custom text")) (embed false))       │
    │ heading              │ Note#Heading     │ ((target (Note)) (fragment ((Heading (Heading)))) (display ()) (embed false)) │
    │ nested heading       │ Note#H1#H2       │ ((target (Note)) (fragment ((Heading (H1 H2)))) (display ()) (embed false))   │
    │ current note heading │ #Heading         │ ((target ()) (fragment ((Heading (Heading)))) (display ()) (embed false))     │
    │ block ref            │ Note#^blockid    │ ((target (Note)) (fragment ((Block_ref blockid))) (display ()) (embed false)) │
    │ block ref hyphen     │ Note#^block-id   │ ((target (Note)) (fragment ((Block_ref block-id))) (display ())               │
    │                      │                  │  (embed false))                                                               │
    │ invalid block_id _   │ Note#^block_id   │ ((target (Note)) (fragment ((Heading (^block_id)))) (display ())              │
    │                      │                  │  (embed false))                                                               │
    │ block ref current    │ #^blockid        │ ((target ()) (fragment ((Block_ref blockid))) (display ()) (embed false))     │
    │ heading + display    │ #H1#H2|text      │ ((target ()) (fragment ((Heading (H1 H2)))) (display (text)) (embed false))   │
    │ embed                │ Note             │ ((target (Note)) (fragment ()) (display ()) (embed false))                    │
    │ hash collapse        │ ##A###B          │ ((target ()) (fragment ((Heading (A B)))) (display ()) (embed false))         │
    │ heading then ^block  │ Note#H1#^blockid │ ((target (Note)) (fragment ((Heading (H1 ^blockid)))) (display ())            │
    │                      │                  │  (embed false))                                                               │
    │ empty target #       │ #                │ ((target ()) (fragment ()) (display ()) (embed false))                        │
    │ empty target         │                  │ ((target ()) (fragment ()) (display ()) (embed false))                        │
    └──────────────────────┴──────────────────┴───────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let parse_cases =
  [ "no wikilinks", "hello world"
  ; "single", "before [[Note]] after"
  ; "multiple", "[[A]] and [[B]]"
  ; "embed", "![[image.png]]"
  ; "unclosed", "[[unclosed"
  ; "adjacent", "[[A]][[B]]"
  ; "with display", "see [[Note|click here]] done"
  ; "block ref", "go to [[#^abc-1]]"
  ]
;;

(** Build a [Text] node with a textloc starting at byte [base] spanning [input]. *)
let text_node ~base input =
  let len = String.length input in
  let loc =
    Cmarkit.Textloc.v
      ~file:Cmarkit.Textloc.file_none
      ~first_byte:base
      ~last_byte:(base + len - 1)
      ~first_line:Cmarkit.Textloc.line_pos_first
      ~last_line:Cmarkit.Textloc.line_pos_first
  in
  Cmarkit.Inline.Text (input, Cmarkit.Meta.make ~textloc:loc ())
;;

let dummy_mapper = Cmarkit.Mapper.make ~inline_ext_default:(fun _m i -> Some i) ()

let%expect_test "parse" =
  let cols =
    [ Ascii_table.Column.create "name" (fun (n, _, _) -> n)
    ; Ascii_table.Column.create "input" (fun (_, i, _) -> i)
    ; Ascii_table.Column.create "nodes" (fun (_, _, r) -> r)
    ]
  in
  let rows =
    List.map parse_cases ~f:(fun (name, input) ->
      let node = text_node ~base:0 input in
      let result =
        match Wikilink.parse dummy_mapper node with
        | `Default -> "Default"
        | `Map None -> "Deleted"
        | `Map (Some inline) -> pp_inline inline
      in
      name, input, result)
  in
  print_string (Ascii_table.to_string_noattr cols rows ~limit_width_to:150);
  [%expect
    {|
    ┌──────────────┬──────────────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
    │ name         │ input                        │ nodes                                                                                    │
    ├──────────────┼──────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ no wikilinks │ hello world                  │ Default                                                                                  │
    │ single       │ before [[Note]] after        │ Inlines(@0..20)[Text(before  @0..6), Wikilink(((target (Note)) (fragment ()) (display () │
    │              │                              │ ) (embed false)) @7..14), Text( after @15..20)]                                          │
    │ multiple     │ [[A]] and [[B]]              │ Inlines(@0..14)[Wikilink(((target (A)) (fragment ()) (display ()) (embed false)) @0..4), │
    │              │                              │  Text( and  @5..9), Wikilink(((target (B)) (fragment ()) (display ()) (embed false)) @10 │
    │              │                              │ ..14)]                                                                                   │
    │ embed        │ ![[image.png]]               │ Inlines(@0..13)[Wikilink(((target (image.png)) (fragment ()) (display ()) (embed true))  │
    │              │                              │ @0..13)]                                                                                 │
    │ unclosed     │ [[unclosed                   │ Inlines(@0..9)[Text([[unclosed @0..9)]                                                   │
    │ adjacent     │ [[A]][[B]]                   │ Inlines(@0..9)[Wikilink(((target (A)) (fragment ()) (display ()) (embed false)) @0..4),  │
    │              │                              │ Wikilink(((target (B)) (fragment ()) (display ()) (embed false)) @5..9)]                 │
    │ with display │ see [[Note|click here]] done │ Inlines(@0..27)[Text(see  @0..3), Wikilink(((target (Note)) (fragment ()) (display ("cli │
    │              │                              │ ck here")) (embed false)) @4..22), Text( done @23..27)]                                  │
    │ block ref    │ go to [[#^abc-1]]            │ Inlines(@0..16)[Text(go to  @0..5), Wikilink(((target ()) (fragment ((Block_ref abc-1))) │
    │              │                              │  (display ()) (embed false)) @6..16)]                                                    │
    └──────────────┴──────────────────────────────┴──────────────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let block_id_cases =
  [ "basic", "Some text ^blockid"
  ; "with hyphen", "Text ^block-id"
  ; "no block id", "Just text"
  ; "invalid _", "Text ^block_id"
  ; "at start", "^blockid"
  ; "trailing space", "Text ^blockid  "
  ; "no space before ^", "Text^blockid"
  ; "multiple ^", "a ^x ^final1"
  ]
;;

let%expect_test "block_id" =
  let cols =
    [ Ascii_table.Column.create "name" (fun (n, _, _) -> n)
    ; Ascii_table.Column.create "input" (fun (_, i, _) -> i)
    ; Ascii_table.Column.create "result" (fun (_, _, r) ->
        Option.value_map r ~default:"-" ~f:(fun bid ->
          Block_id.sexp_of_t bid |> Sexp.to_string_hum))
    ]
  in
  let rows =
    List.map block_id_cases ~f:(fun (name, input) -> name, input, Block_id.make_opt input)
  in
  print_string (Ascii_table.to_string_noattr cols rows);
  [%expect
    {|
    ┌───────────────────┬────────────────────┬──────────────────────────────┐
    │ name              │ input              │ result                       │
    ├───────────────────┼────────────────────┼──────────────────────────────┤
    │ basic             │ Some text ^blockid │ ((id blockid) (byte_pos 10)) │
    │ with hyphen       │ Text ^block-id     │ ((id block-id) (byte_pos 5)) │
    │ no block id       │ Just text          │ -                            │
    │ invalid _         │ Text ^block_id     │ -                            │
    │ at start          │ ^blockid           │ ((id blockid) (byte_pos 0))  │
    │ trailing space    │ Text ^blockid      │ ((id blockid) (byte_pos 5))  │
    │ no space before ^ │ Text^blockid       │ ((id blockid) (byte_pos 4))  │
    │ multiple ^        │ a ^x ^final1       │ ((id final1) (byte_pos 5))   │
    └───────────────────┴────────────────────┴──────────────────────────────┘
    |}]
;;
