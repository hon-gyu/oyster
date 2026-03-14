open! Core
open Oystermark
module Wikilink = Parse.Wikilink
module Block_id = Parse.Block_id

(* Pretty-printing helpers
-------------------- *)

let pp_fragment : Wikilink.fragment -> string = function
  | Wikilink.Heading hs -> "Heading [" ^ String.concat ~sep:"; " hs ^ "]"
  | Wikilink.Block_ref s -> "Block_ref " ^ s
;;

let pp_wikilink (w : Wikilink.t) =
  let parts =
    [ (if w.embed then "embed" else "link")
    ; "target=" ^ Option.value ~default:"-" w.target
    ; "frag=" ^ Option.value_map ~default:"-" ~f:pp_fragment w.fragment
    ; "display=" ^ Option.value ~default:"-" w.display
    ]
  in
  String.concat ~sep:" " parts
;;

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
    Printf.sprintf "Wikilink(%s @%s)" (pp_wikilink w) (pp_textloc m)
  | _ -> "?"
;;

let pp_block_id_result = function
  | None -> "-"
  | Some (bid : Block_id.t) -> Printf.sprintf "(byte_pos=%d, %S)" bid.byte_pos bid.id
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
    [ Ascii_table.Column.create "name" (fun (n, _, _, _, _) -> n)
    ; Ascii_table.Column.create "input" (fun (_, i, _, _, _) -> i)
    ; Ascii_table.Column.create "target" (fun (_, _, t, _, _) -> t)
    ; Ascii_table.Column.create "frag" (fun (_, _, _, f, _) -> f)
    ; Ascii_table.Column.create "display" (fun (_, _, _, _, d) -> d)
    ]
  in
  let rows =
    List.map wikilink_cases ~f:(fun (name, input) ->
      let w = Wikilink.make ~embed:false input in
      ( name
      , input
      , Option.value ~default:"-" w.target
      , Option.value_map ~default:"-" ~f:pp_fragment w.fragment
      , Option.value ~default:"-" w.display ))
  in
  print_string (Ascii_table.to_string_noattr cols rows ~limit_width_to:120);
  [%expect
    {|
    ┌──────────────────────┬──────────────────┬──────────┬────────────────────────┬─────────────┐
    │ name                 │ input            │ target   │ frag                   │ display     │
    ├──────────────────────┼──────────────────┼──────────┼────────────────────────┼─────────────┤
    │ basic note           │ Note             │ Note     │ -                      │ -           │
    │ note with ext        │ Note.md          │ Note.md  │ -                      │ -           │
    │ dir path             │ dir/Note         │ dir/Note │ -                      │ -           │
    │ display text         │ Note|custom text │ Note     │ -                      │ custom text │
    │ heading              │ Note#Heading     │ Note     │ Heading [Heading]      │ -           │
    │ nested heading       │ Note#H1#H2       │ Note     │ Heading [H1; H2]       │ -           │
    │ current note heading │ #Heading         │ -        │ Heading [Heading]      │ -           │
    │ block ref            │ Note#^blockid    │ Note     │ Block_ref blockid      │ -           │
    │ block ref hyphen     │ Note#^block-id   │ Note     │ Block_ref block-id     │ -           │
    │ invalid block_id _   │ Note#^block_id   │ Note     │ Heading [^block_id]    │ -           │
    │ block ref current    │ #^blockid        │ -        │ Block_ref blockid      │ -           │
    │ heading + display    │ #H1#H2|text      │ -        │ Heading [H1; H2]       │ text        │
    │ embed                │ Note             │ Note     │ -                      │ -           │
    │ hash collapse        │ ##A###B          │ -        │ Heading [A; B]         │ -           │
    │ heading then ^block  │ Note#H1#^blockid │ Note     │ Heading [H1; ^blockid] │ -           │
    │ empty target #       │ #                │ -        │ -                      │ -           │
    │ empty target         │                  │ -        │ -                      │ -           │
    └──────────────────────┴──────────────────┴──────────┴────────────────────────┴─────────────┘
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
    │ single       │ before [[Note]] after        │ Inlines(@0..20)[Text(before  @0..6), Wikilink(link target=Note frag=- display=- @7..14), │
    │              │                              │  Text( after @15..20)]                                                                   │
    │ multiple     │ [[A]] and [[B]]              │ Inlines(@0..14)[Wikilink(link target=A frag=- display=- @0..4), Text( and  @5..9), Wikil │
    │              │                              │ ink(link target=B frag=- display=- @10..14)]                                             │
    │ embed        │ ![[image.png]]               │ Inlines(@0..13)[Wikilink(embed target=image.png frag=- display=- @0..13)]                │
    │ unclosed     │ [[unclosed                   │ Inlines(@0..9)[Text([[unclosed @0..9)]                                                   │
    │ adjacent     │ [[A]][[B]]                   │ Inlines(@0..9)[Wikilink(link target=A frag=- display=- @0..4), Wikilink(link target=B fr │
    │              │                              │ ag=- display=- @5..9)]                                                                   │
    │ with display │ see [[Note|click here]] done │ Inlines(@0..27)[Text(see  @0..3), Wikilink(link target=Note frag=- display=click here @4 │
    │              │                              │ ..22), Text( done @23..27)]                                                              │
    │ block ref    │ go to [[#^abc-1]]            │ Inlines(@0..16)[Text(go to  @0..5), Wikilink(link target=- frag=Block_ref abc-1 display= │
    │              │                              │ - @6..16)]                                                                               │
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
    ; Ascii_table.Column.create "(byte_pos, id)" (fun (_, _, r) -> r)
    ]
  in
  let rows =
    List.map block_id_cases ~f:(fun (name, input) ->
      let result = pp_block_id_result (Block_id.make_opt input) in
      name, input, result)
  in
  print_string (Ascii_table.to_string_noattr cols rows);
  [%expect
    {|
    ┌───────────────────┬────────────────────┬──────────────────────────┐
    │ name              │ input              │ (byte_pos, id)           │
    ├───────────────────┼────────────────────┼──────────────────────────┤
    │ basic             │ Some text ^blockid │ (byte_pos=10, "blockid") │
    │ with hyphen       │ Text ^block-id     │ (byte_pos=5, "block-id") │
    │ no block id       │ Just text          │ -                        │
    │ invalid _         │ Text ^block_id     │ -                        │
    │ at start          │ ^blockid           │ (byte_pos=0, "blockid")  │
    │ trailing space    │ Text ^blockid      │ (byte_pos=5, "blockid")  │
    │ no space before ^ │ Text^blockid       │ (byte_pos=4, "blockid")  │
    │ multiple ^        │ a ^x ^final1       │ (byte_pos=5, "final1")   │
    └───────────────────┴────────────────────┴──────────────────────────┘
    |}]
;;
