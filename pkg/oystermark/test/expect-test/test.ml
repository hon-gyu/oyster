open! Core
open Oystermark

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

let rec pp_inline = function
  | Cmarkit.Inline.Text (s, _) -> Printf.sprintf "Text(%s)" s
  | Cmarkit.Inline.Inlines (is, _) ->
    Printf.sprintf "Inlines[%s]" (List.map is ~f:pp_inline |> String.concat ~sep:", ")
  | Wikilink.Ext_wikilink (w, _) -> Printf.sprintf "Wikilink(%s)" (pp_wikilink w)
  | _ -> "?"
;;

let pp_block_id_result = function
  | None -> "-"
  | Some (before, id) -> Printf.sprintf "(%S, %S)" before id
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
      let w = Wikilink.parse_content ~embed:false input in
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

let scan_cases =
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

let%expect_test "scan" =
  let meta = Cmarkit.Meta.none in
  let cols =
    [ Ascii_table.Column.create "name" (fun (n, _, _) -> n)
    ; Ascii_table.Column.create "input" (fun (_, i, _) -> i)
    ; Ascii_table.Column.create "nodes" (fun (_, _, r) -> r)
    ]
  in
  let rows =
    List.map scan_cases ~f:(fun (name, input) ->
      let result =
        match Wikilink.scan input meta with
        | None -> "None"
        | Some inlines -> List.map inlines ~f:pp_inline |> String.concat ~sep:" | "
      in
      name, input, result)
  in
  print_string (Ascii_table.to_string_noattr cols rows ~limit_width_to:150);
  [%expect
    {|
    ┌──────────────┬──────────────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
    │ name         │ input                        │ nodes                                                                                    │
    ├──────────────┼──────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ no wikilinks │ hello world                  │ None                                                                                     │
    │ single       │ before [[Note]] after        │ Text(before ) | Wikilink(link target=Note frag=- display=-) | Text( after)               │
    │ multiple     │ [[A]] and [[B]]              │ Wikilink(link target=A frag=- display=-) | Text( and ) | Wikilink(link target=B frag=- d │
    │              │                              │ isplay=-)                                                                                │
    │ embed        │ ![[image.png]]               │ Wikilink(embed target=image.png frag=- display=-)                                        │
    │ unclosed     │ [[unclosed                   │ None                                                                                     │
    │ adjacent     │ [[A]][[B]]                   │ Wikilink(link target=A frag=- display=-) | Wikilink(link target=B frag=- display=-)      │
    │ with display │ see [[Note|click here]] done │ Text(see ) | Wikilink(link target=Note frag=- display=click here) | Text( done)          │
    │ block ref    │ go to [[#^abc-1]]            │ Text(go to ) | Wikilink(link target=- frag=Block_ref abc-1 display=-)                    │
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
    ; Ascii_table.Column.create "(before, id)" (fun (_, _, r) -> r)
    ]
  in
  let rows =
    List.map block_id_cases ~f:(fun (name, input) ->
      let result = pp_block_id_result (Block_id.extract_trailing input) in
      name, input, result)
  in
  print_string (Ascii_table.to_string_noattr cols rows);
  [%expect
    {|
    ┌───────────────────┬────────────────────┬──────────────────────────┐
    │ name              │ input              │ (before, id)             │
    ├───────────────────┼────────────────────┼──────────────────────────┤
    │ basic             │ Some text ^blockid │ ("Some text", "blockid") │
    │ with hyphen       │ Text ^block-id     │ ("Text", "block-id")     │
    │ no block id       │ Just text          │ -                        │
    │ invalid _         │ Text ^block_id     │ -                        │
    │ at start          │ ^blockid           │ ("", "blockid")          │
    │ trailing space    │ Text ^blockid      │ ("Text", "blockid")      │
    │ no space before ^ │ Text^blockid       │ -                        │
    │ multiple ^        │ a ^x ^final1       │ ("a ^x", "final1")       │
    └───────────────────┴────────────────────┴──────────────────────────┘
    |}]
;;
