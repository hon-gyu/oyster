open! Core
open Oystermark

(* Pretty-printing helpers
-------------------- *)

let pp_textloc ppf (meta : Cmarkit.Meta.t) =
  let loc = Cmarkit.Meta.textloc meta in
  if Cmarkit.Textloc.is_none loc
  then Format.pp_print_string ppf "none"
  else
    Format.fprintf
      ppf
      "%d..%d"
      (Cmarkit.Textloc.first_byte loc)
      (Cmarkit.Textloc.last_byte loc)
;;

let rec pp_inline ppf = function
  | Cmarkit.Inline.Text (s, m) -> Format.fprintf ppf "Text(%s @%a)" s pp_textloc m
  | Cmarkit.Inline.Inlines (is, m) ->
    Format.fprintf
      ppf
      "Inlines(@%a)[%a]"
      pp_textloc
      m
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") pp_inline)
      is
  | Cmarkit.Inline.Ext_wikilink (w, m) ->
    Format.fprintf
      ppf
      "Wikilink(%s @%a)"
      (Parse.Common.sexp_of_wikilink w |> Sexp.to_string_hum)
      pp_textloc
      m
  | _ -> Format.pp_print_string ppf "?"
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
        Parse.Common.sexp_of_wikilink w |> Sexp.to_string_hum)
    ]
  in
  let rows =
    List.map wikilink_cases ~f:(fun (name, input) ->
      let w = Cmarkit.Inline.Wikilink.make ~embed:false input in
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
    │ invalid block_id _   │ Note#^block_id   │ ((target (Note)) (fragment ((Block_ref block_id))) (display ())               │
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

(* Parse a single-line input via the fork's native [~wikilink] parsing and
   pretty-print the resulting paragraph inline. *)
let parse_wikilinks (input : string) : string =
  let doc = Cmarkit.Doc.of_string ~wikilink:true ~locs:true input in
  let rec first_paragraph (b : Cmarkit.Block.t) : Cmarkit.Inline.t option =
    match b with
    | Cmarkit.Block.Paragraph (p, _) -> Some (Cmarkit.Block.Paragraph.inline p)
    | Cmarkit.Block.Blocks (bs, _) -> List.find_map bs ~f:first_paragraph
    | _ -> None
  in
  match first_paragraph (Cmarkit.Doc.block doc) with
  | Some inline -> Format.asprintf "%a" pp_inline inline
  | None -> "<empty>"
;;

let%expect_test "parse" =
  let cols =
    [ Ascii_table.Column.create "name" (fun (n, _, _) -> n)
    ; Ascii_table.Column.create "input" (fun (_, i, _) -> i)
    ; Ascii_table.Column.create "nodes" (fun (_, _, r) -> r)
    ]
  in
  let rows =
    List.map parse_cases ~f:(fun (name, input) -> name, input, parse_wikilinks input)
  in
  print_string (Ascii_table.to_string_noattr cols rows ~limit_width_to:150);
  [%expect
    {|
    ┌──────────────┬──────────────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
    │ name         │ input                        │ nodes                                                                                    │
    ├──────────────┼──────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ no wikilinks │ hello world                  │ Text(hello world @0..10)                                                                 │
    │ single       │ before [[Note]] after        │ Inlines(@0..20)[Text(before  @0..6), Wikilink(((target (Note)) (fragment ()) (display () │
    │              │                              │ ) (embed false)) @7..14), Text( after @15..20)]                                          │
    │ multiple     │ [[A]] and [[B]]              │ Inlines(@0..14)[Wikilink(((target (A)) (fragment ()) (display ()) (embed false)) @0..4), │
    │              │                              │  Text( and  @5..9), Wikilink(((target (B)) (fragment ()) (display ()) (embed false)) @10 │
    │              │                              │ ..14)]                                                                                   │
    │ embed        │ ![[image.png]]               │ Wikilink(((target (image.png)) (fragment ()) (display ()) (embed true)) @0..13)          │
    │ unclosed     │ [[unclosed                   │ Text([[unclosed @0..9)                                                                   │
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

(* Block IDs are parsed natively by the fork via the [~block_id] knob; report
   the id found on the (first) paragraph's metadata. *)
let find_block_id (md : string) : string option =
  let doc = Cmarkit.Doc.of_string ~block_id:true md in
  let folder =
    Cmarkit.Folder.make
      ~block:(fun _f acc -> function
        | Cmarkit.Block.Paragraph (_p, meta) ->
          (match Cmarkit.Block.Block_id.find meta with
           | Some bid -> Cmarkit.Folder.ret (Some (Cmarkit.Block.Block_id.id bid))
           | None -> Cmarkit.Folder.default)
        | _ -> Cmarkit.Folder.default)
      ()
  in
  Cmarkit.Folder.fold_doc folder None doc
;;

let%expect_test "block_id" =
  let cols =
    [ Ascii_table.Column.create "name" (fun (n, _, _) -> n)
    ; Ascii_table.Column.create "input" (fun (_, i, _) -> i)
    ; Ascii_table.Column.create "result" (fun (_, _, r) -> Option.value r ~default:"-")
    ]
  in
  let rows =
    List.map block_id_cases ~f:(fun (name, input) -> name, input, find_block_id input)
  in
  print_string (Ascii_table.to_string_noattr cols rows);
  [%expect {|
    ┌───────────────────┬────────────────────┬──────────┐
    │ name              │ input              │ result   │
    ├───────────────────┼────────────────────┼──────────┤
    │ basic             │ Some text ^blockid │ blockid  │
    │ with hyphen       │ Text ^block-id     │ block-id │
    │ no block id       │ Just text          │ -        │
    │ invalid _         │ Text ^block_id     │ -        │
    │ at start          │ ^blockid           │ blockid  │
    │ trailing space    │ Text ^blockid      │ blockid  │
    │ no space before ^ │ Text^blockid       │ blockid  │
    │ multiple ^        │ a ^x ^final1       │ final1   │
    └───────────────────┴────────────────────┴──────────┘
    |}]
;;
