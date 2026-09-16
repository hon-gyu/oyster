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
  [%expect
    {|
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

(* Keyed nodes and block IDs
   ========================= *)

(* A value segment that is nothing but a block-id marker is an empty value: the
   marker becomes the node's identifier and the node claims what follows, the
   way a bare "key:" does. See specification/oyster/struct.md. *)

let keyed_block_id_cases =
  [ (* The rule itself: an id-only value empties the value and claims. *)
    "id-only value", "foo: ^x\n- bar\n- baz"
  ; "no marker (baseline)", "foo:\n- bar\n- baz"
    (* A marker ending a non-empty value identifies without emptying, so
       nothing is claimed. *)
  ; "non-empty value", "foo: v ^x\n- bar"
    (* Markers that yield no identifier leave the value alone. *)
  ; "not terminal", "foo: ^x extra"
  ; "escaped", "foo: \\^x"
    (* Rule 2: nothing to claim, so not keyed at all -- but still identified. *)
  ; "blank continuation", "foo: ^x\n\n- bar"
  ; "end of input", "foo: ^x"
    (* Chains: identifier on the outermost node, marker consumed from the
       innermost value. *)
  ; "chain", "foo: bar: ^x\n- baz"
  ; "chain, non-empty value", "foo: bar: v ^x"
    (* List items build keyed nodes on their own path. *)
  ; "list item", "- foo: ^x\n  - bar"
  ; "list item, sibling", "- foo: ^x\n- baz"
  ; "list item, non-empty", "- foo: bar ^x"
  ; "list item, blank cont.", "- foo: ^x\n\n- bar"
  ; "list item, no marker", "- foo:\n\n- bar"
  ]
;;

let rec inline_text : Cmarkit.Inline.t -> string = function
  | Cmarkit.Inline.Text (s, _) -> s
  | Cmarkit.Inline.Inlines (is, _) -> String.concat (List.map is ~f:inline_text)
  | Cmarkit.Inline.Break _ -> "\\n"
  | Cmarkit.Inline.Code_span (cs, _) -> "`" ^ Cmarkit.Inline.Code_span.code cs ^ "`"
  | _ -> "?"
;;

(* Compact shape: K(label){body} for a keyed node, P(text) for a paragraph,
   [a, b] for a list. A block identifier shows as a "#id" suffix on its node. *)
let rec block_shape (b : Cmarkit.Block.t) : string =
  let id (m : Cmarkit.Meta.t) =
    match Cmarkit.Block.Block_id.find m with
    | Some bid -> "#" ^ Cmarkit.Block.Block_id.id bid
    | None -> ""
  in
  match b with
  | Cmarkit.Block.Blocks (bs, _) ->
    String.concat ~sep:" " (List.filter_map bs ~f:non_empty_shape)
  | Cmarkit.Block.Ext_keyed ((label, body), m) ->
    sprintf "K%s(%s){%s}" (id m) (inline_text label) (block_shape body)
  | Cmarkit.Block.Paragraph (p, m) ->
    sprintf "P%s(%s)" (id m) (inline_text (Cmarkit.Block.Paragraph.inline p))
  | Cmarkit.Block.List (l, _) ->
    let items =
      List.map (Cmarkit.Block.List'.items l) ~f:(fun (item, _) ->
        block_shape (Cmarkit.Block.List_item.block item))
    in
    sprintf "[%s]" (String.concat ~sep:", " items)
  | Cmarkit.Block.Blank_line _ -> ""
  | _ -> "?"

and non_empty_shape (b : Cmarkit.Block.t) : string option =
  match block_shape b with
  | "" -> None
  | s -> Some s
;;

let show_newlines s = String.substr_replace_all s ~pattern:"\n" ~with_:"\\n"

let%expect_test "keyed_block_id" =
  let cols =
    [ Ascii_table.Column.create "name" (fun (n, _, _) -> n)
    ; Ascii_table.Column.create "input" (fun (_, i, _) -> show_newlines i)
    ; Ascii_table.Column.create "shape" (fun (_, _, s) -> s)
    ]
  in
  let rows =
    List.map keyed_block_id_cases ~f:(fun (name, input) ->
      name, input, block_shape (Cmarkit.Doc.block (Parse.of_string input)))
  in
  print_string (Ascii_table.to_string_noattr cols rows ~limit_width_to:150);
  [%expect
    {|
    ┌────────────────────────┬───────────────────────┬────────────────────────────────┐
    │ name                   │ input                 │ shape                          │
    ├────────────────────────┼───────────────────────┼────────────────────────────────┤
    │ id-only value          │ foo: ^x\n- bar\n- baz │ K#x(foo: ){[P(bar), P(baz)]}   │
    │ no marker (baseline)   │ foo:\n- bar\n- baz    │ K(foo:){[P(bar), P(baz)]}      │
    │ non-empty value        │ foo: v ^x\n- bar      │ K#x(foo: ){P(v ^x)} [P(bar)]   │
    │ not terminal           │ foo: ^x extra         │ K(foo: ){P(^x extra)}          │
    │ escaped                │ foo: \^x              │ K(foo: ){P(^x)}                │
    │ blank continuation     │ foo: ^x\n\n- bar      │ P#x(foo: ^x) [P(bar)]          │
    │ end of input           │ foo: ^x               │ P#x(foo: ^x)                   │
    │ chain                  │ foo: bar: ^x\n- baz   │ K#x(foo: ){K(bar: ){[P(baz)]}} │
    │ chain, non-empty value │ foo: bar: v ^x        │ K#x(foo: ){K(bar: ){P(v ^x)}}  │
    │ list item              │ - foo: ^x\n  - bar    │ [K#x(foo: ){[P(bar)]}]         │
    │ list item, sibling     │ - foo: ^x\n- baz      │ [P#x(foo: ^x), P(baz)]         │
    │ list item, non-empty   │ - foo: bar ^x         │ [K#x(foo: ){P(bar ^x)}]        │
    │ list item, blank cont. │ - foo: ^x\n\n- bar    │ [P#x(foo: ^x), P(bar)]         │
    │ list item, no marker   │ - foo:\n\n- bar       │ [P(foo:), P(bar)]              │
    └────────────────────────┴───────────────────────┴────────────────────────────────┘
    |}]
;;

(* The rewrite is content-invisible: rendering back to CommonMark reproduces the
   source, so an id-only marker -- which lives on no block in the keyed tree --
   must be put back by [Struct.unkey].

   Three rows below are pinned as not byte-identical to their input. None is
   specific to block IDs; each reproduces without any marker present:
   {ul
   {- ["- foo: ^x\n- baz"] re-indents, because a trailing-colon item absorbs its
      remaining siblings into a nested list. The text is preserved, the nesting
      is not -- content-invisibility holds for inlines, not block structure.}
   {- ["foo: \\^x"] loses the backslash: the CommonMark renderer does not
      re-escape ["^"], and ["\\^x"] alone renders as ["^x"] too.}
   {- The loose-list rows gain trailing blanks on their blank line.}} *)
let%expect_test "keyed_block_id_roundtrip" =
  let cols =
    [ Ascii_table.Column.create "input" (fun (i, _) -> show_newlines i)
    ; Ascii_table.Column.create "rendered" (fun (_, r) -> show_newlines r)
    ]
  in
  let rows =
    List.map keyed_block_id_cases ~f:(fun (_, input) ->
      input, String.strip (Cmarkit_commonmark.of_doc (Parse.of_string input)))
  in
  print_string (Ascii_table.to_string_noattr cols rows ~limit_width_to:150);
  [%expect
    {|
    ┌───────────────────────┬───────────────────────┐
    │ input                 │ rendered              │
    ├───────────────────────┼───────────────────────┤
    │ foo: ^x\n- bar\n- baz │ foo: ^x\n- bar\n- baz │
    │ foo:\n- bar\n- baz    │ foo:\n- bar\n- baz    │
    │ foo: v ^x\n- bar      │ foo: v ^x\n- bar      │
    │ foo: ^x extra         │ foo: ^x extra         │
    │ foo: \^x              │ foo: ^x               │
    │ foo: ^x\n\n- bar      │ foo: ^x\n\n- bar      │
    │ foo: ^x               │ foo: ^x               │
    │ foo: bar: ^x\n- baz   │ foo: bar: ^x\n- baz   │
    │ foo: bar: v ^x        │ foo: bar: v ^x        │
    │ - foo: ^x\n  - bar    │ - foo: ^x\n  - bar    │
    │ - foo: ^x\n- baz      │ - foo: ^x\n- baz      │
    │ - foo: bar ^x         │ - foo: bar ^x         │
    │ - foo: ^x\n\n- bar    │ - foo: ^x\n  \n- bar  │
    │ - foo:\n\n- bar       │ - foo:\n  \n- bar     │
    └───────────────────────┴───────────────────────┘
    |}]
;;
