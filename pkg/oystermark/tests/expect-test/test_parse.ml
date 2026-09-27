open! Core
open Oystermark

let show_newlines s = String.substr_replace_all s ~pattern:"\n" ~with_:"\\n"

(* Wikilink targets
   ================ *)

(* djot.v keeps a wikilink's target as written; oyster splits it. *)
let wikilink_cases =
  [ "basic note", "Note"
  ; "note with ext", "Note.md"
  ; "dir path", "dir/Note"
  ; "heading", "Note#Heading"
  ; "nested heading", "Note#H1#H2"
  ; "current note heading", "#Heading"
  ; "caret is a heading text", "Note#^blockid"
  ; "hash collapse", "##A###B"
  ; "surrounding blanks", " Note # H "
  ; "empty target #", "#"
  ; "empty target", ""
  ]
;;

let%expect_test "wikilink targets" =
  let cols =
    [ Ascii_table.Column.create "name" (fun (n, _, _) -> n)
    ; Ascii_table.Column.create "target" (fun (_, i, _) -> i)
    ; Ascii_table.Column.create "reference" (fun (_, _, r) ->
        Sexp.to_string_hum (Note.Link.Ref.sexp_of_t r))
    ]
  in
  let rows =
    List.map wikilink_cases ~f:(fun (name, input) ->
      name, input, Note.Link.Ref.of_wikilink_target input)
  in
  print_string (Ascii_table.to_string_noattr cols rows ~limit_width_to:150);
  [%expect
    {|
    ┌─────────────────────────┬───────────────┬───────────────────────────────────────────┐
    │ name                    │ target        │ reference                                 │
    ├─────────────────────────┼───────────────┼───────────────────────────────────────────┤
    │ basic note              │ Note          │ ((target (Note)) (fragment ()))           │
    │ note with ext           │ Note.md       │ ((target (Note.md)) (fragment ()))        │
    │ dir path                │ dir/Note      │ ((target (dir/Note)) (fragment ()))       │
    │ heading                 │ Note#Heading  │ ((target (Note)) (fragment ((Heading))))  │
    │ nested heading          │ Note#H1#H2    │ ((target (Note)) (fragment ((H1 H2))))    │
    │ current note heading    │ #Heading      │ ((target ()) (fragment ((Heading))))      │
    │ caret is a heading text │ Note#^blockid │ ((target (Note)) (fragment ((^blockid)))) │
    │ hash collapse           │ ##A###B       │ ((target ()) (fragment ((A B))))          │
    │ surrounding blanks      │  Note # H     │ ((target (Note)) (fragment ((H))))        │
    │ empty target #          │ #             │ ((target ()) (fragment ()))               │
    │ empty target            │               │ ((target ()) (fragment ()))               │
    └─────────────────────────┴───────────────┴───────────────────────────────────────────┘
    |}]
;;

let parse_cases =
  [ "no wikilinks", "hello world"
  ; "single", "before [[Note]] after"
  ; "multiple", "[[A]] and [[B]]"
  ; "embed", "![[image.png]]"
  ; "unclosed", "[[unclosed"
  ; "adjacent", "[[A]][[B]]"
  ; "with alias", "see [[Note|click here]] done"
  ]
;;

(* The first paragraph's inlines, each with its byte range. *)
let parse_inlines (input : string) : string =
  let { Parse.doc; _ } = Parse.of_string input in
  let located (node : Djot.Inline.t Djot.node) =
    let loc = Djot.Doc.textloc doc node in
    sprintf
      "%s@%d..%d"
      (Sexp.to_string (Parse.sexp_of_inline node))
      (Djot.Textloc.first_byte loc)
      (Djot.Textloc.last_byte loc)
  in
  match Djot.Doc.blocks doc with
  | Node (_, _, Para inlines) :: _ ->
    String.concat ~sep:", " (List.map inlines ~f:located)
  | _ -> "<no paragraph>"
;;

let%expect_test "parse" =
  let cols =
    [ Ascii_table.Column.create "name" (fun (n, _, _) -> n)
    ; Ascii_table.Column.create "input" (fun (_, i, _) -> i)
    ; Ascii_table.Column.create "inlines" (fun (_, _, r) -> r)
    ]
  in
  let rows =
    List.map parse_cases ~f:(fun (name, input) -> name, input, parse_inlines input)
  in
  print_string (Ascii_table.to_string_noattr cols rows ~limit_width_to:150);
  [%expect
    {|
    ┌──────────────┬──────────────────────────────┬────────────────────────────────────────────────────────────────┐
    │ name         │ input                        │ inlines                                                        │
    ├──────────────┼──────────────────────────────┼────────────────────────────────────────────────────────────────┤
    │ no wikilinks │ hello world                  │ "hello world"@0..10                                            │
    │ single       │ before [[Note]] after        │ "before "@0..6, (Wikilink Note)@7..14, " after"@15..20         │
    │ multiple     │ [[A]] and [[B]]              │ (Wikilink A)@0..4, " and "@5..9, (Wikilink B)@10..14           │
    │ embed        │ ![[image.png]]               │ (Embed image.png)@0..13                                        │
    │ unclosed     │ [[unclosed                   │ [[unclosed@0..9                                                │
    │ adjacent     │ [[A]][[B]]                   │ (Wikilink A)@0..4, (Wikilink B)@5..9                           │
    │ with alias   │ see [[Note|click here]] done │ "see "@0..3, (Wikilink Note"click here")@4..22, " done"@23..27 │
    └──────────────┴──────────────────────────────┴────────────────────────────────────────────────────────────────┘
    |}]
;;

(* Keyed blocks
   ============ *)

(* djot.v's rules for [label: content], see its [.project/keyed-blocks.md]. *)
let keyed_cases =
  [ "inline value", "foo: bar"
  ; "claims a list", "foo:\n- bar\n- baz"
  ; "value then list", "foo: v\n- bar"
  ; "blank line ends it", "foo:\n\n- bar"
  ; "chain", "foo: bar:\n- baz"
  ; "list item", "- foo:\n  - bar"
  ; "list item, sibling", "- foo:\n- baz"
  ; "list item, inline value", "- foo: bar"
  ; "code label", "`x`: value"
  ; "emphasis label", "_x_: value"
  ; "two inlines are no label", "a b: value"
  ; "escaped colon", "foo\\: bar"
  ]
;;

let%expect_test "keyed" =
  let cols =
    [ Ascii_table.Column.create "name" (fun (n, _, _) -> n)
    ; Ascii_table.Column.create "input" (fun (_, i, _) -> show_newlines i)
    ; Ascii_table.Column.create "tree" (fun (_, _, s) -> s)
    ]
  in
  let rows =
    List.map keyed_cases ~f:(fun (name, input) ->
      ( name
      , input
      , Sexp.to_string (Parse.sexp_of_doc (Parse.of_string ~locs:false input).doc) ))
  in
  print_string (Ascii_table.to_string_noattr cols rows ~limit_width_to:150);
  [%expect
    {|
    ┌──────────────────────────┬────────────────────┬──────────────────────────────────────────────────────────────────────┐
    │ name                     │ input              │ tree                                                                 │
    ├──────────────────────────┼────────────────────┼──────────────────────────────────────────────────────────────────────┤
    │ inline value             │ foo: bar           │ ((Keyed(label foo)(Para bar)))                                       │
    │ claims a list            │ foo:\n- bar\n- baz │ ((Keyed(label foo)(Bullet_list(item(Para bar))(item(Para baz)))))    │
    │ value then list          │ foo: v\n- bar      │ ((Keyed(label foo)(Para v))(Bullet_list(item(Para bar))))            │
    │ blank line ends it       │ foo:\n\n- bar      │ ((Para foo:)(Bullet_list(item(Para bar))))                           │
    │ chain                    │ foo: bar:\n- baz   │ ((Keyed(label foo)(Para bar:))(Bullet_list(item(Para baz))))         │
    │ list item                │ - foo:\n  - bar    │ ((Bullet_list(item(Keyed(label foo)(Bullet_list(item(Para bar))))))) │
    │ list item, sibling       │ - foo:\n- baz      │ ((Bullet_list(item(Para foo:))(item(Para baz))))                     │
    │ list item, inline value  │ - foo: bar         │ ((Bullet_list(item(Keyed(label foo)(Para bar)))))                    │
    │ code label               │ `x`: value         │ ((Keyed(label(Verbatim x))(Para value)))                             │
    │ emphasis label           │ _x_: value         │ ((Keyed(label(Emph x))(Para value)))                                 │
    │ two inlines are no label │ a b: value         │ ((Keyed(label"a b")(Para value)))                                    │
    │ escaped colon            │ foo\: bar          │ ((Para"foo: bar"))                                                   │
    └──────────────────────────┴────────────────────┴──────────────────────────────────────────────────────────────────────┘
    |}]
;;

(* Rendering back to source and parsing that again gives the same tree. *)
let%expect_test "keyed round trip" =
  List.iter keyed_cases ~f:(fun (name, input) ->
    let tree s = Parse.sexp_of_doc (Parse.of_string ~locs:false s).doc in
    let rendered = Djot.Source.of_doc (Parse.of_string ~locs:false input).doc in
    if not (Sexp.equal (tree input) (tree rendered))
    then printf "%s: %s -> %s\n" name (show_newlines input) (show_newlines rendered));
  [%expect {| |}]
;;
