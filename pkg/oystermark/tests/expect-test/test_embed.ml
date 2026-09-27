(** Integration tests for note embedding ([!\[\[NOTE\]\]]).

    Tests the core pipeline: parse -> resolve -> expand. The expanded document
    is printed as djot; the transclusion boundaries need no markers of
    their own, since {!Note.Transclusion.transclude} writes them into the text
    as a div and its attribute. *)

open! Core
open Oystermark

(** Print an expanded document without depending on a rendering package. *)
let print_expanded_doc (note : Note.t) : unit = print_string (Djot.Source.of_doc note.doc)

(** Build a mini-vault, run the core pipeline, and print [target].
    [max_depth] controls embed recursion depth. *)
let render ?(max_depth = 5) (files : (string * string) list) (target : string) : unit =
  let docs = List.map files ~f:(fun (path, content) -> path, Parse.of_string content) in
  let index = Vault.build_index ~md_docs:docs ~other_files:[] () in
  let expanded = Vault.Embed.expand_docs ~max_depth ~index docs in
  print_expanded_doc (List.Assoc.find_exn expanded ~equal:String.equal target)
;;

let%expect_test "full note" =
  render [ "a.md", "![[b]]"; "b.md", "Hello.\n\nWorld." ] "a.md";
  [%expect
    {|
    {source="b.md" depth="1"}
    ::: embed
    Hello\.

    World\.
    :::
    |}]
;;

let%expect_test "heading section" =
  render
    [ "a.md", "![[b#Sec]]"
    ; "b.md", "Intro.\n\n## Sec\n\nContent.\n\n## Other\n\nNot this."
    ]
    "a.md";
  [%expect
    {|
    {source="b.md" fragment="Sec" depth="1"}
    ::: embed
    ## Sec

    Content\.
    :::
    |}]
;;

let%expect_test "block ref" =
  render
    [ "a.md", "![[b#myblock]]"; "b.md", "First.\n\n{#myblock}\nTarget.\n\nAfter." ]
    "a.md";
  [%expect
    {|
    {source="b.md" fragment="myblock" depth="1"}
    ::: embed
    {#myblock}
    Target\.
    :::
    |}]
;;

(* Embed an attribute anchor: block attribute ({#note}) wrapping a blockquote.
   See {!page-"feature-attribute-anchors"}. *)
let%expect_test "attribute anchor: block" =
  render
    [ "a.md", "![[b#note]]"; "b.md", "Intro.\n\n{#note}\n> An aside.\n\nAfter." ]
    "a.md";
  [%expect
    {|
    {source="b.md" fragment="note" depth="1"}
    ::: embed
    {#note}
    > An aside\.
    :::
    |}]
;;

(* Inline attribute anchor: the containing paragraph is embedded. *)
let%expect_test "attribute anchor: inline span" =
  render
    [ "a.md", "![[b#kt]]"; "b.md", "Intro.\n\nThe [key term]{#kt} matters.\n" ]
    "a.md";
  [%expect
    {|
    {source="b.md" fragment="kt" depth="1"}
    ::: embed
    The [key term]{#kt} matters\.
    :::
    |}]
;;

let%expect_test "max_depth=0: fallback link" =
  render ~max_depth:0 [ "a.md", "![[b]]"; "b.md", "Should not appear." ] "a.md";
  [%expect {| [[b]] |}]
;;

let%expect_test "max_depth=1: inner embed becomes link" =
  render
    ~max_depth:1
    [ "a.md", "![[b]]"; "b.md", "B content.\n\n![[c]]"; "c.md", "C content." ]
    "a.md";
  [%expect
    {|
    {source="b.md" depth="1"}
    ::: embed
    B content\.

    [[c]]
    :::
    |}]
;;

let%expect_test "self-embed: terminates at max_depth" =
  render ~max_depth:2 [ "a.md", "![[a]]" ] "a.md";
  [%expect
    {|
    {source="a.md" depth="1"}
    ::: embed
    ![[a]]
    :::
    |}]
;;

let%expect_test "mutual cycle A↔B: terminates at max_depth" =
  render ~max_depth:2 [ "a.md", "![[b]]"; "b.md", "![[a]]" ] "a.md";
  [%expect
    {|
    {source="b.md" depth="1"}
    :::: embed
    {source="a.md" depth="2"}
    ::: embed
    ![[b]]
    :::
    ::::
    |}]
;;

let%expect_test "unresolved embed stays as unresolved link" =
  render [ "a.md", "![[no-such-note]]" ] "a.md";
  [%expect {| ![[no-such-note]] |}]
;;

let%expect_test "media embed is not expanded" =
  render [ "a.md", "![[img.png]]" ] "a.md";
  [%expect {| ![[img.png]] |}]
;;

let%expect_test "non-embed wikilink is unchanged" =
  render [ "a.md", "[[b]]"; "b.md", "B content." ] "a.md";
  [%expect {| [[b]] |}]
;;

let%expect_test "embed mixed with other content stays as paragraph" =
  render [ "a.md", "See ![[b]] here."; "b.md", "B text." ] "a.md";
  [%expect {| See ![[b]] here\. |}]
;;

let%expect_test "self-reference: embed current heading" =
  render
    ~max_depth:2
    [ "a.md", "## Intro\n\nSome text.\n\n## Section\n\nContent.\n\n![[#Intro]]" ]
    "a.md";
  [%expect
    {|
    ## Intro

    Some text\.

    ## Section

    Content\.

    {source="a.md" fragment="Intro" depth="1"}
    ::: embed
    ## Intro

    Some text\.
    :::
    |}]
;;

let%expect_test "self-reference: embed current block" =
  render
    ~max_depth:2
    [ "a.md", "{#myid}\nTarget paragraph.\n\nOther text.\n\n![[#myid]]" ]
    "a.md";
  [%expect
    {|
    {#myid}
    Target paragraph\.

    Other text\.

    {source="a.md" fragment="myid" depth="1"}
    ::: embed
    {#myid}
    Target paragraph\.
    :::
    |}]
;;

let%expect_test "self-reference: embed current file" =
  render ~max_depth:2 [ "a.md", "Hello.\n\n![[]]" ] "a.md";
  [%expect
    {|
    Hello\.

    \!\[\[\]\]
    |}]
;;

(* ── Markdown image embeds ─────────────────────────────────────────── *)

let%expect_test "image embed: full note via ![](b.md)" =
  render [ "a.md", "![](b.md)"; "b.md", "Hello.\n\nWorld." ] "a.md";
  [%expect
    {|
    {source="b.md" depth="1"}
    ::: embed
    Hello\.

    World\.
    :::
    |}]
;;

let%expect_test "image embed: heading section via ![](b.md#Sec)" =
  render
    [ "a.md", "![](b.md#Sec)"
    ; "b.md", "Intro.\n\n## Sec\n\nContent.\n\n## Other\n\nNot this."
    ]
    "a.md";
  [%expect
    {|
    {source="b.md" fragment="Sec" depth="1"}
    ::: embed
    ## Sec

    Content\.
    :::
    |}]
;;

let%expect_test "image embed: block ref via ![](b.md#myblock)" =
  render
    [ "a.md", "![](b.md#myblock)"; "b.md", "First.\n\n{#myblock}\nTarget.\n\nAfter." ]
    "a.md";
  [%expect
    {|
    {source="b.md" fragment="myblock" depth="1"}
    ::: embed
    {#myblock}
    Target\.
    :::
    |}]
;;

let%expect_test "image embed: max_depth=0 keeps original image" =
  render ~max_depth:0 [ "a.md", "![](b.md)"; "b.md", "Should not appear." ] "a.md";
  [%expect {| ![](b\.md) |}]
;;

let%expect_test "image embed: non-note file is NOT expanded" =
  render [ "a.md", "![photo](img.png)" ] "a.md";
  [%expect {| ![photo](img\.png) |}]
;;

let%expect_test "image embed: nested — image inside wikilink embed" =
  render [ "a.md", "![[b]]"; "b.md", "![](c.md)"; "c.md", "Inner content." ] "a.md";
  [%expect
    {|
    {source="b.md" depth="1"}
    :::: embed
    {source="c.md" depth="2"}
    ::: embed
    Inner content\.
    :::
    ::::
    |}]
;;

(* ── reverse_embed ─────────────────────────────────────────────────── *)

(** Expand then reverse: the reversed doc should reproduce the embed syntax. *)
let render_reversed ?(max_depth = 5) (files : (string * string) list) (target : string)
  : unit
  =
  let docs = List.map files ~f:(fun (path, content) -> path, Parse.of_string content) in
  let index = Vault.build_index ~md_docs:docs ~other_files:[] () in
  let expanded = Vault.Embed.expand_docs ~max_depth ~index docs in
  let doc = List.Assoc.find_exn expanded ~equal:String.equal target in
  let reversed = Note.Transclusion.reverse_embed_doc doc.doc in
  print_string (Djot.Source.of_doc reversed)
;;

let%expect_test "reverse_embed: full note" =
  render_reversed [ "a.md", "![[b]]"; "b.md", "Hello." ] "a.md";
  [%expect {| ![[b]] |}]
;;

let%expect_test "reverse_embed: heading section" =
  render_reversed
    [ "a.md", "![[b#Sec]]"
    ; "b.md", "Intro.\n\n## Sec\n\nContent.\n\n## Other\n\nNot this."
    ]
    "a.md";
  [%expect {| ![[b#Sec]] |}]
;;

let%expect_test "reverse_embed: block ref" =
  render_reversed
    [ "a.md", "![[b#myblock]]"; "b.md", "First.\n\n{#myblock}\nTarget.\n\nAfter." ]
    "a.md";
  [%expect {| ![[b#myblock]] |}]
;;

let%expect_test "reverse_embed: self-reference produces explicit path" =
  render_reversed ~max_depth:2 [ "a.md", "Hello.\n\n![[]]" ] "a.md";
  [%expect
    {|
    Hello\.

    \!\[\[\]\]
    |}]
;;

let%expect_test "reverse_embed: nested embeds reversed recursively" =
  render_reversed
    [ "a.md", "![[b]]"; "b.md", "B text.\n\n![[c]]"; "c.md", "C text." ]
    "a.md";
  [%expect {| ![[b]] |}]
;;

let%expect_test "reverse_embed: image embed reversed to wikilink" =
  render_reversed [ "a.md", "![](b.md)"; "b.md", "Hello." ] "a.md";
  [%expect {| ![[b]] |}]
;;

(* Keyed nodes as anchors
   ======================

   An attribute on a keyed node names the node, so embedding it pulls in the
   label together with its value. *)

let%expect_test "block ref: keyed subtree" =
  render
    [ "a.md", "![[b#k]]"; "b.md", "Intro.\n\n{#k}\ntopic:\n- one\n- two\n\nAfter." ]
    "a.md";
  [%expect
    {|
    {source="b.md" fragment="k" depth="1"}
    ::: embed
    {#k}
    topic:
    - one
    - two
    :::
    |}]
;;

let%expect_test "block ref: keyed list item" =
  render
    [ "a.md", "![[b#k]]"; "b.md", "- other\n- {#k}\n  topic:\n  - one\n  - two" ]
    "a.md";
  [%expect
    {|
    {source="b.md" fragment="k" depth="1"}
    ::: embed
    {#k}
    topic:
    - one
    - two
    :::
    |}]
;;

(* In djot.v a keyed node's value is one block: the paragraph [inner:] here,
   so the list after it is not part of what the id names. *)
let%expect_test "block ref: keyed chain" =
  render [ "a.md", "![[b#k]]"; "b.md", "{#k}\nouter: inner:\n- leaf" ] "a.md";
  [%expect
    {|
    {source="b.md" fragment="k" depth="1"}
    ::: embed
    {#k}
    outer:
    inner\:
    :::
    |}]
;;

(* Round trip through text
   ======================= *)

(** Expand, render to Markdown, parse the Markdown back, and report the
    transclusions the reparsed document still knows about. Nothing carries over
    in memory, so whatever is printed came from the text alone. *)
let round_trip ?(max_depth = 5) (files : (string * string) list) (target : string) : unit =
  let docs = List.map files ~f:(fun (path, content) -> path, Parse.of_string content) in
  let index = Vault.build_index ~md_docs:docs ~other_files:[] () in
  let expanded = Vault.Embed.expand_docs ~max_depth ~index docs in
  let text = Parse.to_string (List.Assoc.find_exn expanded ~equal:String.equal target) in
  print_endline (String.rstrip text);
  print_endline "--- read back out of the text ---";
  let reparsed = (Parse.of_string text).doc in
  let folder =
    Djot.Folder.make
      ~block:(fun _ () block ->
        Option.iter
          (Note.Transclusion.embed_meta_of_block block)
          ~f:(fun { depth; source_path; fragment } ->
            printf
              "depth=%d source=%s fragment=%s\n"
              depth
              source_path
              (Option.value_map fragment ~default:"-" ~f:(String.concat ~sep:"#")));
        Djot.Folder.default)
      ()
  in
  Djot.Folder.fold_doc folder () reparsed;
  print_endline "--- reversed from text alone ---";
  print_string (Djot.Source.of_doc (Note.Transclusion.reverse_embed_doc reparsed))
;;

let%expect_test "round trip: full note" =
  round_trip [ "a.md", "![[b]]"; "b.md", "Hello.\n\nWorld." ] "a.md";
  [%expect
    {|
    {source="b.md" depth="1"}
    ::: embed
    Hello\.

    World\.
    :::
    --- read back out of the text ---
    depth=1 source=b.md fragment=-
    --- reversed from text alone ---
    ![[b]]
    |}]
;;

let%expect_test "round trip: heading fragment" =
  round_trip [ "a.md", "![[b#Sec]]"; "b.md", "## Sec\n\nContent." ] "a.md";
  [%expect
    {|
    {source="b.md" fragment="Sec" depth="1"}
    ::: embed
    ## Sec

    Content\.
    :::
    --- read back out of the text ---
    depth=1 source=b.md fragment=Sec
    --- reversed from text alone ---
    ![[b#Sec]]
    |}]
;;

let%expect_test "round trip: block ref fragment" =
  round_trip [ "a.md", "![[b#myblock]]"; "b.md", "{#myblock}\nTarget." ] "a.md";
  [%expect
    {|
    {source="b.md" fragment="myblock" depth="1"}
    ::: embed
    {#myblock}
    Target\.
    :::
    --- read back out of the text ---
    depth=1 source=b.md fragment=myblock
    --- reversed from text alone ---
    ![[b#myblock]]
    |}]
;;

let%expect_test "round trip: nested" =
  round_trip
    ~max_depth:2
    [ "a.md", "![[b]]"; "b.md", "B text.\n\n![[c]]"; "c.md", "C text." ]
    "a.md";
  [%expect
    {|
    {source="b.md" depth="1"}
    :::: embed
    B text\.

    {source="c.md" depth="2"}
    ::: embed
    C text\.
    :::
    ::::
    --- read back out of the text ---
    depth=1 source=b.md fragment=-
    depth=2 source=c.md fragment=-
    --- reversed from text alone ---
    ![[b]]
    |}]
;;

(* A path and a heading that the djot attribute cannot write bare: both hold
   spaces, and the heading holds the quote and backslash the attribute escapes.
   Everything below came back out of the text. *)
let%expect_test "round trip: values needing escaping" =
  round_trip
    [ "a.md", "![[my notes/a note#It's \"quoted\", \\ tricky]]"
    ; "my notes/a note.md", "## It's \"quoted\", \\ tricky\n\nBody."
    ]
    "a.md";
  [%expect
    {|
    {source="my notes/a note.md" fragment="It’s quoted, tricky" depth="1"}
    ::: embed
    ## It’s {"quoted"}, \ tricky

    Body\.
    :::
    --- read back out of the text ---
    depth=1 source=my notes/a note.md fragment=It’s quoted, tricky
    --- reversed from text alone ---
    ![[my notes/a note#It’s quoted, tricky]]
    |}]
;;
