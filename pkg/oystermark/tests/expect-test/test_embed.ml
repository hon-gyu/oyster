(** Integration tests for note embedding ([!\[\[NOTE\]\]]).

    Tests the core pipeline: parse -> resolve -> expand.  The expanded document
    is printed as CommonMark, with explicit markers for the transclusion
    boundaries carried by {!Note.Transclusion.embed_meta_key}. *)

open! Core
open Oystermark

(** Print an expanded document without depending on a rendering package. *)
let print_expanded_doc (doc : Cmarkit.Doc.t) : unit =
  let rec print_block = function
    | Cmarkit.Block.Blocks (blocks, meta) ->
      (match Cmarkit.Meta.find Note.Transclusion.embed_meta_key meta with
       | Some { depth; source_path; fragment = _ } ->
         printf "[embed depth=%d source=%s]\n" depth source_path;
         List.iter blocks ~f:print_block;
         print_endline "[/embed]"
       | None -> List.iter blocks ~f:print_block)
    | block ->
      Cmarkit.Doc.make block |> Parse.commonmark_of_doc |> String.rstrip |> print_endline
  in
  print_block (Cmarkit.Doc.block doc)
;;

(** Build a mini-vault, run the core pipeline, and print [target].
    [max_depth] controls embed recursion depth. *)
let render ?(max_depth = 5) (files : (string * string) list) (target : string) : unit =
  let docs = List.map files ~f:(fun (path, content) -> path, Parse.of_string content) in
  let index = Vault.build_index ~md_docs:docs ~other_files:[] () in
  let expanded = Vault.Embed.expand_docs ~max_depth ~index docs in
  let doc = List.Assoc.find_exn expanded ~equal:String.equal target in
  print_expanded_doc doc
;;

let%expect_test "full note" =
  render [ "a.md", "![[b]]"; "b.md", "Hello.\n\nWorld." ] "a.md";
  [%expect
    {|
    [embed depth=1 source=b.md]
    Hello.

    World.
    [/embed]
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
    [embed depth=1 source=b.md]
    ## Sec

    Content.

    [/embed]
    |}]
;;

let%expect_test "block ref" =
  render
    [ "a.md", "![[b#^myblock]]"; "b.md", "First.\n\nTarget. ^myblock\n\nAfter." ]
    "a.md";
  [%expect
    {|
    [embed depth=1 source=b.md]
    Target. ^myblock
    [/embed]
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
    [embed depth=1 source=b.md]
    > An aside.
    [/embed]
    |}]
;;

(* Inline attribute anchor: the containing paragraph is embedded. *)
let%expect_test "attribute anchor: inline span" =
  render
    [ "a.md", "![[b#kt]]"; "b.md", "Intro.\n\nThe [key term]{#kt} matters.\n" ]
    "a.md";
  [%expect
    {|
    [embed depth=1 source=b.md]
    The key term{#kt} matters.
    [/embed]
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
    [embed depth=1 source=b.md]
    B content.

    [[c]]
    [/embed]
    |}]
;;

let%expect_test "self-embed: terminates at max_depth" =
  render ~max_depth:2 [ "a.md", "![[a]]" ] "a.md";
  [%expect
    {|
    [embed depth=1 source=a.md]
    [embed depth=2 source=a.md]
    [[a]]
    [/embed]
    [/embed]
    |}]
;;

let%expect_test "mutual cycle A↔B: terminates at max_depth" =
  render ~max_depth:2 [ "a.md", "![[b]]"; "b.md", "![[a]]" ] "a.md";
  [%expect
    {|
    [embed depth=1 source=b.md]
    [embed depth=2 source=a.md]
    [[b]]
    [/embed]
    [/embed]
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
  render [ "a.md", "See ![[b]] here."; "b.md", "B." ] "a.md";
  [%expect {| See ![[b]] here. |}]
;;

let%expect_test "self-reference: embed current heading" =
  render
    ~max_depth:2
    [ "a.md", "## Intro\n\nSome text.\n\n## Section\n\nContent.\n\n![[#Intro]]" ]
    "a.md";
  [%expect
    {|
    ## Intro

    Some text.

    ## Section

    Content.

    [embed depth=1 source=a.md]
    ## Intro

    Some text.

    [/embed]
    |}]
;;

let%expect_test "self-reference: embed current block" =
  render
    ~max_depth:2
    [ "a.md", "Target paragraph. ^myid\n\nOther text.\n\n![[#^myid]]" ]
    "a.md";
  [%expect
    {|
    Target paragraph. ^myid

    Other text.

    [embed depth=1 source=a.md]
    Target paragraph. ^myid
    [/embed]
    |}]
;;

let%expect_test "self-reference: embed current file" =
  render ~max_depth:2 [ "a.md", "Hello.\n\n![[]]" ] "a.md";
  [%expect
    {|
    Hello.

    [embed depth=1 source=a.md]
    Hello.

    ![[]]
    [/embed]
    |}]
;;

(* ── Markdown image embeds ─────────────────────────────────────────── *)

let%expect_test "image embed: full note via ![](b.md)" =
  render [ "a.md", "![](b.md)"; "b.md", "Hello.\n\nWorld." ] "a.md";
  [%expect
    {|
    [embed depth=1 source=b.md]
    Hello.

    World.
    [/embed]
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
    [embed depth=1 source=b.md]
    ## Sec

    Content.

    [/embed]
    |}]
;;

let%expect_test "image embed: block ref via ![](b.md#^myblock)" =
  render
    [ "a.md", "![](b.md#^myblock)"; "b.md", "First.\n\nTarget. ^myblock\n\nAfter." ]
    "a.md";
  [%expect
    {|
    [embed depth=1 source=b.md]
    Target. ^myblock
    [/embed]
    |}]
;;

let%expect_test "image embed: max_depth=0 keeps original image" =
  render ~max_depth:0 [ "a.md", "![](b.md)"; "b.md", "Should not appear." ] "a.md";
  [%expect {| ![](b.md) |}]
;;

let%expect_test "image embed: non-note file is NOT expanded" =
  render [ "a.md", "![photo](img.png)" ] "a.md";
  [%expect {| ![photo](img.png) |}]
;;

let%expect_test "image embed: nested — image inside wikilink embed" =
  render [ "a.md", "![[b]]"; "b.md", "![](c.md)"; "c.md", "Inner content." ] "a.md";
  [%expect
    {|
    [embed depth=1 source=b.md]
    [embed depth=2 source=c.md]
    Inner content.
    [/embed]
    [/embed]
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
  let reversed = Note.Transclusion.reverse_embed_doc doc in
  print_string (Parse.commonmark_of_doc reversed)
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
    [ "a.md", "![[b#^myblock]]"; "b.md", "First.\n\nTarget. ^myblock\n\nAfter." ]
    "a.md";
  [%expect {| ![[b#^myblock]] |}]
;;

let%expect_test "reverse_embed: self-reference produces explicit path" =
  render_reversed ~max_depth:2 [ "a.md", "Hello.\n\n![[]]" ] "a.md";
  [%expect
    {|
    Hello.

    ![[a]]
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

   A block id on a keyed node names the node and everything the key claimed, so
   embedding it pulls in the label together with its whole subtree. See
   specification/oyster/struct.md. *)

let%expect_test "block ref: keyed subtree" =
  render
    [ "a.md", "![[b#^k]]"; "b.md", "Intro.\n\ntopic: ^k\n- one\n- two\n\nAfter." ]
    "a.md";
  [%expect
    {|
    [embed depth=1 source=b.md]
    topic: ^k
    - one
    - two
    [/embed]
    |}]
;;

let%expect_test "block ref: keyed list item" =
  render [ "a.md", "![[b#^k]]"; "b.md", "- other\n- topic: ^k\n  - one\n  - two" ] "a.md";
  [%expect
    {|
    [embed depth=1 source=b.md]
    topic: ^k
    - one
    - two
    [/embed]
    |}]
;;

(* The id names the outermost node of a colon chain, so the embed spans the
   whole chain rather than the inner key alone. *)
let%expect_test "block ref: keyed chain" =
  render [ "a.md", "![[b#^k]]"; "b.md", "outer: inner: ^k\n- leaf" ] "a.md";
  [%expect
    {|
    [embed depth=1 source=b.md]
    outer: inner: ^k
    - leaf
    [/embed]
    |}]
;;
