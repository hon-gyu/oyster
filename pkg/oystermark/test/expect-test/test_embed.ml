(** Integration tests for note embedding (![[NOTE]]).

    Tests the full pipeline: parse → resolve → expand → render HTML.
    Uses {!Html.of_doc} for rendering — no custom test helpers. *)

open! Core
open Oystermark

(** Build a mini-vault, run the full pipeline, render [target] to HTML.
    [max_depth] controls embed recursion depth. *)
let render ?(max_depth = 5) (files : (string * string) list) (target : string) : unit =
  let docs = List.map files ~f:(fun (path, content) -> path, Parse.of_string content) in
  let index = Vault.build_index ~md_docs:docs ~other_files:[] ~dirs:[] in
  let resolved = Vault.Resolve.resolve_docs docs index in
  let expanded = Vault.Embed.expand_docs ~max_depth resolved in
  let doc = List.Assoc.find_exn expanded ~equal:String.equal target in
  print_string (Html.of_doc ~backend_blocks:true ~safe:false doc)
;;

let%expect_test "full note" =
  render [ "a.md", "![[b]]"; "b.md", "Hello.\n\nWorld." ] "a.md";
  [%expect
    {|
    <div class="embed" data-embed-depth="1">
    <p>Hello.</p>
    <p>World.</p>
    </div>
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
    <div class="embed" data-embed-depth="1">
    <h2 id="sec">Sec</h2>
    <p>Content.</p>
    </div>
    |}]
;;

let%expect_test "block ref" =
  render
    [ "a.md", "![[b#^myblock]]"; "b.md", "First.\n\nTarget. ^myblock\n\nAfter." ]
    "a.md";
  [%expect
    {|
    <div class="embed" data-embed-depth="1">
    <p id="^myblock">Target. ^myblock</p>
    </div>
    |}]
;;

let%expect_test "max_depth=0: fallback link" =
  render ~max_depth:0 [ "a.md", "![[b]]"; "b.md", "Should not appear." ] "a.md";
  [%expect {| <p><a href="/b/">b</a></p> |}]
;;

let%expect_test "max_depth=1: inner embed becomes link" =
  render
    ~max_depth:1
    [ "a.md", "![[b]]"; "b.md", "B content.\n\n![[c]]"; "c.md", "C content." ]
    "a.md";
  [%expect
    {|
    <div class="embed" data-embed-depth="1">
    <p>B content.</p>
    <p><a href="/c/">c</a></p>
    </div>
    |}]
;;

let%expect_test "self-embed: terminates at max_depth" =
  render ~max_depth:2 [ "a.md", "![[a]]" ] "a.md";
  [%expect
    {|
    <div class="embed" data-embed-depth="1">
    <div class="embed" data-embed-depth="2">
    <p><a href="/a/">a</a></p>
    </div>
    </div>
    |}]
;;

let%expect_test "mutual cycle A↔B: terminates at max_depth" =
  render ~max_depth:2 [ "a.md", "![[b]]"; "b.md", "![[a]]" ] "a.md";
  [%expect
    {|
    <div class="embed" data-embed-depth="1">
    <div class="embed" data-embed-depth="2">
    <p><a href="/b/">b</a></p>
    </div>
    </div>
    |}]
;;

let%expect_test "unresolved embed stays as unresolved link" =
  render [ "a.md", "![[no-such-note]]" ] "a.md";
  [%expect {| <p><a href="#" class="unresolved">no-such-note</a></p> |}]
;;

let%expect_test "media embed is rendered by HTML (not expanded)" =
  render [ "a.md", "![[img.png]]" ] "a.md";
  [%expect {| <p><a href="#" class="unresolved">img.png</a></p> |}]
;;

let%expect_test "non-embed wikilink is unchanged" =
  render [ "a.md", "[[b]]"; "b.md", "B content." ] "a.md";
  [%expect {| <p><a href="/b/">b</a></p> |}]
;;

let%expect_test "embed mixed with other content stays as paragraph" =
  render [ "a.md", "See ![[b]] here."; "b.md", "B." ] "a.md";
  [%expect {| <p>See <a href="/b/">b</a> here.</p> |}]
;;

let%expect_test "self-reference: embed current heading" =
  render
    ~max_depth:2
    [ "a.md", "## Intro\n\nSome text.\n\n## Section\n\nContent.\n\n![[#Intro]]" ]
    "a.md";
  [%expect {|
    <h2 id="intro">Intro</h2>
    <p>Some text.</p>
    <h2 id="section">Section</h2>
    <p>Content.</p>
    <div class="embed" data-embed-depth="1">
    <h2 id="intro">Intro</h2>
    <p>Some text.</p>
    </div>
    |}]
;;

let%expect_test "self-reference: embed current block" =
  render
    ~max_depth:2
    [ "a.md", "Target paragraph. ^myid\n\nOther text.\n\n![[#^myid]]" ]
    "a.md";
  [%expect {|
    <p id="^myid">Target paragraph. ^myid</p>
    <p>Other text.</p>
    <div class="embed" data-embed-depth="1">
    <p id="^myid">Target paragraph. ^myid</p>
    </div>
    |}]
;;

let%expect_test "self-reference: embed current file" =
  render ~max_depth:2 [ "a.md", "Hello.\n\n![[]]" ] "a.md";
  [%expect {|
    <p>Hello.</p>
    <div class="embed" data-embed-depth="1">
    <p>Hello.</p>
    <p><a href=""></a></p>
    </div>
    |}]
;;

(* ── Markdown image embeds ─────────────────────────────────────────── *)

let%expect_test "image embed: full note via ![](b.md)" =
  render [ "a.md", "![](b.md)"; "b.md", "Hello.\n\nWorld." ] "a.md";
  [%expect
    {|
    <div class="embed" data-embed-depth="1">
    <p>Hello.</p>
    <p>World.</p>
    </div>
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
    <div class="embed" data-embed-depth="1">
    <h2 id="sec">Sec</h2>
    <p>Content.</p>
    </div>
    |}]
;;

let%expect_test "image embed: block ref via ![](b.md#^myblock)" =
  render
    [ "a.md", "![](b.md#^myblock)"; "b.md", "First.\n\nTarget. ^myblock\n\nAfter." ]
    "a.md";
  [%expect
    {|
    <div class="embed" data-embed-depth="1">
    <p id="^myblock">Target. ^myblock</p>
    </div>
    |}]
;;

let%expect_test "image embed: max_depth=0 keeps original image" =
  render ~max_depth:0 [ "a.md", "![](b.md)"; "b.md", "Should not appear." ] "a.md";
  [%expect {| <p><a href="/b/"><img src="/b/" alt=""/></a></p> |}]
;;

let%expect_test "image embed: non-note file is NOT expanded" =
  render [ "a.md", "![photo](img.png)" ] "a.md";
  [%expect {| <p><a href="#"><img src="#" alt="photo"/></a></p> |}]
;;

let%expect_test "image embed: nested — image inside wikilink embed" =
  render
    [ "a.md", "![[b]]"; "b.md", "![](c.md)"; "c.md", "Inner content." ]
    "a.md";
  [%expect
    {|
    <div class="embed" data-embed-depth="1">
    <div class="embed" data-embed-depth="2">
    <p>Inner content.</p>
    </div>
    </div>
    |}]
;;

(* ── reverse_embed ─────────────────────────────────────────────────── *)

(** Expand then reverse: the reversed doc should reproduce the embed syntax. *)
let render_reversed
      ?(max_depth = 5)
      (files : (string * string) list)
      (target : string)
  : unit
  =
  let docs = List.map files ~f:(fun (path, content) -> path, Parse.of_string content) in
  let index = Vault.build_index ~md_docs:docs ~other_files:[] ~dirs:[] in
  let resolved = Vault.Resolve.resolve_docs docs index in
  let expanded = Vault.Embed.expand_docs ~max_depth resolved in
  let doc = List.Assoc.find_exn expanded ~equal:String.equal target in
  let reversed = Vault.Embed.reverse_embed_doc doc in
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
  [%expect {|
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
