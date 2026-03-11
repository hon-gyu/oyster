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
