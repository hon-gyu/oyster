open! Core
open Oystermark

let test_index : Vault.Index.t =
  { files =
      [ { rel_path = "Note 1.md"
        ; headings =
            [ { text = "Level 3 title"; level = 3; slug = "level-3-title"; loc = None }
            ; { text = "L2"; level = 2; slug = "l2"; loc = None }
            ; { text = "L3"; level = 3; slug = "l3"; loc = None }
            ]
        ; blocks = [ { id = "para1"; loc = None }; { id = "block-2"; loc = None } ]
        }
      ; { rel_path = "Note 2.md"
        ; headings =
            [ { text = "Some heading"; level = 2; slug = "some-heading"; loc = None } ]
        ; blocks = []
        }
      ; { rel_path = "image.png"; headings = []; blocks = [] }
      ; { rel_path = "video.mp4"; headings = []; blocks = [] }
      ; { rel_path = "audio.mp3"; headings = []; blocks = [] }
      ; { rel_path = "doc.pdf"; headings = []; blocks = [] }
      ; { rel_path = "dir/deep.md"
        ; headings = []
        ; blocks = [ { id = "d1"; loc = None } ]
        }
      ]
  ; dirs = []
  }
;;

let render ?(curr_file = "Note 1.md") (md : string) : unit =
  let doc = Oystermark.Parse.of_string md in
  let mapper = Vault.Resolve.resolution_cmarkit_mapper ~index:test_index ~curr_file in
  let resolved = Cmarkit.Mapper.map_doc mapper doc in
  print_string (Html.of_doc ~backend_blocks:true ~safe:false resolved)
;;

(* Wikilinks
   ==================================================================== *)

let%expect_test "wikilink: basic file link" =
  render "[[Note 2]]";
  [%expect {| <p><a href="/Note 2/">Note 2</a></p> |}]
;;

let%expect_test "wikilink: with display text" =
  render "[[Note 2|click here]]";
  [%expect {| <p><a href="/Note 2/">click here</a></p> |}]
;;

let%expect_test "wikilink: with heading fragment" =
  render "[[Note 2#Some heading]]";
  [%expect {| <p><a href="/Note 2/#some-heading">Note 2#Some heading</a></p> |}]
;;

let%expect_test "wikilink: with block ref" =
  render "[[Note 1#^para1]]";
  [%expect {| <p><a href="/Note 1/#^para1">Note 1#^para1</a></p> |}]
;;

let%expect_test "wikilink: self-reference heading" =
  render "[[#L2]]";
  [%expect {| <p><a href="#l2">L2</a></p> |}]
;;

let%expect_test "wikilink: self-reference block" =
  render "[[#^para1]]";
  [%expect {| <p><a href="#^para1">^para1</a></p> |}]
;;

let%expect_test "wikilink: unresolved" =
  render "[[nonexistent]]";
  [%expect {| <p><a href="#" class="unresolved">nonexistent</a></p> |}]
;;

let%expect_test "wikilink: deep path" =
  render "[[deep]]";
  [%expect {| <p><a href="/dir/deep/">deep</a></p> |}]
;;

(* Wikilink embeds
   ==================================================================== *)

let%expect_test "wikilink: embed image" =
  render "![[image.png]]";
  [%expect {| <p><a href="/image.png"><img src="/image.png" alt="image.png"/></a></p> |}]
;;

let%expect_test "wikilink: embed video" =
  render "![[video.mp4]]";
  [%expect
    {| <p><video controls="controls"><source src="/video.mp4"/>video.mp4</video></p> |}]
;;

let%expect_test "wikilink: embed audio" =
  render "![[audio.mp3]]";
  [%expect
    {| <p><audio controls="controls"><source src="/audio.mp3"/>audio.mp3</audio></p> |}]
;;

let%expect_test "wikilink: embed pdf" =
  render "![[doc.pdf]]";
  [%expect {| <p><iframe src="/doc.pdf" title="doc.pdf"></iframe></p> |}]
;;

let%expect_test "wikilink: embed with display text" =
  render "![[image.png|alt text here]]";
  [%expect
    {| <p><a href="/image.png"><img src="/image.png" alt="alt text here"/></a></p> |}]
;;

let%expect_test "wikilink: embed unresolved" =
  render "![[nonexistent.png]]";
  [%expect {| <p><a href="#" class="unresolved">nonexistent.png</a></p> |}]
;;

(* Standard markdown links with resolution
   ==================================================================== *)

let%expect_test "md link: resolved file" =
  render "[click](Note%202)";
  [%expect {| <p><a href="/Note 2/">click</a></p> |}]
;;

let%expect_test "md link: resolved with emphasis" =
  render "[**bold** link](Note%202)";
  [%expect {| <p><a href="/Note 2/"><strong>bold</strong> link</a></p> |}]
;;

let%expect_test "md link: unresolved" =
  render "[text](nonexistent)";
  [%expect {| <p><a href="#" class="unresolved">text</a></p> |}]
;;

(* Standard markdown images with resolution
   ==================================================================== *)

let%expect_test "md image: resolved" =
  render "![photo](image.png)";
  [%expect {| <p><a href="/image.png"><img src="/image.png" alt="photo"/></a></p> |}]
;;

(* Block IDs
   ==================================================================== *)

let%expect_test "block-id: paragraph with block id" =
  render "Some text ^myid";
  [%expect {| <p id="^myid">Some text ^myid</p> |}]
;;

let%expect_test "block-id: no block id" =
  render "Some normal text";
  [%expect {| <p>Some normal text</p> |}]
;;

(* Mixed content
   ==================================================================== *)

let%expect_test "mixed: paragraph with wikilink and plain text" =
  render "See [[Note 2]] for details.";
  [%expect {| <p>See <a href="/Note 2/">Note 2</a> for details.</p> |}]
;;

let%expect_test "mixed: multiple wikilinks" =
  render "[[Note 1]] and [[Note 2]]";
  [%expect {| <p><a href="/Note 1/">Note 1</a> and <a href="/Note 2/">Note 2</a></p> |}]
;;

let%expect_test "plain markdown renders normally" =
  render "# Hello\n\nA paragraph with **bold** and *italic*.";
  [%expect
    {|
    <h1 id="hello">Hello</h1>
    <p>A paragraph with <strong>bold</strong> and <em>italic</em>.</p>
    |}]
;;

(* Callouts
   ==================================================================== *)

let%expect_test "callout: basic" =
  render "> [!info] My Title\n> Body text here.";
  [%expect
    {|
    <div class="callout" data-callout="info">
    <div class="callout-title">My Title</div>
    <div class="callout-content">
    <p>Body text here.</p>
    </div>
    </div>
    |}]
;;

let%expect_test "callout: default title" =
  render "> [!tip]\n> Some content.";
  [%expect
    {|
    <div class="callout" data-callout="tip">
    <div class="callout-title">Tip</div>
    <div class="callout-content">
    <p>Some content.</p>
    </div>
    </div>
    |}]
;;

let%expect_test "callout: foldable closed" =
  render "> [!faq]- Are callouts foldable?\n> Yes they are.";
  [%expect
    {|
    <details class="callout" data-callout="faq">
    <summary class="callout-title">Are callouts foldable?</summary>
    <div class="callout-content">
    <p>Yes they are.</p>
    </div>
    </details>
    |}]
;;

let%expect_test "callout: foldable open" =
  render "> [!note]+ Expanded\n> Content here.";
  [%expect
    {|
    <details class="callout" data-callout="note" open>
    <summary class="callout-title">Expanded</summary>
    <div class="callout-content">
    <p>Content here.</p>
    </div>
    </details>
    |}]
;;

let%expect_test "callout: title only" =
  render "> [!tip] Title only callout";
  [%expect
    {|
    <div class="callout" data-callout="tip">
    <div class="callout-title">Title only callout</div>
    <div class="callout-content">
    </div>
    </div>
    |}]
;;

let%expect_test "callout: not a callout" =
  render "> Just a normal blockquote.";
  [%expect
    {|
    <blockquote>
    <p>Just a normal blockquote.</p>
    </blockquote>
    |}]
;;

(* Div
   ==================================================================== *)

let%expect_test "div: basic with class" =
  render
    {|::: warning
Here is a paragraph.

And here is another.
:::|};
  [%expect
    {|
    <div class="warning">
    <p>Here is a paragraph.</p>
    <p>And here is another.</p>
    </div>
    |}]
;;

let%expect_test "div: no class" =
  render
    {|:::
content
:::|};
  [%expect
    {|
    <div>
    <p>content</p>
    </div>
    |}]
;;

let%expect_test "div: nested" =
  render
    {|:::: outer
::: inner
content
:::
::::|};
  [%expect
    {|
    <div class="outer">
    <div class="inner">
    <p>content</p>
    </div>
    </div>
    |}]
;;
