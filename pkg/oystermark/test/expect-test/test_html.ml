open! Core
open Oystermark

let test_index : Vault.Index.t =
  { files =
      [ { rel_path = "Note 1.md"
        ; headings =
            [ { text = "Level 3 title"; level = 3 }
            ; { text = "L2"; level = 2 }
            ; { text = "L3"; level = 3 }
            ]
        ; block_ids = [ "para1"; "block-2" ]
        }
      ; { rel_path = "Note 2.md"
        ; headings = [ { text = "Some heading"; level = 2 } ]
        ; block_ids = []
        }
      ; { rel_path = "image.png"; headings = []; block_ids = [] }
      ; { rel_path = "video.mp4"; headings = []; block_ids = [] }
      ; { rel_path = "audio.mp3"; headings = []; block_ids = [] }
      ; { rel_path = "doc.pdf"; headings = []; block_ids = [] }
      ; { rel_path = "dir/deep.md"; headings = []; block_ids = [ "d1" ] }
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
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <p><a href="/Note 2/">Note 2</a></p>
    </body>
    </html>
    |}]
;;

let%expect_test "wikilink: with display text" =
  render "[[Note 2|click here]]";
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <p><a href="/Note 2/">click here</a></p>
    </body>
    </html>
    |}]
;;

let%expect_test "wikilink: with heading fragment" =
  render "[[Note 2#Some heading]]";
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <p><a href="/Note 2/#some-heading">Note 2#Some heading</a></p>
    </body>
    </html>
    |}]
;;

let%expect_test "wikilink: with block ref" =
  render "[[Note 1#^para1]]";
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <p><a href="/Note 1/#^para1">Note 1#^para1</a></p>
    </body>
    </html>
    |}]
;;

let%expect_test "wikilink: self-reference heading" =
  render "[[#L2]]";
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <p><a href="#l2">L2</a></p>
    </body>
    </html>
    |}]
;;

let%expect_test "wikilink: self-reference block" =
  render "[[#^para1]]";
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <p><a href="#^para1">^para1</a></p>
    </body>
    </html>
    |}]
;;

let%expect_test "wikilink: unresolved" =
  render "[[nonexistent]]";
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <p><a href="#" class="unresolved">nonexistent</a></p>
    </body>
    </html>
    |}]
;;

let%expect_test "wikilink: deep path" =
  render "[[deep]]";
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <p><a href="/dir/deep/">deep</a></p>
    </body>
    </html>
    |}]
;;

(* Wikilink embeds
   ==================================================================== *)

let%expect_test "wikilink: embed image" =
  render "![[image.png]]";
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <p><img src="/image.png" alt="image.png"/></p>
    </body>
    </html>
    |}]
;;

let%expect_test "wikilink: embed video" =
  render "![[video.mp4]]";
  [%expect
    {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <p><video controls="controls"><source src="/video.mp4"/>video.mp4</video></p>
    </body>
    </html>
    |}]
;;

let%expect_test "wikilink: embed audio" =
  render "![[audio.mp3]]";
  [%expect
    {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <p><audio controls="controls"><source src="/audio.mp3"/>audio.mp3</audio></p>
    </body>
    </html>
    |}]
;;

let%expect_test "wikilink: embed pdf" =
  render "![[doc.pdf]]";
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <p><iframe src="/doc.pdf" title="doc.pdf"></iframe></p>
    </body>
    </html>
    |}]
;;

let%expect_test "wikilink: embed with display text" =
  render "![[image.png|alt text here]]";
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <p><img src="/image.png" alt="alt text here"/></p>
    </body>
    </html>
    |}]
;;

let%expect_test "wikilink: embed unresolved" =
  render "![[nonexistent.png]]";
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <p><a href="#" class="unresolved">nonexistent.png</a></p>
    </body>
    </html>
    |}]
;;

(* Standard markdown links with resolution
   ==================================================================== *)

let%expect_test "md link: resolved file" =
  render "[click](Note%202)";
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <p><a href="/Note 2/">click</a></p>
    </body>
    </html>
    |}]
;;

let%expect_test "md link: resolved with emphasis" =
  render "[**bold** link](Note%202)";
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <p><a href="/Note 2/"><strong>bold</strong> link</a></p>
    </body>
    </html>
    |}]
;;

let%expect_test "md link: unresolved" =
  render "[text](nonexistent)";
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <p><a href="#" class="unresolved">text</a></p>
    </body>
    </html>
    |}]
;;

(* Standard markdown images with resolution
   ==================================================================== *)

let%expect_test "md image: resolved" =
  render "![photo](image.png)";
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <p><img src="/image.png" alt="photo"/></p>
    </body>
    </html>
    |}]
;;

(* Block IDs
   ==================================================================== *)

let%expect_test "block-id: paragraph with block id" =
  render "Some text ^myid";
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <p id="^myid">Some text ^myid</p>
    </body>
    </html>
    |}]
;;

let%expect_test "block-id: no block id" =
  render "Some normal text";
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <p>Some normal text</p>
    </body>
    </html>
    |}]
;;

(* Mixed content
   ==================================================================== *)

let%expect_test "mixed: paragraph with wikilink and plain text" =
  render "See [[Note 2]] for details.";
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <p>See <a href="/Note 2/">Note 2</a> for details.</p>
    </body>
    </html>
    |}]
;;

let%expect_test "mixed: multiple wikilinks" =
  render "[[Note 1]] and [[Note 2]]";
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <p><a href="/Note 1/">Note 1</a> and <a href="/Note 2/">Note 2</a></p>
    </body>
    </html>
    |}]
;;

let%expect_test "plain markdown renders normally" =
  render "# Hello\n\nA paragraph with **bold** and *italic*.";
  [%expect
    {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <h1>Hello</h1>
    <p>A paragraph with <strong>bold</strong> and <em>italic</em>.</p>
    </body>
    </html>
    |}]
;;
