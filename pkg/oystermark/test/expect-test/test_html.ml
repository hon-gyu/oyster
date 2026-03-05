open! Core
open Oystermark

let test_index : Index.t =
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
  }
;;

let render ?(curr_file = "Note 1.md") (md : string) : unit =
  let doc =
    Oystermark.Base.of_string md |> Oystermark.resolve ~index:test_index ~curr_file
  in
  print_string (Html.of_doc ~safe:true doc)
;;

(* Wikilinks
   ==================================================================== *)

let%expect_test "wikilink: basic file link" =
  render "[[Note 2]]";
  [%expect {| <p><a href="Note 2">Note 2</a></p> |}]
;;

let%expect_test "wikilink: with display text" =
  render "[[Note 2|click here]]";
  [%expect {| <p><a href="Note 2">click here</a></p> |}]
;;

let%expect_test "wikilink: with heading fragment" =
  render "[[Note 2#Some heading]]";
  [%expect {| <p><a href="Note 2#some-heading">Note 2#Some heading</a></p> |}]
;;

let%expect_test "wikilink: with block ref" =
  render "[[Note 1#^para1]]";
  [%expect {| <p><a href="Note 1#^para1">Note 1#^para1</a></p> |}]
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
  [%expect {| <p><a href="dir/deep">deep</a></p> |}]
;;

(* Wikilink embeds
   ==================================================================== *)

let%expect_test "wikilink: embed image" =
  render "![[image.png]]";
  [%expect {| <p><img src="image.png" alt="image.png"/></p> |}]
;;

let%expect_test "wikilink: embed video" =
  render "![[video.mp4]]";
  [%expect
    {|
    <p><video controls="controls"><source src="video.mp4"/>video.mp4</video></p>
    |}]
;;

let%expect_test "wikilink: embed audio" =
  render "![[audio.mp3]]";
  [%expect
    {|
    <p><audio controls="controls"><source src="audio.mp3"/>audio.mp3</audio></p>
    |}]
;;

let%expect_test "wikilink: embed pdf" =
  render "![[doc.pdf]]";
  [%expect {| <p><iframe src="doc.pdf" title="doc.pdf"></iframe></p> |}]
;;

let%expect_test "wikilink: embed with display text" =
  render "![[image.png|alt text here]]";
  [%expect {| <p><img src="image.png" alt="alt text here"/></p> |}]
;;

let%expect_test "wikilink: embed unresolved" =
  render "![[nonexistent.png]]";
  [%expect {| <p><a href="#" class="unresolved">nonexistent.png</a></p> |}]
;;

(* Standard markdown links with resolution
   ==================================================================== *)

let%expect_test "md link: resolved file" =
  render "[click](Note%202)";
  [%expect {| <p><a href="Note 2">click</a></p> |}]
;;

let%expect_test "md link: resolved with emphasis" =
  render "[**bold** link](Note%202)";
  [%expect {| <p><a href="Note 2"><strong>bold</strong> link</a></p> |}]
;;

let%expect_test "md link: unresolved" =
  render "[text](nonexistent)";
  [%expect {| <p><a href="#" class="unresolved">text</a></p> |}]
;;

(* Standard markdown images with resolution
   ==================================================================== *)

let%expect_test "md image: resolved" =
  render "![photo](image.png)";
  [%expect {| <p><img src="image.png" alt="photo"/></p> |}]
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
  [%expect {| <p>See <a href="Note 2">Note 2</a> for details.</p> |}]
;;

let%expect_test "mixed: multiple wikilinks" =
  render "[[Note 1]] and [[Note 2]]";
  [%expect {| <p><a href="Note 1">Note 1</a> and <a href="Note 2">Note 2</a></p> |}]
;;

let%expect_test "plain markdown renders normally" =
  render "# Hello\n\nA paragraph with **bold** and *italic*.";
  [%expect
    {|
    <h1>Hello</h1>
    <p>A paragraph with <strong>bold</strong> and <em>italic</em>.</p>
    |}]
;;
