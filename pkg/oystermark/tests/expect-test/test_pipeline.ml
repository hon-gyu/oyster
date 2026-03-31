open! Core
open Oystermark

let vault_root = "../data/vault/pipeline"

let%expect_test "render_vault: draft excluded" =
  let pipeline = Pipeline.exclude_drafts in
  let results =
    Oystermark.render_vault ~pipeline ~backend_blocks:true ~safe:false vault_root
  in
  let files = List.map results ~f:fst |> List.sort ~compare:String.compare in
  List.iter files ~f:(fun f -> printf "%s\n" f);
  [%expect
    {|
    home/index.html
    subdir/index.html
    subdir/note-a/index.html
    subdir/note-b/index.html
    |}]
;;

let%expect_test "render_vault: home page" =
  let results =
    Oystermark.render_vault
      ~pipeline:Pipeline.id
      ~backend_blocks:true
      ~safe:false
      vault_root
  in
  let home_html = List.Assoc.find_exn results ~equal:String.equal "home/index.html" in
  printf "%s" home_html;
  [%expect
    {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <nav class="breadcrumb"><a href="/home/">Home</a></nav><h1 id="home-page">Home Page</h1>
    </body>
    </html>
    |}]
;;

let%expect_test "render_vault: subdir index" =
  let results =
    Oystermark.render_vault
      ~pipeline:Pipeline.id
      ~backend_blocks:true
      ~safe:false
      vault_root
  in
  let index_html = List.Assoc.find_exn results ~equal:String.equal "subdir/index.html" in
  printf "%s" index_html;
  [%expect
    {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <nav class="breadcrumb"><a href="/home/">Home</a></nav><ul>
    <li><a href="/home/">home</a></li>
    <li><a href="/secret/">secret</a></li>
    <li style="list-style: none"><details><summary><a href="/subdir/">subdir</a></summary><ul>
    <li><a href="/subdir/">index</a></li>
    <li><a href="/subdir/note-a/">note-a</a></li>
    <li><a href="/subdir/note-b/">note-b</a></li>
    </ul></details></li>
    </ul><h1 id="sub-index">Sub Index</h1>
    </body>
    </html>
    |}]
;;

let%expect_test "render_vault: regular note unchanged" =
  let results =
    Oystermark.render_vault
      ~pipeline:Pipeline.id
      ~backend_blocks:true
      ~safe:false
      vault_root
  in
  let note_html =
    List.Assoc.find_exn results ~equal:String.equal "subdir/note-a/index.html"
  in
  printf "%s" note_html;
  [%expect
    {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <nav class="breadcrumb"><a href="/home/">Home</a><span class="sep">/</span><a href="/subdir/">subdir</a></nav><ul>
    <li><a href="/home/">home</a></li>
    <li><a href="/secret/">secret</a></li>
    <li style="list-style: none"><details><summary><a href="/subdir/">subdir</a></summary><ul>
    <li><a href="/subdir/">index</a></li>
    <li><a href="/subdir/note-a/">note-a</a></li>
    <li><a href="/subdir/note-b/">note-b</a></li>
    </ul></details></li>
    </ul><h1 id="note-a">Note A</h1>
    </body>
    </html>
    |}]
;;

let%expect_test "render_vault: custom pipeline can drop files" =
  let drop_home : Pipeline.t =
    Pipeline.make ~on_discover:(fun path _paths -> not (String.equal path "home.md")) ()
  in
  let pipeline = Pipeline.compose drop_home Pipeline.id in
  let results =
    Oystermark.render_vault ~pipeline ~backend_blocks:true ~safe:false vault_root
  in
  let files = List.map results ~f:fst |> List.sort ~compare:String.compare in
  List.iter files ~f:(fun f -> printf "%s\n" f);
  [%expect
    {|
    secret/index.html
    subdir/index.html
    subdir/note-a/index.html
    subdir/note-b/index.html
    |}]
;;

(* dir-resolve vault: mydir/ exists but mydir.md does not
   ==================================================================== *)

let dir_resolve_root = "../data/vault/dir-resolve"

let%expect_test "wikilink to dir-only name is unresolved" =
  let results =
    Oystermark.render_vault
      ~pipeline:Pipeline.id
      ~backend_blocks:true
      ~safe:false
      dir_resolve_root
  in
  let main_html = List.Assoc.find_exn results ~equal:String.equal "main/index.html" in
  printf "%s" main_html;
  [%expect
    {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <nav class="breadcrumb"><a href="/home/">Home</a></nav><ul>
    <li><a href="/main/">main</a></li>
    <li style="list-style: none"><details><summary><a href="/mydir/">mydir</a></summary><ul>
    <li><a href="/mydir/child/">child</a></li>
    </ul></details></li>
    </ul><div class="frontmatter"><table><tr><th>publish</th><td>true</td></tr></table></div>
    <p>Link to <a href="#" class="unresolved">mydir</a> here.</p>
    </body>
    </html>
    |}]
;;

let%expect_test "dir_index: generates index page for directory" =
  let results =
    Oystermark.render_vault
      ~pipeline:Pipeline.(dir_index ())
      ~backend_blocks:true
      ~safe:false
      dir_resolve_root
  in
  let files = List.map results ~f:fst |> List.sort ~compare:String.compare in
  List.iter files ~f:(fun f -> printf "%s\n" f);
  [%expect
    {|
    main/index.html
    mydir/child/index.html
    mydir/index.html
    |}]
;;

let%expect_test "dir_index: generated page has TOC with children" =
  let results =
    Oystermark.render_vault
      ~pipeline:Pipeline.(dir_index ())
      ~backend_blocks:true
      ~safe:false
      dir_resolve_root
  in
  let index_html = List.Assoc.find_exn results ~equal:String.equal "mydir/index.html" in
  printf "%s" index_html;
  [%expect
    {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <nav class="breadcrumb"><a href="/home/">Home</a></nav><ul>
    <li><a href="/main/">main</a></li>
    <li style="list-style: none"><details><summary><a href="/mydir/">mydir</a></summary><ul>
    <li><a href="/mydir/child/">child</a></li>
    <li><a href="/mydir/">index</a></li>
    </ul></details></li>
    </ul><ul>
    <li><a href="/mydir/child/">child</a></li>
    </ul>
    </body>
    </html>
    |}]
;;

(* transclude_code_files vault
   ==================================================================== *)

let code_embed_root = "../data/vault/code-embed"

let%expect_test "transclude_code_files: wikilink embed replaced with code block" =
  let pipeline = Pipeline.transclude_code_files in
  let results =
    Oystermark.render_vault ~pipeline ~backend_blocks:true ~safe:false code_embed_root
  in
  let html = List.Assoc.find_exn results ~equal:String.equal "note/index.html" in
  printf "%s" html;
  [%expect
    {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <nav class="breadcrumb"><a href="/home/">Home</a></nav><ul>
    <li><a href="/note/">note</a></li>
    </ul><div class="frontmatter"><table><tr><th>publish</th><td>true</td></tr></table></div>
    <h1 id="note">Note</h1>
    <pre><code class="language-py">print(&quot;hello&quot;)
    </code></pre>
    </body>
    </html>
    |}]
;;

let%expect_test "dir_index: skips dir when index.md already exists" =
  let results =
    Oystermark.render_vault
      ~pipeline:Pipeline.(dir_index ())
      ~backend_blocks:true
      ~safe:false
      vault_root
  in
  let index_html = List.Assoc.find_exn results ~equal:String.equal "subdir/index.html" in
  printf "%s" index_html;
  [%expect
    {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <nav class="breadcrumb"><a href="/home/">Home</a></nav><ul>
    <li><a href="/home/">home</a></li>
    <li><a href="/secret/">secret</a></li>
    <li style="list-style: none"><details><summary><a href="/subdir/">subdir</a></summary><ul>
    <li><a href="/subdir/">index</a></li>
    <li><a href="/subdir/note-a/">note-a</a></li>
    <li><a href="/subdir/note-b/">note-b</a></li>
    </ul></details></li>
    </ul><h1 id="sub-index">Sub Index</h1>
    </body>
    </html>
    |}]
;;
