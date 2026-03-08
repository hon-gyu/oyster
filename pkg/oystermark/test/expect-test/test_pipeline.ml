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
    home.md
    subdir/index.md
    subdir/note-a.md
    subdir/note-b.md
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
  let home_html = List.Assoc.find_exn results ~equal:String.equal "home.md" in
  printf "%s" home_html;
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <h1>Home Page</h1>
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
  let index_html = List.Assoc.find_exn results ~equal:String.equal "subdir/index.md" in
  printf "%s" index_html;
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <h1>Sub Index</h1>
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
  let note_html = List.Assoc.find_exn results ~equal:String.equal "subdir/note-a.md" in
  printf "%s" note_html;
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <h1>Note A</h1>
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
    secret.md
    subdir/index.md
    subdir/note-a.md
    subdir/note-b.md
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
  let main_html = List.Assoc.find_exn results ~equal:String.equal "main.md" in
  printf "%s" main_html;
  [%expect
    {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <div class="frontmatter"><table><tr><th>publish</th><td>true</td></tr></table></div>
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
    main.md
    mydir/child.md
    mydir/index.md
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
  let index_html = List.Assoc.find_exn results ~equal:String.equal "mydir/index.md" in
  printf "%s" index_html;
  [%expect
    {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <ul>
    <li><a href="/mydir/child/">child</a></li>
    </ul>
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
  let index_html = List.Assoc.find_exn results ~equal:String.equal "subdir/index.md" in
  printf "%s" index_html;
  [%expect {|
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body>
    <h1>Sub Index</h1>
    </body>
    </html>
    |}]
;;
