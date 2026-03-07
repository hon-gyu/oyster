open! Core
open Oystermark

let vault_root = "../data/vault/pipeline"

let%expect_test "render_vault: draft excluded" =
  let pipeline = Pipeline.exclude_drafts in
  let results = Oystermark.render_vault ~pipeline ~backend_blocks:true ~safe:false vault_root in
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
  let results = Oystermark.render_vault ~pipeline:Pipeline.id ~backend_blocks:true ~safe:false vault_root in
  let home_html =
    List.Assoc.find_exn results ~equal:String.equal "home.md"
  in
  printf "%s" home_html;
  [%expect {| <h1>Home Page</h1> |}]
;;

let%expect_test "render_vault: subdir index" =
  let results = Oystermark.render_vault ~pipeline:Pipeline.id ~backend_blocks:true ~safe:false vault_root in
  let index_html =
    List.Assoc.find_exn results ~equal:String.equal "subdir/index.md"
  in
  printf "%s" index_html;
  [%expect {| <h1>Sub Index</h1> |}]
;;

let%expect_test "render_vault: regular note unchanged" =
  let results = Oystermark.render_vault ~pipeline:Pipeline.id ~backend_blocks:true ~safe:false vault_root in
  let note_html =
    List.Assoc.find_exn results ~equal:String.equal "subdir/note-a.md"
  in
  printf "%s" note_html;
  [%expect {| <h1>Note A</h1> |}]
;;

let%expect_test "render_vault: custom pipeline can drop files" =
  let drop_home : Pipeline.t =
    Pipeline.make ~on_discover:(fun path -> not (String.equal path "home.md")) ()
  in
  let pipeline = Pipeline.compose drop_home Pipeline.id in
  let results = Oystermark.render_vault ~pipeline ~backend_blocks:true ~safe:false vault_root in
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
