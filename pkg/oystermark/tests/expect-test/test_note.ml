(** {!Note.select} and {!Note.expand}, and the laws {!Note} states for them. *)

open! Core
open Oystermark

let print (n : Note.t) : unit = print_string (Parse.commonmark_of_doc (Note.to_doc n))
let note (content : string) : Note.t = Note.of_doc (Parse.of_string content)

let same (a : Note.t) (b : Note.t) : unit =
  let render n = Parse.commonmark_of_doc (Note.to_doc n) in
  print_s [%sexp (String.equal (render a) (render b) : bool)]
;;

(** An [env] that resolves through a vault index, and selects the part an
    anchor names with {!Note.Query.of_address}. *)
let env_of_files (files : (string * string) list)
  : from:string -> Note.Link.Ref.t -> Note.source option
  =
  let docs = List.map files ~f:(fun (path, content) -> path, Parse.of_string content) in
  let index = Vault.build_index ~md_docs:docs ~other_files:[] () in
  fun ~from ref ->
    match Vault.Index.resolve index from ref with
    | Error _ | Ok (Vault.Index.Asset _) -> None
    | Ok target ->
      let path = Vault.Index.target_path target in
      let whole = Note.of_doc (List.Assoc.find_exn docs ~equal:String.equal path) in
      (match target with
       | Vault.Index.Anchor { anchor = { definition; _ }; _ } ->
         Some
           { path
           ; fragment = Note.Transclusion.fragment definition
           ; note =
               Note.select (Note.Query.of_address (Note.Anchor.address definition)) whole
           }
       | Note _ | Asset _ -> Some { path; fragment = None; note = whole })
;;

let expand ?max_depth (files : (string * string) list) (path : string) : Note.t =
  let content = List.Assoc.find_exn files ~equal:String.equal path in
  Note.expand ?max_depth ~env:(env_of_files files) ~path (note content)
;;

(* of_doc and to_doc
   ================= *)

let%expect_test "frontmatter is split from the body and put back" =
  let n = note "---\ntitle: A\n---\n\n# A\n\nText." in
  print_s [%sexp (Option.is_some n.frontmatter : bool)];
  print_endline (Parse.commonmark_of_doc n.body);
  print n;
  [%expect
    {|
    true
    # A

    Text.
    ---
    title: A
    ---
    # A

    Text.
    |}]
;;

(* select
   ====== *)

let doc =
  "---\n\
   title: Doc\n\
   ---\n\n\
   # A\n\n\
   text\n\n\
   ## B\n\n\
   body\n\n\
   > [!note] Title\n\
   > inside\n\n\
   # C\n\n\
   - one\n\
   - two"
;;

let%expect_test "a section becomes the root, without its heading" =
  print (Note.select Note.Query.(empty |> section [ "a" ]) (note doc));
  [%expect
    {|
    ---
    title: Doc
    ---
    text

    ## B

    body

    > \[!note\] Title
    > inside
    |}]
;;

let%expect_test "a callout becomes the root, without its header" =
  print (Note.select Note.Query.(empty |> descend ~where:[ is "callout" ]) (note doc));
  [%expect
    {|
    ---
    title: Doc
    ---
    inside
    |}]
;;

let%expect_test "several matches are separated by a blank line" =
  print (Note.select Note.Query.(empty |> descend ~where:[ is "list_item" ]) (note doc));
  [%expect
    {|
    ---
    title: Doc
    ---
    one



    two
    |}]
;;

let%expect_test "law 1: selecting with the empty query is the identity" =
  let n = note doc in
  same (Note.select Note.Query.empty n) n;
  [%expect {| true |}]
;;

let%expect_test "law 2: selecting twice is selecting the concatenated query" =
  let n = note doc in
  let check a b = same (Note.select b (Note.select a n)) (Note.select (a @ b) n) in
  let open Note.Query in
  check (empty |> section [ "a" ]) (empty |> child ~nth:1);
  check (empty |> section [ "a" ]) (empty |> section [ "b" ]);
  check (empty |> descend ~where:[ is "callout" ]) (empty |> child);
  check (empty |> descend ~where:[ is "list_item" ] ~nth:0) (empty |> child);
  [%expect
    {|
    true
    true
    true
    true
    |}]
;;

(* expand
   ====== *)

let%expect_test "an embed of a note and of a heading" =
  print
    (expand
       [ "a.md", "---\ntitle: A\n---\n\n![[b]]\n\n![[c#Sec]]"
       ; "b.md", "---\ntitle: B\n---\n\nFrom b."
       ; "c.md", "Intro.\n\n## Sec\n\nFrom c."
       ]
       "a.md");
  [%expect
    {|
    ---
    title: A
    ---
    {source="b.md" depth=1}
    ::: embed
    From b.
    :::

    {source="c.md" fragment=Sec depth=1}
    ::: embed
    From c.
    :::
    |}]
;;

let%expect_test "an embedded note's embeds are resolved from its own path" =
  print
    (expand
       [ "a.md", "![[sub/b]]"; "sub/b.md", "![[c]]"; "sub/c.md", "From sub/c." ]
       "a.md");
  [%expect
    {|
    {source="sub/b.md" depth=1}
    ::: embed
    {source="sub/c.md" depth=2}
    ::: embed
    From sub/c.
    :::
    :::
    |}]
;;

let%expect_test "a cycle shows the note unexpanded" =
  print (expand [ "a.md", "![[b]]"; "b.md", "![[a]]" ] "a.md");
  [%expect
    {|
    {source="b.md" depth=1}
    ::: embed
    {source="a.md" depth=2}
    ::: embed
    ![[b]]
    :::
    :::
    |}]
;;

let%expect_test "past the depth limit a wikilink embed becomes a link" =
  print (expand ~max_depth:1 [ "a.md", "![[b]]"; "b.md", "![[c]]"; "c.md", "C." ] "a.md");
  [%expect
    {|
    {source="b.md" depth=1}
    ::: embed
    [[c]]
    :::
    |}]
;;

let%expect_test "law 3: expanding keeps the frontmatter" =
  let files =
    [ "a.md", "---\ntitle: A\n---\n\n![[b]]"; "b.md", "---\ntitle: B\n---\n\nB." ]
  in
  print_s
    [%sexp
      (Option.equal
         Yaml.equal
         (expand files "a.md").frontmatter
         (note (List.Assoc.find_exn files ~equal:String.equal "a.md")).frontmatter
       : bool)];
  [%expect {| true |}]
;;

let%expect_test "law 4: reversing the embeds gives the note back" =
  let n =
    expand
      [ "a.md", "Before.\n\n![[b]]\n\nAfter."; "b.md", "![[c]]"; "c.md", "C." ]
      "a.md"
  in
  print_string (Parse.commonmark_of_doc (Note.Transclusion.reverse_embed_doc n.body));
  [%expect
    {|
    Before.

    ![[b]]

    After.
    |}]
;;
