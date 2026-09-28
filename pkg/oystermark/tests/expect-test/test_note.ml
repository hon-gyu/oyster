(** {!Note.expand} and its reversal law. *)

open! Core
open Oystermark

let print (n : Note.t) : unit = print_string (Parse.to_string n)
let note (content : string) : Note.t = Parse.of_string content

(** An [env] that resolves through a vault index and takes the part an anchor
    names with {!Note.Private.Address_utils.find}. *)
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
      let whole = List.Assoc.find_exn docs ~equal:String.equal path in
      (match target with
       | Vault.Index.Anchor { anchor = { definition; _ }; _ } ->
         Some
           { path
           ; fragment = Some (Note.Transclusion.fragment definition)
           ; note =
               (let address = Note.Anchor.address definition in
                let blocks = Note.Private.Address_utils.find whole.doc address in
                let blocks =
                  List.concat_map blocks ~f:(function
                    | Djot.Node (_, _, Djot.Block.Section (_heading :: body)) -> body
                    | block -> [ block ])
                in
                { whole with doc = Parse.doc_of_blocks blocks })
           }
       | Note _ | Asset _ -> Some { path; fragment = None; note = whole })
;;

let expand ?max_depth (files : (string * string) list) (path : string) : Note.t =
  let content = List.Assoc.find_exn files ~equal:String.equal path in
  Note.expand ?max_depth ~env:(env_of_files files) ~path (note content)
;;

(* Frontmatter
   =========== *)

let%expect_test "frontmatter is split from the body and put back" =
  let n = note "---\ntitle: A\n---\n\n# A\n\nText." in
  print_s [%sexp (Option.is_some n.frontmatter : bool)];
  print_endline (Djot.Source.of_doc n.doc);
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
    {source="b.md" depth="1"}
    ::: embed
    From b.
    :::

    {source="c.md" fragment="Sec" depth="1"}
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
    {source="sub/b.md" depth="1"}
    :::: embed
    {source="sub/c.md" depth="2"}
    ::: embed
    From sub/c.
    :::
    ::::
    |}]
;;

let%expect_test "a cycle shows the note unexpanded" =
  print (expand [ "a.md", "![[b]]"; "b.md", "![[a]]" ] "a.md");
  [%expect
    {|
    {source="b.md" depth="1"}
    :::: embed
    {source="a.md" depth="2"}
    ::: embed
    ![[b]]
    :::
    ::::
    |}]
;;

let%expect_test "past the depth limit a wikilink embed becomes a link" =
  print
    (expand ~max_depth:1 [ "a.md", "![[b]]"; "b.md", "![[c]]"; "c.md", "C text." ] "a.md");
  [%expect
    {|
    {source="b.md" depth="1"}
    ::: embed
    [[c]]
    :::
    |}]
;;

let%expect_test "law 3: expanding keeps the frontmatter" =
  let files =
    [ "a.md", "---\ntitle: A\n---\n\n![[b]]"; "b.md", "---\ntitle: B\n---\n\nB text." ]
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
      [ "a.md", "Before.\n\n![[b]]\n\nAfter."; "b.md", "![[c]]"; "c.md", "C text." ]
      "a.md"
  in
  print_string (Djot.Source.of_doc (Note.Transclusion.reverse_embed_doc n.doc));
  [%expect
    {|
    Before.

    ![[b]]

    After.
    |}]
;;
