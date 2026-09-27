open Core

type block = Djot.Block.t Djot.node

let has_id (id : string) (attrs : Djot.Attr.t) : bool =
  Option.equal String.equal (Djot.Attr.id attrs) (Some id)
;;

let inlines_have_id (id : string) (inlines : Djot.Inline.t Djot.node list) : bool =
  let folder =
    Djot.Folder.make
      ~inline:(fun _ found (Node (_, attrs, _)) ->
        if found || has_id id attrs then Djot.Folder.ret true else Djot.Folder.default)
      ()
  in
  List.fold inlines ~init:false ~f:(Djot.Folder.fold_inline folder)
;;

(** The first block of [doc], in document order, that [matches]. *)
let find_block (doc : Djot.Doc.t) ~(matches : block -> bool) : block option =
  let folder =
    Djot.Folder.make
      ~block:(fun _ found block ->
        match found with
        | Some _ -> Djot.Folder.ret found
        | None ->
          if matches block then Djot.Folder.ret (Some block) else Djot.Folder.default)
      ~inline:(fun _ found _ -> Djot.Folder.ret found)
      ()
  in
  Djot.Folder.fold_doc folder None doc
;;

let find (doc : Djot.Doc.t) (address : Anchor.Address.t) : block list =
  let matches (Djot.Node (_, attrs, block) : block) =
    match address, block with
    | Heading id, (Section _ | Heading _) -> has_id id attrs
    | Heading _, _ -> false
    | Attr id, (Para inlines | Heading (_, inlines)) ->
      has_id id attrs || inlines_have_id id inlines
    | Attr id, _ -> has_id id attrs
  in
  Option.to_list (find_block doc ~matches)
;;

let source_text (doc : Djot.Doc.t) (content : string) (blocks : block list)
  : string option
  =
  let locs =
    List.filter_map blocks ~f:(fun block ->
      let loc = Anchor.extent doc block in
      Option.some_if (not (Djot.Textloc.is_none loc)) loc)
  in
  match
    ( List.min_elt (List.map locs ~f:Djot.Textloc.first_byte) ~compare:Int.compare
    , List.max_elt (List.map locs ~f:Djot.Textloc.last_byte) ~compare:Int.compare )
  with
  | Some pos, Some last ->
    let stop = Int.min (last + 1) (String.length content) in
    Some (String.sub content ~pos ~len:(stop - pos) |> String.rstrip)
  | _ -> None
;;

let%test_module "find" =
  (module struct
    let show content (address : Anchor.Address.t) =
      let { Parse.doc; _ } = Parse.of_string content in
      find doc address
      |> source_text doc content
      |> Option.value ~default:"<none>"
      |> print_endline
    ;;

    let%expect_test "heading: the section, to the next heading of its level" =
      show "## Sec\n\nContent.\n\n### Sub\n\nx\n\n## Other\n\nNot this.\n" (Heading "Sec");
      [%expect
        {|
        ## Sec

        Content.

        ### Sub

        x
        |}]
    ;;

    let%expect_test "heading: inside a div, the heading alone" =
      show "# Top\n\n::: warning\n## Inside\n\nbody\n:::\n\nafter\n" (Heading "Inside");
      [%expect {| ## Inside |}]
    ;;

    let%expect_test "heading: a div after the heading belongs to the section whole" =
      show "## A\n\ntext\n\n::: note\n## B\n:::\n\nmore\n\n## C\n" (Heading "A");
      [%expect
        {|
        ## A

        text

        ::: note
        ## B
        :::

        more
        |}]
    ;;

    let%expect_test "heading: an authored id" =
      show "{#intro}\n# Introduction\n\nbody\n\n# Next\n" (Heading "intro");
      [%expect
        {|
        {#intro}
        # Introduction

        body
        |}]
    ;;

    let%expect_test "heading: a hash inside a code block does not end the section" =
      show "# Alpha\n\n```\n# not a heading\n```\n\ntail\n\n# Beta\n" (Heading "Alpha");
      [%expect
        {|
        # Alpha

        ```
        # not a heading
        ```

        tail
        |}]
    ;;

    let%expect_test "attr: on inlines, the containing paragraph as written" =
      show "The [key term]{#kt} is here.\n" (Attr "kt");
      [%expect {| The [key term]{#kt} is here. |}]
    ;;

    let%expect_test "attr: on a block, the block with its attribute" =
      show "# H\n\n{#aside}\n> An aside block.\n" (Attr "aside");
      [%expect
        {|
        {#aside}
        > An aside block.
        |}]
    ;;

    let%expect_test "attr: in a footnote" =
      show "a[^1]\n\n[^1]: a [note]{#fn-x}\n" (Attr "fn-x");
      [%expect {| a [note]{#fn-x} |}]
    ;;

    let%expect_test "not found" =
      show "# H\n\nPlain paragraph.\n" (Heading "missing");
      [%expect {| <none> |}]
    ;;
  end)
;;
