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

(** The inlines [block] holds itself, outside its child blocks. *)
let own_inlines : Djot.Block.t -> Djot.Inline.t Djot.node list = function
  | Para inlines | Heading (_, inlines) -> inlines
  | Table (caption, rows) ->
    Option.value caption ~default:[]
    @ List.concat_map
        rows
        ~f:(List.concat_map ~f:(fun (Djot.Block.Cell (_, _, ns)) -> ns))
  | Ext_callout (_, _, title, _) -> title
  | Ext_keyed (label, _) -> label
  | DefinitionList (_, items) -> List.concat_map items ~f:fst
  | Section _
  | BlockQuote _
  | CodeBlock _
  | Div _
  | OrderedList _
  | BulletList _
  | TaskList _
  | ThematicBreak
  | RawBlock _
  | FootnoteDef _
  | RefDef _ -> []
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
    | Attr id, block -> has_id id attrs || inlines_have_id id (own_inlines block)
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

    let%expect_test "attr: on inlines in a table cell, the table" =
      show "Before.\n\n| a | [x]{#c} |\n|---|---|\n| 1 | 2 |\n\nAfter.\n" (Attr "c");
      [%expect
        {|
        | a | [x]{#c} |
        |---|---|
        | 1 | 2 |
        |}]
    ;;

    let%expect_test "attr: on inlines in a callout title, the callout" =
      show "> [!note] A [title]{#ct}\n> Body.\n\nAfter.\n" (Attr "ct");
      [%expect
        {|
        > [!note] A [title]{#ct}
        > Body.
        |}]
    ;;

    let%expect_test "attr: on inlines in a keyed label, the keyed block" =
      show "[owner]{#ok}: alice\n\nAfter.\n" (Attr "ok");
      [%expect {| [owner]{#ok}: alice |}]
    ;;

    let%expect_test "attr: on inlines in a definition term, the definition list" =
      show ": [term]{#dt}\n\n  Its definition.\n\nAfter.\n" (Attr "dt");
      [%expect
        {|
        : [term]{#dt}

          Its definition.
        |}]
    ;;

    let%expect_test "every anchor of a note is found" =
      let content =
        "# H\n\n\
         The [kt]{#kt}.\n\n\
         | [x]{#c} |\n\
         |---|\n\
         | 1 |\n\n\
         > [!note] [t]{#ct}\n\n\
         [k]{#ok}: v\n\n\
         : [term]{#dt}\n\n\
        \  d\n\n\
         [^1]: [n]{#fn}\n"
      in
      let { Parse.doc; _ } = Parse.of_string content in
      Anchor.of_doc doc
      |> List.iter ~f:(fun (a : Anchor.t) ->
        let address = Anchor.address a.definition in
        let found = not (List.is_empty (find doc address)) in
        print_s [%sexp (address : Anchor.Address.t), (found : bool)]);
      [%expect
        {|
        ((Heading H) true)
        ((Attr kt) true)
        ((Attr c) true)
        ((Attr ct) true)
        ((Attr ok) true)
        ((Attr dt) true)
        ((Attr fn) true)
        |}]
    ;;

    let%expect_test "not found" =
      show "# H\n\nPlain paragraph.\n" (Heading "missing");
      [%expect {| <none> |}]
    ;;
  end)
;;
