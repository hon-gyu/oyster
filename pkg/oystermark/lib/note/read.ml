open Core

(** Flatten a block list by splicing any top-level [Blocks] nodes into a flat
    sequence. *)
let rec flatten (blocks : Cmarkit.Block.t list) : Cmarkit.Block.t list =
  List.concat_map blocks ~f:(fun block ->
    match block with
    | Cmarkit.Block.Blocks (children, _meta) -> flatten children
    | other -> [ other ])
;;

(** The [Heading] case of {!read}. *)
let get_heading_section (blocks : Cmarkit.Block.t list) (heading_id : string)
  : Cmarkit.Block.t list
  =
  let open Cmarkit in
  (* A heading carrying a block attribute is wrapped in [Ext_attributes]. *)
  let rec heading_of (block : Block.t) : (Block.t * Block.Heading.t) option =
    match block with
    | Block.Heading (h, _) -> Some (block, h)
    | Block.Ext_attributes (a, _) -> heading_of (Block.Attributes.block a)
    | _ -> None
  in
  let rec in_list (blocks : Block.t list) : Block.t list option =
    in_siblings (flatten blocks)
  and in_siblings (blocks : Block.t list) : Block.t list option =
    match blocks with
    | [] -> None
    | block :: rest ->
      (match heading_of block with
       | Some (heading, h)
         when Option.equal String.equal (Parse.Common.heading_id h) (Some heading_id) ->
         let level = Block.Heading.level h in
         let ends (b : Block.t) =
           match heading_of b with
           | Some (_, h) -> Block.Heading.level h <= level
           | None -> false
         in
         Some (heading :: List.take_while rest ~f:(fun b -> not (ends b)))
       | _ ->
         (match in_children block with
          | Some _ as found -> found
          | None -> in_siblings rest))
  and in_children (block : Block.t) : Block.t list option =
    match block with
    | Block.Block_quote (bq, _) -> in_list [ Block.Block_quote.block bq ]
    | Block.List (l, _) ->
      List.find_map (Block.List'.items l) ~f:(fun (item, _) ->
        in_list [ Block.List_item.block item ])
    | Block.Ext_div (d, _) -> in_list [ Block.Div.block d ]
    | Block.Ext_keyed ((_label, body), _) -> in_list [ body ]
    | Block.Ext_footnote_definition (fn, _) -> in_list [ Block.Footnote.block fn ]
    | Block.Ext_attributes (a, _) -> in_children (Block.Attributes.block a)
    | _ -> None
  in
  Option.value (in_list blocks) ~default:[]
;;

(** The [Caret] case of {!read}. *)
let get_block_by_caret_id (blocks : Cmarkit.Block.t list) (id : string)
  : Cmarkit.Block.t option
  =
  let open Cmarkit in
  let has_matching_id (meta : Meta.t) : bool =
    match Block.Block_id.find meta with
    | Some (block_id : Block.Block_id.t) -> String.equal (Block.Block_id.id block_id) id
    | None -> false
  in
  (* A standalone [^id] paragraph is one whose entire inline content is just
     the block identifier: a single [Text] node starting with [^]. *)
  let is_standalone_id_paragraph (p : Block.Paragraph.t) : bool =
    match Block.Paragraph.inline p with
    | Inline.Text (s, _meta) -> String.is_prefix s ~prefix:"^"
    | _ -> false
  in
  (* Search a flat list of blocks, tracking the previous non-blank block
     for standalone [^id] references. *)
  let rec search (prev : Block.t option) (blocks : Block.t list) : Block.t option =
    match blocks with
    | [] -> None
    | block :: rest ->
      (match block with
       | Block.Paragraph (p, meta) ->
         (match has_matching_id meta with
          | true ->
            if is_standalone_id_paragraph p
            then (* Standalone: references the previous block *)
              prev
            else (* Inline: the paragraph itself is the target *)
              Some block
          | false -> search (Some block) rest)
       | Block.Ext_keyed ((_label, body), meta) ->
         if has_matching_id meta
         then Some block
         else (
           match search None (flatten [ body ]) with
           | Some _ as found -> found
           | None -> search (Some block) rest)
       | Block.Blank_line _ -> search prev rest
       | Block.List (l, _meta) ->
         (* Recurse into list items *)
         let items : Block.List_item.t node list = Block.List'.items l in
         (match search_items items with
          | Some _ as found -> found
          | None -> search (Some block) rest)
       | Block.Block_quote (bq, _meta) ->
         let inner : Block.t = Block.Block_quote.block bq in
         (match search None (flatten [ inner ]) with
          | Some _ as found -> found
          | None -> search (Some block) rest)
       | _ -> search (Some block) rest)
  and search_items (items : Block.List_item.t node list) : Block.t option =
    match items with
    | [] -> None
    | (item, _meta) :: rest ->
      let inner : Block.t = Block.List_item.block item in
      (match search None (flatten [ inner ]) with
       | Some _ as found -> found
       | None -> search_items rest)
  in
  search None (flatten blocks)
;;

(** The [Attr] case of {!read}. *)
let get_block_by_attr_id (blocks : Cmarkit.Block.t list) (id : string)
  : Cmarkit.Block.t option
  =
  let open Cmarkit in
  let attr_matches (attr : Attribute.t) : bool =
    match Attribute.id attr with
    | Some i -> String.equal i id
    | None -> false
  in
  (* Does [inline]'s tree carry an inline attribute with the target id? *)
  let inline_has_attr (inline : Inline.t) : bool =
    let folder =
      Folder.make
        ~inline:(fun _f found i ->
          match i with
          | Inline.Ext_attributes (a, _)
            when attr_matches (Inline.Attributes.attributes a) -> Folder.ret true
          | _ -> if found then Folder.ret true else Folder.default)
        ~inline_ext_default:(fun _f found _ -> found)
        ~block_ext_default:(fun _f found _ -> found)
        ()
    in
    Folder.fold_inline folder false inline
  in
  let rec find_in (blocks : Block.t list) : Block.t option =
    List.find_map (flatten blocks) ~f:find_block
  and find_block (block : Block.t) : Block.t option =
    match block with
    | Block.Ext_attributes (a, _) when attr_matches (Block.Attributes.attributes a) ->
      Some (Block.Attributes.block a)
    | Block.Ext_attributes (a, _) -> find_block (Block.Attributes.block a)
    | Block.Block_quote (bq, _) -> find_block (Block.Block_quote.block bq)
    | Block.List (l, _) ->
      List.find_map (Block.List'.items l) ~f:(fun (item, _) ->
        find_block (Block.List_item.block item))
    | Block.Blocks (bs, _) -> find_in bs
    | Block.Paragraph (p, _) ->
      if inline_has_attr (Block.Paragraph.inline p) then Some block else None
    | Block.Heading (h, _) ->
      if inline_has_attr (Block.Heading.inline h) then Some block else None
    | _ -> None
  in
  find_in blocks
;;

let read (blocks : Cmarkit.Block.t list) (address : Anchor.Address.t)
  : Cmarkit.Block.t list
  =
  match address with
  | Heading id -> get_heading_section blocks id
  | Caret id -> Option.to_list (get_block_by_caret_id blocks id)
  | Attr id -> Option.to_list (get_block_by_attr_id blocks id)
;;

let source_text (content : string) (blocks : Cmarkit.Block.t list) : string option =
  let locs =
    List.filter_map blocks ~f:(fun block ->
      let loc = Cmarkit.Meta.textloc (Parse.Common.meta_of_block block) in
      Option.some_if (not (Cmarkit.Textloc.is_none loc)) loc)
  in
  match
    ( List.min_elt (List.map locs ~f:Cmarkit.Textloc.first_byte) ~compare:Int.compare
    , List.max_elt (List.map locs ~f:Cmarkit.Textloc.last_byte) ~compare:Int.compare )
  with
  | Some pos, Some last ->
    let stop = Int.min (last + 1) (String.length content) in
    Some (String.sub content ~pos ~len:(stop - pos) |> String.rstrip)
  | _ -> None
;;

module For_test = struct
  let example_headings =
    {|\
# Heading 1
## Heading 2
### Heading 3
#### Heading 4
##### Heading 5
###### Heading 6
# Heading 7
## Heading 8
### Heading 9
## Heading 10
#### Heading 11
### Heading 12|}
  ;;

  let example_inline_caret_id =
    {|\
First paragraph.

Second paragraph text ^abc123|}
  ;;

  let example_blockquote_caret_id =
    {|\
> A blockquote here.

^bq001
|}
  ;;

  let example_not_found =
    {|
Some text ^exists
|}
  ;;

  let example_list_caret_id =
    {|
- Item one
- Item two

^lst001
|}
  ;;

  let example_nested_list_caret_id =
    {|
- a nested list ^firstline
    - item
      ^inneritem
|}
  ;;
end

(* Test
   ==== *)

let%test_module "Read" =
  (module struct
    open Parse.For_test
    open For_test

    let of_string = Parse.of_string
    let commonmark_of_doc = Parse.commonmark_of_doc

    let pp_section (ppf : Format.formatter) (blocks : Cmarkit.Block.t list) : unit =
      Format.fprintf
        ppf
        "%s@\n"
        (commonmark_of_doc
           (Cmarkit.Doc.make (Cmarkit.Block.Blocks (blocks, Cmarkit.Meta.none))))
    ;;

    let pp_block_opt (ppf : Format.formatter) (block : Cmarkit.Block.t option) : unit =
      match block with
      | None -> Format.fprintf ppf "<none>@\n"
      | Some b -> Format.fprintf ppf "%s@\n" (commonmark_of_doc (Cmarkit.Doc.make b))
    ;;

    let%expect_test "get_heading_section: heading-1" =
      let block = make_block example_headings in
      Format.printf "%a%!" pp_section (get_heading_section [ block ] "heading-1");
      [%expect
        {|
    # Heading 1
    ## Heading 2
    ### Heading 3
    #### Heading 4
    ##### Heading 5
    ###### Heading 6
    |}]
    ;;

    let%expect_test "get_heading_section: heading-8" =
      let block = make_block example_headings in
      Format.printf "%a%!" pp_section (get_heading_section [ block ] "heading-8");
      [%expect
        {|
    ## Heading 8
    ### Heading 9
    |}]
    ;;

    let%expect_test "get_block_by_caret_id: inline" =
      let doc = of_string example_inline_caret_id in
      Format.printf
        "%a%!"
        pp_block_opt
        (get_block_by_caret_id [ Cmarkit.Doc.block doc ] "abc123");
      [%expect {| Second paragraph text ^abc123 |}]
    ;;

    let%expect_test "get_block_by_caret_id: standalone blockquote" =
      let doc = of_string example_blockquote_caret_id in
      Format.printf
        "%a%!"
        pp_block_opt
        (get_block_by_caret_id [ Cmarkit.Doc.block doc ] "bq001");
      [%expect {| > A blockquote here. |}]
    ;;

    let%expect_test "get_block_by_caret_id: not found" =
      let doc = of_string example_not_found in
      Format.printf
        "%a%!"
        pp_block_opt
        (get_block_by_caret_id [ Cmarkit.Doc.block doc ] "nope");
      [%expect {| <none> |}]
    ;;

    let%expect_test "get_block_by_caret_id: standalone list" =
      let doc = of_string example_list_caret_id in
      Format.printf
        "%a%!"
        pp_block_opt
        (get_block_by_caret_id [ Cmarkit.Doc.block doc ] "lst001");
      [%expect
        {|
    - Item one
    - Item two
    |}]
    ;;

    let%expect_test "get_block_by_caret_id: nested list" =
      let doc = of_string example_nested_list_caret_id in
      Format.printf
        "%a%!"
        pp_block_opt
        (get_block_by_caret_id [ Cmarkit.Doc.block doc ] "firstline");
      [%expect {| a nested list ^firstline |}];
      Format.printf
        "%a%!"
        pp_block_opt
        (get_block_by_caret_id [ Cmarkit.Doc.block doc ] "inneritem");
      [%expect
        {|
    item
    ^inneritem
    |}]
    ;;

    (* get_block_by_attr_id. See {!page-"feature-attribute-anchors"}. *)

    let%expect_test "get_block_by_attr_id: inline attribute → containing paragraph" =
      let doc = of_string "# H\n\nThe [key term]{#kt} is here.\n" in
      Format.printf
        "%a%!"
        pp_block_opt
        (get_block_by_attr_id [ Cmarkit.Doc.block doc ] "kt");
      [%expect {| The key term{#kt} is here. |}]
    ;;

    let%expect_test "get_block_by_attr_id: block attribute → wrapped block" =
      let doc = of_string "# H\n\n{#aside}\n> An aside block.\n" in
      Format.printf
        "%a%!"
        pp_block_opt
        (get_block_by_attr_id [ Cmarkit.Doc.block doc ] "aside");
      [%expect {| > An aside block. |}]
    ;;

    let%expect_test "get_block_by_attr_id: not found" =
      let doc = of_string "# H\n\nPlain paragraph.\n" in
      Format.printf
        "%a%!"
        pp_block_opt
        (get_block_by_attr_id [ Cmarkit.Doc.block doc ] "missing");
      [%expect {| <none> |}]
    ;;
  end)
;;

let%test_module "read" =
  (module struct
    let show content (address : Anchor.Address.t) =
      let doc = Parse.of_string content in
      read [ Cmarkit.Doc.block doc ] address
      |> source_text content
      |> Option.value ~default:"<none>"
      |> print_endline
    ;;

    let%expect_test "heading: section of a top-level heading" =
      show "## Sec\n\nContent.\n\n## Other\n\nNot this.\n" (Heading "sec");
      [%expect
        {|
        ## Sec

        Content.
        |}]
    ;;

    let%expect_test "heading: inside a div, the section ends with the div" =
      show "# Top\n\n::: warning\n## Inside\n\nbody\n:::\n\nafter\n" (Heading "inside");
      [%expect
        {|
        ## Inside

        body
        |}]
    ;;

    let%expect_test "heading: a div after the heading belongs to the section whole" =
      show "## A\n\ntext\n\n::: note\n## B\n:::\n\nmore\n\n## C\n" (Heading "a");
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

    let%expect_test "heading: inside a block quote" =
      show "> ## Q\n> text\n\nafter\n" (Heading "q");
      [%expect
        {|
        ## Q
        > text
        |}]
    ;;

    let%expect_test "heading: an authored id starts the section at the heading" =
      show "{#intro}\n# Introduction\n\nbody\n\n# Next\n" (Heading "intro");
      [%expect
        {|
        # Introduction

        body
        |}]
    ;;

    let%expect_test "heading: a hash inside a code block does not end the section" =
      show "# Alpha\n\n```\n# not a heading\n```\n\ntail\n\n# Beta\n" (Heading "alpha");
      [%expect
        {|
        # Alpha

        ```
        # not a heading
        ```

        tail
        |}]
    ;;

    let%expect_test "caret: the whole paragraph, marker included" =
      show "# H\n\nFirst line\nsecond line ^abc\n\nafter\n" (Caret "abc");
      [%expect
        {|
        First line
        second line ^abc
        |}]
    ;;

    let%expect_test "caret: on a line of its own, the previous block" =
      show "> A quote.\n\n^q1\n" (Caret "q1");
      [%expect {| > A quote. |}]
    ;;

    let%expect_test "attr: on inlines, the containing paragraph as written" =
      show "The [key term]{#kt} is here.\n" (Attr "kt");
      [%expect {| The [key term]{#kt} is here. |}]
    ;;

    let%expect_test "attr: on a block, the wrapped block" =
      show "# H\n\n{#aside}\n> An aside block.\n" (Attr "aside");
      [%expect {| > An aside block. |}]
    ;;

    let%expect_test "not found" =
      show "# H\n\nPlain paragraph.\n" (Heading "missing");
      [%expect {| <none> |}]
    ;;
  end)
;;
