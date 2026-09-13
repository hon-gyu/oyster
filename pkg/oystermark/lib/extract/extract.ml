(** Read-only queries over parsed blocks: the blocks a link target denotes, used
    by embedding and hover, and a walk over every addressable block of a note. *)
open Core

(** Flatten a block list by splicing any top-level [Blocks] nodes into a flat
    sequence. *)
let rec flatten (blocks : Cmarkit.Block.t list) : Cmarkit.Block.t list =
  List.concat_map blocks ~f:(fun block ->
    match block with
    | Cmarkit.Block.Blocks (children, _meta) -> flatten children
    | other -> [ other ])
;;

(** Collect the section starting at the heading whose identifier (see
    {!Parse.Common.heading_id}) is [heading_id], up to (but not including) the
    next heading of equal or lesser level.  Returns [] when the heading is not
    found. *)
let get_heading_section (blocks : Cmarkit.Block.t list) (heading_id : string)
  : Cmarkit.Block.t list
  =
  let open Cmarkit in
  let blocks = flatten blocks in
  (* Phase 1: skip blocks until we find the target heading *)
  let rec find_heading
    : Cmarkit.Block.t list -> (Cmarkit.Block.t * int * Cmarkit.Block.t list) option
    = function
    | [] -> None
    | block :: rest ->
      (match block with
       | Block.Heading (h, _meta) ->
         (match Parse.Common.heading_id h with
          | Some id when String.equal id heading_id ->
            Some (block, Block.Heading.level h, rest)
          | _ -> find_heading rest)
       | _ -> find_heading rest)
  in
  (* Phase 2: collect blocks until a heading of equal or lesser level *)
  let rec collect (level : int) (acc : Cmarkit.Block.t list)
    : Cmarkit.Block.t list -> Cmarkit.Block.t list
    = function
    | [] -> List.rev acc
    | block :: rest ->
      (match block with
       | Block.Heading (h, _meta) when Block.Heading.level h <= level -> List.rev acc
       | _ -> collect level (block :: acc) rest)
  in
  match find_heading blocks with
  | None -> []
  | Some (heading, level, rest) -> heading :: collect level [] rest
;;

(** Extract the block that {!Cmarkit.Block.Block_id.t} points to.

    Three cases:
    - {b Inline}: the [^id] appears at the end of a paragraph with other content.
      The paragraph itself is the target.
    - {b Standalone}: the [^id] is the entire paragraph.
      It references the previous non-blank block.
    - {b Keyed}: the id is on a {!Cmarkit.Block.Ext_keyed} node, having been
      forwarded from the paragraph or list item the node supplanted. The node
      itself is the target, so the reference denotes the label {e and} everything
      the key claimed as children. It anchors a whole subtree. *)
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

(** Extract the block carrying an explicit djot attribute id ([{#id}]).

    Two cases (see {!page-"feature-attribute-anchors"}):
    - {b Block attribute}: a [Block.Ext_attributes] whose merged attribute has
      the id.  The {e wrapped} block is returned.
    - {b Inline attribute}: an [Inline.Ext_attributes] carrying the id somewhere
      in a paragraph's or heading's inline content.  The containing block is
      returned.

    Container blocks (block quotes, list items, [Blocks]) are searched
    recursively; the first match in document order wins. *)
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

(* Walk
   ==== *)

(** A block in document order, with everything needed to select it. No field is
    derivable from another. What is derivable from [block], such as its kind or a
    code block's info string, is a function of the block instead. *)
type located_block =
  { block : Cmarkit.Block.t
  ; index : int (** 1-based position in the walk, over the whole note *)
  ; attr_id : string option
    (** a djot [ {#id} ] attribute. Unlike an [ ^id ] (see
        {!Parse.Common.caret_id_of_block}) this is not recoverable from [block]: the walk
        unwraps the [Ext_attributes] node that carries it. *)
  ; heading_path : string list (** enclosing heading ids, outermost first *)
  ; heading_text : string list (** the same headings as plain text *)
  }

(** The block's kind, as a stable name for [-kind] and for JSON. A callout is
    reported as [callout] rather than [block_quote]: it is a quote only in
    representation, and an author selecting one is not asking for quotes. *)
let kind_of_block (block : Cmarkit.Block.t) : string =
  let open Cmarkit in
  match block with
  | Block.Blank_line _ -> "blank_line"
  | Block.Block_quote (_, meta) ->
    (match Block.Callout.find meta with
     | Some _ -> "callout"
     | None -> "block_quote")
  | Block.Blocks _ -> "blocks"
  | Block.Code_block _ -> "code_block"
  | Block.Heading _ -> "heading"
  | Block.Html_block _ -> "html_block"
  | Block.Link_reference_definition _ -> "link_reference_definition"
  | Block.List _ -> "list"
  | Block.Paragraph _ -> "paragraph"
  | Block.Thematic_break _ -> "thematic_break"
  | Block.Ext_attributes _ -> "attributes"
  | Block.Ext_definition_list _ -> "definition_list"
  | Block.Ext_div _ -> "div"
  | Block.Ext_footnote_definition _ -> "footnote_definition"
  | Block.Ext_jsx_block _ -> "jsx_block"
  | Block.Ext_keyed _ -> "keyed"
  | Block.Ext_math_block _ -> "math_block"
  | Block.Ext_raw_block _ -> "raw_block"
  | Block.Ext_table _ -> "table"
  | _ -> "unknown"
;;

(** The id of the first inline [ {#id} ] attribute in [inline], if any. The
    block-level counterpart is an [Ext_attributes] wrapper, handled in the walk
    itself. *)
let inline_attr_id (inline : Cmarkit.Inline.t) : string option =
  let open Cmarkit in
  let folder =
    Folder.make
      ~inline:(fun _f found i ->
        match found with
        | Some _ -> Folder.ret found
        | None ->
          (match i with
           | Inline.Ext_attributes (a, _) ->
             (match Attribute.id (Inline.Attributes.attributes a) with
              | Some _ as id -> Folder.ret id
              | None -> Folder.default)
           | _ -> Folder.default))
      ~inline_ext_default:(fun _f found _ -> found)
      ~block_ext_default:(fun _f found _ -> found)
      ()
  in
  Folder.fold_inline folder None inline
;;

(** Every addressable block of [blocks] in document order, containers before
    their contents.

    Blank lines, [Blocks] groupings and [Ext_attributes] wrappers are not blocks
    an author would name, so they are not reported: an attributes wrapper
    forwards its id to the block it wraps, which is what [ {#id} ] denotes.

    A list item is not reported: it has no [Cmarkit.Block.t] of its own, its
    syntax being the marker. Its contents are walked as the blocks they are. *)
let walk (blocks : Cmarkit.Block.t list) : located_block list =
  let open Cmarkit in
  let next_index =
    let count = ref 0 in
    fun () ->
      incr count;
      !count
  in
  let emit ~headings ~attr_id block =
    { block
    ; index = next_index ()
    ; attr_id
    ; heading_path = List.rev_map headings ~f:(fun (_level, id, _text) -> id)
    ; heading_text = List.rev_map headings ~f:(fun (_level, _id, text) -> text)
    }
  in
  let acc = ref [] in
  let push located = acc := located :: !acc in
  (* [headings] is the enclosing heading stack, innermost first, as
     (level, id, text). *)
  let rec siblings ~headings blocks =
    List.fold (flatten blocks) ~init:headings ~f:(fun headings block ->
      visit ~headings ~attr_id:None block)
  and visit ~headings ~attr_id block =
    match block with
    | Block.Blank_line _ | Block.Link_reference_definition _ -> headings
    | Block.Blocks (bs, _) -> siblings ~headings bs
    | Block.Ext_attributes (a, _) ->
      (* The wrapper is not the block; its id belongs to what it wraps. *)
      let attr_id =
        match Attribute.id (Block.Attributes.attributes a) with
        | Some _ as id -> id
        | None -> attr_id
      in
      visit ~headings ~attr_id (Block.Attributes.block a)
    | Block.Heading (h, _) ->
      let level = Block.Heading.level h in
      let headings =
        List.drop_while headings ~f:(fun (enclosing, _, _) -> enclosing >= level)
      in
      let attr_id =
        match attr_id with
        | Some _ as id -> id
        | None -> inline_attr_id (Block.Heading.inline h)
      in
      push (emit ~headings ~attr_id block);
      let id = Option.value (Parse.Common.heading_id h) ~default:"" in
      let text = Parse.Common.inline_to_plain_text (Block.Heading.inline h) in
      (level, id, text) :: headings
    | Block.Paragraph (p, _) ->
      let attr_id =
        match attr_id with
        | Some _ as id -> id
        | None -> inline_attr_id (Block.Paragraph.inline p)
      in
      push (emit ~headings ~attr_id block);
      headings
    | Block.Block_quote (bq, _) ->
      push (emit ~headings ~attr_id block);
      ignore (siblings ~headings [ Block.Block_quote.block bq ] : _ list);
      headings
    | Block.Ext_div (d, _) ->
      push (emit ~headings ~attr_id block);
      ignore (siblings ~headings [ Block.Div.block d ] : _ list);
      headings
    | Block.Ext_keyed ((_label, body), _) ->
      push (emit ~headings ~attr_id block);
      ignore (siblings ~headings [ body ] : _ list);
      headings
    | Block.Ext_footnote_definition (fn, _) ->
      push (emit ~headings ~attr_id block);
      ignore (siblings ~headings [ Block.Footnote.block fn ] : _ list);
      headings
    | Block.List (l, _) ->
      push (emit ~headings ~attr_id block);
      List.iter (Block.List'.items l) ~f:(fun (item, _meta) ->
        ignore (siblings ~headings [ Block.List_item.block item ] : _ list));
      headings
    | block ->
      (* A block kind this module does not know is not one an author can name.
         Frontmatter is the case in practice: it carries no metadata at all. *)
      if String.equal (kind_of_block block) "unknown"
      then headings
      else (
        push (emit ~headings ~attr_id block);
        headings)
  in
  ignore (siblings ~headings:[] blocks : _ list);
  List.rev !acc
;;

(* Content
   ======= *)

(** What a container holds, once its own syntax is taken off.

    The two cases are not a convenience: a code block holds {e text}, which the
    parser has already stripped of its fence and indentation, so its content is
    exact. A block quote holds {e blocks}, whose source still carries the
    [>] marker on every line, so the content is a markdown value, and the only
    faithful way to write a markdown value back out is to render it. Rendering
    normalizes (fences, list markers, wrapping), so [Markdown] content is not
    byte-for-byte what the author typed, while [Literal] content is. *)
type content =
  | Literal of string
  | Markdown of Cmarkit.Block.t
  | Not_a_container

(** The contents of the container [located] addresses.

    [Not_a_container] for a paragraph, heading, table or thematic break: they
    hold inlines, rows, or nothing at all, so there is no single value inside to
    ask for. A list is not a container either: its syntax lives in the item
    markers, and its items' blocks are walked in their own right. *)
let content_of_located_block (located : located_block) : content =
  let open Cmarkit in
  let code_lines cb =
    Literal
      (Block.Code_block.code cb
       |> List.map ~f:Block_line.to_string
       |> String.concat ~sep:"\n")
  in
  match located.block with
  | Block.Code_block (cb, _) | Block.Ext_math_block (cb, _) -> code_lines cb
  | Block.Ext_raw_block (rb, _) -> code_lines (Block.Raw_block.code_block rb)
  | Block.Html_block (lines, _) ->
    Literal (lines |> List.map ~f:Block_line.to_string |> String.concat ~sep:"\n")
  | Block.Block_quote (bq, meta) ->
    let inner = Block.Block_quote.block bq in
    (* A callout's [ [!note] Title ] header is the callout's own syntax, the
         way a fence is a code block's; its contents are what follows. *)
    (match Block.Callout.find meta with
     | Some _ -> Markdown (Block.Callout.strip_header inner)
     | None -> Markdown inner)
  | Block.Ext_div (d, _) -> Markdown (Block.Div.block d)
  | Block.Ext_keyed ((_label, body), _) -> Markdown body
  | Block.Ext_footnote_definition (fn, _) -> Markdown (Block.Footnote.block fn)
  | _ -> Not_a_container
;;

(** The contents of [located] as a string, given the document's label
    definitions (a rendered container may hold reference links).

    [Error kind] names the kind that has no contents to give. *)
let content_string ~(defs : Cmarkit.Label.defs) (located : located_block)
  : (string, string) Result.t
  =
  match content_of_located_block located with
  | Literal text -> Ok text
  | Markdown block -> Ok (Cmarkit_commonmark.of_doc (Cmarkit.Doc.make ~defs block))
  | Not_a_container -> Error (kind_of_block located.block)
;;

(* Test
   ==== *)

let%test_module "Extract" =
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
