(** Utils for extracting block(s) from blocks, i.e., extracting sub-tree from the AST
    Mostly used in embedding
*)
open Core

(** Flatten a block list by splicing any top-level [Blocks] nodes into a flat
    sequence. *)
let rec flatten (blocks : Cmarkit.Block.t list) : Cmarkit.Block.t list =
  List.concat_map blocks ~f:(fun block ->
    match block with
    | Cmarkit.Block.Blocks (children, _meta) -> flatten children
    | other -> [ other ])
;;

(** Collect the section starting at the heading with id stamped in
    {!module:Heading_slug}, up to (but not including) the
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
       | Block.Heading (h, meta) ->
         (match Meta.find Heading_slug.meta_key meta with
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

    Two cases:
    - {b Inline}: the [^id] appears at the end of a paragraph with other content.
      The paragraph itself is the target.
    - {b Standalone}: the [^id] is the entire paragraph.
      It references the previous non-blank block. *)
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
     the block identifier — a single [Text] node starting with [^]. *)
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
