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

(** Extract the block that {!Block_id.t} points to.

    Two cases:
    - {b Inline}: the [^id] appears at the end of a paragraph with other content
      ([byte_pos > 0]). The paragraph itself is the target.
    - {b Standalone}: the [^id] is the entire paragraph ([byte_pos = 0]).
      It references the previous non-blank block. *)
let get_block_by_caret_id (blocks : Cmarkit.Block.t list) (id : string)
  : Cmarkit.Block.t option
  =
  let open Cmarkit in
  let has_matching_id (meta : Meta.t) : bool =
    match Meta.find Block_id.meta_key meta with
    | Some (block_id : Block_id.t) -> String.equal block_id.id id
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
