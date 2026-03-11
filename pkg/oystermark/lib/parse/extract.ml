(** Extracting block(s) from blocks, i.e., extracting sub-tree from the AST  *)
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
  let blocks = flatten blocks in
  let rec search (prev : Cmarkit.Block.t option) : Cmarkit.Block.t list -> Cmarkit.Block.t option
    = function
    | [] -> None
    | block :: rest ->
      (match block with
       | Cmarkit.Block.Paragraph (_p, meta) ->
         (match Cmarkit.Meta.find Block_id.meta_key meta with
          | Some (block_id : Block_id.t) when String.equal block_id.id id ->
            if block_id.byte_pos > 0
            then (* Inline: the paragraph itself is the target *)
              Some block
            else (* Standalone: references the previous block *)
              prev
          | _ -> search (Some block) rest)
       | Cmarkit.Block.Blank_line _ -> search prev rest
       | _ -> search (Some block) rest)
  in
  search None blocks
;;
