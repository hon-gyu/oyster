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
