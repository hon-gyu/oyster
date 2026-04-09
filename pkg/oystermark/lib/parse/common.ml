open Core
open Cmarkit

let compose_block_map (m1 : Block.t Mapper.mapper) (m2 : Block.t Mapper.mapper) =
  fun m b ->
  match m1 m b with
  | `Default -> m2 m b
  | `Map None -> `Map None
  | `Map (Some b') -> m2 m b'
;;

let compose_inline_map (m1 : Inline.t Mapper.mapper) (m2 : Inline.t Mapper.mapper) =
  fun m i ->
  match m1 m i with
  | `Default -> m2 m i
  | `Map None -> `Map None
  | `Map (Some i') -> m2 m i'
;;

let compose_all_block_maps (ms : Block.t Mapper.mapper list) =
  List.fold_right ms ~init:(fun m b -> Mapper.default) ~f:compose_block_map
;;

let compose_all_inline_maps (ms : Inline.t Mapper.mapper list) =
  List.fold_right ms ~init:(fun m i -> Mapper.default) ~f:compose_inline_map
;;
