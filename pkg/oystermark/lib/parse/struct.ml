(** {0 Struct: colon-keyed tree restructuring}

    Oystermark uses {!Cmarkit.Struct} for colon-keyed restructuring. Keyed
    nodes are represented by the single constructor {!Cmarkit.Block.Ext_keyed};
    whether a keyed node behaves as a list item or as a free block is determined
    by its position in the tree.

    This module exposes the pass plus oystermark's s-expression converter and
    test fixtures. *)

open Core
open Cmarkit

let rewrite_doc = Cmarkit.Struct.rewrite_doc

let sexp_of_block : Common.block_sexp =
  fun ~recurse_inline ~recurse_block ~with_meta b ->
  match b with
  | Block.Ext_keyed ((label, body), meta) ->
    Some
      (with_meta
         meta
         (Sexp.List [ Atom "Keyed"; recurse_inline label; recurse_block body ]))
  | _ -> None
;;

module For_test = Struct_for_test
