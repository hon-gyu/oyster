(** Types and operations for a single parsed note: addresses, anchors, links,
    querying blocks, and transclusion. Resolving links across notes is done in
    [Vault]. *)

module Anchor = Anchor
module Link = Link
module Transclusion = Transclusion
module Node = Node
module Query = Query

module Private : sig
  module Address_utils = Address_utils
end
