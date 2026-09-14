(** Types and operations for a single parsed note: addresses, anchors, links,
    reading blocks, querying blocks, and transclusion. Resolving links across notes is done in
    [Vault]. *)

module Anchor = Anchor
module Link = Link
module Transclusion = Transclusion
module Node = Node
module Query = Query
module Query_flags = Query_flags

include module type of struct
  include Read
end
