(** Types and operations for a single parsed note: addresses, anchors, links,
    reading blocks, and transclusion. Resolving links across notes is done in
    [Vault]. *)

module Address = Address
module Anchor = Anchor
module Link_ref = Link_ref
module Link = Link
module Transclusion = Transclusion

include module type of struct
  include Read
end
