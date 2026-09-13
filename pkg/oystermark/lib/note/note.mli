(** Types and operations for a single parsed note: addresses, anchors, links,
    reading blocks, and transclusion. Resolving links across notes is done in
    [Vault]. *)

module Anchor = Anchor
module Link = Link
module Transclusion = Transclusion

include module type of struct
  include Read
end
