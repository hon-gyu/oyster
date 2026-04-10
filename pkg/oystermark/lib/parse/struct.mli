type t = { label : Cmarkit.Inline.t }
type Cmarkit.Block.t += Ext_keyed_list_item of t * Cmarkit.Block.t
type Cmarkit.Block.t += Ext_keyed_block of t * Cmarkit.Block.t

val block_commonmark_renderer : Cmarkit_renderer.block
val sexp_of_block : Common.block_sexp
val rewrite_doc : Cmarkit.Doc.t -> Cmarkit.Doc.t

module For_test : sig
  val examples : string list
  val gen_markdown : string Core.Quickcheck.Generator.t
end
