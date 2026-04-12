type t = { label : Cmarkit.Inline.t }
type Cmarkit.Block.t += Ext_keyed_list_item of t * Cmarkit.Block.t
type Cmarkit.Block.t += Ext_keyed_block of t * Cmarkit.Block.t

val block_commonmark_renderer : Cmarkit_renderer.block
val sexp_of_block : Common.block_sexp
val rewrite_doc : ?paragraph_inline_value:bool -> Cmarkit.Doc.t -> Cmarkit.Doc.t

module For_test : sig
  val count_keyed : Cmarkit.Doc.t -> int

  type example =
    { name : string
    ; content : string
    }

  val examples : example list
  val gen_markdown : string Core.Quickcheck.Generator.t
end
