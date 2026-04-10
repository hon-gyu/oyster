type t = { label : Cmarkit.Inline.t }
type Cmarkit.Block.t += Ext_keyed_list_item of t * Cmarkit.Block.t
type Cmarkit.Block.t += Ext_keyed_block of t * Cmarkit.Block.t

val block_commonmark_renderer : Cmarkit_renderer.block
val sexp_of_block : Common.block_sexp
val rewrite_doc : source:string option -> Cmarkit.Doc.t -> Cmarkit.Doc.t

module For_test : sig
  val keyed_bodies_non_empty : Cmarkit.Doc.t -> bool
  val keying_is_maximal : source:string option -> Cmarkit.Doc.t -> bool
  val rule1_keyed_list_item_with_indented_content : string
  val rule2_keyed_list_item_followed_by_blank_line : string
  val rule3_keyed_list_item_with_contiguous_blocks : string
  val rule4_keyed_paragraph : string
  val rule5_keyed_paragraph_multiple_children : string
  val rule6_nesting : string
  val colon_chain_inline_keying : string
  val non_example_no_colon : string
  val non_example_colon_in_code_span : string
  val all_examples : string list
  val gen_markdown : string Core.Quickcheck.Generator.t
end
