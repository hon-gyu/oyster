(** Struct: colon-keyed tree restructuring.

    A {b keyed node} is a list item or paragraph whose inline text ends with
    an unescaped [:].  The struct rewrite reparents contiguous following
    content as children of the keyed node.

    {2 Rules}

    {ol
    {- {b Keyed list item with indented content.}  The indented sub-blocks
       are already children in the CommonMark AST; they become the body of
       an {!Ext_keyed_list_item}.}
    {- {b Keyed list item followed by a blank line.}  No transformation —
       the trailing colon is treated as literal punctuation.}
    {- {b Keyed list item followed by contiguous blocks.}  Unindented
       blocks immediately after the list are reparented under the last
       item as an {!Ext_keyed_list_item}.}
    {- {b Keyed paragraph.}  A paragraph ending with [:] claims all
       immediately following contiguous blocks (no blank-line separation)
       as children, producing an {!Ext_keyed_block}.}
    {- Same as rule 4 but with multiple child block types.}
    {- {b Nesting.}  Keyed nodes nest: a keyed paragraph can contain a
       list whose items are themselves keyed.}}

    {2 Colon chains}

    When the label is pure text and contains interior [: ]
    (colon-space) boundaries, e.g. [foo: bar:], each segment produces
    a nesting level:
    [Ext_keyed_list_item("foo", Ext_keyed_list_item("bar", body))].

    {b Chain splitting only applies to pure-text labels.}  If the label
    contains any non-text inline — code span, emphasis, link, image,
    raw HTML, hard/soft break, or extension (e.g. wikilink) — the whole
    inline becomes a single label, preserved verbatim.  Rationale: a
    [: ] inside a code span is literal punctuation, not a chain
    delimiter, and splitting across inline boundaries would silently
    corrupt the label.

    {2 Escaped colons}

    A backslash-escaped colon ([\:]) in the original source is {b not} a
    key delimiter.  Detection requires [~source] to be passed to
    {!rewrite_doc}. *)

(** Label metadata carried by keyed nodes. *)
type t = { label : Cmarkit.Inline.t }

(** A list item whose trailing-colon label has been detected. *)
type Cmarkit.Block.t += Ext_keyed_list_item of t * Cmarkit.Block.t

(** A paragraph whose trailing-colon label has been detected. *)
type Cmarkit.Block.t += Ext_keyed_block of t * Cmarkit.Block.t

val block_commonmark_renderer : Cmarkit_renderer.block

(** Sexp converter for keyed blocks; composes into {!Common.make_sexp_of}. *)
val sexp_of_block : Common.block_sexp

(** Rewrite a document, converting keyed paragraphs and list items into
    {!Ext_keyed_block} / {!Ext_keyed_list_item} nodes.

    @param source  The original markdown source string (after frontmatter
    extraction).  Used for escaped-colon detection via byte positions.
    Pass [None] to skip escape checking. *)
val rewrite_doc : source:string option -> Cmarkit.Doc.t -> Cmarkit.Doc.t

(** {1 Specification}

    [Spec] codifies the struct rules as {b universal predicates} over
    [Cmarkit.Doc.t] and a set of named example markdown strings —
    one per rule in [specification/oyster/struct.md].

    Predicates have varied signatures and are plain functions, not
    wrapped in a uniform record.  The expected rewritten tree for
    each example is pinned in expect-tests in [parse.ml], so we don't
    encode expected output twice. *)

module Spec : sig
  (** {2 Universal predicates}

      Each holds for every doc produced by {!rewrite_doc}.  A failure
      means the rewriter violated the spec. *)

  (** No keyed node has an empty body. *)
  val keyed_bodies_non_empty : Cmarkit.Doc.t -> bool

  (** No sibling-level keyed paragraph or keyed-last-item list is
      immediately followed by a non-blank block — the rewriter has
      absorbed every follower it could (Rules 3, 4, 5).  Needs
      [~source] for escaped-colon detection. *)
  val keying_is_maximal : source:string option -> Cmarkit.Doc.t -> bool

  (** {2 Examples}

      Named markdown strings corresponding to the rules in
      [specification/oyster/struct.md]. *)

  val rule1_keyed_list_item_with_indented_content : string
  val rule2_keyed_list_item_followed_by_blank_line : string
  val rule3_keyed_list_item_with_contiguous_blocks : string
  val rule4_keyed_paragraph : string
  val rule5_keyed_paragraph_multiple_children : string
  val rule6_nesting : string
  val colon_chain_inline_keying : string
  val non_example_no_colon : string
  val non_example_colon_in_code_span : string

  (** All examples as a list — passed as [~examples:] to
      [Core.Quickcheck.test] so that hand-picked witnesses always run
      and seed shrinking, and also usable for commonmark-roundtrip
      checks in [parse.ml]. *)
  val all_examples : string list

  (** {2 Generator}

      A small line-based generator that samples from a vocabulary of
      lines likely to exercise keying, nesting, blank lines, and
      escape handling. *)
  val gen_markdown : string Core.Quickcheck.Generator.t
end
