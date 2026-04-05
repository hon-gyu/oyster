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

    When the label text contains interior [: ] (colon-space) boundaries,
    e.g. [foo: bar:], each segment produces a nesting level:
    [Ext_keyed_list_item("foo", Ext_keyed_list_item("bar", body))].

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

(** Rewrite a document, converting keyed paragraphs and list items into
    {!Ext_keyed_block} / {!Ext_keyed_list_item} nodes.

    @param source  The original markdown source string (after frontmatter
    extraction).  Used for escaped-colon detection via byte positions.
    Pass [None] to skip escape checking. *)
val rewrite_doc : source:string option -> Cmarkit.Doc.t -> Cmarkit.Doc.t

(** {1 For testing} *)

module For_test : sig
  val example_rule1_indented : string
  val example_rule2_blank_after : string
  val example_rule3_contiguous_after_list : string
  val example_rule4_keyed_paragraph : string
  val example_rule5_multiple_children : string
  val example_rule6_nesting : string
  val example_colon_chain : string
  val non_example_no_colon : string
  val non_example_colon_in_code : string
  val all_examples : string list
end
