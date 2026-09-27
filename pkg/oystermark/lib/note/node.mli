(** The nodes a {!Query} walks and what it reports about them.

    A node is the root of a note, a block of djot's tree, a list item, or a
    footnote definition. Reference definitions are not nodes.

    {1 Kinds}

    Each node has a kind, the [kind] property:

    - [root]
    - [section]: a heading of the document, not inside a container, with the
      blocks up to the next heading of the same or a higher level. Its first
      child is its heading.
    - [heading]
    - [paragraph]
    - [keyed]: [label: value], see {!Djot.Block.Ext_keyed}
    - [code_block], [raw_block]
    - [callout], [block_quote], [div]
    - [list], [list_item]
    - [footnote_definition]
    - [table], [definition_list], [thematic_break]

    {1 Properties}

    - all nodes: [kind : string]
    - heading: [level : int], [text : string]
    - section: same as its heading
    - code block: [lang : string] when it has one, [text : string]
    - raw block: [format : string], [text : string]
    - callout: [type : string] (lowercased: [note] for [ [!NOTE] ]),
      [title : string] when it has one
    - list: [ordered : bool], [tight : bool], [length : int]
    - list item: [task : string] ([unchecked] or [checked]) when it is a task,
      [key : string] when its only block is a keyed node
    - keyed: [key : string]
    - footnote definition: [label : string]

    followed by the node's djot attributes: [id], each class under [class], and
    each key/value pair under its key. An attribute value is a string; a
    predicate comparing it with a number or a boolean reads it as one, so
    [Prop(depth, =, 1)] holds for [depth=1]. A name can repeat, when a node has
    several classes or an attribute names a property the node already has. *)

val kinds : string list

type value =
  | Int of int
  | String of string
  | Bool of bool

val value_to_string : value -> string

type span =
  { first_line : int
  ; last_line : int
  ; first_byte : int
  ; last_byte : int
  }

(** A node in context. *)
type found_t =
  { kind : string
  ; path : int list
    (** Its position, from the root: its index among its siblings at each
        level, outermost first. A child step taking that index at each level,
        in order, reaches it again. *)
  ; names : Anchor.Address.t list (** The addresses that name it. *)
  ; headings : Anchor.heading list
    (** The headings whose sections contain it, outermost first. *)
  ; span : span option
    (** Its source, with the attribute specs of a block. [None] unless the note
        was parsed with locations. *)
  ; props : (string * value) list (** Its properties, see above. *)
  ; blocks : Djot.Block.t Djot.node list
    (** The node as blocks: itself for a block, its blocks for the root, a list
        item or a footnote definition. See {!Query.markdown}. *)
  }

(** The property named [name] of [found], the first if it repeats. *)
val prop : string -> found_t -> value option

(** The source text of [found] in [content], the text its note was parsed from
    with locations: from the start of its [span] to the end, with trailing
    whitespace removed. [None] without a span.

    The slice is taken byte for byte, so inside a container that prefixes each
    line (a block quote's [>], a list item's indentation) every line but the
    first keeps that prefix. *)
val source_text : string -> found_t -> string option
