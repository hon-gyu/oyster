(** A node  *)

type task =
  [ `Unchecked
  | `Checked
  | `Cancelled
  | `Other of Uchar.t
  ]

type t =
  | Root
  | Heading of
      { level : int
      ; id : string (** See {!Parse.Common.heading_id}. *)
      ; text : string (** As plain text. *)
      }
  | Section of
      { heading : t
      ; children : t list
      }
  (** A section never crosses a container boundary, so a heading inside
    a block quote or list item sections  that container's blocks only
    *)
  | Paragraph
  | Keyed_paragraph of { key : string }
  | Code_block of
      { info : string option (** [python] for [ ```python ]. *)
      ; text : string (** Without the fence. *)
      }
  | Math_block of { text : string }
  | Raw_block of
      { format : string
      ; text : string
      }
  | Callout of
      { type_ : string (** Lowercased: [note] for [ [!NOTE] ]. *)
      ; title : string option (** As plain text. *)
      }
  | Block_quote
  | List of
      { ordered : bool
      ; tight : bool
      ; length : int (** The number of items. *)
      }
  | List_item of
      { task : task option
      ; key : string option
        (** The key of the item's keyed node, when that node is the item's only
            block: [name] for [- name: oyster]. *)
      }
  | Div of { class_ : string option }
  | Footnote_definition of { label : string }
  | Table
  | Definition_list
  | Thematic_break
  | Html_block of { text : string }

type span =
  { first_line : int
  ; last_line : int
  ; first_byte : int
  ; last_byte : int
  }

(** The name of the node's kind in lower snake case, such as [heading],
    [code_block] or [list_item]. It is also the node's [kind] property. *)
val kind : t -> string

(** Every kind name. *)
val kinds : string list

(** [None] for what is not a node: blank lines, link reference definitions, a
    block attribute such as [ {#id} ] on its own line (it belongs to the block
    after it), and syntax whose kind is not in {!kinds}, such as frontmatter.

    Raises [Invalid_argument] on a JSX block. *)
val of_block : Cmarkit.Block.t -> t option

val of_item : Cmarkit.Block.List_item.t Cmarkit.node -> t

type value =
  | Int of int
  | String of string
  | Bool of bool

val value_to_string : value -> string

(** A node in context. *)
type found_t =
  { node : t (** What the query selected. *)
  ; path : int list
    (** Its position, from {!Root}: its index among its siblings at each level,
            outermost first. A child step taking that index at each level, in
            order, reaches it again. Sections are levels of their own, since
            they are nodes. *)
  ; names : Anchor.Address.t list
    (** The addresses that name it, see {!Anchor.Address}. A [ ^id ] on a line
            of its own names the previous block, not the paragraph it is written
            in. Sorted, without duplicates. *)
  ; headings : Anchor.heading list
    (** The headings whose sections contain it, outermost first. Sections are
            those of {!Section}, so a heading inside a container encloses that
            container's blocks only. *)
  ; span : span option (** [None] unless the note was parsed with [~locs:true]. *)
  ; markdown : string (* CR: v this is too verbose. The first sentence might be enough *)
    (** The node rendered as Markdown, including its own syntax: a callout
            renders with its [ [!type] title ] header, a section with its
            heading. For what is inside without that syntax, query the node's
            children. Rendering can differ from the source in formatting
            (fences, list markers, wrapping); use [span] to quote the source
            instead. A node without source text of its own, such as the value
            [oyster] of [- name: oyster], has only this. *)
  ; props : (string * value) list
    (** Every property a predicate could have tested on it, in the order
            {!val-props} lists them, followed by the [id] of each of its [names]
            and the [class] and key/value pairs of the djot attribute written
            on it. A name can repeat; the first entry is
            the one {!val-props} would have produced. *)
  }

(** The source text of [found] in [content], the text its note was parsed from
    with locations: from the start of its [span] to the end, with trailing
    whitespace removed. [None] without a span.

    The slice is taken byte for byte, so inside a container that prefixes each
    line (a block quote's [>], a list item's indentation) every line but the
    first keeps that prefix. *)
val source_text : string -> found_t -> string option

(** {1 Properties}

    A node carries the properties below. What names it and what a djot attribute
    written on it says are not among them: a node cannot know what is written
    around it, so {!Query} adds the identifier of each address that names it,
    heading, caret and attribute alike, under [id], and the attribute's classes
    and key/value pairs under [class] and their own names. A predicate sees
    both sets, and a name can repeat, when a node carries more than one
    identifier or class, or when an attribute names a property the node already
    has.

    An attribute value is written without a type, so it is offered both as the
    text and as the number or boolean it spells, and a predicate is satisfied by
    either reading.

- all nodes:
  - [kind : string]
  - [is_container: bool]
- heading:
  - [level : int]
  - [text : string]
- section: same as [heading]
- code block:
  - [lang : string option]
  - [text : string]
- math and HTML block: [text]
- raw block:
  - [format : string]
  - [text : string]
- callout:
  - [type : string]
  - [title : string option]
- list:
  - [ordered : bool]
  - [tight : bool]
  - [length : int]
- paragraph:
  - [key : string option]
- list item:
  - [task : string option]: [unchecked], [checked] or [cancelled] if it is a task
  - [key : string option]
- footnote definition:
  - [label : string]
 *)

(** The value of the property named [name] on [node], if it has one. *)
val prop : string -> t -> value option

(** Non-null key/value pairs *)
val props : t -> (string * value) list
