(** A block or list item of a note: its kind and its properties.

    A node does not hold its children; the [Children] step of {!Query} moves
    to them. *)

type task =
  [ `Unchecked
  | `Checked
  | `Cancelled
  | `Other of Uchar.t
  ]

type t =
  | Heading of
      { level : int
      ; id : string (** See {!Parse.Common.heading_id}. *)
      ; text : string (** As plain text. *)
      }
  | Paragraph
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
  | Keyed of { key : string (** The label as plain text, without the colon. *) }
  | Div of { class_ : string option }
  | Footnote_definition of { label : string }
  | Table
  | Definition_list
  | Thematic_break
  | Html_block of { text : string }

(** The name of the node's kind in lower snake case, such as [heading],
    [code_block] or [list_item]. It is also the node's [kind] property. *)
val kind : t -> string

(** Every kind name. *)
val kinds : string list

(** [None] for what is not a node: blank lines, link reference definitions, a
    block attribute such as [ {#id} ] on its own line (it belongs to the block
    after it), and syntax whose kind is not in {!kinds}, such as frontmatter.

    @raise Invalid_argument on a JSX block, which is not supported. *)
val of_block : Cmarkit.Block.t -> t option

val of_item : Cmarkit.Block.List_item.t Cmarkit.node -> t

(** {1 Properties} *)

type value =
  | Int of int
  | String of string
  | Bool of bool

val value_to_string : value -> string

(** The properties of [node] as name/value pairs, in this order:
    - every node: [kind] first, see {!kind};
    - heading: [level], [id], [text];
    - code block: [info] if it has an info string, then [text];
    - math and HTML block: [text];
    - raw block: [format], [text];
    - callout: [type], then [title] if it has one;
    - list: [ordered], [tight], [length] (the number of items);
    - list item: [task] ([unchecked], [checked] or [cancelled]) if it is a task,
      then [key] if it has one;
    - keyed node: [key];
    - div: [class] if it has one;
    - footnote definition: [label];
    - any other node: none. *)
val props : t -> (string * value) list

(** The value of the property named [name] on [node], if it has one. *)
val prop : string -> t -> value option
