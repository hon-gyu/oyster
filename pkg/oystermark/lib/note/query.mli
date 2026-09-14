(** Querying the blocks of a note by composing small steps.

    A query is a list of {!step}s run from a list of cursors, usually {!top}.
    Each step maps the cursors it receives to new cursors. The flags of
    [oyster block] are sugar: {!of_flags} translates them to steps. *)

(** {1 Cursor} *)

(** What a cursor points at. *)
type view =
  | Block of Cmarkit.Block.t
  (** Never [Blocks], [Ext_attributes], [Blank_line] or
        [Link_reference_definition]: groupings and attribute wrappers are looked
        through, and blank lines and label definitions are skipped. *)
  | Item of Cmarkit.Block.List_item.t Cmarkit.node
  (** A list item, which is not a [Cmarkit.Block.t]. *)

(** A view and where it is in the note. *)
type cursor

(** The cursors of the blocks at the top level of [doc]. *)
val top : Cmarkit.Doc.t -> cursor list

val view : cursor -> view

(** The position of [cursor]: its index among its siblings at each level,
    outermost first. From {!top}, running [Nth (i + 1)] for the first index,
    then [Children; Nth (i + 1)] for each other index, returns [cursor] again. *)
val path : cursor -> int list

(** The cursors directly inside [cursor], in document order.

    - A block quote or callout, a div, a keyed block, a footnote
      definition and a list item: the blocks they hold.
    - A list: its items.
    - Any other block: none.

    Blocks of a kind {!kind} does not name, such as frontmatter, are skipped. A
    heading has no children; its section is a run of its siblings, see
    {!section}. *)
val children : cursor -> cursor list

(** The cursors below [cursor] in document order, containers before their
    contents. [cursor] itself is not included. *)
val descendants : cursor -> cursor list

(** The section of a heading: the siblings after it, up to the next heading of
    the same or a higher level, or the end of its container. With
    [~nested:false], up to the next heading of any level instead. [ [] ] if
    [cursor] is not a heading.

    This is the section {!Read.read} returns for a heading address, without the
    heading. *)
val section : nested:bool -> cursor -> cursor list

(** {2 Metadata} *)

(** A stable name for the kind of [cursor]: [list_item], or the kind of its
    block, such as [code_block] or [heading]. A callout is named [callout], not
    [block_quote]. *)
val kind : cursor -> string

(** The metadata of the view: its location, and ids such as a caret id. *)
val meta : cursor -> Cmarkit.Meta.t

(** The info string of a fenced code block: [python] for [ ```python ]. *)
val info_string : cursor -> string option

(** The headings whose sections contain [cursor], outermost first. Sections are
    those of {!section}, so a heading inside a container does not enclose the
    blocks after that container. *)
val headings : cursor -> Anchor.heading list

(** The addresses that name [cursor]: those {!Read.read} resolves to its block.
    A [ ^id ] on a line of its own names the previous block, not the paragraph
    it is written in. Sorted, without duplicates.

    @raise when a heading has no identifier: parse with {!Parse.of_string}. *)
val names : cursor -> Anchor.Address.t list

(** {1 Query} *)

type pred =
  | Kind of string (** {!kind} is this. *)
  | Lang of string (** {!info_string} is exactly this. *)
  | Named of Anchor.Address.t (** {!names} contains this address. *)

type step =
  | Children (** {!children} of each cursor. *)
  | Descendants (** {!descendants} of each cursor. *)
  | Descendants_or_self (** Each cursor, followed by its {!descendants}. *)
  | Section of { nested : bool } (** {!section} of each heading. *)
  | Filter of pred (** The cursors for which the predicate holds. *)
  | Nth of int (** The [n]th cursor, 1-based. *)

type t = step list

(** One step of a run: what reached the step and what it returned. *)
type stage =
  { step : step
  ; input : cursor list
  ; output : cursor list
  }

type result =
  { matches : cursor list
  ; stages : stage list (** In the order the steps ran. *)
  }

(** Run [query] from [start]. A step that returns nothing is not an error: the
    remaining steps run on [ [] ]. *)
val run : t -> cursor list -> result

(** Why [result] has no matches: the first stage that returned nothing, and
    what it could have matched instead, such as the headings or kinds among its
    input, or that the query started from no blocks. [None] when there are
    matches. *)
val why_empty : result -> string option

(** {2 Flags} *)

(** The flags of [oyster block]. *)
type flags =
  { under : string option
    (** Only blocks in the section of the heading with this id or text. The
        value is converted to an id with {!Parse.Common.heading_id_of_text}. *)
  ; direct : bool (** With [under], stop the section at the first subheading. *)
  ; kind : string option
  ; lang : string option
  ; attr_id : string option (** Only the block a link to [ #id ] names. *)
  ; caret_id : string option (** Only the block a link to [ #^id ] names. *)
  ; nth : int option (** Of the blocks the other flags keep, only the [n]th. *)
  }

(** The steps [flags] stand for:
    - the scope: [Descendants_or_self], or with [under h]
      [Descendants_or_self; Filter (Named (Heading h)); Section { nested }; Descendants_or_self]
      where [nested] is [not direct];
    - then [Filter] for each of [kind], [lang], [attr_id] and [caret_id];
    - then [Nth] for [nth]. *)
val of_flags : flags -> t

(** {1 Content} *)

(** The contents of a container, without the container's own syntax.

    [Literal] is text whose fence and indentation the parser already removed,
    exactly as written. [Markdown] is the inner blocks of a container such as a
    block quote; {!content_string} renders them, so the result can differ from
    the source in formatting (fences, list markers, wrapping). *)
type content =
  | Literal of string
  | Markdown of Cmarkit.Block.t
  | Not_a_container

(** The contents of [cursor]. A heading's contents are its section, with
    subsections. [Not_a_container] for paragraphs, tables, thematic breaks and
    lists. *)
val content : cursor -> content

(** The contents of [cursor] as a string, rendered with the note's label
    definitions. [Error kind] when it is not a container. *)
val content_string : cursor -> (string, string) Result.t
