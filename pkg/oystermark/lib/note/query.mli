(** Querying the blocks of a note by composing small steps.

    A query is a list of {!step}s run from a list of cursors, usually {!top}.
    Each step maps the cursors it receives to new cursors. There are few
    primitive steps; {!Sugar} builds common queries from them. [oyster block]'s
    flags are one syntax for queries, see {!Query_flags}. *)

(** {1 Cursor} *)

(** A block or list item of a note, and where it is. *)
type cursor

(** The cursors of the blocks at the top level of [doc]. *)
val top : Cmarkit.Doc.t -> cursor list

(** What [cursor] points at. *)
val node : cursor -> Node.t

(** The position of [cursor]: its index among its siblings at each level,
    outermost first. From {!top}, running [Nth (i + 1)] for the first index,
    then [Children; Nth (i + 1)] for each other index, returns [cursor] again. *)
val path : cursor -> int list

(** The cursors directly inside [cursor], in document order.

    - A block quote, a div, a footnote definition and a list item: the blocks
      they hold.
    - A callout: the blocks of its body, without the [ [!type] title ] header.
    - A keyed node: the blocks of its value, without the label.
    - A list: its items.
    - Any other node: none.

    Blank lines, link reference definitions and block attributes are not nodes,
    see {!Node.of_block}. A heading has no children; its section is a run of its
    siblings, see {!section}. *)
val children : cursor -> cursor list

(** The section of a heading: the siblings after it, up to the next heading of
    the same or a higher level, or the end of its container. With
    [~nested:false], up to the next heading of any level instead. [ [] ] if
    [cursor] is not a heading.

    This is the section {!Read.read} returns for a heading address, without the
    heading. *)
val section : nested:bool -> cursor -> cursor list

(** {2 Metadata} *)

(** The metadata of what [cursor] points at: its location, and ids such as a
    caret id. *)
val meta : cursor -> Cmarkit.Meta.t

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

type cmp =
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge

type pred =
  | Prop of string * cmp * Node.value
  (** The node's value of the named property, see {!Node.props}, compares to
        the given value. Does not hold if the node does not have the property,
        or if its value is of another type. *)
  | Has of string (** The node has the named property. *)
  | Named of Anchor.Address.t (** {!names} contains the address. *)
  | Count of t * cmp * int
  (** The number of cursors the query returns, run from the cursor alone,
        compares to the given number. *)
  | And of pred list
  | Or of pred list
  | Not of pred
  | Custom of string * (cursor -> bool)
  (** Any OCaml predicate. The string names it in {!why_empty}. *)

and step =
  | Children (** {!children} of each cursor. *)
  | Section of { nested : bool } (** {!section} of each heading. *)
  | Filter of pred (** The cursors for which the predicate holds. *)
  | Nth of int (** The [n]th cursor, 1-based. *)
  | Each of t list
  (** A map: for each cursor, what each query returns when run from that
        cursor alone, concatenated in order. Steps inside see one cursor at a
        time, so [Each [ [ Children; Nth 1 ] ]] is the first child of each
        cursor, where [Children; Nth 1] is the first child of all of them. The
        empty query returns its input, so [Each [ []; q ]] keeps each cursor and
        adds what [q] returns from it. *)
  | Recurse of
      { steps : t
      ; emit : pred (** Return a result for which this holds. *)
      ; descend : pred (** Run [steps] again from a result for which this holds. *)
      }
  (** For each cursor, depth first: run [steps] from it, then for each result,
        return it if [emit] holds and recurse into it if [descend] holds.
        [emit] and [descend] are tested on results, not on the cursors the step
        starts from. [And []] always holds. [steps] must eventually return
        nothing, as [Children] does, unless [descend] stops the recursion. *)

and t = step list

(** Common queries, built from the primitive steps. *)
module Sugar : sig
  (** [Prop ("kind", Eq, String kind)]: the node is of this kind, see
      {!Node.kinds}. *)
  val is : string -> pred

  (** [ [Recurse { steps; emit = And []; descend = And [] }] ]: every result, at
      any depth. *)
  val recurse : t -> t

  (** [ [Recurse { steps; emit = pred; descend = Not pred }] ]: on each branch,
      the first results for which [pred] holds, without looking inside them. *)
  val until : t -> pred -> t

  (** [recurse [ Children ]]: every cursor below. *)
  val descendants : t

  (** [ [Each [ []; descendants ]] ]: each cursor, then every cursor below it. *)
  val descendants_or_self : t

  (** [query], [n] times in a row. *)
  val times : int -> t -> t

  (** [Count (query @ [Filter pred], Gt, 0)]. *)
  val exists : t -> pred -> pred

  (** [Count (query @ [Filter (Not pred)], Eq, 0)]: true when [query] returns
      nothing. *)
  val for_all : t -> pred -> pred

  (** The value of a keyed node, or of a list item with a key: the keyed node's
      children. *)
  val value : t

  (** The value under [key], from a keyed node or list item with that key, or
      from a container of those such as a list:
      [Each [ []; [Children] ]; Filter (Prop ("key", Eq, String key))] then
      {!value}. *)
  val field : string -> t
end

val to_string : t -> string
val pred_to_string : pred -> string

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
    what its input had instead, such as the kinds or the values of a property;
    or that the query started from no blocks. [None] when there are matches. *)
val why_empty : result -> string option

(** {1 Content} *)

(** What [cursor] holds, without its own syntax: the text of a code, math, HTML
    or raw block, exactly as written; the rendered {!children} of a container;
    the rendered section of a heading. Rendered Markdown can differ from the
    source in formatting (fences, list markers, wrapping). [Error kind] for a
    node that holds nothing to print, such as a paragraph or a list. *)
val content_string : cursor -> (string, string) Result.t

(** [cursor] rendered as Markdown, for a node without source text of its own,
    such as the value [oyster] of [- name: oyster]. A list item is rendered as
    its blocks, without the marker. *)
val markdown : cursor -> string
