(** Querying the blocks of a note by composing small steps.

    A query is a list of {!step}s, run over a document by {!val-run}. Each step maps
    the blocks it receives to new blocks; the query starts from the blocks at
    the top level of the document. There are few primitive steps; {!Sugar}
    builds common queries from them. [oyster block]'s flags are one syntax for
    queries, see {!Query_flags}.

    A query selects blocks and list items; what it can select and what it can
    ask about them is {!Node.t}. Where a block is, how to reach the blocks
    inside it, and how it is rendered back to Markdown are internal to this
    module: a query returns a {!found}, not a position to navigate from. *)

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
  | Named of Anchor.Address.t
  (** The address names the node: it is what {!Read.read} resolves the address
        to. A [ ^id ] on a line of its own names the previous block, not the
        paragraph it is written in. *)
  | Count of t * cmp * int
  (** The number of blocks the query returns, run from the node alone, compares
        to the given number. *)
  | And of pred list
  | Or of pred list
  | Not of pred
  | Custom of string * (Node.t -> bool)
  (** Any OCaml predicate on the node. The string names it in {!why_empty}. *)

and step =
  | Children
  (** The nodes directly inside each node, in document order.

        - A block quote, a div, a footnote definition and a list item: the
          blocks they hold.
        - A callout: the blocks of its body, without the [ [!type] title ]
          header.
        - A keyed node: the blocks of its value, without the label.
        - A list: its items.
        - Any other node: none.

        Blank lines, link reference definitions and block attributes are not
        nodes, see {!Node.of_block}. A heading has no children; its section is a
        run of its siblings, see [Section]. *)
  | Section of { nested : bool }
  (** The section of each heading: the siblings after it, up to the next
        heading of the same or a higher level, or the end of its container.
        With [nested = false], up to the next heading of any level instead.
        Nothing for a node that is not a heading.

        This is the section {!Read.read} returns for a heading address, without
        the heading. *)
  | Filter of pred (** The nodes for which the predicate holds. *)
  | Nth of int (** The [n]th node, 1-based. *)
  | Each of t list
  (** A map: for each node, what each query returns when run from that node
        alone, concatenated in order. Steps inside see one node at a time, so
        [Each [ [ Children; Nth 1 ] ]] is the first child of each node, where
        [Children; Nth 1] is the first child of all of them. The empty query
        returns its input, so [Each [ []; q ]] keeps each node and adds what [q]
        returns from it. *)
  | Recurse of
      { steps : t
      ; emit : pred (** Return a result for which this holds. *)
      ; descend : pred (** Run [steps] again from a result for which this holds. *)
      }
  (** For each node, depth first: run [steps] from it, then for each result,
        return it if [emit] holds and recurse into it if [descend] holds.
        [emit] and [descend] are tested on results, not on the nodes the step
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

  (** [recurse [ Children ]]: every node below. *)
  val descendants : t

  (** [ [Each [ []; descendants ]] ]: each node, then every node below it. *)
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

(** {1 Run} *)

(** Where a node is in the source of the note. Lines and bytes are 0-based, and
    both ends are inclusive. *)
type span =
  { first_line : int
  ; last_line : int
  ; first_byte : int
  ; last_byte : int
  }

(** A node a query returned, and what is known about it in its note. *)
type found =
  { node : Node.t (** What the query selected. *)
  ; path : int list
    (** Its position: its index among its siblings at each level, outermost
            first. Running [Nth (i + 1)] for the first index, then
            [Children; Nth (i + 1)] for each other index, returns it again. *)
  ; names : Anchor.Address.t list
    (** The addresses that name it, see [Named]. Sorted, without duplicates. *)
  ; headings : Anchor.heading list
    (** The headings whose sections contain it, outermost first. Sections are
            those of [Section], so a heading inside a container does not
            enclose the blocks after that container. *)
  ; span : span option (** [None] unless the note was parsed with [~locs:true]. *)
  ; content : (string, string) Result.t
    (** What the node holds, without its own syntax: the text of a code, math,
            HTML or raw block, exactly as written; the rendered children of a
            container; the rendered section of a heading. [Error kind] for a
            node that holds nothing to print, such as a paragraph or a list. *)
  ; markdown : string
    (** The node rendered as Markdown, for a node without source text of its
            own, such as the value [oyster] of [- name: oyster]. A list item is
            rendered as its blocks, without the marker. Rendered Markdown can
            differ from the source in formatting (fences, list markers,
            wrapping); use [span] to quote the source instead. *)
  }

type result

(** Run [query] over [doc], from the blocks at its top level. A step that
    returns nothing is not an error: the remaining steps run on [ [] ].

    @raise when a heading has no identifier: parse with {!Parse.of_string}. *)
val run : t -> Cmarkit.Doc.t -> result

val matches : result -> found list

(** Why [result] has no matches: the first step that returned nothing, and what
    reached it instead, such as the kinds or the values of a property; or that
    the note has no blocks. [None] when there are matches. *)
val why_empty : result -> string option
