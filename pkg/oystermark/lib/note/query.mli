(** Querying the nodes of a note.

    A query is a list of {!step}s, run over a document by {!val-run}. Each step
    moves along an {!axis} from the nodes it receives, keeps those its
    predicates hold for, and optionally takes one of them; the query starts from
    {!Node.Root}.

    A query selects blocks and list items; what it can select and what it can
    ask about them is {!Node.t}, whose properties are also what a predicate
    tests and what a caller reads off a match. Where a node is and how to reach
    the nodes inside it are internal to this module: a query returns a
    {!Node.found_t}, not a position to navigate from. *)

(** {1 Predicates} *)

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
  | Exists of steps
  (** The sub-query, run from the node alone, returns something: a section
        that holds a Python code block, a task whose list has an unchecked
        item. [Exists q] is [Count (q, Gt, 0)]. *)
  | Count of steps * cmp * int
  (** The number of nodes the sub-query returns, run from the node alone,
        compares to the given number. *)
  | And of pred list
  | Or of pred list
  | Not of pred

(** {1 Steps} *)

(** Where to move, from one node. Every axis returns nodes in document order,
    without duplicates: when the nodes a step receives include both a node and
    one of its ancestors, what they reach is merged, not repeated. *)
and axis =
  | Self (** The node itself: a filter in place. *)
  | Child
  (** The nodes directly inside the node.

        - A block quote, a div, a footnote definition and a list item: the
          blocks they hold.
        - A callout: the blocks of its body, without the [ [!type] title ]
          header.
        - A keyed node: the blocks of its value, without the label.
        - A list: its items.
        - A section: its blocks and its subsections, without its own heading.
        - Any other node: none.

        Blank lines, link reference definitions and block attributes are not
        nodes, see {!Node.of_block}. *)
  | Descendant (** Every node below the node, depth first. *)
  | Field of string
  (** The value of this key, from the keyed syntax. The fields of a list are
        its keyed items. The fields of any other node are its keyed children,
        and, when the node is a list item or a keyed node, the keyed items of
        any list among those children: under such a node a list describes it,
        whereas under a section or a div a list is content of its own. Fields
        are not looked for inside another field's value, so a field path is
        exactly as deep as the nesting in the source.

        Several children can carry the same key; the axis returns all of them,
        and [nth] picks one. *)
  | Section of
      { path : string list
      ; exact : bool
      }
  (** The sections under the node whose headings match [path], compared by
        heading identifier, see {!Parse.Common.heading_id}. With [exact], the
        headings from the node down to the section are exactly [path]; without
        it, [path] is a sub-path of them: gaps are allowed, and it need not
        start at the node. This is the sub-path matching of a heading address,
        see {!Anchor.Address}.

        A section never crosses a container boundary, and this axis does not
        reach into containers: a heading inside a block quote or a list item is
        reached through {!Child} or {!Field} first. *)
(* Upward axes, when something needs them. [found_t.headings] covers the
   common case of naming the section a match is in.

   | Parent
   | Ancestor *)

(** Move along [axis] from each node, keep the nodes every predicate of [where]
    holds for, then take the one at [nth] among them, if given.

    [where] is a conjunction, empty for no filtering. [nth] is 0-based over the
    whole sequence the step produced, and counts from the end when negative:
    [-1] is the last node. A step that produces nothing is not an error: the
    steps after it run on [ [] ]. *)
and step =
  { axis : axis
  ; where : pred list
  ; nth : int option
  }

and steps = step list

(** [Prop ("kind", Eq, String kind)]: the node is of this kind, see
    {!Node.kinds}. *)
val is : string -> pred

(** {2 Combinators}

    Each one appends a step, so a query reads left to right:

    {[
      empty |> section [ "Other" ] |> child ~where:[ is "code_block" ] ~nth:1
    ]} *)

val empty : steps
val self : ?where:pred list -> ?nth:int -> steps -> steps
val child : ?where:pred list -> ?nth:int -> steps -> steps
val descend : ?where:pred list -> ?nth:int -> steps -> steps
val field : ?where:pred list -> ?nth:int -> string -> steps -> steps

(** [exact] defaults to [true]. *)
val section : ?exact:bool -> ?where:pred list -> ?nth:int -> string list -> steps -> steps

type t = steps

val to_string : t -> string
val step_to_string : step -> string
val pred_to_string : pred -> string

(** {1 Run} *)

(** Which part of a step emptied the sequence. Each one is a different report:
    what the note has where the axis looked, what the candidates were, or how
    many there were to index. *)
type stage =
  | No_candidate (** The axis returned nothing from the nodes that reached it. *)
  | Filtered_out of Node.found_t list
  (** What the axis returned, all of which [where] rejected. Their kinds, or
        their values of the property a predicate tested, are what a caller
        reports. *)
  | Out_of_range of { length : int }
  (** [nth] is outside the [length] nodes [where] kept. *)

(** The first step that returned nothing. The steps after it ran on [ [] ] and
    are not reported. A note with no nodes needs no case of its own: the first
    step returns nothing from {!Node.Root}. *)
type no_match =
  { index : int
  ; step : step
  ; reached : Node.found_t list (** The nodes that arrived at the step. *)
  ; stage : stage
  }

type result =
  { matches : Node.found_t list (** In document order, without duplicates. *)
  ; why_empty : no_match option (** [Some] exactly when [matches] is empty. *)
  }

(** Run [query] over [doc], from {!Node.Root}. A step that returns nothing is
    not an error: the steps after it run on [ [] ], and the result says which
    step it was.

    @raise when a heading has no identifier: parse with {!Parse.of_string}. *)
val run : t -> Cmarkit.Doc.t -> result

val no_match_to_string : no_match -> string
