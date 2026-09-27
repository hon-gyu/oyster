(** Path-like query for nodes of a note.

    A query is composed from {!type-step}s, run over a document by {!val-run}, starting
    from the root. The nodes it walks are described in {!Node}.

    Each step moves along an {!type-axis} from the nodes it receives, keeps those its
    predicates hold for, and optionally takes one of them. *)

(* ai-disclousure: ai-generated *)

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
  (** Compare a named property's value with a given value. *)
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
  (** The nodes directly inside the node: the blocks it holds in djot's tree.

        - The root: its blocks, then its footnote definitions.
        - A section: its heading, then the blocks and subsections under it.
        - A callout: the blocks of its body; its title is inlines.
        - A keyed node: its value; its label is inlines.
        - A list: its items.
        - Any other node without blocks: none.

        Reference definitions are not nodes. *)
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
        {!Parse.Common.heading_key}, as wikilink fragments are. With [exact],
        the headings from the node down to the section are exactly [path];
        without it, [path] is a sub-path of them: gaps are allowed, and it need
        not start at the node.

        Only headings of the document open sections: a heading inside a block
        quote, div, callout or list item is a plain heading, which this axis
        does not find. *)
(* TODO: upward axes. [found_t.headings] covers the common case of naming the section a match is in.
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

(** @param exact if [false], sub-path (ordered) can be matched against; defaults to [true]. *)
val section : ?exact:bool -> ?where:pred list -> ?nth:int -> string list -> steps -> steps

(** The node [address] names: the first node in document order whose [id] is
    the address's. Heading and attribute identifiers share one namespace per
    note, see {!Anchor.Address.id}, so [Heading x] and [Attr x] find the same
    node: a section for a heading of the document, the heading for any other,
    and the block an attribute is on. An attribute on inlines names no node. *)
val of_address : Anchor.Address.t -> steps

type t = steps

(** {2 Syntax}

    One syntax, written by {!to_string} and read by {!of_string}. A query is a
    list of steps, and each step and predicate is written as its constructor
    called with its arguments:

    {v
    query := [STEP, ...]
    STEP  := Self | Child | Descendant | Field(KEY) | Section([NAME, ...])
             each optionally with where=[PRED, ...], nth=INT,
             and a section with exact=false
    PRED  := Is(KIND) | Prop(NAME, OP, VALUE) | Has(NAME) | Not(PRED)
           | And([PRED, ...]) | Or([PRED, ...])
           | Exists(query) | Count(query, OP, INT)
    OP    := = | != | < | <= | > | >=
    v}

    [Is(KIND)] is [Prop(kind, =, KIND)]. A step without arguments may be
    written [Child()] as well as [Child]. A word that reads as an integer is an
    [Int], [true] and [false] are a [Bool], and anything else is a [String]; a
    string in double quotes is always a [String], as [Prop(key, =, "12")].

    {[
      [Section([other], exact=false), Descendant(where=[Is(code_block)], nth=1)]
    ]} *)

val to_string : t -> string

(** The steps [text] describes, or why it cannot be read. *)
val of_string : string -> (t, string) Result.t

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
    step returns nothing from the root. *)
type no_match =
  { index : int
  ; step : step
  ; reached : Node.found_t list (** The nodes that arrived at the step. *)
  ; stage : stage
  }

type result =
  { matches : Node.found_t list (** In document order, without duplicates. *)
  ; no_match : no_match option (** [Some] exactly when [matches] is empty. *)
  }

(** Run [query] over [doc], from the root. A step that returns nothing is not
    an error: the steps after it run on [ [] ], and the result says which step
    it was. *)
val run : t -> Djot.Doc.t -> result

(** The node [found] as djot source: its {!Node.found_t.blocks}, rendered by
    {!Djot.Source.of_doc}. A callout renders with its [ [!type] title ] header,
    a section with its heading. Rendering can differ from the source in
    formatting; use {!Node.source_text} to quote the source instead. *)
val markdown : Node.found_t -> string

(** The document whose root holds what [query] matches in [doc]: the blocks of
    a section without its heading, of a list item, of a block quote, callout
    (without its header), div, keyed node (without its label) or footnote
    definition, and any other block itself. [doc]'s reference and footnote
    definitions are kept. The result has no locations.

    For a single match that is the root, a list item or one of those
    containers, running a query on the result is running it from the match:
    [run b (extract a doc)] matches the nodes [run (a @ b) doc] does, with
    [path] and [headings] counted from the match. For a section, the heading
    that was its first child is not in the result. *)
val extract : t -> Djot.Doc.t -> Djot.Doc.t

val no_match_to_string : no_match -> string
