Cmarkit Mapper API Documentation

Based on my search through the Cmarkit source and oystermark codebase,
here's a comprehensive understanding of the Mapper API:

1. Mapper.t Type

From
cmarkit/src/cmarkit.mli
(line 1470):
type t
(** The type for abstract syntax tree mappers. *)

Mapper.t is an opaque type that represents an abstract syntax tree mapper.
It's created using Mapper.make and holds the configuration for mapping both
inline and block elements.

2. Mapper.result Type

From
cmarkit/src/cmarkit.mli
(lines 1455-1457):
type 'a result =
[ `Default (** Do the default map. *) | `Map of 'a filter_map ]
(** The type for mapper results. *)

And 'a filter_map is defined as (line 1451):
type 'a filter_map = 'a option
(** The type for maps. [None] is for node deletion. [Some n] is a map to
[n]. *)

So Mapper.result is a polymorphic variant with two cases:
-  `Default - tells the mapper to use its default mapping behavior
-  `Map of 'a option - explicitly returns a mapped value (or None to delete
the node)

3. Mapper.default and Mapper.ret

From
cmarkit/src/cmarkit.mli
(lines 1459-1466):
val default : 'a result
(** [default] is [`Default]. *)

val delete : 'a result
(** [delete] is [`Map None]. *)

val ret : 'a -> 'a result
(** [ret v] is [`Map (Some v)]. *)

- Mapper.default: A convenience value that returns  `Default, telling the
mapper to apply default handling for that node
- Mapper.ret v: A convenience function that wraps value v as  `Map (Some v)
to explicitly return a mapped version of the node
- Mapper.delete: Returns  `Map None to delete a node from the AST

4. Inline Mapper Signature

From
cmarkit/src/cmarkit.mli
(lines 1473-1482):
type 'a map = t -> 'a -> 'a filter_map
(** The type for maps on values of type ['a]. *)

type 'a mapper = t -> 'a -> 'a result
(** The type for mappers on values of type ['a].

    This is what you specify. Return [`Default] if you are not
    interested in handling the given case. Use {!map_inline} or
    {!map_block} with the given mapper if you need to call the
    mapper recursively. *)

An inline mapper has the signature: Mapper.t -> Inline.t -> Inline.t
Mapper.result

This is a function that:
1. Takes a Mapper.t (the mapper itself, for recursive calls)
2. Takes an Inline.t (the inline node to process)
3. Returns Inline.t Mapper.result indicating what to do with this node

5. Inline.Inlines Type

From
cmarkit/src/cmarkit.mli
(lines 817-829):
type t +=
| Autolink of Autolink.t node
| Break of Break.t node
| Code_span of Code_span.t node
| Emphasis of Emphasis.t node
| Image of Link.t node
| Inlines of t list node (** Splicing *)
| Link of Link.t node
| Raw_html of Raw_html.t node
| Strong_emphasis of Emphasis.t node
| Text of Text.t node (** *)

And 'a node is defined as (line 264):
type 'a node = 'a * Meta.t
(** The type for abstract syntax tree nodes. The data of type ['a] and its
    metadata. *)

So Inline.Inlines is a variant case that:
- Takes t list node - a list of inline elements paired with metadata
- Represents "splicing" - a way to replace a single inline with multiple
inlines
- Construction: Inline.Inlines (inlines_list, metadata)
- Pattern matching: Inline.Inlines (inlines, meta)

Also note (line 831-832):
val empty : t
(** [empty] is [Inlines ([], Meta.none)]. *)

7. Mapper.make Signature

From
cmarkit/src/cmarkit.mli
(lines 1484-1497):
val make :
  ?inline_ext_default:Inline.t map -> ?block_ext_default:Block.t map ->
  ?inline:Inline.t mapper -> ?block:Block.t mapper -> unit -> t
(** [make ?inline ?block ()] is a mapper using [inline] and [block]
    to map the abstract syntax tree. Both default to [fun _ _ -> `Default].

    The mapper knows how to default the built-in abstract syntax
    tree and the built-in {{!extensions}extensions}. It maps
    them in document and depth-first order.

    If you extend the abstract syntax tree you need to indicate how to
default
    these new cases by providing [inline_ext_default] or
    [block_ext_default] functions. By default these functions raise
    [Invalid_argument]. *)
