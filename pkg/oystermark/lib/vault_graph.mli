(** Graph extracted from a resolved vault.

    Edges are directed: source note links to target note.
    Each edge carries an {!edge_kind} indicating whether the link targets
    the note itself, a heading, or a block reference. *)

open Core

(** What the link points to within the target note. *)
type edge_kind =
  | To_note (** [[Note]] — targets the note itself *)
  | To_heading of
      { heading : string
      ; slug : string
      }  (** [[Note#Heading]] *)
  | To_block of { block_id : string } (** [[Note#^block]] *)
[@@deriving sexp, compare]

(** A graph node is a file path (relative to the vault root). *)
type vertex = string

(** The underlying persistent directed graph.  Vertices are file paths,
    edge labels are {!edge_kind}. *)
module G :
  Graph.Sig.P
  with type V.t = vertex
   and type V.label = vertex
   and type E.t = vertex * edge_kind * vertex
   and type E.label = edge_kind

(** The link graph: a persistent labelled digraph. *)
type t = G.t

(** Extract the link graph from a resolved vault.
    Walks all docs and collects resolved link targets.
    Self-links ([Curr_*]) and [Unresolved] targets are skipped. *)
val of_vault : Vault.t -> t

(** Output the graph in Graphviz DOT format (note-level).
    Heading/block edges are collapsed to note-level and deduplicated. *)
val to_dot : t -> string
