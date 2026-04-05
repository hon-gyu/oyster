(** Graph extracted from a resolved vault.

    Each vertex is a located reference point in the vault: either the source
    side of a link (where the [[\[\[...\]\]]] syntax appears) or the target side
    (a note, heading, or block).  Edges connect source vertices to target
    vertices. *)

open Core

(** {1 Location} *)

(** Byte-range and line-range in a file. *)
type loc =
  { first_byte : int
  ; last_byte : int
  ; first_line : int (** 1-based *)
  ; last_line : int (** 1-based *)
  }
[@@deriving sexp, compare]

(** {1 Vertex} *)

(** What role this vertex plays in the graph. *)
type vertex_kind =
  | Src of loc (** Source side of a link — position of the link syntax *)
  | Tgt_note (** Target: the whole note *)
  | Tgt_heading of
      { heading : string
      ; slug : string
      } (** Target: a heading within the note *)
  | Tgt_block of { block_id : string } (** Target: a block reference *)
[@@deriving sexp, compare]

(** A vertex in the link graph. *)
type vertex =
  { path : string (** Relative path from vault root *)
  ; kind : vertex_kind
  }
[@@deriving sexp, compare]

(** {1 Edge} *)

(** Edge label (reserved for future use). *)
type edge_kind = Link [@@deriving sexp, compare]

(** {1 Graph} *)

module G :
  Graph.Sig.P
  with type V.t = vertex
   and type V.label = vertex
   and type E.t = vertex * edge_kind * vertex
   and type E.label = edge_kind

type t = G.t

(** Extract the link graph from a resolved vault.
    Walks all docs and collects resolved link targets.
    Self-links ([Curr_*]) and [Unresolved] targets are skipped. *)
val of_vault : Vault.t -> t

(** Output the graph in Graphviz DOT format.
    Collapses vertices to note-level for a clean overview. *)
val to_dot : t -> string
