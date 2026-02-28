(** Obsidian wikilink types and inline-level parser. *)

(** The type for wikilink fragment references. *)
type fragment =
  | Heading of string list (** e.g. [["H1"; "H2"]] for [[Note#H1#H2]] *)
  | Block_ref of string (** e.g. ["blockid"] for [[Note#^blockid]] *)

(** The type for wikilinks. *)
type t =
  { target : string option (** File path, [None] = current note *)
  ; fragment : fragment option
  ; display : string option (** Text after [|] *)
  ; embed : bool (** [![[...]]] *)
  }

(** Inline extension constructor for wikilinks. *)
type Cmarkit.Inline.t += Ext_wikilink of t Cmarkit.node

(** Meta key to tag wikilink nodes. *)
val meta_key : unit Cmarkit.Meta.key

(** [parse_content ~embed s] parses the content between [[\[[\]] and [\]\]]]
    into a wikilink value. *)
val parse_content : embed:bool -> string -> t

(** [scan s meta] scans text [s] for wikilinks. Returns [Some inlines] if
    any [[\[[\]\]]] found, [None] otherwise (fast path, no allocation). *)
val scan : string -> Cmarkit.Meta.t -> Cmarkit.Inline.t list option
