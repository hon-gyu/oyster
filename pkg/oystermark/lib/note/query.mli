(** An RSQL query over a note's frontmatter, the target of a query link
    [[[?id==foo]]].

    Spec: {!page-"query-link"}. *)

type comparison =
  | Eq (** [==] *)
  | Neq (** [!=] *)
  | Lt (** [=lt=] or [<] *)
  | Le (** [=le=] or [<=] *)
  | Gt (** [=gt=] or [>] *)
  | Ge (** [=ge=] or [>=] *)
  | In (** [=in=] *)
  | Out (** [=out=] *)
[@@deriving sexp, equal, compare]

type t =
  | Compare of
      { selector : string
      ; comparison : comparison
      ; arguments : string list (** Non-empty; one element unless [In] or [Out]. *)
      }
  | And of t list (** [;], at least two operands *)
  | Or of t list (** [,], at least two operands *)
[@@deriving sexp, equal, compare]

(** Parse an RSQL expression, without the leading [?] of a query link. The
    error is a message for the author. See {!page-"query-link".syntax}. *)
val of_string : string -> (t, string) result

(** Whether a note with frontmatter [frontmatter] satisfies [t]. See
    {!page-"query-link".matching}. *)
val matches : t -> Yaml.value option -> bool
