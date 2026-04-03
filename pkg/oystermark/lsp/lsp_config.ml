(** LSP feature configuration. *)

open Core

(** How to treat a link whose file exists but whose heading or block-ID
    fragment cannot be found. *)
type fragment_behavior =
  | Fallback
  (** Go-to-definition falls back to the top of the file;
      diagnostics produce no warning. *)
  | Strict
  (** Go-to-definition returns no result;
      diagnostics report the fragment as unresolved. *)
[@@deriving sexp, equal]

type t =
  { gtd_unresolved_fragment : fragment_behavior
    (** Fragment behavior for {!Go_to_definition}. *)
  ; diag_unresolved_fragment : fragment_behavior
    (** Fragment behavior for {!Diagnostics}. *)
  ; hover_max_chars : int
    (** Maximum number of bytes of note content to include in a hover
      response.  Content exceeding this limit is truncated at the
      previous newline and a [*(truncated)*] suffix is appended.
      See {!page-"feature-hover".truncation}. *)
  }
[@@deriving sexp, equal]

(** Default configuration: both features use {!Fallback}, matching the
    lenient behavior described in the go-to-definition spec.
    Hover content is capped at 2 000 bytes. *)
let default =
  { gtd_unresolved_fragment = Fallback
  ; diag_unresolved_fragment = Fallback
  ; hover_max_chars = 2000
  }
;;
