(** LSP feature configuration. *)

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
  }
[@@deriving sexp, equal]

(** Default configuration: both features use {!Fallback}, matching the
    lenient behavior described in the go-to-definition spec. *)
let default = { gtd_unresolved_fragment = Fallback; diag_unresolved_fragment = Fallback }
