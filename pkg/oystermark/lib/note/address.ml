open Core

type t =
  | Heading of string
  | Caret of string
  | Attr of string
[@@deriving sexp, equal, compare]

let id = function
  | Heading id | Caret id | Attr id -> id
;;
