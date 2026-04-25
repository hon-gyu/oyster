(** {0 Generator for cmarkit without any extensions} *)

open Core
open Cmarkit

let parse : string -> Doc.t = fun s -> Doc.of_string ~strict:true ~locs:true s
let commonmark_of_doc : Doc.t -> string = fun doc -> Cmarkit_commonmark.of_doc doc

(* Does there exist some commonmark AST that cannot be parsed from a string? *)

let ser_then_parse : Doc.t -> Doc.t = fun doc -> parse (commonmark_of_doc doc)

let ser_then_parse_roundtrip ~(eq: Doc.t -> Doc.t -> bool) d1 d2 : bool =
  let parsed = ser_then_parse d1 in
  eq parsed d2

let parse_idempotent (s : string) : bool =
  String.equal s (s |> parse |> commonmark_of_doc)
