(** Obsidian callout extension.

    Transforms blockquotes whose first line matches [\[!type\]] into callout
    metadata attached to the block_quote's {!Cmarkit.Meta.t}.

    {1 Syntax}

    {v
> [!type] Optional title
> Body content (markdown)
    v}

    {ul
    {- {b Type identifier}: case-insensitive, e.g. [note], [tip], [warning].}
    {- {b Fold indicator}: optional [+] or [-] after the type, before the title.
       {ul
       {- [+] — expanded by default (foldable)}
       {- [-] — collapsed by default (foldable)}
       {- absent — not foldable}}}
    {- {b Title}: optional text after the type/fold indicator. Defaults to the
       type in title case.}
    {- {b Body}: remaining blockquote content. May be empty (title-only callout).}
    {- Nested callouts: a callout body may contain another blockquote that is
       itself a callout.}}

    {1 Parsing}

    First line regex: [\[!([a-zA-Z_-]+)\]([+-])?\s*(.*\)]

    {ul
    {- Group 1: type identifier}
    {- Group 2: fold indicator (optional)}
    {- Group 3: title text (optional, rest of line)}}

    Parsing is performed by {!parse_header}. The block mapper
    {!map_callout} detects callout syntax in
    {!Cmarkit.Block.Block_quote} nodes and attaches {!t} to the
    blockquote's {!Cmarkit.Meta.t} via {!meta_key}.

    {1 Data types}

    A callout's foldability is represented by {!fold}:

    {ul
    {- {!Foldable_open} — expanded by default}
    {- {!Foldable_closed} — collapsed by default}}

    The callout metadata is {!t}:

    {ul
    {- [kind] — lowercased type identifier (e.g. ["info"], ["tip"])}
    {- [fold] — [None] means not foldable, [Some _] selects the initial state}
    {- [title] — explicit title text, or the kind in title case}}

    {1 HTML output}

    Non-foldable callouts render as:

    {v
<div class="callout" data-callout="info">
  <div class="callout-title">Title here</div>
  <div class="callout-content">
    <!-- rendered body markdown -->
  </div>
</div>
    v}

    Foldable callouts use [<details>] / [<summary>]:

    {v
<details class="callout" data-callout="faq" open>
  <summary class="callout-title">Title</summary>
  <div class="callout-content">...</div>
</details>
    v}

    {ul
    {- [open] attribute present when fold = {!Foldable_open}}
    {- [open] attribute absent when fold = {!Foldable_closed}}}

    {1 Supported types}

    The following type identifiers are styled. Aliases share the same style.
    Any unsupported type defaults to the [note] style.

    {ul
    {- [note]}
    {- [abstract] / [summary] / [tldr]}
    {- [info]}
    {- [todo]}
    {- [tip] / [hint] / [important]}
    {- [success] / [check] / [done]}
    {- [question] / [help] / [faq]}
    {- [warning] / [caution] / [attention]}
    {- [failure] / [fail] / [missing]}
    {- [danger] / [error]}
    {- [bug]}
    {- [example]}
    {- [quote] / [cite]}} *)

open Core

type fold =
  | Foldable_open
  | Foldable_closed
[@@deriving sexp]

type t =
  { kind : string (** Lowercased type identifier, e.g. "info", "tip". *)
  ; fold : fold option (** [None] = not foldable. *)
  ; title : string (** Explicit title or titlecased kind. *)
  }
[@@deriving sexp]

let meta_key : t Cmarkit.Meta.key = Cmarkit.Meta.key ()

(** Parse the callout header from the first text node of the first paragraph
    inside a blockquote.  Expected format: [\[!type\](+|-)? optional title]. *)
let parse_header (s : string) : (t * int) option =
  (* Returns (callout, byte position after the header) so we can strip it
     from the inline text. *)
  let len = String.length s in
  if len < 4 || not (Char.equal s.[0] '[' && Char.equal s.[1] '!')
  then None
  else (
    match String.index s ']' with
    | None -> None
    | Some close_pos ->
      let kind = String.sub s ~pos:2 ~len:(close_pos - 2) |> String.lowercase in
      if String.is_empty kind
      then None
      else (
        let after_close = close_pos + 1 in
        let fold, after_fold =
          if after_close < len && Char.equal s.[after_close] '+'
          then Some Foldable_open, after_close + 1
          else if after_close < len && Char.equal s.[after_close] '-'
          then Some Foldable_closed, after_close + 1
          else None, after_close
        in
        let rest =
          if after_fold < len
          then String.strip (String.sub s ~pos:after_fold ~len:(len - after_fold))
          else ""
        in
        let title = if String.is_empty rest then String.capitalize kind else rest in
        Some ({ kind; fold; title }, len)))
;;

(** Extract the leading text string from an inline, if it starts with a Text node. *)
let rec leading_text (inline : Cmarkit.Inline.t) : (string * Cmarkit.Meta.t) option =
  match inline with
  | Cmarkit.Inline.Text (s, meta) -> Some (s, meta)
  | Cmarkit.Inline.Inlines (inlines, _) ->
    (match inlines with
     | first :: _ -> leading_text first
     | [] -> None)
  | _ -> None
;;

(** Strip the callout header prefix from the first text node.
    Returns a new inline with the [\[!type\]...] prefix removed. *)
let strip_header_from_inline (inline : Cmarkit.Inline.t) (end_pos : int)
  : Cmarkit.Inline.t
  =
  let is_break : Cmarkit.Inline.t -> bool = function
    | Cmarkit.Inline.Break _ -> true
    | _ -> false
  in
  let drop_leading_break (xs : Cmarkit.Inline.t list) : Cmarkit.Inline.t list =
    match xs with
    | first :: rest when is_break first -> rest
    | _ -> xs
  in
  let rec strip = function
    | Cmarkit.Inline.Text (s, meta) ->
      let rest = String.strip (String.drop_prefix s end_pos) in
      if String.is_empty rest then None else Some (Cmarkit.Inline.Text (rest, meta))
    | Cmarkit.Inline.Inlines (inlines, meta) ->
      (match inlines with
       | first :: tail ->
         (match strip first with
          | Some first' -> Some (Cmarkit.Inline.Inlines (first' :: tail, meta))
          | None ->
            let tail = drop_leading_break tail in
            (match tail with
             | [] -> None
             | [ single ] -> Some single
             | _ -> Some (Cmarkit.Inline.Inlines (tail, meta))))
       | [] -> None)
    | other -> Some other
  in
  match strip inline with
  | Some i -> i
  | None -> Cmarkit.Inline.Text ("", Cmarkit.Meta.none)
;;

(** Extract the first paragraph and remaining blocks from a block. *)
let decompose_block (block : Cmarkit.Block.t)
  : (Cmarkit.Block.Paragraph.t * Cmarkit.Meta.t * Cmarkit.Block.t list) option
  =
  match block with
  | Cmarkit.Block.Paragraph (p, meta) -> Some (p, meta, [])
  | Cmarkit.Block.Blocks (blocks, _) ->
    (match blocks with
     | Cmarkit.Block.Paragraph (p, meta) :: rest -> Some (p, meta, rest)
     | _ -> None)
  | _ -> None
;;

(** Rebuild a block from a (possibly modified) first paragraph and remaining blocks. *)
let is_empty_inline (inline : Cmarkit.Inline.t) : bool =
  match inline with
  | Cmarkit.Inline.Text (s, _) -> String.is_empty s
  | _ -> false
;;

let recompose_block
      (para : Cmarkit.Block.Paragraph.t)
      (para_meta : Cmarkit.Meta.t)
      (rest : Cmarkit.Block.t list)
  : Cmarkit.Block.t
  =
  let para_empty = is_empty_inline (Cmarkit.Block.Paragraph.inline para) in
  match para_empty, rest with
  | true, [] -> Cmarkit.Block.empty
  | true, [ single ] -> single
  | true, _ -> Cmarkit.Block.Blocks (rest, Cmarkit.Meta.none)
  | false, [] -> Cmarkit.Block.Paragraph (para, para_meta)
  | false, _ ->
    Cmarkit.Block.Blocks
      (Cmarkit.Block.Paragraph (para, para_meta) :: rest, Cmarkit.Meta.none)
;;

(** Block mapper: detects callout syntax in blockquotes and attaches
    {!t} to the blockquote's metadata. Strips the header from the first
    paragraph's inline text. *)
let map_callout (_mapper : Cmarkit.Mapper.t) (block : Cmarkit.Block.t)
  : Cmarkit.Block.t Cmarkit.Mapper.result
  =
  match block with
  | Cmarkit.Block.Block_quote (bq, bq_meta) ->
    let inner = Cmarkit.Block.Block_quote.block bq in
    (match decompose_block inner with
     | Some (para, para_meta, rest_blocks) ->
       let inline = Cmarkit.Block.Paragraph.inline para in
       (match leading_text inline with
        | Some (text, _) ->
          (match parse_header text with
           | Some (callout, end_pos) ->
             let new_inline = strip_header_from_inline inline end_pos in
             let new_para = Cmarkit.Block.Paragraph.make new_inline in
             let new_inner = recompose_block new_para para_meta rest_blocks in
             let new_bq = Cmarkit.Block.Block_quote.make new_inner in
             let new_meta = Cmarkit.Meta.add meta_key callout bq_meta in
             Cmarkit.Mapper.ret (Cmarkit.Block.Block_quote (new_bq, new_meta))
           | None -> Cmarkit.Mapper.default)
        | None -> Cmarkit.Mapper.default)
     | None -> Cmarkit.Mapper.default)
  | _ -> Cmarkit.Mapper.default
;;

let%expect_test "parse_header basic" =
  let test s =
    match parse_header s with
    | Some (c, pos) -> printf !"%{sexp: t} @ %d" c pos
    | None -> print_string "None"
  in
  test "[!info] Here's a callout title";
  [%expect {| ((kind info) (fold ()) (title "Here's a callout title")) @ 30 |}];
  test "[!tip]";
  [%expect {| ((kind tip) (fold ()) (title Tip)) @ 6 |}];
  test "[!faq]- Are callouts foldable?";
  [%expect
    {| ((kind faq) (fold (Foldable_closed)) (title "Are callouts foldable?")) @ 30 |}];
  test "[!note]+ Expanded";
  [%expect {| ((kind note) (fold (Foldable_open)) (title Expanded)) @ 17 |}];
  test "[!WARNING] Watch out";
  [%expect {| ((kind warning) (fold ()) (title "Watch out")) @ 20 |}];
  test "not a callout";
  [%expect {| None |}];
  test "[!] empty kind";
  [%expect {| None |}]
;;
