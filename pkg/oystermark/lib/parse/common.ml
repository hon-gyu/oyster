open Core
open Cmarkit

let compose_block_map (m1 : Block.t Mapper.mapper) (m2 : Block.t Mapper.mapper) =
  fun m b ->
  match m1 m b with
  | `Default -> m2 m b
  | `Map None -> `Map None
  | `Map (Some b') ->
    (match m2 m b' with
     | `Default -> `Map (Some b')
     | other -> other)
;;

let compose_inline_map (m1 : Inline.t Mapper.mapper) (m2 : Inline.t Mapper.mapper) =
  fun m i ->
  match m1 m i with
  | `Default -> m2 m i
  | `Map None -> `Map None
  | `Map (Some i') ->
    (match m2 m i' with
     | `Default -> `Map (Some i')
     | other -> other)
;;

let compose_all_block_maps (ms : Block.t Mapper.mapper list) =
  List.fold_right ms ~init:(fun m b -> Mapper.default) ~f:compose_block_map
;;

let compose_all_inline_maps (ms : Inline.t Mapper.mapper list) =
  List.fold_right ms ~init:(fun m i -> Mapper.default) ~f:compose_inline_map
;;

(* {1 Sexp conversion scaffolding} *)

(** A sexp-converter for inlines. Returns [None] to fall through to the
    next converter in the composed chain. [recurse] is the fully-composed
    [sexp_of_inline] for recursing into children.

    Both core and extension converters share this type; composition is
    just list order with [None] as the fallthrough signal, analogous to
    [Cmarkit.Mapper]'s [`Default]. *)
type inline_sexp = (Inline.t -> Sexp.t) -> Inline.t -> Sexp.t option

(** A sexp-converter for blocks. Receives both [recurse_inline] and
    [recurse_block]. [with_meta] wraps a block sexp with its metadata
    sub-sexps — pass through to keep metadata in the output. *)
type block_sexp =
  recurse_inline:(Inline.t -> Sexp.t)
  -> recurse_block:(Block.t -> Sexp.t)
  -> with_meta:(Meta.t -> Sexp.t -> Sexp.t)
  -> Block.t
  -> Sexp.t option

(** A sexp-converter for a single metadata key. *)
type meta_sexp = Meta.t -> Sexp.t option

type sexp_of =
  { inline : Inline.t -> Sexp.t
  ; block : Block.t -> Sexp.t
  ; meta : Meta.t -> Sexp.t list
  }

(** Core inline converter. Always returns [Some] — unknown constructors
    emit [<unknown-inline>]. Placed last in the composed chain. *)
let sexp_of_inline_core : inline_sexp =
  fun recurse i ->
  let s =
    match i with
    | Inline.Text (s, _) -> Sexp.List [ Atom "Text"; Atom s ]
    | Inline.Autolink (a, _) ->
      let link = fst (Inline.Autolink.link a) in
      Sexp.List [ Atom "Autolink"; Atom link ]
    | Inline.Break (b, _) ->
      let type_s =
        match Inline.Break.type' b with
        | `Hard -> "hard"
        | `Soft -> "soft"
      in
      Sexp.List [ Atom "Break"; Atom type_s ]
    | Inline.Code_span (cs, _) ->
      Sexp.List [ Atom "Code_span"; Atom (Inline.Code_span.code cs) ]
    | Inline.Emphasis (e, _) ->
      Sexp.List [ Atom "Emphasis"; recurse (Inline.Emphasis.inline e) ]
    | Inline.Strong_emphasis (e, _) ->
      Sexp.List [ Atom "Strong_emphasis"; recurse (Inline.Emphasis.inline e) ]
    | Inline.Link (l, _) -> Sexp.List [ Atom "Link"; recurse (Inline.Link.text l) ]
    | Inline.Image (l, _) -> Sexp.List [ Atom "Image"; recurse (Inline.Link.text l) ]
    | Inline.Raw_html (html, _) ->
      let s =
        List.map html ~f:(fun bl -> Block_line.tight_to_string bl)
        |> String.concat ~sep:""
      in
      Sexp.List [ Atom "Raw_html"; Atom s ]
    | Inline.Inlines (is, _) -> Sexp.List (Atom "Inlines" :: List.map is ~f:recurse)
    | Inline.Ext_strikethrough (s, _) ->
      Sexp.List [ Atom "Strikethrough"; recurse (Inline.Strikethrough.inline s) ]
    | Inline.Ext_math_span (m, _) ->
      Sexp.List [ Atom "Math_span"; Atom (Inline.Math_span.tex m) ]
    | _ -> Sexp.Atom "<unknown-inline>"
  in
  Some s
;;

(** Core block converter. Always returns [Some]. Placed last in the chain. *)
let sexp_of_block_core : block_sexp =
  fun ~recurse_inline ~recurse_block ~with_meta b ->
  let s =
    match b with
    | Block.Blank_line (_, meta) -> with_meta meta (Sexp.Atom "Blank_line")
    | Block.Paragraph (p, meta) ->
      with_meta
        meta
        (Sexp.List [ Atom "Paragraph"; recurse_inline (Block.Paragraph.inline p) ])
    | Block.Heading (h, meta) ->
      with_meta
        meta
        (Sexp.List
           [ Atom "Heading"
           ; Atom (Int.to_string (Block.Heading.level h))
           ; recurse_inline (Block.Heading.inline h)
           ])
    | Block.Code_block (cb, meta) ->
      let info =
        match Block.Code_block.info_string cb with
        | None -> Sexp.Atom "no-info"
        | Some (s, _) -> Sexp.Atom s
      in
      let code =
        List.map (Block.Code_block.code cb) ~f:(fun bl ->
          Sexp.Atom (Block_line.to_string bl))
      in
      with_meta meta (Sexp.List (Atom "Code_block" :: info :: code))
    | Block.Html_block (lines, meta) ->
      let s =
        List.map lines ~f:(fun bl -> Block_line.to_string bl) |> String.concat ~sep:"\n"
      in
      with_meta meta (Sexp.List [ Atom "Html_block"; Atom s ])
    | Block.Block_quote (bq, meta) ->
      with_meta
        meta
        (Sexp.List [ Atom "Block_quote"; recurse_block (Block.Block_quote.block bq) ])
    | Block.List (l, meta) ->
      let items =
        List.map (Block.List'.items l) ~f:(fun (item, _item_meta) ->
          recurse_block (Block.List_item.block item))
      in
      with_meta meta (Sexp.List (Atom "List" :: items))
    | Block.Blocks (bs, meta) ->
      with_meta meta (Sexp.List (Atom "Blocks" :: List.map bs ~f:recurse_block))
    | Block.Link_reference_definition _ -> Sexp.Atom "Link_reference_definition"
    | Block.Thematic_break (_, meta) -> with_meta meta (Sexp.Atom "Thematic_break")
    | _ -> Sexp.Atom "<unknown-block>"
  in
  Some s
;;

(** Compose a list of converters (extensions followed by core) into a
    mutually-recursive triple. Converters are tried in list order; the
    first to return [Some] wins. The caller is responsible for placing
    [sexp_of_inline_core] / [sexp_of_block_core] last. *)
let make_sexp_of
      ~(inlines : inline_sexp list)
      ~(blocks : block_sexp list)
      ~(metas : meta_sexp list)
  : sexp_of
  =
  let rec sexp_of_inline (i : Inline.t) : Sexp.t =
    let rec try_ = function
      | [] -> Sexp.Atom "<unknown-inline>"
      | f :: rest ->
        (match f sexp_of_inline i with
         | Some s -> s
         | None -> try_ rest)
    in
    try_ inlines
  and sexp_of_block (b : Block.t) : Sexp.t =
    let rec try_ = function
      | [] -> Sexp.Atom "<unknown-block>"
      | f :: rest ->
        (match
           f ~recurse_inline:sexp_of_inline ~recurse_block:sexp_of_block ~with_meta b
         with
         | Some s -> s
         | None -> try_ rest)
    in
    try_ blocks
  and sexp_of_meta (meta : Meta.t) : Sexp.t list =
    List.filter_map metas ~f:(fun ext -> ext meta)
  and with_meta (meta : Meta.t) (sexp : Sexp.t) : Sexp.t =
    match sexp_of_meta meta with
    | [] -> sexp
    | items -> Sexp.List [ sexp; Sexp.List (Atom "meta" :: items) ]
  in
  { inline = sexp_of_inline; block = sexp_of_block; meta = sexp_of_meta }
;;
