(** Add slug to the metadata of headings

    Heading slug generation: GitHub-style anchors with deduplication.

    Slugs are stamped onto heading blocks' [Cmarkit.Meta.t] during parsing,
    providing a single source of truth for heading identifiers. *)

open Core

let meta_key : string Cmarkit.Meta.key = Cmarkit.Meta.key ()

(** GitHub-style slug: lowercase, non-alphanum to [-], collapse runs, strip edges. *)
let slugify (s : string) : string =
  s
  |> String.lowercase
  |> String.map ~f:(fun c ->
    if Char.is_alphanum c || Char.equal c '-' || Char.equal c '_' then c else '-')
  |> String.split ~on:'-'
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> String.concat ~sep:"-"
;;

(** Compute a deduplicated slug. [seen] tracks base slug -> count. *)
let dedup_slug (seen : (string, int) Hashtbl.t) (text : string) : string =
  let base : string = slugify text in
  let count : int = Hashtbl.find seen base |> Option.value ~default:0 in
  Hashtbl.set seen ~key:base ~data:(count + 1);
  if count = 0 then base else sprintf "%s-%d" base count
;;

(** Render inlines to plain text, losing their markdown syntax. Used in rendering
    heading to plain text. *)
let inline_to_plain_text (inline : Cmarkit.Inline.t) : string =
  let lines =
    Cmarkit.Inline.to_plain_text
      ~ext:(fun ~break_on_soft inline ->
        match inline with
        | Wikilink.Ext_wikilink (wl, _meta) ->
          let text = Wikilink.to_plain_text wl in
          Cmarkit.Inline.Text (text, Cmarkit.Meta.none)
        | other -> other)
      ~break_on_soft:false
      inline
  in
  String.concat ~sep:"\n" (List.map lines ~f:(String.concat ~sep:""))
;;

let mk_block_map () : Cmarkit.Block.t Cmarkit.Mapper.mapper =
  let open Cmarkit.Mapper in
  let slug_seen = Hashtbl.create (module String) in
  fun (m : t) (b : Cmarkit.Block.t) ->
    match b with
    | Cmarkit.Block.Heading (h, meta) ->
      let orig_inline = Cmarkit.Block.Heading.inline h in
      let mapped_inline =
        Cmarkit.Mapper.map_inline m orig_inline |> Option.value ~default:orig_inline
      in
      let text = inline_to_plain_text mapped_inline in
      let slug = dedup_slug slug_seen text in
      let meta' = Cmarkit.Meta.add meta_key slug meta in
      let h' =
        Cmarkit.Block.Heading.make
          ?id:(Cmarkit.Block.Heading.id h)
          ~layout:(Cmarkit.Block.Heading.layout h)
          ~level:(Cmarkit.Block.Heading.level h)
          mapped_inline
      in
      ret (Cmarkit.Block.Heading (h', meta'))
    | _ -> Cmarkit.Mapper.default
;;
