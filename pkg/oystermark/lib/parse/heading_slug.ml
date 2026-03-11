(** Heading slug generation: GitHub-style anchors with deduplication.

    Slugs are stamped onto heading blocks' {!Cmarkit.Meta.t} during parsing,
    providing a single source of truth for heading identifiers. *)

open Core

(** Meta key for the slug attached to heading blocks. *)
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
