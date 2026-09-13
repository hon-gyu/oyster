(** A vault snapshot as a Jinja template context.

    - a client of {!Vault.Index} (alongside the LSP and the command line)
    - serializes the whole snapshot to JSON so a template engine can compute a
    document from it. E.g. a table of contents over a subtree, an index filtered
    by tag, a list of recently updated notes.

    The exposed JSON namespace is specified in {!page-"template-context"}.

    {@meta[
    ai-disclosure: autonomous
    ]}
    *)

open Core
module Index = Vault.Index

(* YAML to JSON
   =========== *)

(* YAML has one numeric type, so an authored [3] arrives as [3.]. Integral
   values are re-narrowed to [`Int] so a template prints [3] and not [3.0]. *)
let rec json_of_yaml (v : Yaml.value) : Yojson.Safe.t =
  match v with
  | `Null -> `Null
  | `Bool b -> `Bool b
  | `String s -> `String s
  | `Float f ->
    if Float.equal f (Float.round_down f) && Float.( < ) (Float.abs f) 1e16
    then `Int (Float.to_int f)
    else `Float f
  | `A items -> `List (List.map items ~f:json_of_yaml)
  | `O fields -> `Assoc (List.map fields ~f:(fun (k, v) -> k, json_of_yaml v))
;;

(* Scalars
   ======= *)

(* ISO-8601, so that a template sorting on the string sorts chronologically. *)
let json_of_date : (int * int * int) option -> Yojson.Safe.t = function
  | None -> `Null
  | Some (y, m, d) -> `String (sprintf "%04d-%02d-%02d" y m d)
;;

let json_of_string_opt : string option -> Yojson.Safe.t = function
  | None -> `Null
  | Some s -> `String s
;;

(* 1-based, as an editor shows them. *)
let line_of_loc (loc : Cmarkit.Textloc.t) : int = fst (Cmarkit.Textloc.first_line loc)

(* The authored fragment, rendered back to the syntax it was written in. *)
let json_of_fragment : Note.Link.Ref.fragment option -> Yojson.Safe.t = function
  | None -> `Null
  | Some fragment -> `String (Note.Link.Ref.string_of_fragment fragment)
;;

let string_of_link_kind : Index.Link.kind -> string = function
  | Link -> "link"
  | Embed -> "embed"
;;

let string_of_resolution_error : Index.resolution_error -> string = function
  | Missing_path -> "missing_path"
  | Missing_anchor _ -> "missing_anchor"
;;

(* Path components
   =============== *)

(* [""] at the vault root, not [Filename]'s ["."], so a template can compare it
   against an authored prefix without special-casing the root. *)
let dir_of_path (path : Index.Path.t) : string =
  Option.value (Index.Path.dirname path) ~default:""
;;

(* The directory split into components, for grouping by subtree. *)
let segments_of_path (path : Index.Path.t) : Yojson.Safe.t =
  match Index.Path.dirname path with
  | None -> `List []
  | Some dir -> `List (List.map (String.split dir ~on:'/') ~f:(fun s -> `String s))
;;

let stem_of_path (path : Index.Path.t) : string =
  let base = Index.Path.basename path in
  match String.rsplit2 base ~on:'.' with
  | Some ("", _) | None -> base
  | Some (stem, _) -> stem
;;

(* Notes
   ===== *)

let json_of_heading ((h : Index.heading), loc) : Yojson.Safe.t =
  `Assoc
    [ "text", `String h.text
    ; "level", `Int h.level
    ; "slug", `String h.slug
    ; "line", `Int (line_of_loc loc)
    ]
;;

(* [broken] restates [resolved]'s nullity as a boolean, which a template can
   branch on directly. *)
let json_of_link index ~(source : Index.Path.t) (link : Index.Link.t) : Yojson.Safe.t =
  let resolution = Index.resolve index source link.reference in
  `Assoc
    [ "kind", `String (string_of_link_kind link.kind)
    ; "target", json_of_string_opt link.reference.target
    ; "fragment", json_of_fragment link.reference.fragment
    ; ( "resolved"
      , match resolution with
        | Ok target -> `String (Index.target_path target)
        | Error _ -> `Null )
    ; "broken", `Bool (Result.is_error resolution)
    ; ( "reason"
      , match resolution with
        | Ok _ -> `Null
        | Error e -> `String (string_of_resolution_error e) )
    ; "line", `Int (line_of_loc link.loc)
    ]
;;

(* [title] is the source note's, so rendering a backlink list needs no second
   lookup into [notes_by_path]. *)
let json_of_backlink index (b : Index.backlink) : Yojson.Safe.t =
  `Assoc
    [ "source", `String b.source
    ; ( "title"
      , match Index.find_note index b.source with
        | Some note -> `String (Index.Entry.title note)
        | None -> `Null )
    ; "kind", `String (string_of_link_kind b.link.kind)
    ; "line", `Int (line_of_loc b.link.loc)
    ]
;;

let json_of_note index (note : Index.Entry.t) : Yojson.Safe.t =
  let path = Index.Entry.path note in
  let backlinks = Index.backlinks_of_note ~include_anchors:true index path in
  let links = Index.Entry.links note in
  `Assoc
    [ "path", `String path
    ; "dir", `String (dir_of_path path)
    ; "segments", segments_of_path path
    ; "name", `String (Index.Path.basename path)
    ; "stem", `String (stem_of_path path)
    ; "title", `String (Index.Entry.title note)
    ; "tags", `List (List.map (Index.Entry.tags note) ~f:(fun t -> `String t))
    ; ( "frontmatter"
      , match Index.Entry.frontmatter note with
        | None -> `Assoc []
        | Some v ->
          (* A template indexes into [note.frontmatter]; a scalar or sequence at
             the top level has no keys to index, so it is normalised away. *)
          (match json_of_yaml v with
           | `Assoc _ as assoc -> assoc
           | _ -> `Assoc []) )
    ; "created", json_of_date (Index.Entry.created note)
    ; "modified", json_of_date (Index.Entry.modified note)
    ; "headings", `List (List.map (Index.Entry.headings note) ~f:json_of_heading)
    ; "links", `List (List.map links ~f:(json_of_link index ~source:path))
    ; "link_count", `Int (List.length links)
    ; "backlinks", `List (List.map backlinks ~f:(json_of_backlink index))
    ; "backlink_count", `Int (List.length backlinks)
    ; "is_orphan", `Bool (Index.is_orphan index path)
    ]
;;

let json_of_asset (asset : Index.Asset.t) : Yojson.Safe.t =
  let path = Index.Asset.path asset in
  `Assoc
    [ "path", `String path
    ; "dir", `String (dir_of_path path)
    ; "segments", segments_of_path path
    ; "name", `String (Index.Path.basename path)
    ; "stem", `String (stem_of_path path)
    ]
;;

(* Vault-level views
   ================= *)

(* Tags sorted; the paths under each in ascending canonical path order. *)
let json_of_tags index : Yojson.Safe.t =
  List.fold (Index.notes index) ~init:String.Map.empty ~f:(fun acc note ->
    List.fold (Index.Entry.tags note) ~init:acc ~f:(fun acc tag ->
      Map.add_multi acc ~key:tag ~data:(Index.Entry.path note)))
  (* [add_multi] prepends and [Index.notes] is already ordered, so reversing
     each bucket restores ascending path order. *)
  |> Map.map ~f:(fun paths -> `List (List.rev_map paths ~f:(fun p -> `String p)))
  |> Map.to_alist
  |> fun alist -> `Assoc alist
;;

let json_of_unresolved index : Yojson.Safe.t =
  `List
    (List.concat_map (Index.notes index) ~f:(fun note ->
       let path = Index.Entry.path note in
       List.map (Index.unresolved_links index path) ~f:(fun (link, error) ->
         `Assoc
           [ "source", `String path
           ; "target", json_of_string_opt link.reference.target
           ; "fragment", json_of_fragment link.reference.fragment
           ; "kind", `String (string_of_link_kind link.kind)
           ; "reason", `String (string_of_resolution_error error)
           ; "line", `Int (line_of_loc link.loc)
           ])))
;;

(* Entry point
   =========== *)

let of_vault (vault : Vault.t) : Yojson.Safe.t =
  let index = vault.index in
  let notes = Index.notes index in
  let assets = Index.assets index in
  let orphans = Index.orphans index in
  let unresolved = json_of_unresolved index in
  let notes_json =
    List.map notes ~f:(fun note -> Index.Entry.path note, json_of_note index note)
  in
  let link_count =
    List.sum (module Int) notes ~f:(fun note -> List.length (Index.Entry.links note))
  in
  `Assoc
    [ ( "vault"
      , `Assoc
          [ "root", `String vault.vault_root
          ; "note_count", `Int (List.length notes)
          ; "asset_count", `Int (List.length assets)
          ; "link_count", `Int link_count
          ; "orphan_count", `Int (List.length orphans)
          ; ( "unresolved_count"
            , `Int
                (match unresolved with
                 | `List l -> List.length l
                 | _ -> 0) )
          ] )
      (* Both shapes of the same notes: a list for iteration in canonical path
         order, and a map for lookup by path from a backlink or a link target. *)
    ; "notes", `List (List.map notes_json ~f:snd)
    ; "notes_by_path", `Assoc notes_json
    ; "assets", `List (List.map assets ~f:json_of_asset)
    ; "tags", json_of_tags index
    ; "orphans", `List (List.map orphans ~f:(fun p -> `String p))
    ; "unresolved", unresolved
    ]
;;
