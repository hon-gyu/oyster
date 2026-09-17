(* A unified vault index API, centralizing all relevant queries / operations

  - provide a unified vault API, centralizing all relevant queries / operations
  - downstream client code: LSP, oyster-publish (not in this repo yet), vault_cli (oyster)
  - compact, persistent snapshots that clients can update one file at a time
*)

open Core
open Parse

type loc = Cmarkit.Textloc.t

let sexp_of_loc = Textloc_conv.sexp_of_t
let loc_of_sexp = Textloc_conv.t_of_sexp
let compare_loc = Textloc_conv.compare
let equal_loc a b = Int.equal (compare_loc a b) 0

module Path = struct
  type t = string

  let sexp_of_t = String.sexp_of_t
  let t_of_sexp = String.t_of_sexp

  let of_string : string -> (t, Error.t) result =
    fun s ->
    let cs = String.split s ~on:'/' in
    if String.is_empty s
    then Error (Error.of_string "vault path must not be empty")
    else if String.is_prefix s ~prefix:"/"
    then Error (Error.of_string "vault path must be relative")
    else if
      List.exists cs ~f:(fun c ->
        String.is_empty c || String.equal c "." || String.equal c "..")
    then Error (Error.of_string "vault path contains an empty, . or .. component")
    else Ok s
  ;;

  let to_string : t -> string = Fun.id
  let dirname : t -> t option = fun t -> Option.map (String.rsplit2 t ~on:'/') ~f:fst
  let basename : t -> string = Filename.basename
  let compare : t -> t -> int = String.compare
  let equal : t -> t -> bool = String.equal
end

type heading = Note.Anchor.heading =
  { text : string
  ; level : int
  ; slug : string
  }
[@@deriving sexp, equal, compare]

type file_stat =
  { rel_path : Path.t
  ; birthtime : (int * int * int) option
  ; mtime : (int * int * int) option
  }

type anchor_value = Note.Anchor.value =
  | Heading of heading
  | Caret of string
  | Attr of
      { id : string
      ; inline : bool
      }
[@@deriving sexp, equal, compare]

module Anchor = struct
  type t = Note.Anchor.t =
    { value : anchor_value
    ; loc : loc
    }
  [@@deriving sexp, equal, compare]
end

module Link = struct
  type kind = Note.Link.kind =
    | Link
    | Embed
  [@@deriving sexp, equal, compare]

  type t = Note.Link.t =
    { reference : Note.Link.Ref.t
    ; kind : kind
    ; loc : loc
    }
  [@@deriving sexp, equal, compare]
end

(** Frontmatter accessors *)
module Frontmatter_ = struct
  let field (fm : Yaml.value option) (key : string) : Yaml.value option =
    match fm with
    | Some (`O fields) -> List.Assoc.find fields key ~equal:String.equal
    | _ -> None
  ;;

  (** A YAML scalar as a string. [None] for a sequence or a mapping. *)
  let scalar : Yaml.value option -> string option = function
    | Some (`String s) -> Some s
    | Some (`Float f) ->
      (* YAML has a single number type, so an authored [2026] arrives as a
         float. Render integral values without the [.] so they round-trip. *)
      Some
        (if Float.equal f (Float.round_down f) && Float.( < ) (Float.abs f) 1e16
         then Int.to_string (Float.to_int f)
         else Float.to_string f)
    | Some (`Bool b) -> Some (Bool.to_string b)
    | _ -> None
  ;;

  (** A YAML value as a list of strings: a sequence maps its scalar items, a
      string splits on commas and spaces, and any other scalar is a single
      element. *)
  let string_list (v : Yaml.value option) : string list =
    match v with
    | Some (`A items) -> List.filter_map items ~f:(fun i -> scalar (Some i))
    | Some (`String s) ->
      String.split_on_chars s ~on:[ ','; ' ' ]
      |> List.filter_map ~f:(fun s ->
        let s = String.strip s in
        if String.is_empty s then None else Some s)
    | other -> Option.to_list (scalar other)
  ;;

  (** A [YYYY-MM-DD] or [YYYY/MM/DD] scalar. A trailing time is ignored. *)
  let date (v : Yaml.value option) : (int * int * int) option =
    let open Option.Let_syntax in
    let%bind s = scalar v in
    let head = String.strip s |> String.split_on_chars ~on:[ ' '; 'T' ] |> List.hd_exn in
    match String.split_on_chars head ~on:[ '-'; '/' ] with
    | [ y; m; d ] ->
      let%bind y = Int.of_string_opt y in
      let%bind m = Int.of_string_opt m in
      let%map d = Int.of_string_opt d in
      y, m, d
    | _ -> None
  ;;
end

module Entry = struct
  type t =
    { file_stat : file_stat
    ; frontmatter : Yaml.value option
    ; anchors : Anchor.t list
    ; links : Link.t list
    }

  let of_doc (file_stat : file_stat) (doc : Cmarkit.Doc.t) : (t, string) result =
    let anchors = Note.Anchor.of_doc doc in
    let links = Note.Link.of_doc doc in
    if
      List.exists anchors ~f:(fun a -> Cmarkit.Textloc.is_none a.loc)
      || List.exists links ~f:(fun l -> Cmarkit.Textloc.is_none l.loc)
    then Error "document is missing source locations"
    else Ok { file_stat; frontmatter = Frontmatter.of_doc doc; anchors; links }
  ;;

  let of_doc_exn (file_stat : file_stat) (doc : Cmarkit.Doc.t) : t =
    Result.ok_or_failwith (of_doc file_stat doc)
  ;;

  let path : t -> Path.t = fun note -> note.file_stat.rel_path
  let file_stat : t -> file_stat = fun note -> note.file_stat
  let frontmatter : t -> Yaml.value option = fun note -> note.frontmatter

  let frontmatter_field (note : t) (key : string) : Yaml.value option =
    Frontmatter_.field note.frontmatter key
  ;;

  let tags (note : t) : string list =
    Frontmatter_.string_list (frontmatter_field note "tags")
    |> List.fold ~init:([], String.Set.empty) ~f:(fun (acc, seen) tag ->
      if Set.mem seen tag then acc, seen else tag :: acc, Set.add seen tag)
    |> fst
    |> List.rev
  ;;

  let created (note : t) : (int * int * int) option =
    List.find_map
      [ lazy (Frontmatter_.date (frontmatter_field note "created"))
      ; lazy (Frontmatter_.date (frontmatter_field note "date"))
      ; lazy note.file_stat.birthtime
      ]
      ~f:force
  ;;

  let modified (note : t) : (int * int * int) option =
    Option.first_some
      (Frontmatter_.date (frontmatter_field note "updated"))
      note.file_stat.mtime
  ;;

  let title (note : t) : string =
    let from_heading () =
      List.find_map note.anchors ~f:(fun a ->
        match a.Anchor.value with
        | Heading h when h.level = 1 -> Some h.text
        | _ -> None)
    in
    let from_basename () =
      let base = Path.basename (path note) in
      match String.rsplit2 base ~on:'.' with
      (* A leading dot marks a hidden file, not an extension: the stem of
         [.hidden] is [.hidden], not the empty string. *)
      | Some ("", _) | None -> base
      | Some (stem, _) -> stem
    in
    match Frontmatter_.scalar (frontmatter_field note "title") with
    | Some t when not (String.is_empty (String.strip t)) -> t
    | _ -> Option.value_or_thunk (from_heading ()) ~default:from_basename
  ;;

  let anchors (note : t) : Anchor.t list = note.anchors

  let headings (note : t) : (heading * loc) list =
    anchors note
    |> List.filter_map ~f:(fun anchor ->
      match anchor.value with
      | Heading heading -> Some (heading, anchor.loc)
      | Caret _ | Attr _ -> None)
  ;;

  let links (note : t) : Link.t list = note.links
end

module Asset = struct
  type t = { file_stat : file_stat }

  let create (file_stat : file_stat) : t = { file_stat }
  let path (asset : t) : Path.t = asset.file_stat.rel_path
end

type target =
  | Note of Path.t
  | Asset of Path.t
  | Anchor of
      { note_path : Path.t
      ; anchor : Anchor.t
      }
[@@deriving sexp, equal, compare]

type resolution_error =
  | Missing_path
  | Missing_anchor of Path.t

type resolution = (target, resolution_error) result

(* TODO: we need a human-readable representation for unresolved links
src_path, src_loc, tgt_path, tgt_fragment, reason

%{src_path}:%{src_loc} -> %{tgt_path}:%{tgt_fragment} : %{reason}
*)

type backlink =
  { source : Path.t
  ; link : Link.t
  }

let target_path : target -> Path.t = function
  | Note path | Asset path -> path
  | Anchor { note_path; _ } -> note_path
;;

(** Every resolved incoming edge in the vault, bucketed by the {e path} of the
    target it resolves to. *)
type backlink_map = (target * backlink) list String.Map.t

type t =
  { notes_by_path : Entry.t String.Map.t
  ; assets_by_path : Asset.t String.Map.t
  ; backlinks : backlink_map Lazy.t
    (* Resolving every link in the vault is [O(notes + links)] and each
        reverse-reference query needs the whole result, so a snapshot computes
        it at most once and shares it. Laziness keeps snapshots that are only
        written to (the LSP's incremental updates) from paying for it. *)
  }

let notes (index : t) : Entry.t list = Map.data index.notes_by_path
let assets (index : t) : Asset.t list = Map.data index.assets_by_path

let find_note (index : t) (path : Path.t) : Entry.t option =
  Map.find index.notes_by_path path
;;

let find_asset (index : t) (path : Path.t) : Asset.t option =
  Map.find index.assets_by_path path
;;

module Resolve_ = struct
  let normalize_relative_target ~source target =
    if not (String.is_prefix target ~prefix:"./" || String.is_prefix target ~prefix:"../")
    then target
    else
      Filename.concat (Filename.dirname source) target
      |> String.split ~on:'/'
      |> List.fold ~init:[] ~f:(fun acc component ->
        match component, acc with
        | ".", _ | "", _ -> acc
        | "..", _ :: rest -> rest
        | "..", [] -> []
        | component, _ -> component :: acc)
      |> List.rev
      |> String.concat ~sep:"/"
  ;;

  let is_path_subsequence ~haystack ~needle =
    let rec loop hs = function
      | [] -> true
      | n :: ns ->
        (match List.drop_while hs ~f:(fun h -> not (String.equal h n)) with
         | [] -> false
         | _ :: hs -> loop hs ns)
    in
    loop haystack needle
  ;;

  let match_rank ~source_dir p =
    ( List.length (String.split p ~on:'/')
    , if String.equal (Filename.dirname p) source_dir then 0 else 1 )
  ;;

  let resolve_path index ~source target =
    let target = normalize_relative_target ~source target in
    let normalized = if String.mem target '.' then target else target ^ ".md" in
    let paths = Map.keys index.notes_by_path @ Map.keys index.assets_by_path in
    match List.find paths ~f:(String.equal normalized) with
    | Some p -> Some p
    | None ->
      let needle = String.split normalized ~on:'/' in
      let source_dir = Filename.dirname source in
      List.filter paths ~f:(fun p ->
        is_path_subsequence ~haystack:(String.split p ~on:'/') ~needle)
      |> List.min_elt ~compare:(fun a b ->
        [%compare: int * int] (match_rank ~source_dir a) (match_rank ~source_dir b))
  ;;
end

open Resolve_

let resolve (index : t) (source : Path.t) (ref : Note.Link.Ref.t) : resolution =
  let path =
    match ref.Note.Link.Ref.target with
    | None -> Some source
    | Some t -> resolve_path index ~source t
  in
  match path with
  | None -> Error Missing_path
  | Some p ->
    (match ref.fragment with
     | None ->
       if Map.mem index.notes_by_path p
       then Ok (Note p)
       else if Map.mem index.assets_by_path p
       then Ok (Asset p)
       else Error Missing_path
     | Some f ->
       (match find_note index p with
        | None -> Error (Missing_anchor p)
        | Some n ->
          Option.value_map
            (Note.Link.Ref.resolve_fragment (Entry.anchors n) f)
            ~default:(Error (Missing_anchor p))
            ~f:(fun anchor -> Ok (Anchor { note_path = p; anchor }))))
;;

(** Resolve every authored link in the vault once, bucketed by target path.

    Notes are visited in ascending canonical path order and their links in
    document order; each bucket preserves that order. *)
let compute_backlinks (index : t) : backlink_map =
  List.fold (notes index) ~init:String.Map.empty ~f:(fun acc note ->
    let source = Entry.path note in
    List.fold (Entry.links note) ~init:acc ~f:(fun acc link ->
      match resolve index source link.reference with
      | Error _ -> acc
      | Ok target ->
        Map.add_multi acc ~key:(target_path target) ~data:(target, { source; link })))
  (* [add_multi] prepends, so each bucket is built in reverse. *)
  |> Map.map ~f:List.rev
;;

(** Build a snapshot from its two path maps, tying the lazy reverse index to it. *)
let make notes_by_path assets_by_path : t =
  let rec t = { notes_by_path; assets_by_path; backlinks = lazy (compute_backlinks t) } in
  t
;;

let empty : t = make String.Map.empty String.Map.empty

let set_note : t -> Entry.t -> t =
  fun t n ->
  let p = Entry.path n in
  make (Map.set t.notes_by_path ~key:p ~data:n) (Map.remove t.assets_by_path p)
;;

let remove_note : t -> Path.t -> t =
  fun t p -> make (Map.remove t.notes_by_path p) t.assets_by_path
;;

let set_asset : t -> Asset.t -> t =
  fun t a ->
  let p = Asset.path a in
  make (Map.remove t.notes_by_path p) (Map.set t.assets_by_path ~key:p ~data:a)
;;

let remove_asset : t -> Path.t -> t =
  fun t p -> make t.notes_by_path (Map.remove t.assets_by_path p)
;;

let map_paths (t : t) ~(f : Path.t -> Path.t) : t =
  let move_stat (stat : file_stat) = { stat with rel_path = f stat.rel_path } in
  let notes =
    Map.data t.notes_by_path
    |> List.map ~f:(fun (note : Entry.t) ->
      let note = { note with file_stat = move_stat note.file_stat } in
      Entry.path note, note)
    |> String.Map.of_alist_reduce ~f:(fun _ last -> last)
  in
  let assets =
    Map.data t.assets_by_path
    |> List.map ~f:(fun (asset : Asset.t) ->
      let asset : Asset.t = { file_stat = move_stat asset.file_stat } in
      Asset.path asset, asset)
    |> String.Map.of_alist_reduce ~f:(fun _ last -> last)
  in
  make notes assets
;;

let unresolved_links (index : t) (note_path : Path.t) : (Link.t * resolution_error) list =
  let path = note_path in
  Option.value_map (find_note index path) ~default:[] ~f:(fun n ->
    List.filter_map (Entry.links n) ~f:(fun l ->
      match resolve index path l.reference with
      | Ok _ -> None
      | Error e -> Some (l, e)))
;;

let all_backlinks (index : t) : (target * backlink) list =
  Map.data (force index.backlinks) |> List.concat
;;

let backlinks_at_path (index : t) (path : Path.t) : (target * backlink) list =
  Map.find (force index.backlinks) path |> Option.value ~default:[]
;;

let backlinks_of_note ?(include_anchors : bool = false) (index : t) (tgt_path : Path.t)
  : backlink list
  =
  List.filter_map (backlinks_at_path index tgt_path) ~f:(fun (target, b) ->
    match target with
    | Note _ -> Some b
    | Anchor _ when include_anchors -> Some b
    | _ -> None)
;;

let backlinks_of_target (index : t) (target : target) : backlink list =
  List.filter_map
    (backlinks_at_path index (target_path target))
    ~f:(fun (t, b) -> if equal_target t target then Some b else None)
;;

let is_orphan (index : t) (note_path : Path.t) : bool =
  let path = note_path in
  not
    (List.exists (backlinks_of_note ~include_anchors:true index path) ~f:(fun b ->
       (not (Path.equal b.source path))
       &&
       match b.link.kind with
       | Link.Link | Embed -> true))
;;

let orphans (index : t) : Path.t list =
  List.filter (Map.keys index.notes_by_path) ~f:(is_orphan index)
;;
