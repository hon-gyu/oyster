(** User-facing configuration for an Oystermark vault build.

    A {!t} bundles every knob the renderer exposes — theme, CSS snippets,
    pipeline profile, and home-page graph view — and is typically loaded from
    a JSON file via {!of_file}. Unknown fields and malformed values are
    tolerated: parsing falls back to {!default} (or per-field defaults via
    [\[@default\]]) rather than raising, so a partial or slightly stale config
    file still produces a usable build.
*)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives
module J = Yojson.Safe

(** {1 Utils} *)

module type Defaultable = sig
  type t

  val default : t
end

(** Wrap a [t_of_yojson]-style parser so any failure falls back to [default]. *)
let or_default ~default f j =
  try f j with
  | _ -> default
;;

(** A string-enum module functor: define a table of [(canonical, variant, aliases)]
    and a default, get [of_string]/[to_string]/[t_of_yojson]/[yojson_of_t] for free,
    with invalid JSON values falling back to [default]. *)
module type String_enum = sig
  type t

  val table : (string * t * string list) list
  val default : t
end

module Make_string_enum (E : String_enum) = struct
  type t = E.t

  let default = E.default

  let of_string (s : string) : t =
    match
      List.find_opt
        (fun (canonical, _, aliases) -> String.equal s canonical || List.mem s aliases)
        E.table
    with
    | Some (_, t, _) -> t
    | None -> failwith ("Invalid value: " ^ s)
  ;;

  let to_string (t : t) : string =
    let canonical, _, _ = List.find (fun (_, t', _) -> t = t') E.table in
    canonical
  ;;

  let t_of_yojson (j : J.t) : t =
    or_default
      ~default
      (function
        | `String s -> of_string s
        | _ -> failwith "expected string")
      j
  ;;

  let yojson_of_t (t : t) : J.t = `String (to_string t)
end

(* {1 Sub-configs}  *)

module Struct_style_def = struct
  type t =
    | Plain
    | Basic
    | Graph

  let table = [ "plain", Plain, []; "basic", Basic, []; "graph", Graph, [] ]
  let default = Plain
end

module Struct_style = Make_string_enum (Struct_style_def)

module Ext_struct = struct
  type t =
    { enable : bool
    ; style : Struct_style.t [@default Struct_style.default]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let default = { enable = true; style = Struct_style.default }
  let t_of_yojson j = or_default ~default t_of_yojson j
end

module Theme_def = struct
  type t =
    | Tokyonight
    | Gruvbox
    | Atom_one_light
    | Atom_one_dark
    | Bluloco_light
    | Bluloco_dark
    | No_theme

  let table =
    [ "tokyonight", Tokyonight, []
    ; "gruvbox", Gruvbox, []
    ; "atom_one_light", Atom_one_light, [ "atom-one-light" ]
    ; "atom_one_dark", Atom_one_dark, [ "atom-one-dark" ]
    ; "bluloco_light", Bluloco_light, [ "bluloco-light" ]
    ; "bluloco_dark", Bluloco_dark, [ "bluloco-dark" ]
    ; "no_theme", No_theme, [ "none" ]
    ]
  ;;

  let default = Bluloco_dark
end

module Theme = Make_string_enum (Theme_def)

module Pipeline_profile_def = struct
  type t =
    | Default
    | Basic
    | None_profile

  let table = [ "default", Default, []; "basic", Basic, []; "none", None_profile, [] ]
  let default = Default
end

module Pipeline_profile = Make_string_enum (Pipeline_profile_def)

(** Ordering spec for TOC generation.

    A list of name patterns with a single wildcard [*] acting as a placeholder
    for "everything not otherwise matched". The position of [*] determines
    where unmatched entries land; items before [*] come first, items after
    come last.

    Patterns match a TOC entry's top-level segment name (with any [.md]
    extension stripped). Items with equal rank tiebreak alphabetically.

    Examples:
    - [\[ "introduction"; "guides"; "*"; "changelog" \]] — fixed head, fixed tail
    - [\[ "*" \]] (default) — no custom order; everything sorts alphabetically
    - [\[ "*"; "appendix" \]] — push a specific name to the end *)
module Toc_order : sig
  type t = string list

  val default : t
  val t_of_yojson : J.t -> t
  val yojson_of_t : t -> J.t

  (** Rank of [name] under [patterns]. Lower ranks sort earlier. *)
  val rank_of : t -> string -> int
end = struct
  type t = string list

  let default : t = [ "*" ]

  let find_index (f : 'a -> bool) (xs : 'a list) : int option =
    let rec loop i = function
      | [] -> None
      | x :: _ when f x -> Some i
      | _ :: rest -> loop (i + 1) rest
    in
    loop 0 xs
  ;;

  let is_wildcard (s : string) : bool = String.equal s "*"

  let validate (xs : t) : t =
    let wildcard_count = List.length (List.filter is_wildcard xs) in
    if wildcard_count > 1 then failwith "toc_order: at most one '*' allowed";
    xs
  ;;

  let t_of_yojson (j : J.t) : t =
    or_default ~default (fun j -> j |> list_of_yojson string_of_yojson |> validate) j
  ;;

  let yojson_of_t (xs : t) : J.t = yojson_of_list yojson_of_string xs

  let rank_of (patterns : t) (name : string) : int =
    let wildcard_rank =
      match find_index is_wildcard patterns with
      | Some i -> i
      | None -> max_int
    in
    match
      find_index (fun p -> (not (is_wildcard p)) && String.equal p name) patterns
    with
    | Some i -> i
    | None -> wildcard_rank
  ;;
end

(** A selector for include/exclude lists. JSON shape:
    - [`String "all"] -> [Include_all]
    - [`String "none"] -> [Exclude_all]
    - [{ "include": [...] }] -> [Include [...]]
    - [{ "exclude": [...] }] -> [Exclude [...]] *)
module Selector : sig
  type t =
    | Include_all
    | Exclude_all
    | Include of string list
    | Exclude of string list

  val t_of_yojson : J.t -> t
  val yojson_of_t : t -> J.t
end = struct
  type t =
    | Include_all
    | Exclude_all
    | Include of string list
    | Exclude of string list

  let t_of_yojson : J.t -> t = function
    | `String "all" -> Include_all
    | `String "none" -> Exclude_all
    | `Assoc [ ("include", xs) ] -> Include (list_of_yojson string_of_yojson xs)
    | `Assoc [ ("exclude", xs) ] -> Exclude (list_of_yojson string_of_yojson xs)
    | _ -> failwith "invalid selector"
  ;;

  let yojson_of_t : t -> J.t = function
    | Include_all -> `String "all"
    | Exclude_all -> `String "none"
    | Include xs -> `Assoc [ "include", yojson_of_list yojson_of_string xs ]
    | Exclude xs -> `Assoc [ "exclude", yojson_of_list yojson_of_string xs ]
  ;;
end

module Home_graph_view : sig
  type t =
    { dir : Selector.t (** Dir to use as clusters *)
    ; tag : Selector.t (** Tag to use as clusters *)
    ; default_dir : Selector.t (** Dir to be selected by default *)
    ; default_tag : Selector.t (** Tag to be selected by default *)
    }

  val default : t
  val t_of_yojson : J.t -> t
  val yojson_of_t : t -> J.t
end = struct
  let default_dir : Selector.t = Include_all
  let default_tag : Selector.t = Include_all
  let default_default_dir : Selector.t = Include [ "*" ]
  let default_default_tag : Selector.t = Exclude_all

  (* No [@yojson_drop_default]: this type is serialized as the wire
     format consumed by [static/graph_view/widget.ts], which expects every
     field present. [@default] is kept so user-written config files can
     omit fields. *)
  type t =
    { dir : Selector.t [@default default_dir]
    ; tag : Selector.t [@default default_tag]
    ; default_dir : Selector.t [@default default_default_dir]
    ; default_tag : Selector.t [@default default_default_tag]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let default : t =
    { dir = default_dir
    ; tag = default_tag
    ; default_dir = default_default_dir
    ; default_tag = default_default_tag
    }
  ;;

  (* Wrap derived parser so a malformed object falls back to default rather
     than raising. Per-field invalid values are tolerated by the derived parser
     via [@default]. *)
  let t_of_yojson j = or_default ~default t_of_yojson j
end

(** {1 Config config} *)

type t =
  { ext_struct : Ext_struct.t [@default Ext_struct.default]
  ; theme : Theme.t [@default Theme.default]
  ; css_snippets : string list [@default []]
  ; pipeline_profile : Pipeline_profile.t [@default Pipeline_profile.default]
  ; home_graph_view : Home_graph_view.t [@default Home_graph_view.default]
  ; toc_order : Toc_order.t [@default Toc_order.default]
  }
[@@deriving yojson] [@@yojson.allow_extra_fields]

let default : t =
  { ext_struct = Ext_struct.default
  ; theme = Theme.default
  ; css_snippets = []
  ; pipeline_profile = Pipeline_profile.default
  ; home_graph_view = Home_graph_view.default
  ; toc_order = Toc_order.default
  }
;;

let of_file (path : string) : t =
  let contents : string = In_channel.with_open_text path In_channel.input_all in
  or_default ~default t_of_yojson (J.from_string contents)
;;

let rec merge_json (base : J.t) (overlay : J.t) : J.t =
  match base, overlay with
  | `Assoc base_fields, `Assoc overlay_fields ->
    let merged =
      List.fold_left
        (fun acc (k, v) ->
           let base_v = List.assoc_opt k acc in
           let v' =
             match base_v with
             | Some bv -> merge_json bv v
             | None -> v
           in
           (k, v') :: List.remove_assoc k acc)
        base_fields
        overlay_fields
    in
    `Assoc merged
  | _, overlay -> overlay
;;

(** Merge two configs: keys in [overlay] override [base].
    Non-object inputs: [overlay] wins. *)
let merge (base : t) (overlay : t) : t =
  let base_j = yojson_of_t base in
  let overlay_j = yojson_of_t overlay in
  let j = merge_json base_j overlay_j in
  t_of_yojson j
;;

(** {1 Per-file config from frontmatter} *)

(** Convert a [Yaml.value] to [Yojson.Safe.t]. *)
let rec yaml_to_yojson : Yaml.value -> J.t = function
  | `Null -> `Null
  | `Bool b -> `Bool b
  | `Float f -> `Float f
  | `String s -> `String s
  | `A xs -> `List (List.map yaml_to_yojson xs)
  | `O pairs -> `Assoc (List.map (fun (k, v) -> k, yaml_to_yojson v) pairs)
;;

(** Extract per-file config from frontmatter YAML, merged over [default].
    Merges the raw JSON onto [default]'s JSON {e before} parsing into {!t},
 *)
let of_frontmatter ?(default = default) ?(config_key = "oyster") (fm : Yaml.value option)
  : t
  =
  match fm with
  | None | Some `Null -> default
  | Some (`O pairs) ->
    (match List.assoc_opt config_key pairs with
     | None -> default
     | Some ov_y ->
       let base_j = yojson_of_t default in
       let overlay_j = yaml_to_yojson ov_y in
       or_default ~default t_of_yojson (merge_json base_j overlay_j))
  | Some _ -> default
;;

(** {1 Tests} *)

(* Wire-format contract with [static/graph_view/config.d.ts]
   ----------
   This expect-test pins the JSON shape that the OCaml side serializes for
   the browser widget. If it fails, update [config.d.ts] in the same commit
   so the TypeScript wire types stay in sync. *)
let%expect_test "Home_graph_view wire format" =
  let example : Home_graph_view.t =
    { dir = Include_all
    ; tag = Exclude [ "draft" ]
    ; default_dir = Include [ "notes"; "blog" ]
    ; default_tag = Exclude_all
    }
  in
  example |> Home_graph_view.yojson_of_t |> J.pretty_to_string |> print_endline;
  [%expect
    {|
    {
      "dir": "all",
      "tag": { "exclude": [ "draft" ] },
      "default_dir": { "include": [ "notes", "blog" ] },
      "default_tag": "none"
    }
    |}]
;;

let%expect_test "of_frontmatter overrides ext_struct" =
  let fm : Yaml.value option =
    Some (`O [ "oyster", `O [ "ext_struct", `O [ "struct_style", `String "graph" ] ] ])
  in
  let merged = fm |> of_frontmatter |> fun fm -> merge default fm in
  merged |> yojson_of_t |> J.pretty_to_string |> print_endline;
  [%expect
    {|
    {
      "ext_struct": { "enable": true, "style": "plain" },
      "theme": "bluloco_dark",
      "css_snippets": [],
      "pipeline_profile": "default",
      "home_graph_view": {
        "dir": "all",
        "tag": "all",
        "default_dir": { "include": [ "*" ] },
        "default_tag": "none"
      },
      "toc_order": [ "*" ]
    }
    |}]
;;

let%expect_test "of_frontmatter no oystermark key returns base" =
  let fm : Yaml.value option = Some (`O [ "title", `String "Hello" ]) in
  let merged = fm |> of_frontmatter |> fun fm -> merge default fm in
  assert (merged = default)
;;

let%expect_test "Toc_order.rank_of" =
  let patterns = [ "intro"; "guides"; "*"; "changelog" ] in
  List.iter
    (fun name -> Printf.printf "%s -> %d\n" name (Toc_order.rank_of patterns name))
    [ "intro"; "guides"; "random"; "changelog"; "other" ];
  [%expect
    {|
    intro -> 0
    guides -> 1
    random -> 2
    changelog -> 3
    other -> 2
    |}]
;;

let%expect_test "Toc_order no wildcard — unmatched go last" =
  let patterns = [ "intro"; "guides" ] in
  List.iter
    (fun name -> Printf.printf "%s -> %d\n" name (Toc_order.rank_of patterns name))
    [ "intro"; "other" ];
  [%expect
    {|
    intro -> 0
    other -> 4611686018427387903
    |}]
;;

let%expect_test "Toc_order two wildcards falls back to default" =
  let j : J.t = `List [ `String "*"; `String "foo"; `String "*" ] in
  let parsed = Toc_order.t_of_yojson j in
  print_endline (J.to_string (Toc_order.yojson_of_t parsed));
  [%expect {| ["*"] |}]
;;

let%expect_test "Config default" =
  default |> yojson_of_t |> J.pretty_to_string |> print_endline;
  [%expect
    {|
    {
      "ext_struct": { "enable": true, "style": "plain" },
      "theme": "bluloco_dark",
      "css_snippets": [],
      "pipeline_profile": "default",
      "home_graph_view": {
        "dir": "all",
        "tag": "all",
        "default_dir": { "include": [ "*" ] },
        "default_tag": "none"
      },
      "toc_order": [ "*" ]
    }
    |}]
;;
