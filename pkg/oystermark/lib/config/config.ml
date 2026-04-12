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

module Ext_struct = struct
  type t = { enable : bool } [@@deriving yojson] [@@yojson.allow_extra_fields]

  let default = { enable = true }
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

type t =
  { ext_struct : Ext_struct.t [@default Ext_struct.default]
  ; theme : Theme.t [@default Theme.default]
  ; css_snippets : string list [@default []]
  ; pipeline_profile : Pipeline_profile.t [@default Pipeline_profile.default]
  ; home_graph_view : Home_graph_view.t [@default Home_graph_view.default]
  }
[@@deriving yojson] [@@yojson.allow_extra_fields]

let default : t =
  { ext_struct = Ext_struct.default
  ; theme = Theme.default
  ; css_snippets = []
  ; pipeline_profile = Pipeline_profile.default
  ; home_graph_view = Home_graph_view.default
  }
;;

let of_file (path : string) : t =
  let contents : string = In_channel.with_open_text path In_channel.input_all in
  or_default ~default t_of_yojson (J.from_string contents)
;;

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

let%expect_test "Config default" =
  default |> yojson_of_t |> J.pretty_to_string |> print_endline;
  [%expect
    {|
    {
      "ext_struct": { "enable": true },
      "theme": "bluloco_dark",
      "css_snippets": [],
      "pipeline_profile": "default",
      "home_graph_view": {
        "dir": "all",
        "tag": "all",
        "default_dir": { "include": [ "*" ] },
        "default_tag": "none"
      }
    }
    |}]
;;
