open Ppx_yojson_conv_lib.Yojson_conv.Primitives
module J = Yojson.Safe

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

module Make_string_enum (E : String_enum) : sig
  type t = E.t

  val default : t
  val of_string : string -> t
  val to_string : t -> string
  val t_of_yojson : J.t -> t
  val yojson_of_t : t -> J.t
end = struct
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

module Theme = Make_string_enum (struct
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
  end)

module Pipeline_profile = Make_string_enum (struct
    type t =
      | Default
      | Basic
      | None_profile

    let table = [ "default", Default, []; "basic", Basic, []; "none", None_profile, [] ]
    let default = Default
  end)

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
    { dir : Selector.t
    ; tag : Selector.t
    ; default_dir : Selector.t (** Dir cluster selected by default *)
    ; default_tag : Selector.t (** Tag cluster selected by default *)
    }

  val default : t
  val t_of_yojson : J.t -> t
end = struct
  let default_dir : Selector.t = Include_all
  let default_tag : Selector.t = Include_all
  let default_default_dir : Selector.t = Include [ "*" ]
  let default_default_tag : Selector.t = Exclude_all

  type t =
    { dir : Selector.t [@default default_dir] [@yojson_drop_default ( = )]
    ; tag : Selector.t [@default default_tag] [@yojson_drop_default ( = )]
    ; default_dir : Selector.t [@default default_default_dir] [@yojson_drop_default ( = )]
    ; default_tag : Selector.t [@default default_default_tag] [@yojson_drop_default ( = )]
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
  { theme : Theme.t [@default Theme.default]
  ; css_snippets : string list [@default []]
  ; pipeline_profile : Pipeline_profile.t [@default Pipeline_profile.default]
  ; home_graph_view : Home_graph_view.t [@default Home_graph_view.default]
  }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

let default : t =
  { theme = Theme.default
  ; css_snippets = []
  ; pipeline_profile = Pipeline_profile.default
  ; home_graph_view = Home_graph_view.default
  }
;;

let of_file (path : string) : t =
  let contents : string = In_channel.with_open_text path In_channel.input_all in
  or_default ~default t_of_yojson (J.from_string contents)
;;
