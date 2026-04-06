module type Defaultable = sig
  type t

  val default : t
end

module Theme : sig
  include Defaultable

  val of_string : string -> t
  val to_string : t -> string
end = struct
  type t =
    | Tokyonight
    | Gruvbox
    | Atom_one_light
    | Atom_one_dark
    | Bluloco_light
    | Bluloco_dark
    | No_theme

  (** Canonical name, variant, plus aliases (extra strings that also map to it). *)
  let theme_table : (string * t * string list) list =
    [ "tokyonight", Tokyonight, []
    ; "gruvbox", Gruvbox, []
    ; "atom_one_light", Atom_one_light, [ "atom-one-light" ]
    ; "atom_one_dark", Atom_one_dark, [ "atom-one-dark" ]
    ; "bluloco_light", Bluloco_light, [ "bluloco-light" ]
    ; "bluloco_dark", Bluloco_dark, [ "bluloco-dark" ]
    ; "no_theme", No_theme, [ "none" ]
    ]
  ;;

  let of_string (s : string) : t =
    match
      List.find_opt
        (fun (canonical, _, aliases) -> String.equal s canonical || List.mem s aliases)
        theme_table
    with
    | Some (_, t, _) -> t
    | None -> failwith ("Invalid theme: " ^ s)
  ;;

  let to_string (t : t) : string =
    let canonical, _, _ = List.find (fun (_, t', _) -> t = t') theme_table in
    canonical
  ;;

  let default = Bluloco_dark
end

module Pipeline_profile : sig
  include Defaultable

  val of_string : string -> t
  val to_string : t -> string
end = struct
  type t =
    | Default
    | Basic
    | None_profile

  let pipeline_profile_table : (string * t) list =
    [ "default", Default; "basic", Basic; "none", None_profile ]
  ;;

  let of_string (s : string) : t =
    match List.assoc_opt s pipeline_profile_table with
    | Some p -> p
    | None -> failwith ("Invalid pipeline profile: " ^ s)
  ;;

  let to_string (p : t) : string =
    let canonical, _ = List.find (fun (_, p') -> p = p') pipeline_profile_table in
    canonical
  ;;

  let default = Default
end

module Home_graph_view : Defaultable = struct
  type dir =
    | Include_all
    | Exclude_all
    | Include of string list (** Directories to include (supports glob patterns) *)
    | Exclude of string list (** Directories to exclude (supports glob patterns) *)

  type tag =
    | Include_all
    | Exclude_all
    | Include of string list
    | Exclude of string list

  (** Dir cluster that are selected by default *)
  type default_dir =
    | Include_all
    | Exclude_all
    | Include of string list
    | Exclude of string list

  (** Tag cluster that are selected by default *)
  type default_tag =
    | Include_all
    | Exclude_all
    | Include of string list
    | Exclude of string list

  type t =
    { dir : dir
    ; tag : tag
    ; default_dir : default_dir
    ; default_tag : default_tag
    }

  let default : t =
    { dir = Include_all
    ; tag = Include_all
    ; default_dir = Include [ "*" ]
    ; default_tag = Exclude_all
    }
  ;;
end

type t =
  { theme : Theme.t
  ; css_snippets : string list
  ; pipeline_profile : Pipeline_profile.t
  ; home_graph_view : Home_graph_view.t
  }

let default : t =
  { theme = Theme.default
  ; css_snippets = []
  ; pipeline_profile = Pipeline_profile.default
  ; home_graph_view = Home_graph_view.default
  }
;;

(* let of_yaml_value (v : Yaml.value) : t =
  match v with
  | `O pairs ->
    let theme : theme =
      match List.assoc_opt "theme" pairs with
      | Some (`String s) -> theme_of_string s
      | Some _ -> failwith "config: 'theme' must be a string"
      | None -> default.theme
    in
    let css_snippets : string list =
      match List.assoc_opt "css_snippets" pairs with
      | Some (`A items) ->
        List.map
          (fun (v : Yaml.value) ->
             match v with
             | `String s -> s
             | _ -> failwith "config: each css_snippet must be a string")
          items
      | Some `Null | None -> default.css_snippets
      | Some _ -> failwith "config: 'css_snippets' must be a list"
    in
    let pipeline_profile : pipeline_profile =
      match List.assoc_opt "pipeline_profile" pairs with
      | Some (`String s) -> pipeline_profile_of_string s
      | Some _ -> failwith "config: 'pipeline_profile' must be a string"
      | None -> default.pipeline_profile
    in
    { theme; css_snippets; pipeline_profile }
  | _ -> failwith "config: expected a YAML mapping"
;;

let of_yaml_string (s : string) : t =
  match Yaml.of_string s with
  | Ok v -> of_yaml_value v
  | Error (`Msg msg) -> failwith ("config: failed to parse YAML: " ^ msg)
;;
*)

(* let of_file (path : string) : t =
  let contents : string = In_channel.with_open_text path In_channel.input_all in
  of_yaml_string contents
;; *)
