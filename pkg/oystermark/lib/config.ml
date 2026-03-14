type theme =
  | Tokyonight
  | Gruvbox
  | Atom_one_light
  | Atom_one_dark
  | Bluloco_light
  | Bluloco_dark
  | No_theme

(** Canonical name, variant, plus aliases (extra strings that also map to it). *)
let theme_table : (string * theme * string list) list =
  [ "tokyonight", Tokyonight, []
  ; "gruvbox", Gruvbox, []
  ; "atom_one_light", Atom_one_light, [ "atom-one-light" ]
  ; "atom_one_dark", Atom_one_dark, [ "atom-one-dark" ]
  ; "bluloco_light", Bluloco_light, [ "bluloco-light" ]
  ; "bluloco_dark", Bluloco_dark, [ "bluloco-dark" ]
  ; "no_theme", No_theme, [ "none" ]
  ]
;;

let theme_of_string (s : string) : theme =
  match
    List.find_opt
      (fun (canonical, _, aliases) -> String.equal s canonical || List.mem s aliases)
      theme_table
  with
  | Some (_, t, _) -> t
  | None -> failwith ("Invalid theme: " ^ s)
;;

let theme_to_string (t : theme) : string =
  let canonical, _, _ = List.find (fun (_, t', _) -> t = t') theme_table in
  canonical
;;

type pipeline_profile =
  | Default
  | Basic
  | None_profile

let pipeline_profile_table : (string * pipeline_profile) list =
  [ "default", Default; "basic", Basic; "none", None_profile ]
;;

let pipeline_profile_of_string (s : string) : pipeline_profile =
  match List.assoc_opt s pipeline_profile_table with
  | Some p -> p
  | None -> failwith ("Invalid pipeline profile: " ^ s)
;;

let pipeline_profile_to_string (p : pipeline_profile) : string =
  let canonical, _ = List.find (fun (_, p') -> p = p') pipeline_profile_table in
  canonical
;;

type t =
  { theme : theme
  ; css_snippets : string list
  ; pipeline_profile : pipeline_profile
  }

let default : t = { theme = Bluloco_dark; css_snippets = []; pipeline_profile = Default }

let of_yaml_value (v : Yaml.value) : t =
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

let of_file (path : string) : t =
  let contents : string = In_channel.with_open_text path In_channel.input_all in
  of_yaml_string contents
;;
