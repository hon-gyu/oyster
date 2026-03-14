type theme =
  | Tokyonight
  | Gruvbox
  | Atom_one_light
  | Atom_one_dark
  | Bluloco_light
  | Bluloco_dark
  | No_theme

let theme_of_string = function
  | "tokyonight" -> Tokyonight
  | "gruvbox" -> Gruvbox
  | "atom_one_light" | "atom-one-light" -> Atom_one_light
  | "atom_one_dark" | "atom-one-dark" -> Atom_one_dark
  | "bluloco_light" | "bluloco-light" -> Bluloco_light
  | "bluloco_dark" | "bluloco-dark" -> Bluloco_dark
  | "no_theme" | "none" -> No_theme
  | _ -> failwith "Invalid theme"
;;

let theme_to_string = function
  | Tokyonight -> "tokyonight"
  | Gruvbox -> "gruvbox"
  | Atom_one_light -> "atom_one_light"
  | Atom_one_dark -> "atom_one_dark"
  | Bluloco_light -> "bluloco_light"
  | Bluloco_dark -> "bluloco_dark"
  | No_theme -> "no_theme"
;;

type t =
  { theme : theme
  ; css_snippets : string list
  }

let default : t = { theme = Bluloco_dark; css_snippets = [] }

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
    { theme; css_snippets }
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
