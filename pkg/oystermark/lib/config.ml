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
  | "bluloco_dark"  | "bluloco-dark"  -> Bluloco_dark
  | "no_theme" | "none" -> No_theme
  | _ -> failwith "Invalid theme"

let theme_to_string = function
  | Tokyonight -> "tokyonight"
  | Gruvbox -> "gruvbox"
  | Atom_one_light -> "atom_one_light"
  | Atom_one_dark -> "atom_one_dark"
  | Bluloco_light -> "bluloco_light"
  | Bluloco_dark -> "bluloco_dark"
  | No_theme -> "no_theme"

type t = {
  theme: theme;
  css_snippets: string list;
}
