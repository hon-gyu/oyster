(** Strip YAML frontmatter from a markdown string.

    Frontmatter is delimited by exactly [---] on its own line.
    The opening delimiter must be the very first line of the file.
    The closing delimiter is also [---] on its own line.

    Exact 3 dashes are required in both the opening and closing delimiters.
    *)

open Core

type t =
  { yaml : Yaml.value option
  ; body : string
  }

let delimiter : string = "---"
let is_delimiter (line : string) : bool = String.equal (String.rstrip line) delimiter

(** [of_string s] splits [s] into frontmatter YAML and the remaining body.
    If [s] does not start with [---], returns [{ yaml = None; body = s }]. *)
let of_string (s : string) : t =
  match String.lsplit2 s ~on:'\n' with
  | None -> { yaml = None; body = s }
  | Some (first_line, rest) ->
    if not (is_delimiter first_line)
    then { yaml = None; body = s }
    else (
      (* Find the closing delimiter *)
      let lines = String.split_lines rest in
      let rec find_close (acc : string list) (remaining : string list) : t =
        match remaining with
        | [] ->
          (* No closing delimiter found — treat everything as body *)
          { yaml = None; body = s }
        | line :: tl ->
          if is_delimiter line
          then (
            let yaml_str = String.concat ~sep:"\n" (List.rev acc) in
            let yaml =
              match Yaml.of_string yaml_str with
              | Ok v -> Some v
              | Error _ -> None
            in
            let body = String.concat ~sep:"\n" tl in
            { yaml; body })
          else find_close (line :: acc) tl
      in
      find_close [] lines)
;;

let escape_html (s : string) : string =
  String.concat_map s ~f:(function
    | '&' -> "&amp;"
    | '<' -> "&lt;"
    | '>' -> "&gt;"
    | '"' -> "&quot;"
    | c -> String.make 1 c)
;;

(** Render a YAML value as an HTML fragment. *)
let rec value_to_html (v : Yaml.value) : string =
  match v with
  | `Null -> ""
  | `Bool b -> escape_html (Bool.to_string b)
  | `Float f ->
    if Float.is_integer f
    then escape_html (Int.to_string (Float.to_int f))
    else escape_html (Float.to_string f)
  | `String s -> escape_html s
  | `A items ->
    let lis = List.map items ~f:(fun v -> "<li>" ^ value_to_html v ^ "</li>") in
    "<ul>" ^ String.concat lis ^ "</ul>"
  | `O pairs ->
    let rows =
      List.map pairs ~f:(fun (k, v) ->
        "<tr><th>" ^ escape_html k ^ "</th><td>" ^ value_to_html v ^ "</td></tr>")
    in
    "<table>" ^ String.concat rows ^ "</table>"
;;

(** Render frontmatter as an HTML table wrapped in a frontmatter div.
    Returns empty string if there is no frontmatter. *)
let to_html (fm : Yaml.value option) : string =
  match fm with
  | None | Some `Null -> ""
  | Some v -> "<div class=\"frontmatter\">" ^ value_to_html v ^ "</div>\n"
;;

let pp_yaml (v : Yaml.value option) : string =
  match v with
  | None -> "<none>"
  | Some v -> Yaml.to_string_exn v
;;

let%expect_test "no frontmatter" =
  let result = of_string "# Hello\n\nSome text" in
  Printf.printf "yaml: %s\nbody: %s\n" (pp_yaml result.yaml) result.body;
  [%expect
    {|
    yaml: <none>
    body: # Hello

    Some text
    |}]
;;

let%expect_test "with frontmatter" =
  let result = of_string "---\ntitle: Hello\ntags: [a, b]\n---\n# Hello\n\nSome text" in
  Printf.printf "yaml: %sbody: %s\n" (pp_yaml result.yaml) result.body;
  [%expect
    {|
    yaml: title: Hello
    tags:
    - a
    - b
    body: # Hello

    Some text
    |}]
;;

let%expect_test "unclosed frontmatter" =
  let result = of_string "---\ntitle: Hello\nno closing" in
  Printf.printf "yaml: %s\nbody: %s\n" (pp_yaml result.yaml) result.body;
  [%expect
    {|
    yaml: <none>
    body: ---
    title: Hello
    no closing
    |}]
;;

let%expect_test "empty frontmatter" =
  let result = of_string "---\n---\n# Body" in
  Printf.printf "yaml: %sbody: %s\n" (pp_yaml result.yaml) result.body;
  [%expect
    {|
    yaml:
    body: # Body
    |}]
;;
