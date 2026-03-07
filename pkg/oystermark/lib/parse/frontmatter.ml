(** Strip YAML frontmatter from a markdown string.

    Frontmatter is delimited by exactly [---] on its own line.
    The opening delimiter must be the very first line of the file.
    The closing delimiter is also [---] on its own line.

    Exact 3 dashes are required in both the opening and closing delimiters.
    *)

open Core

type t = Yaml.value

let to_commonmark (fm : Yaml.value) : string = "---\n" ^ Yaml.to_string_exn fm ^ "---\n"

type Cmarkit.Block.t += Frontmatter of Yaml.value

let make_block_mapper (f : Yaml.value -> Yaml.value)
  : Cmarkit.Block.t Cmarkit.Mapper.mapper
  =
  let open Cmarkit in
  fun (_m : Mapper.t) (block : Block.t) ->
    match block with
    | Frontmatter y -> Mapper.ret (Frontmatter (f y))
    | other -> Mapper.default
;;

let delimiter : string = "---"
let is_delimiter (line : string) : bool = String.equal (String.rstrip line) delimiter

(** [of_string s] splits [s] into frontmatter YAML and the remaining body.
    If [s] does not start with [---], returns [{ yaml = None; body = s }]. *)
let of_string (s : string) : Yaml.value option * string =
  match String.lsplit2 s ~on:'\n' with
  | None -> None, s
  | Some (first_line, rest) ->
    if not (is_delimiter first_line)
    then None, s
    else (
      (* Find the closing delimiter *)
      let lines = String.split_lines rest in
      let rec find_close (acc : string list) (remaining : string list)
        : Yaml.value option * string
        =
        match remaining with
        | [] ->
          (* No closing delimiter found — treat everything as body *)
          None, s
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
            yaml, body)
          else find_close (line :: acc) tl
      in
      find_close [] lines)
;;

let escape_html (s : string) : string =
  let buf = Buffer.create (String.length s) in
  Cmarkit_html.buffer_add_html_escaped_string buf s;
  Buffer.contents buf
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

(** Extract the frontmatter value from a doc's top-level block, if present. *)
let of_doc (doc : Cmarkit.Doc.t) : Yaml.value option =
  match Cmarkit.Doc.block doc with
  | Cmarkit.Block.Blocks (blocks, _) ->
    (match blocks with
     | Frontmatter y :: _ -> Some y
     | _ -> None)
  | Frontmatter y -> Some y
  | _ -> None
;;

module For_test = struct
  let pp_yaml (v : t option) : string =
    match v with
    | None -> "<none>"
    | Some v -> Yaml.to_string_exn v
  ;;

  let%expect_test "no frontmatter" =
    let yaml, body = of_string "# Hello\n\nSome text" in
    Printf.printf "yaml: %s\nbody: %s\n" (pp_yaml yaml) body;
    [%expect
      {|
    yaml: <none>
    body: # Hello

    Some text
    |}]
  ;;

  let%expect_test "with frontmatter" =
    let yaml, body =
      of_string "---\ntitle: Hello\ntags: [a, b]\n---\n# Hello\n\nSome text"
    in
    Printf.printf "yaml: %sbody: %s\n" (pp_yaml yaml) body;
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
    let yaml, body = of_string "---\ntitle: Hello\nno closing" in
    Printf.printf "yaml: %s\nbody: %s\n" (pp_yaml yaml) body;
    [%expect
      {|
    yaml: <none>
    body: ---
    title: Hello
    no closing
    |}]
  ;;

  let%expect_test "empty frontmatter" =
    let yaml, body = of_string "---\n---\n# Body" in
    Printf.printf "yaml: %sbody: %s\n" (pp_yaml yaml) body;
    [%expect
      {|
    yaml:
    body: # Body
    |}]
  ;;
end
