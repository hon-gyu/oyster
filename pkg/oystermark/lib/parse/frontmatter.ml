(** Strip YAML frontmatter from a markdown string.

    Frontmatter is delimited by exactly [---] on its own line.
    The opening delimiter must be the very first line of the file.
    The closing delimiter is also [---] on its own line.

    Exact 3 dashes are required in both the opening and closing delimiters.
    *)

open Core
open Cmarkit

type t = Yaml.value

let to_commonmark (fm : Yaml.value) : string = "---\n" ^ Yaml.to_string_exn fm ^ "---\n"

type Cmarkit.Block.t += Frontmatter of Yaml.value

let block_commonmark_renderer : Cmarkit_renderer.block =
  let open Cmarkit_renderer in
  fun (c : context) (b : Block.t) ->
    match b with
    | Frontmatter y ->
      Context.string c (to_commonmark y);
      true
    | _ -> false
;;

let sexp_of_block : Common.block_sexp =
  fun ~recurse_inline:_ ~recurse_block:_ ~with_meta:_ b ->
  match b with
  | Frontmatter _ -> Some (Sexp.Atom "Frontmatter")
  | _ -> None
;;

let make_block_mapper (f : Yaml.value -> Yaml.value option)
  : Cmarkit.Block.t Cmarkit.Mapper.mapper
  =
  let open Cmarkit in
  fun (_m : Mapper.t) (block : Block.t) ->
    match block with
    | Frontmatter y ->
      (match f y with
       | Some y -> Mapper.ret (Frontmatter y)
       | None -> Mapper.ret (Block.Blocks ([], Meta.none)))
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

(** [blank_frontmatter s] is [(yaml, input)] where [input] is [s] with any
    leading frontmatter block replaced by whitespace — each non-newline byte
    becomes a space, newlines are kept — so every byte and line position is
    preserved. Parsing [input] instead of the stripped {!of_string} body keeps
    the parsed AST's [Cmarkit.Textloc] offsets aligned with the {e original}
    file, which LSP positions depend on. When [s] has no (closed) frontmatter,
    [input] is [s] unchanged. *)
let blank_frontmatter (s : string) : Yaml.value option * string =
  let n = String.length s in
  let line_end pos =
    match String.index_from s pos '\n' with
    | Some i -> i
    | None -> n
  in
  let first_end = line_end 0 in
  if first_end >= n || not (is_delimiter (String.sub s ~pos:0 ~len:first_end))
  then None, s
  else (
    let rec find_close pos yaml_lines =
      if pos >= n
      then None (* reached end without a closing delimiter *)
      else (
        let e = line_end pos in
        let line = String.sub s ~pos ~len:(e - pos) in
        if is_delimiter line
        then (
          let body_start = if e < n then e + 1 else n in
          let yaml_str = String.concat ~sep:"\n" (List.rev yaml_lines) in
          let yaml =
            match Yaml.of_string yaml_str with
            | Ok v -> Some v
            | Error _ -> None
          in
          Some (yaml, body_start))
        else find_close (e + 1) (line :: yaml_lines))
    in
    match find_close (first_end + 1) [] with
    | None -> None, s (* unclosed: whole string is body, matching {!of_string} *)
    | Some (yaml, body_start) ->
      let b = Bytes.of_string s in
      for i = 0 to body_start - 1 do
        if not (Char.equal (Bytes.get b i) '\n') then Bytes.set b i ' '
      done;
      yaml, Bytes.to_string b)
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
  | Some v -> value_to_html v
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
  let to_string (v : t option) : string =
    match v with
    | None -> "<none>"
    | Some v -> Yaml.to_string_exn v
  ;;

  let%expect_test "no frontmatter" =
    let yaml, body = of_string "# Hello\n\nSome text" in
    Printf.printf "yaml: %s\nbody: %s\n" (to_string yaml) body;
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
    Printf.printf "yaml: %sbody: %s\n" (to_string yaml) body;
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
    Printf.printf "yaml: %s\nbody: %s\n" (to_string yaml) body;
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
    Printf.printf "yaml: %sbody: %s\n" (to_string yaml) body;
    [%expect
      {|
    yaml:
    body: # Body
    |}]
  ;;

  (* [blank_frontmatter] preserves byte and line positions: the frontmatter
     region becomes whitespace (newlines kept), the body is byte-identical, and
     the total length is unchanged. Shown with [|] markers around the result. *)
  let show_blank (s : string) : unit =
    let yaml, input = blank_frontmatter s in
    Printf.printf
      "yaml: %s\nsame_length: %b\ninput:\n|%s|\n"
      (to_string yaml)
      (String.length s = String.length input)
      input
  ;;

  let%expect_test "blank_frontmatter: preserves positions" =
    show_blank "---\ntitle: K\n---\n# Kap\n\nBody.\n";
    [%expect
      {|
      yaml: title: K

      same_length: true
      input:
      |


      # Kap

      Body.
      |
      |}]
  ;;

  let%expect_test "blank_frontmatter: no frontmatter is unchanged" =
    show_blank "# Kap\n\nBody.\n";
    [%expect
      {|
      yaml: <none>
      same_length: true
      input:
      |# Kap

      Body.
      |
      |}]
  ;;

  let%expect_test "blank_frontmatter: unclosed is unchanged" =
    show_blank "---\ntitle: K\nno closing\n";
    [%expect
      {|
      yaml: <none>
      same_length: true
      input:
      |---
      title: K
      no closing
      |
      |}]
  ;;
end
