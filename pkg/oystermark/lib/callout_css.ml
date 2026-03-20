(** Post-process CSS to expand Obsidian-style callout definitions.

    Transforms compact callout blocks:
    {v
    .callout[data-callout="note"] {
      --callout-color: var(--accent);
      --callout-icon: lucide-pencil;
    }
    v}

    Into full CSS with mask-image rules:
    {v
    .callout[data-callout="note"] {
      --callout-color: var(--accent);
    }
    .callout[data-callout="note"] .callout-title::before {
      -webkit-mask-image: url("data:image/svg+xml,...");
      mask-image: url("data:image/svg+xml,...");
    }
    v}

    Also wraps bare [r, g, b] color values in [rgb()].

    TODO: fully conform to the following
    {v
     --callout-icon can be an icon ID from lucide.dev,
     or an SVG element.
    v}
    *)

open Core

(** Percent-encode a character for use in SVG data URIs. *)
let pct_encode_char : char -> string = function
  | '<' -> "%3C"
  | '>' -> "%3E"
  | '#' -> "%23"
  | '"' -> "%22"
  | c -> String.of_char c
;;

(** Percent-encode a string for use in a CSS [url("data:image/svg+xml,...")]. *)
let pct_encode (s : string) : string = String.concat_map s ~f:pct_encode_char

(** Build a CSS [url("data:image/svg+xml,...")] from inner SVG elements. *)
let svg_data_uri (body : string) : string =
  let svg =
    Printf.sprintf
      "<svg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 \
       24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' \
       stroke-linejoin='round'>%s</svg>"
      body
  in
  Printf.sprintf "url(\"%s\")" ("data:image/svg+xml," ^ pct_encode svg)
;;

(** Lucide icon SVG bodies, keyed by icon name (with [lucide-] prefix).
    Values are raw SVG elements (paths, circles, etc.) using single-quoted attributes. *)
let lucide_icon_body (name : string) : string option =
  match name with
  | "lucide-pencil" ->
    Some
      "<path d='M12 20h9'/><path d='M16.376 3.622a1 1 0 0 1 3.002 3.002L7.368 18.635a2 2 \
       0 0 1-.855.506l-2.872.838a.5.5 0 0 1-.62-.62l.838-2.872a2 2 0 0 1 .506-.854z'/>"
  | "lucide-info" ->
    Some "<circle cx='12' cy='12' r='10'/><path d='M12 16v-4'/><path d='M12 8h.01'/>"
  | "lucide-flame" ->
    Some
      "<path d='M8.5 14.5A2.5 2.5 0 0 0 11 12c0-1.38-.5-2-1-3-1.072-2.143-.224-4.054 2-6 \
       .5 2.5 2 4.9 4 6.5 2 1.6 3 3.5 3 5.5a7 7 0 1 1-14 0c0-1.153.433-2.294 1-3a2.5 2.5 \
       0 0 0 2.5 2.5z'/>"
  | "lucide-clipboard-list" ->
    Some
      "<rect width='8' height='4' x='8' y='2' rx='1' ry='1'/><path d='M16 4h2a2 2 0 0 1 \
       2 2v14a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2h2'/><path d='M12 \
       11h4'/><path d='M12 16h4'/><path d='M8 11h.01'/><path d='M8 16h.01'/>"
  | "lucide-circle-check" ->
    Some "<circle cx='12' cy='12' r='10'/><path d='m9 12 2 2 4-4'/>"
  | "lucide-check" -> Some "<path d='M20 6 9 17l-5-5'/>"
  | "lucide-circle-help" ->
    Some
      "<circle cx='12' cy='12' r='10'/><path d='M9.09 9a3 3 0 0 1 5.83 1c0 2-3 3-3 \
       3'/><path d='M12 17h.01'/>"
  | "lucide-triangle-alert" ->
    Some
      "<path d='m21.73 18-8-14a2 2 0 0 0-3.48 0l-8 14A2 2 0 0 0 4 21h16a2 2 0 0 0 \
       1.73-3'/><path d='M12 9v4'/><path d='M12 17h.01'/>"
  | "lucide-x" -> Some "<path d='M18 6 6 18'/><path d='m6 6 12 12'/>"
  | "lucide-zap" ->
    Some
      "<path d='M4 14a1 1 0 0 1-.78-1.63l9.9-10.2a.5.5 0 0 1 .86.46l-1.92 6.02A1 1 0 0 0 \
       13 10h7a1 1 0 0 1 .78 1.63l-9.9 10.2a.5.5 0 0 1-.86-.46l1.92-6.02A1 1 0 0 0 11 \
       14z'/>"
  | "lucide-bug" ->
    Some
      "<path d='m8 2 1.88 1.88'/><path d='M14.12 3.88 16 2'/><path d='M9 7.13v-1a3.003 \
       3.003 0 1 1 6 0v1'/><path d='M12 20c-3.3 0-6-2.7-6-6v-3a4 4 0 0 1 4-4h4a4 4 0 0 1 \
       4 4v3c0 3.3-2.7 6-6 6'/><path d='M12 20v-9'/><path d='M6.53 9C4.6 8.8 3 7.1 3 \
       5'/><path d='M6 13H2'/><path d='M3 21c0-2.1 1.7-3.9 3.8-4'/><path d='M20.97 5c0 \
       2.1-1.6 3.8-3.5 4'/><path d='M22 13h-4'/><path d='M17.2 17c2.1.1 3.8 1.9 3.8 4'/>"
  | "lucide-list" ->
    Some
      "<line x1='8' x2='21' y1='6' y2='6'/><line x1='8' x2='21' y1='12' y2='12'/><line \
       x1='8' x2='21' y1='18' y2='18'/><line x1='3' x2='3.01' y1='6' y2='6'/><line \
       x1='3' x2='3.01' y1='12' y2='12'/><line x1='3' x2='3.01' y1='18' y2='18'/>"
  | "lucide-quote" -> Some "<path d='M6 21 3 9h4l3-6'/><path d='M17 21 14 9h4l3-6'/>"
  | "lucide-bot-message-square" ->
    Some
      "<path d='M12 6V2H8'/><path d='m8 18-4 4V8a2 2 0 0 1 2-2h12a2 2 0 0 1 2 2v8a2 2 0 \
       0 1-2 2Z'/><path d='M2 12h2'/><path d='M9 11v2'/><path d='M15 11v2'/><path d='M20 \
       12h2'/>"
  | "lucide-square-chevron-right" ->
    Some "<rect width='18' height='18' x='3' y='3' rx='2'/><path d='m10 8 4 4-4 4'/>"
  | _ -> None
;;

(** Check whether a [--callout-color] value is a bare [r, g, b] triple
    (no [rgb()], [var()], or [#] prefix). *)
let is_bare_rgb (value : string) : bool =
  String.mem value ','
  && (not (String.is_substring value ~substring:"("))
  && not (String.is_prefix value ~prefix:"#")
;;

(** Parse a single CSS declaration block.  Returns [(selectors, props)] where
    [selectors] is the raw selector text before [{] and [props] is a list of
    [(property, value)] pairs. *)
type css_block =
  { selector : string
  ; props : (string * string) list
  }

let parse_block (selector_text : string) (body : string) : css_block =
  let selector = String.strip selector_text in
  let props =
    String.split body ~on:';'
    |> List.filter_map ~f:(fun decl ->
      let decl = String.strip decl in
      match String.lsplit2 decl ~on:':' with
      | Some (prop, value) -> Some (String.strip prop, String.strip value)
      | None -> None)
  in
  { selector; props }
;;

(** Extract all [data-callout] selectors from a comma-separated selector string.
    E.g. [.callout\[data-callout="tip"\],\n.callout\[data-callout="hint"\]]
    yields [".callout\[data-callout=\"tip\"\]"; ".callout\[data-callout=\"hint\"\]"]. *)
let extract_callout_selectors (selector : string) : string list =
  String.split selector ~on:','
  |> List.map ~f:String.strip
  |> List.filter ~f:(fun s -> String.is_substring s ~substring:"data-callout")
;;

(** Generate the [.callout-title::before] mask-image rule for a given selector
    and Lucide icon name. *)
let generate_icon_rule (selectors : string list) (icon_name : string) : string option =
  match lucide_icon_body icon_name with
  | None -> None
  | Some body ->
    let uri = svg_data_uri body in
    let before_selectors =
      List.map selectors ~f:(fun s -> s ^ " .callout-title::before")
      |> String.concat ~sep:",\n"
    in
    Some
      (Printf.sprintf
         "%s {\n  -webkit-mask-image: %s;\n  mask-image: %s;\n}\n"
         before_selectors
         uri
         uri)
;;

(** Expand Obsidian-style callout CSS.  Scans for [--callout-icon] declarations,
    generates [mask-image] rules, wraps bare RGB colors, and strips the
    [--callout-icon] property from the output. *)
let expand (css : string) : string =
  let buf = Buffer.create (String.length css + 512) in
  let len = String.length css in
  let rec scan (pos : int) =
    if pos >= len
    then ()
    else (
      (* Find next '{' *)
      match String.substr_index css ~pos ~pattern:"{" with
      | None -> Buffer.add_string buf (String.sub css ~pos ~len:(len - pos))
      | Some brace_open ->
        (* Find matching '}' (simple: no nesting awareness needed for our CSS) *)
        (match String.substr_index css ~pos:(brace_open + 1) ~pattern:"}" with
         | None -> Buffer.add_string buf (String.sub css ~pos ~len:(len - pos))
         | Some brace_close ->
           let selector_text = String.sub css ~pos ~len:(brace_open - pos) in
           let body =
             String.sub css ~pos:(brace_open + 1) ~len:(brace_close - brace_open - 1)
           in
           let block = parse_block selector_text body in
           let has_callout_icon =
             List.exists block.props ~f:(fun (p, _) -> String.equal p "--callout-icon")
           in
           if has_callout_icon
           then (
             let icon_name =
               List.find_map block.props ~f:(fun (p, v) ->
                 if String.equal p "--callout-icon" then Some v else None)
             in
             let selectors = extract_callout_selectors block.selector in
             (* Emit the block without --callout-icon, with color fix *)
             let remaining_props =
               List.filter block.props ~f:(fun (p, _) ->
                 not (String.equal p "--callout-icon"))
               |> List.map ~f:(fun (p, v) ->
                 if String.equal p "--callout-color" && is_bare_rgb v
                 then p, Printf.sprintf "rgb(%s)" v
                 else p, v)
             in
             Buffer.add_string buf selector_text;
             Buffer.add_string buf "{\n";
             List.iter remaining_props ~f:(fun (p, v) ->
               Buffer.add_string buf (Printf.sprintf "  %s: %s;\n" p v));
             Buffer.add_string buf "}\n";
             (* Emit the ::before mask-image rule *)
             (match icon_name with
              | Some name ->
                (match generate_icon_rule selectors name with
                 | Some rule -> Buffer.add_string buf rule
                 | None ->
                   Buffer.add_string buf (Printf.sprintf "/* unknown icon: %s */\n" name))
              | None -> ());
             scan (brace_close + 1))
           else (
             (* Pass through unchanged *)
             Buffer.add_string buf (String.sub css ~pos ~len:(brace_close + 1 - pos));
             scan (brace_close + 1))))
  in
  scan 0;
  Buffer.contents buf
;;

let%expect_test "expand bare RGB color" =
  let css =
    {|.callout[data-callout="test"] {
  --callout-color: 127, 134, 193;
  --callout-icon: lucide-info;
}|}
  in
  let result = expand css in
  (* Check the color is wrapped in rgb() *)
  assert (String.is_substring result ~substring:"--callout-color: rgb(127, 134, 193);");
  (* Check the mask-image rule is generated *)
  assert (
    String.is_substring
      result
      ~substring:".callout[data-callout=\"test\"] .callout-title::before");
  assert (String.is_substring result ~substring:"mask-image: url(");
  (* Check --callout-icon is removed *)
  assert (not (String.is_substring result ~substring:"--callout-icon"));
  print_endline "ok";
  [%expect {| ok |}]
;;

let%expect_test "expand preserves var() color" =
  let css =
    {|.callout[data-callout="note"] {
  --callout-color: var(--accent);
  --callout-icon: lucide-pencil;
}|}
  in
  let result = expand css in
  assert (String.is_substring result ~substring:"--callout-color: var(--accent);");
  print_endline "ok";
  [%expect {| ok |}]
;;

let%expect_test "expand multi-selector" =
  let css =
    {|.callout[data-callout="tip"],
.callout[data-callout="hint"] {
  --callout-color: #10b981;
  --callout-icon: lucide-flame;
}|}
  in
  let result = expand css in
  assert (
    String.is_substring
      result
      ~substring:
        ".callout[data-callout=\"tip\"] .callout-title::before,\n\
         .callout[data-callout=\"hint\"] .callout-title::before");
  print_endline "ok";
  [%expect {| ok |}]
;;

let%expect_test "expand passes through non-callout blocks" =
  let css = {|body { color: red; }|} in
  let result = expand css in
  assert (String.equal (String.strip result) (String.strip css));
  print_endline "ok";
  [%expect {| ok |}]
;;

let%expect_test "expand unknown icon" =
  let css =
    {|.callout[data-callout="custom"] {
  --callout-color: #ff0000;
  --callout-icon: lucide-nonexistent;
}|}
  in
  let result = expand css in
  assert (String.is_substring result ~substring:"/* unknown icon: lucide-nonexistent */");
  print_endline "ok";
  [%expect {| ok |}]
;;
