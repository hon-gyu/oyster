(** Add Obsidian-style wikilinks extension to Cmarkit.

A wikilink looks like [ [[Note#^blockid|display text]] ] or [ [[Note#Heading1#Heading2|display text]] ].

When there's a [!] before the \[\[, the content is embedded. *)

open Core
open Cmarkit

(** The type for wikilink fragment references. *)
type fragment =
  | Heading of string list (** e.g. [["H1"; "H2"]] for [[Note#H1#H2]] *)
  | Block_ref of string (** e.g. ["blockid"] for [[Note#^blockid]] *)
[@@deriving sexp, equal]

type t =
  { target : string option
  ; fragment : fragment option
  ; display : string option
  ; embed : bool
  }
[@@deriving sexp, equal]

(** Render wikilink back to markdown syntax. *)
let to_commonmark (wl : t) : string =
  let buf = Buffer.create 16 in
  (* Left bracket *)
  if wl.embed then Buffer.add_string buf "![[" else Buffer.add_string buf "[[";
  (* Target *)
  Buffer.add_string buf (Option.value ~default:"" wl.target);
  (* Fragment *)
  (match wl.fragment with
   | None -> ()
   | Some (Heading frag) ->
     Buffer.add_string buf "#";
     Buffer.add_string buf (String.concat ~sep:"#" frag)
   | Some (Block_ref frag) -> Buffer.add_string buf ("#^" ^ frag));
  (* Display *)
  (match wl.display with
   | None -> ()
   | Some d -> Buffer.add_string buf ("|" ^ d));
  (* Right bracket *)
  Buffer.add_string buf "]]";
  Buffer.contents buf
;;

let%expect_test "to_commonmark" =
  let wl : t =
    { target = Some "foo"
    ; fragment = Some (Heading [ "bar"; "baz" ])
    ; display = Some "quux"
    ; embed = false
    }
  in
  print_endline (to_commonmark wl);
  [%expect {| [[foo#bar#baz|quux]] |}];
  let wl : t =
    { target = Some "foo"
    ; fragment = Some (Block_ref "block-id")
    ; display = Some "quux"
    ; embed = false
    }
  in
  print_endline (to_commonmark wl);
  [%expect {| [[foo#^block-id|quux]] |}]
;;

(** Inline extension constructor for wikilinks. *)
type Inline.t += Ext_wikilink of t node

(** Meta key to tag wikilink nodes. *)
let meta_key : unit Meta.key = Meta.key ()

(** Parse a fragment string (the part after '#') into a fragment value. *)
let parse_fragment (frag_str : string) : fragment option =
  if String.is_empty frag_str
  then None
  else if String.is_prefix frag_str ~prefix:"^"
  then (
    let candidate = String.drop_prefix frag_str 1 in
    if Block_id.is_valid_block_id candidate
    then Some (Block_ref candidate)
    else (
      (* Treat as heading with literal ^ *)
      let parts =
        String.split frag_str ~on:'#' |> List.filter ~f:(fun s -> not (String.is_empty s))
      in
      if List.is_empty parts then None else Some (Heading parts)))
  else (
    let parts =
      String.split frag_str ~on:'#' |> List.filter ~f:(fun s -> not (String.is_empty s))
    in
    if List.is_empty parts then None else Some (Heading parts))
;;

(** Parse wikilink given the content inside [[]] *)
let make ~(embed : bool) (content : string) : t =
  (* Split on first unescaped | *)
  let ref_part, display =
    match String.lsplit2 content ~on:'|' with
    | Some (r, d) -> String.strip r, Some (String.strip d)
    | None -> String.strip content, None
  in
  (* Split on first # *)
  let target, fragment =
    match String.lsplit2 ref_part ~on:'#' with
    | None ->
      let target = if String.is_empty ref_part then None else Some ref_part in
      target, None
    | Some (t, frag_str) ->
      let target = if String.is_empty t then None else Some t in
      let fragment = parse_fragment frag_str in
      target, fragment
  in
  { target; fragment; display; embed }
;;

(* Find the index of substring [needle] in [haystack] starting at [from]. *)
let find_substring (haystack : string) ~(needle : string) ~(from : int) : int option =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if from + nlen > hlen
  then None
  else (
    let rec loop i =
      if i + nlen > hlen
      then None
      else if String.is_substring_at haystack ~pos:i ~substring:needle
      then Some i
      else loop (i + 1)
    in
    loop from)
;;

let is_escaped s pos = pos > 0 && Char.equal (String.get s (pos - 1)) '\\'

(** Inline mapper that recognises wikilinks inside [Text] nodes.

    Intended for use as the [~inline] callback of [Cmarkit.Mapper.make].
    Scans the text for \[\[…\]\] (and !\[\[…\]\]) delimiters, splitting it into a
    list of plain [Text] and [Ext_wikilink] nodes spliced via [Inlines].
    Returns [Mapper.default] for non-[Text] nodes or when no \[\[…\]\] is present,
    so the mapper falls through to its default behaviour. *)
let inline_map : Inline.t Mapper.mapper = fun (_mapper : Mapper.t) (i : Inline.t) : Inline.t Mapper.result ->
  match i with
  | Inline.Text (text, meta) ->
    let len = String.length text in
    if len < 4 || not (String.is_substring text ~substring:"[[")
    then Mapper.default
    else (
      let parent_loc = Meta.textloc meta in
      (* Compute a sub-textloc for the byte range [off, off+n) within the
         parent text.  Byte positions are shifted relative to the parent's
         first_byte; line positions are approximated using the parent's
         first_line (accurate for the common single-line case). *)
      let sub_meta ~off ~n =
        if Textloc.is_none parent_loc
        then Meta.none
        else (
          let base = Textloc.first_byte parent_loc in
          let first_line = Textloc.first_line parent_loc in
          let loc =
            Textloc.v
              ~file:(Textloc.file parent_loc)
              ~first_byte:(base + off)
              ~last_byte:(base + off + n - 1)
              ~first_line
              ~last_line:first_line
          in
          Meta.make ~textloc:loc ())
      in
      (* Walk text left-to-right, accumulating inlines in reverse order.
         pos is the start of the not-yet-emitted prefix. *)
      let rec loop acc pos =
        match find_substring text ~needle:"[[" ~from:pos with
        | None ->
          (* No more openers — emit any remaining text. *)
          let tail =
            if pos < len
            then
              [ Cmarkit.Inline.Text
                  (String.drop_prefix text pos, sub_meta ~off:pos ~n:(len - pos))
              ]
            else []
          in
          List.rev acc @ tail
        | Some open_pos ->
          (* Check for a preceding unescaped '!' (embed syntax). *)
          let embed, start_pos =
            if
              open_pos > 0
              && Char.equal (String.get text (open_pos - 1)) '!'
              && not (is_escaped text (open_pos - 1))
            then true, open_pos - 1
            else false, open_pos
          in
          (* Skip escaped openers. *)
          if is_escaped text (if embed then open_pos - 1 else open_pos)
          then loop acc (open_pos + 2)
          else (
            let content_start = open_pos + 2 in
            match find_substring text ~needle:"]]" ~from:content_start with
            | None ->
              (* Unmatched opener — treat the rest as plain text. *)
              let tail =
                [ Cmarkit.Inline.Text
                    (String.drop_prefix text pos, sub_meta ~off:pos ~n:(len - pos))
                ]
              in
              List.rev acc @ tail
            | Some close_pos ->
              let content =
                String.sub text ~pos:content_start ~len:(close_pos - content_start)
              in
              let wikilink = make ~embed content in
              (* Wikilink span: from start_pos (incl. '!' for embeds) to close_pos+1 (incl. ']]'). *)
              let wl_span = close_pos + 2 - start_pos in
              let wl_meta = Meta.tag meta_key (sub_meta ~off:start_pos ~n:wl_span) in
              (* Emit any literal text between the previous position and this wikilink. *)
              let before =
                if start_pos > pos
                then
                  [ Cmarkit.Inline.Text
                      ( String.sub text ~pos ~len:(start_pos - pos)
                      , sub_meta ~off:pos ~n:(start_pos - pos) )
                  ]
                else []
              in
              let wl_node = Ext_wikilink (wikilink, wl_meta) in
              let acc = wl_node :: (List.rev before @ acc) in
              loop acc (close_pos + 2))
      in
      let inlines = loop [] 0 in
      Mapper.ret (Inline.Inlines (inlines, meta)))
  | _ -> Mapper.default
;;

(** Render a wikilink to plain text, losing its markdown syntax. Used in rendering
    heading to plain text. *)
let to_plain_text (wl : t) : string =
  match wl.display with
  | Some d -> d
  | None ->
    (match wl.target with
     | Some t -> t
     | None -> "")
;;

let%expect_test "wikilink to plain text" =
  let wl : t =
    { target = Some "foo"
    ; fragment = Some (Heading [ "bar"; "baz" ])
    ; display = Some "quux"
    ; embed = false
    }
  in
  print_endline (to_plain_text wl);
  [%expect {| quux |}]
;;

let%test_module "roundtrip: make (content_of (to_commonmark wl)) = wl" =
  (module struct
    (** Characters that are safe in wikilink target/display/heading strings
        (no delimiters that would break parsing). *)
    let gen_safe_string : string Quickcheck.Generator.t =
      let open Quickcheck.Generator in
      let safe_char =
        Char.gen_print
        |> filter ~f:(fun c ->
          not (Char.equal c '#' || Char.equal c '|' || Char.is_whitespace c))
      in
      Let_syntax.(
        let%bind len = Int.gen_uniform_incl 1 20 in
        String.gen_with_length len safe_char)
      |> filter ~f:(fun s ->
        not
          (String.is_substring s ~substring:"[[" || String.is_substring s ~substring:"]]"))
    ;;

    let gen_block_id : string Quickcheck.Generator.t =
      let open Quickcheck.Generator in
      let alphanum = Char.gen_alphanum in
      let body_char = Quickcheck.Generator.union [ Char.gen_alphanum; return '-' ] in
      Let_syntax.(
        let%bind first = alphanum in
        let%bind rest_len = Int.gen_uniform_incl 0 10 in
        let%map rest = String.gen_with_length rest_len body_char in
        String.of_char first ^ rest)
    ;;

    let gen_fragment : fragment Quickcheck.Generator.t =
      let open Quickcheck.Generator in
      union
        [ Let_syntax.(
            let%map headings =
              List.gen_non_empty
                (gen_safe_string
                 |> filter ~f:(fun s -> not (String.is_prefix s ~prefix:"^")))
            in
            Heading headings)
        ; Let_syntax.(
            let%map id = gen_block_id in
            Block_ref id)
        ]
    ;;

    let quickcheck_generator : t Quickcheck.Generator.t =
      let open Quickcheck.Generator in
      Let_syntax.(
        let%bind target = Option.quickcheck_generator gen_safe_string in
        let%bind fragment = Option.quickcheck_generator gen_fragment in
        let%bind display = Option.quickcheck_generator gen_safe_string in
        let%map embed = Bool.quickcheck_generator in
        { target; fragment; display; embed })
    ;;

    let quickcheck_shrinker : t Quickcheck.Shrinker.t = Base_quickcheck.Shrinker.atomic

    let%quick_test "roundtrip: make (content_of (to_commonmark wl)) = wl" =
      fun (wl : (t[@generator quickcheck_generator])) ->
      let cm = to_commonmark wl in
      (* Strip [[ ]] or ![[ ]] *)
      let content =
        if wl.embed
        then String.drop_prefix (String.drop_suffix cm 2) 3
        else String.drop_prefix (String.drop_suffix cm 2) 2
      in
      let parsed = make ~embed:wl.embed content in
      if not (equal wl parsed)
      then Error.raise_s [%message "roundtrip failed" ~expect:(wl : t) ~got:(parsed : t)]
    ;;
  end)
;;
