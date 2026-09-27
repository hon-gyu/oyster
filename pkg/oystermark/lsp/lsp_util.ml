(** Shared utilities for oystermark LSP features: position conversion and
    document parsing. *)

open Core

(** {1 Position utilities} *)

(** Position encoding for the [character] component of an LSP position.  See
    {!page-"feature-utf16-positions"}.

    - [Utf16]: UTF-16 code units (LSP mandatory baseline, the default).
    - [Utf32]: Unicode code points.
    - [Utf8]: bytes (identity — the previous ASCII-only behaviour). *)
type encoding =
  | Utf8
  | Utf16
  | Utf32

(** Number of [encoding] code units spanned by [u]. *)
let units_of_uchar (encoding : encoding) (u : Uchar.t) : int =
  match encoding with
  | Utf8 -> Stdlib.Uchar.utf_8_byte_length u
  | Utf16 -> Stdlib.Uchar.utf_16_byte_length u / 2
  | Utf32 -> 1
;;

(** Byte position of the start of 0-based [line] in [content] (or the content
    length if [line] is past the end — matching the previous clamp). *)
let line_start_byte (content : string) ~(line : int) : int =
  let len = String.length content in
  let cur_line = ref 0 in
  let i = ref 0 in
  while !cur_line < line && !i < len do
    if Char.equal (String.get content !i) '\n' then incr cur_line;
    incr i
  done;
  !i
;;

(** Convert a 0-based [(line, character)] position to a byte offset in
    [content].  [character] is a code-unit offset within the line in the given
    [encoding] (default {!Utf16}).  A [character] past the end of the line
    clamps to the line's end (before the newline); a [line] past the end clamps
    to the content length.

    See {!page-"feature-utf16-positions"} and
    {!page-"feature-go-to-definition".edge_cases}. *)
let byte_offset_of_position
      ?(encoding = Utf16)
      (content : string)
      ~(line : int)
      ~(character : int)
  : int
  =
  Trace_core.with_span ~__FILE__ ~__LINE__ "byte_offset_of_position"
  @@ fun _sp ->
  let len = String.length content in
  let pos = ref (line_start_byte content ~line) in
  let units = ref 0 in
  let stop = ref false in
  while
    (not !stop)
    && !pos < len
    && (not (Char.equal (String.get content !pos) '\n'))
    && !units < character
  do
    let dec = Stdlib.String.get_utf_8_uchar content !pos in
    let ulen = units_of_uchar encoding (Stdlib.Uchar.utf_decode_uchar dec) in
    if !units + ulen > character
    then stop := true (* [character] lands inside this code point: stop before it *)
    else (
      units := !units + ulen;
      pos := !pos + Stdlib.Uchar.utf_decode_length dec)
  done;
  Trace_core.add_data_to_span
    _sp
    [ "line", `Int line; "character", `Int character; "offset", `Int !pos ];
  !pos
;;

(** Convert a byte [offset] in [content] to a 0-based [(line, character)]
    position.  [character] is a code-unit offset within the line in the given
    [encoding] (default {!Utf16}).  Clamps to the end of content if [offset] is
    out of range.  See {!page-"feature-utf16-positions"}. *)
let position_of_byte_offset ?(encoding = Utf16) (content : string) (offset : int)
  : int * int
  =
  let len = String.length content in
  let offset = min offset len in
  let line = ref 0 in
  let line_start = ref 0 in
  for i = 0 to offset - 1 do
    if Char.equal (String.get content i) '\n'
    then (
      incr line;
      line_start := i + 1)
  done;
  let units = ref 0 in
  let pos = ref !line_start in
  while !pos < offset do
    let dec = Stdlib.String.get_utf_8_uchar content !pos in
    units := !units + units_of_uchar encoding (Stdlib.Uchar.utf_decode_uchar dec);
    pos := !pos + Stdlib.Uchar.utf_decode_length dec
  done;
  !line, !units
;;

(** Convert a [Djot.Textloc.t] to a 0-based [(line, character)] position for
    its first byte.  [line] comes from the [Textloc] directly.

    [character] depends on [content], which must be the {e same string the
    location refers to} (i.e. the frontmatter-stripped body the parser saw):
    - given, it is a code-unit offset within the line in [encoding] (default
      {!Utf16}), decoded from that line's bytes;
    - omitted, it degrades to a byte offset within the line (correct for
      ASCII), needing no content — [Djot.Textloc.line_pos] carries the
      line's start byte.

    A [none] location maps to [(0, 0)].  See
    {!page-"feature-utf16-positions"} and
    {!page-"feature-go-to-definition".target_position}. *)
let position_of_textloc ?content ?(encoding = Utf16) (tl : Djot.Textloc.t) : int * int =
  if Djot.Textloc.is_none tl
  then 0, 0
  else (
    let line_num, line_start = Djot.Textloc.first_line tl in
    let first_byte = Djot.Textloc.first_byte tl in
    let character =
      match content with
      | Some c
        when line_start >= 0 && line_start <= first_byte && first_byte <= String.length c
        ->
        let pos = ref line_start in
        let units = ref 0 in
        while !pos < first_byte do
          let dec = Stdlib.String.get_utf_8_uchar c !pos in
          units := !units + units_of_uchar encoding (Stdlib.Uchar.utf_decode_uchar dec);
          pos := !pos + Stdlib.Uchar.utf_decode_length dec
        done;
        !units
      | _ -> first_byte - line_start
    in
    line_num - 1, character)
;;

(** {1 Parsing} *)

(** Parse [content] with locations enabled. *)
let parse_doc (content : string) : Oystermark.Parse.t =
  Trace_core.with_span ~__FILE__ ~__LINE__ "parse_doc"
  @@ fun _sp ->
  Trace_core.add_data_to_span _sp [ "content_len", `Int (String.length content) ];
  Oystermark.Parse.of_string ~locs:true content
;;

(* Tests
==================== *)

let%test_module "byte_offset_of_position" =
  (module struct
    let offset = byte_offset_of_position ~encoding:Utf16
    let%test "line 0, char 0" = offset "hello\nworld" ~line:0 ~character:0 = 0
    let%test "line 0, char 3" = offset "hello\nworld" ~line:0 ~character:3 = 3
    let%test "line 1, char 0" = offset "hello\nworld" ~line:1 ~character:0 = 6
    let%test "line 1, char 2" = offset "hello\nworld" ~line:1 ~character:2 = 8
    let%test "past end clamps" = offset "hi" ~line:0 ~character:99 = 2
    let%test "line past end" = offset "hi\n" ~line:5 ~character:0 = 3
  end)
;;

let%test_module "utf-16 position encoding" =
  (module struct
    (* "aébc": a=byte0, é=bytes1-2 (1 UTF-16 unit), b=byte3, c=byte4. *)
    let%test "byte_offset: before multibyte" =
      byte_offset_of_position "aébc" ~line:0 ~character:1 = 1
    ;;

    let%test "byte_offset: after multibyte" =
      byte_offset_of_position "aébc" ~line:0 ~character:2 = 3
    ;;

    let%test "position: after multibyte" =
      [%equal: int * int] (position_of_byte_offset "aébc" 3) (0, 2)
    ;;

    (* "x😀y": 😀 (U+1F600) is 4 UTF-8 bytes and 2 UTF-16 units (surrogate
       pair): x=byte0, 😀=bytes1-4, y=byte5. *)
    let%test "byte_offset: after surrogate pair" =
      byte_offset_of_position "x😀y" ~line:0 ~character:3 = 5
    ;;

    let%test "byte_offset: mid surrogate pair clamps to its start" =
      (* character 2 falls between the two UTF-16 units of the emoji *)
      byte_offset_of_position "x😀y" ~line:0 ~character:2 = 1
    ;;

    let%test "position: after surrogate pair" =
      [%equal: int * int] (position_of_byte_offset "x😀y" 5) (0, 3)
    ;;

    let%test "utf-8 encoding is byte identity" =
      byte_offset_of_position ~encoding:Utf8 "x😀y" ~line:0 ~character:5 = 5
    ;;

    let%test "utf-32 counts code points" =
      [%equal: int * int] (position_of_byte_offset ~encoding:Utf32 "x😀y" 5) (0, 2)
    ;;

    (* Round-trip on a line with mixed multibyte content. *)
    let%test "round-trip" =
      let content = "café 日本 😀 tail" in
      List.for_all [ 0; 1; 4; 5; 7; 8 ] ~f:(fun ch ->
        let b = byte_offset_of_position content ~line:0 ~character:ch in
        [%equal: int * int] (position_of_byte_offset content b) (0, ch))
    ;;
  end)
;;

let%test_module "position_of_textloc" =
  (module struct
    (* The location of the sole inline attribute anchor, exercising the
       byte-column derivation on real parser output. *)
    let inline_attr_loc (content : string) : Djot.Textloc.t =
      let { Oystermark.Parse.doc; _ } = Oystermark.Parse.of_string ~locs:true content in
      List.find_map_exn (Oystermark.Note.Anchor.of_doc doc) ~f:(fun anchor ->
        match anchor.definition with
        | Attr { inline = true; _ } -> Some anchor.loc
        | Attr _ | Heading _ -> None)
    ;;

    let pos_of_inline_attr (content : string) : int * int =
      position_of_textloc (inline_attr_loc content)
    ;;

    (* [key]{#k} starts at byte 15 on line 3 (0-based line 2); the line begins
       at byte 11, so the column is 15 - 11 = 4. *)
    let%expect_test "inline anchor column" =
      let line, character = pos_of_inline_attr "# H\n\nThe [key]{#k} span.\n" in
      Printf.printf "%d,%d\n" line character;
      [%expect {| 2,4 |}]
    ;;

    let%test "none location maps to origin" =
      [%equal: int * int] (position_of_textloc Djot.Textloc.none) (0, 0)
    ;;

    (* With content, the column is UTF-16: two CJK chars (3 bytes each, 1 unit)
       precede the inline anchor, so its byte column (6) becomes UTF-16 column 2. *)
    let pos_of_inline_attr_with_content (content : string) : int * int =
      position_of_textloc ~content (inline_attr_loc content)
    ;;

    let%expect_test "content gives UTF-16 column" =
      (* line 2: "日本[key]{#k} ..." — anchor at byte 6, UTF-16 column 2 *)
      let line, character = pos_of_inline_attr_with_content "# H\n\n日本[key]{#k}.\n" in
      Printf.printf "%d,%d\n" line character;
      [%expect {| 2,2 |}]
    ;;
  end)
;;
