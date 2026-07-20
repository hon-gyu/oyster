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
    (not !stop) && !pos < len && (not (Char.equal (String.get content !pos) '\n'))
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

(** Convert a [Cmarkit.Textloc.t] to a 0-based [(line, character)] position for
    its first byte.  [character] is a byte offset within the line (correct for
    ASCII; see {!page-"feature-utf16-positions"}).  No content is needed:
    [Cmarkit.Textloc]'s [line_pos] already carries the byte position of the
    start of the line.  A [none] location maps to [(0, 0)].
    See {!page-"feature-go-to-definition".target_position}. *)
let position_of_textloc (tl : Cmarkit.Textloc.t) : int * int =
  if Cmarkit.Textloc.is_none tl
  then 0, 0
  else (
    let line_num, line_start = Cmarkit.Textloc.first_line tl in
    let first_byte = Cmarkit.Textloc.first_byte tl in
    line_num - 1, first_byte - line_start)
;;

(** {1 Parsing} *)

(** Parse [content] into a [Cmarkit.Doc.t] with locations enabled. *)
let parse_doc (content : string) : Cmarkit.Doc.t =
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
    (* Build a doc and pull the textloc of the sole inline attribute anchor,
       exercising the byte-column derivation on real parser output. *)
    let pos_of_inline_attr (content : string) : int * int =
      let doc = Oystermark.Parse.of_string ~locs:true content in
      let found = ref None in
      let folder =
        Cmarkit.Folder.make
          ~inline:(fun _f acc i ->
            match i with
            | Cmarkit.Inline.Ext_attributes (a, meta) ->
              (match Cmarkit.Attribute.id (Cmarkit.Inline.Attributes.attributes a) with
               | Some _ ->
                 found := Some (Cmarkit.Meta.textloc meta);
                 Cmarkit.Folder.ret acc
               | None -> Cmarkit.Folder.default)
            | _ -> Cmarkit.Folder.default)
          ~inline_ext_default:(fun _f acc _i -> acc)
          ~block_ext_default:(fun _f acc _b -> acc)
          ()
      in
      let (_ : unit) = Cmarkit.Folder.fold_doc folder () doc in
      position_of_textloc (Option.value_exn !found)
    ;;

    (* [key]{#k} starts at byte 15 on line 3 (0-based line 2); the line begins
       at byte 11, so the column is 15 - 11 = 4. *)
    let%expect_test "inline anchor column" =
      let line, character = pos_of_inline_attr "# H\n\nThe [key]{#k} span.\n" in
      Printf.printf "%d,%d\n" line character;
      [%expect {| 2,4 |}]
    ;;

    let%test "none location maps to origin" =
      [%equal: int * int] (position_of_textloc Cmarkit.Textloc.none) (0, 0)
    ;;
  end)
;;
