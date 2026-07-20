(** Shared utilities for oystermark LSP features: position conversion and
    document parsing. *)

open Core

(** {1 Position utilities} *)

(** Convert a 0-based (line, character) position to a byte offset in [content].
    [character] is treated as a byte offset within the line (correct for ASCII).

    See {!page-"feature-go-to-definition".edge_cases} for clamping
    behaviour. *)
let byte_offset_of_position (content : string) ~(line : int) ~(character : int) : int =
  Trace_core.with_span ~__FILE__ ~__LINE__ "byte_offset_of_position"
  @@ fun _sp ->
  let len = String.length content in
  let cur_line = ref 0 in
  let i = ref 0 in
  while !cur_line < line && !i < len do
    if Char.equal (String.get content !i) '\n' then incr cur_line;
    incr i
  done;
  let offset = min (!i + character) len in
  Trace_core.add_data_to_span
    _sp
    [ "line", `Int line; "character", `Int character; "offset", `Int offset ];
  offset
;;

(** Convert a byte [offset] in [content] to a 0-based [(line, character)]
    position.  [character] is a byte offset within the line (correct for ASCII).
    Clamps to the end of content if [offset] is out of range. *)
let position_of_byte_offset (content : string) (offset : int) : int * int =
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
  !line, offset - !line_start
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
    let offset = byte_offset_of_position
    let%test "line 0, char 0" = offset "hello\nworld" ~line:0 ~character:0 = 0
    let%test "line 0, char 3" = offset "hello\nworld" ~line:0 ~character:3 = 3
    let%test "line 1, char 0" = offset "hello\nworld" ~line:1 ~character:0 = 6
    let%test "line 1, char 2" = offset "hello\nworld" ~line:1 ~character:2 = 8
    let%test "past end clamps" = offset "hi" ~line:0 ~character:99 = 2
    let%test "line past end" = offset "hi\n" ~line:5 ~character:0 = 3
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
