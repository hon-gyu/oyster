(** Shared utilities for oystermark LSP features: position conversion and
    document parsing. *)

open Core

(** {1 Position utilities} *)

(** Convert a 0-based (line, character) position to a byte offset in [content].
    [character] is treated as a byte offset within the line (correct for ASCII).

    See {!page-"feature-go-to-definition"} §{e Edge cases} for clamping
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
