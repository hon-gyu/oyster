open Core

(** Whether [u], outside ASCII, is punctuation: Latin-1 punctuation, the
    general punctuation block (curly quotes, dashes, ellipsis), and CJK and
    fullwidth punctuation. *)
let is_unicode_punctuation (u : Stdlib.Uchar.t) : bool =
  let c = Stdlib.Uchar.to_int u in
  (0xA1 <= c && c <= 0xBF && not (c = 0xAA || c = 0xB5 || c = 0xBA))
  || (0x2010 <= c && c <= 0x205E)
  || (0x3000 <= c && c <= 0x303F)
  || (0xFF01 <= c && c <= 0xFF0F)
  || (0xFF1A <= c && c <= 0xFF20)
  || (0xFF3B <= c && c <= 0xFF40)
  || (0xFF5B <= c && c <= 0xFF65)
;;

(** Whether [u], outside ASCII, is a blank: no-break and typographic spaces. *)
let is_unicode_blank (u : Stdlib.Uchar.t) : bool =
  let c = Stdlib.Uchar.to_int u in
  c = 0xA0 || (0x2000 <= c && c <= 0x200A) || c = 0x202F || c = 0x205F || c = 0x3000
;;

(** The key a heading is matched by when it is named as text: by a wikilink
    fragment such as [ [[note#Some Heading]] ], or by a section query. The
    heading's plain text, ASCII letters lowercased, with punctuation other than
    [-] and [_] removed and each run of blanks replaced by one [-]. [Some
    Heading], [some heading] and [some-heading] have the same key, and so do
    [It's] and [It’s]. *)
let heading_key (text : string) : string =
  let b = Buffer.create (String.length text) in
  let pending_blank = ref false in
  let add (u : Stdlib.Uchar.t) =
    if !pending_blank && Buffer.length b > 0 then Buffer.add_char b '-';
    pending_blank := false;
    Stdlib.Buffer.add_utf_8_uchar b u
  in
  let rec loop i =
    if i < String.length text
    then (
      let d = Stdlib.String.get_utf_8_uchar text i in
      let u = Stdlib.Uchar.utf_decode_uchar d in
      if Stdlib.Uchar.to_int u < 128
      then (
        match Stdlib.Uchar.to_char u with
        | ' ' | '\t' | '\n' | '\r' -> pending_blank := true
        | c when Char.is_alphanum c || Char.equal c '-' || Char.equal c '_' ->
          add (Stdlib.Uchar.of_char (Char.lowercase c))
        | _ -> ())
      else if is_unicode_blank u
      then pending_blank := true
      else if not (is_unicode_punctuation u)
      then add u;
      loop (i + Stdlib.Uchar.utf_decode_length d))
  in
  loop 0;
  Buffer.contents b
;;

(** Inlines as plain text, losing their syntax. *)
let plain_text (inlines : Djot.Inline.t Djot.node list) : string =
  Djot.Inline.to_plain_text inlines
;;

let%expect_test "heading_key" =
  List.iter
    [ "Some Heading"
    ; "some-heading"
    ; "  a  b  "
    ; "What's new?"
    ; "What’s “new”?"
    ; "snake_case"
    ; "Ünï"
    ]
    ~f:(fun s -> print_endline (heading_key s));
  [%expect
    {|
    some-heading
    some-heading
    a-b
    whats-new
    whats-new
    snake_case
    Ünï
    |}]
;;
