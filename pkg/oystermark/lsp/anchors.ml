(** Where a note's anchors are, according to the parser.

    Four features need the same thing — {i is there a heading with this slug,
    and where does its section end?}, {i what anchor is the cursor sitting
    on?} — and each used to answer it by reading lines: count the leading
    [#]s, slugify what follows, look for a trailing [ ^id], match a [\{#id\}]
    line. That is a second implementation of Markdown, and it drifts from the
    one that decides what the note actually is: it finds headings inside
    fenced code blocks, and it derives a slug from the heading's text, so a
    heading carrying an authored [ \{#id\} ] or a parser-deduplicated
    [heading-1] cannot be found at all.

    This module is the single parser-based answer, built on the same
    extractors the vault index uses. Nothing here inspects a character of
    Markdown syntax.

    See {!page-"feature-index"}, and {!page-"feature-attribute-anchors"} for
    what an anchor {e is}. *)

open Core

(** {1 Anchors} *)

(** One located anchor.

    Its {!address} is what a [#fragment] must name to reach it: for a heading
    that is the identifier {e the parser assigned} — an authored [ \{#id\} ]
    included, and deduplicated with [-1], [-2] — not a slug re-derived from the
    text.

    [first_line] and [last_line] are the anchor's own extent, 0-based. An
    attribute anchor starts at the [ \{#id\} ] line and runs through the block
    it attributes; a caret id spans its whole paragraph. *)
type t =
  { value : Oystermark.Extract.Anchor.value
  ; first_line : int
  ; last_line : int (** Inclusive. *)
  ; first_byte : int
  ; last_byte : int (** Exclusive. *)
  }
[@@deriving sexp, equal, compare]

let address (a : t) : Oystermark.Extract.Address.t =
  Oystermark.Extract.Anchor.address a.value
;;

(** The line the id is {e written} on — where a rename edits and a definition
    jump should land. For a caret id that is the paragraph's last line, since
    the [ ^id] closes it; for the others it is where the anchor starts. *)
let write_line (a : t) : int =
  match a.value with
  | Heading _ | Attr _ -> a.first_line
  | Caret _ -> a.last_line
;;

let of_loc (loc : Cmarkit.Textloc.t option) : (int * int * int * int) option =
  match loc with
  | Some tl when not (Cmarkit.Textloc.is_none tl) ->
    Some
      ( fst (Cmarkit.Textloc.first_line tl) - 1
      , fst (Cmarkit.Textloc.last_line tl) - 1
      , Cmarkit.Textloc.first_byte tl
      , Cmarkit.Textloc.last_byte tl + 1 )
  | _ -> None
;;

(** Every anchor of [doc], in source order.  Anchors the parser could not
    locate are dropped: without a position there is nothing to answer with.
    An attribute anchor's location covers its [ \{#id\} ] line as well as the
    block it attributes: the parser spans the specifier, so nothing here has
    to guess where it was written. *)
let of_doc (doc : Cmarkit.Doc.t) : t list =
  Oystermark.Extract.Anchor.of_doc doc
  |> List.filter_map ~f:(fun (anchor : Oystermark.Extract.Anchor.t) ->
    of_loc (Some anchor.loc)
    |> Option.map ~f:(fun (first_line, last_line, first_byte, last_byte) ->
      { value = anchor.value; first_line; last_line; first_byte; last_byte }))
;;

let of_content (content : string) : t list = of_doc (Lsp_util.parse_doc content)

(** {1 Lookup} *)

(** The anchor the cursor is on, [None] when the line holds none.

    Line-granular rather than byte-granular on purpose: asking the reader to
    put the cursor exactly on a [ ^id] would be a worse question than "which
    line are you on".  A heading and an attribute answer anywhere in their own
    extent — for an attribute that is its [ \{#id\} ] line and the block it
    attributes, both of which are that anchor.  A caret id answers only on the
    line it is written on: its paragraph is ordinary prose that happens to end
    with a marker, not an anchor throughout.

    Headings come first, then caret ids, then attributes — the order fragments
    resolve in.  See {!page-"feature-find-references".activation}. *)
let at_line (ts : t list) ~(line : int) : t option =
  let covers (a : t) =
    match a.value with
    | Heading _ | Attr _ -> a.first_line <= line && line <= a.last_line
    | Caret _ -> a.last_line = line
  in
  let on_line k = List.find ts ~f:(fun a -> covers a && k a.value) in
  List.find_map
    [ (function
        | Oystermark.Extract.Anchor.Heading _ -> true
        | _ -> false)
    ; (function
        | Caret _ -> true
        | _ -> false)
    ; (function
        | Attr _ -> true
        | _ -> false)
    ]
    ~f:on_line
;;

(* Tests
   ======

   Recognition is the parser's, so what is worth pinning is the cases a
   line-based reading gets wrong. *)

let%test_module "of_content" =
  (module struct
    let show content =
      List.iter (of_content content) ~f:(fun a -> print_s [%sexp (a : t)])
    ;;

    let%expect_test "headings, caret ids and attributes together" =
      show "# Alpha\n\nBody ^b1\n\nThe [key]{#kt} span.\n";
      [%expect
        {|
        ((value (Heading ((text Alpha) (level 1) (slug alpha)))) (first_line 0)
         (last_line 0) (first_byte 0) (last_byte 7))
        ((value (Caret b1)) (first_line 2) (last_line 2) (first_byte 9)
         (last_byte 17))
        ((value (Attr (id kt) (inline true))) (first_line 4) (last_line 4)
         (first_byte 23) (last_byte 33))
        |}]
    ;;

    (* The identifier is the parser's, so an authored anchor is findable by
       the id the author wrote — a slug re-derived from the text would be
       [introduction] and match nothing. *)
    let%expect_test "authored heading id" =
      show "{#intro}\n# Introduction\n";
      [%expect
        {|
        ((value (Attr (id intro) (inline false))) (first_line 0) (last_line 1)
         (first_byte 0) (last_byte 23))
        ((value (Heading ((text Introduction) (level 1) (slug intro))))
         (first_line 1) (last_line 1) (first_byte 9) (last_byte 23))
        |}]
    ;;

    let%expect_test "duplicate heading texts are deduplicated" =
      show "# Same\n\n# Same\n";
      [%expect
        {|
        ((value (Heading ((text Same) (level 1) (slug same)))) (first_line 0)
         (last_line 0) (first_byte 0) (last_byte 6))
        ((value (Heading ((text Same) (level 1) (slug same-1)))) (first_line 2)
         (last_line 2) (first_byte 8) (last_byte 14))
        |}]
    ;;

    let%expect_test "a heading inside a code block is not a heading" =
      show "```\n# Not a heading\n```\n";
      [%expect {| |}]
    ;;

    let%expect_test "a heading inside a div is one" =
      show "::: warning\n# Inside\n:::\n";
      [%expect
        {|
        ((value (Heading ((text Inside) (level 1) (slug inside)))) (first_line 1)
         (last_line 1) (first_byte 12) (last_byte 20))
        |}]
    ;;
  end)
;;

let%test_module "at_line" =
  (module struct
    let show content ~line =
      match at_line (of_content content) ~line with
      | None -> print_endline "<none>"
      | Some a -> print_s [%sexp (a : t)]
    ;;

    let%expect_test "on a heading" =
      show "# Alpha\n\ntext ^b\n" ~line:0;
      [%expect
        {|
        ((value (Heading ((text Alpha) (level 1) (slug alpha)))) (first_line 0)
         (last_line 0) (first_byte 0) (last_byte 7))
        |}]
    ;;

    (* The caret is on the paragraph's last line, which is where the reader
       sees it — not on the line the paragraph started. *)
    let%expect_test "on the caret line of a multi-line paragraph" =
      show "one\ntwo ^b\n" ~line:1;
      [%expect
        {|
        ((value (Caret b)) (first_line 0) (last_line 1) (first_byte 0)
         (last_byte 10))
        |}]
    ;;

    let%expect_test "on the first line of that paragraph" =
      show "one\ntwo ^b\n" ~line:0;
      [%expect {| <none> |}]
    ;;

    let%expect_test "on an inline attribute's line" =
      show "The [key]{#kt} span.\n" ~line:0;
      [%expect
        {|
        ((value (Attr (id kt) (inline true))) (first_line 0) (last_line 0)
         (first_byte 4) (last_byte 14))
        |}]
    ;;

    let%expect_test "on a plain line" =
      show "# Alpha\n\njust prose\n" ~line:2;
      [%expect {| <none> |}]
    ;;
  end)
;;
