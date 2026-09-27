(** Anchor positions in a note, taken from the parser.

    Features that need anchor positions, such as finding the anchor under the
    cursor, use this module instead of scanning lines for [#] or [\{#id\}]. A line scan disagrees with the parser: it finds headings inside
    fenced code blocks, and it cannot find a heading with an authored
    [ \{#id\} ] or a deduplicated identifier such as [heading-1].

    Built on {!Oystermark.Note.Anchor}, the same scan the vault index uses.

    See {!page-"feature-index"}, and {!page-"feature-attribute-anchors"} for
    what an anchor is. *)

open Core

(** {1 Anchors} *)

(** An anchor with its position.

    For a heading, {!address} uses the identifier the parser assigned: an
    authored [ \{#id\} ], or one deduplicated with [-1], [-2]. It is not
    recomputed from the heading text.

    [first_line] and [last_line] are 0-based. An attribute anchor spans its
    [ \{#id\} ] and what it applies to. *)
type t =
  { value : Oystermark.Note.Anchor.definition
  ; first_line : int
  ; last_line : int (** Inclusive. *)
  ; first_byte : int
  ; last_byte : int (** Exclusive. *)
  }
[@@deriving sexp, equal, compare]

let address (a : t) : Oystermark.Note.Anchor.Address.t =
  Oystermark.Note.Anchor.address a.value
;;

(** The line the id is written on, where a rename edits and go-to-definition
    lands. *)
let write_line (a : t) : int = a.first_line

let of_loc (loc : Djot.Textloc.t option) : (int * int * int * int) option =
  match loc with
  | Some tl when not (Djot.Textloc.is_none tl) ->
    Some
      ( fst (Djot.Textloc.first_line tl) - 1
      , fst (Djot.Textloc.last_line tl) - 1
      , Djot.Textloc.first_byte tl
      , Djot.Textloc.last_byte tl + 1 )
  | _ -> None
;;

(** Every anchor of [doc] in source order. Anchors without a location are
    dropped. *)
let of_doc (doc : Djot.Doc.t) : t list =
  Oystermark.Note.Anchor.of_doc doc
  |> List.filter_map ~f:(fun (anchor : Oystermark.Note.Anchor.t) ->
    of_loc (Some anchor.loc)
    |> Option.map ~f:(fun (first_line, last_line, first_byte, last_byte) ->
      { value = anchor.definition; first_line; last_line; first_byte; last_byte }))
;;

let of_content (content : string) : t list = of_doc (Lsp_util.parse_doc content).doc

(** {1 Lookup} *)

(** The anchor the cursor is on, [None] when the line holds none.

    Line-granular rather than byte-granular on purpose: "which line are you
    on" is an easier question than asking for the cursor on the id itself. An
    anchor answers anywhere in its own extent — for an attribute that is its
    [ \{#id\} ] and what it attributes, both of which are that anchor.

    Headings come first, then attributes — the order fragments resolve in.
    See {!page-"feature-find-references".activation}. *)
let at_line (ts : t list) ~(line : int) : t option =
  let covers (a : t) = a.first_line <= line && line <= a.last_line in
  let on_line k = List.find ts ~f:(fun a -> covers a && k a.value) in
  List.find_map
    [ (function
        | Oystermark.Note.Anchor.Heading _ -> true
        | Attr _ -> false)
    ; (function
        | Oystermark.Note.Anchor.Attr _ -> true
        | Heading _ -> false)
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

    let%expect_test "headings and attributes together" =
      show "# Alpha\n\nBody\n\nThe [key]{#kt} span.\n";
      [%expect
        {|
        ((value (Heading ((text Alpha) (level 1) (slug Alpha)))) (first_line 0)
         (last_line 0) (first_byte 0) (last_byte 7))
        ((value (Attr (id kt) (inline true))) (first_line 4) (last_line 4)
         (first_byte 19) (last_byte 29))
        |}]
    ;;

    (* The identifier is the parser's, so an authored anchor is findable by
       the id the author wrote — a slug re-derived from the text would be
       [introduction] and match nothing. *)
    let%expect_test "authored heading id" =
      show "{#intro}\n# Introduction\n";
      [%expect
        {|
        ((value (Heading ((text Introduction) (level 1) (slug intro))))
         (first_line 1) (last_line 1) (first_byte 9) (last_byte 23))
        |}]
    ;;

    let%expect_test "duplicate heading texts are deduplicated" =
      show "# Same\n\n# Same\n";
      [%expect
        {|
        ((value (Heading ((text Same) (level 1) (slug Same)))) (first_line 0)
         (last_line 0) (first_byte 0) (last_byte 6))
        ((value (Heading ((text Same) (level 1) (slug Same-1)))) (first_line 2)
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
        ((value (Heading ((text Inside) (level 1) (slug Inside)))) (first_line 1)
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
      show "# Alpha\n\ntext\n" ~line:0;
      [%expect
        {|
        ((value (Heading ((text Alpha) (level 1) (slug Alpha)))) (first_line 0)
         (last_line 0) (first_byte 0) (last_byte 7))
        |}]
    ;;

    let%expect_test "on a block attribute's spec line" =
      show "{#blk}\nA paragraph.\n" ~line:0;
      [%expect
        {|
        ((value (Attr (id blk) (inline false))) (first_line 0) (last_line 1)
         (first_byte 0) (last_byte 19))
        |}]
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
