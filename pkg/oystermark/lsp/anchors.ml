(** Anchor positions in a note, taken from the parser.

    Features that need anchor positions, such as finding the anchor under the
    cursor, use this module instead of scanning lines for [#], [ ^id] or
    [\{#id\}]. A line scan disagrees with the parser: it finds headings inside
    fenced code blocks, and it cannot find a heading with an authored
    [ \{#id\} ] or a deduplicated identifier such as [heading-1].

    Built on {!Oystermark.Note.Anchor}, the same scan the vault index uses.

    See {!page-"feature-index"}, and {!page-"feature-attribute-anchors"} for
    what an anchor is. *)

open Core

(** {1 Anchors} *)

(** An anchor with its position.

    For a heading, {!address} uses the identifier the parser assigned: an
    authored [ \{#id\} ], or a slug deduplicated with [-1], [-2]. It is not
    recomputed from the heading text.

    [first_line] and [last_line] are 0-based. An attribute anchor starts at its
    [ \{#id\} ] line and ends with the block it applies to; a caret anchor
    spans its whole paragraph. *)
type t =
  { value : Oystermark.Note.Anchor.value
  ; first_line : int
  ; last_line : int (** Inclusive. *)
  ; first_byte : int
  ; last_byte : int (** Exclusive. *)
  }
[@@deriving sexp, equal, compare]

let address (a : t) : Oystermark.Note.Address.t = Oystermark.Note.Anchor.address a.value

(** The line the id is written on, where a rename edits and go-to-definition
    lands: the paragraph's last line for a caret id, the first line otherwise. *)
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

(** Every anchor of [doc] in source order. Anchors without a location are
    dropped. An attribute anchor's location includes its [ \{#id\} ] line. *)
let of_doc (doc : Cmarkit.Doc.t) : t list =
  Oystermark.Note.Anchor.of_doc doc
  |> List.filter_map ~f:(fun (anchor : Oystermark.Note.Anchor.t) ->
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
        | Oystermark.Note.Anchor.Heading _ -> true
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
