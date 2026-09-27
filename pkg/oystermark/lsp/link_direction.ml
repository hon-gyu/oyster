(** Inlay hints for intra-note links: which way the target is, and how far.

    Spec: {!page-"feature-inlay-hints-link-direction"}.
    Resolution is {!Oystermark.Vault.Index.resolve}, the same operation
    {!Go_to_definition} jumps by, so the arrow and the jump can never
    disagree. *)

open Core

(** {1:implementation Implementation}
    {2 Label}

    The two pure decisions — which glyph, and whether there is a hint at all —
    kept free of the AST so they can be read (and tested) as a table.  See
    {!page-"feature-inlay-hints-link-direction".label}. *)

(** Where the target sits relative to the link, for a target on the {e same}
    line.  [Enclosing] is the link that lies inside its own target's span: it
    would point at itself. *)
type side =
  | Before
  | After
  | Enclosing
[@@deriving sexp, equal, compare]

(** Byte ranges are compared rather than columns: [target_first] before the
    link's own first byte is the whole of "to the left", whatever the encoding
    of what lies between. *)
let side ~(link_first_byte : int) ~(link_last_byte : int) ~(target_first_byte : int)
  : side
  =
  if target_first_byte < link_first_byte
  then Before
  else if target_first_byte > link_last_byte
  then After
  else Enclosing
;;

(** [label ~delta ~side] is what the hint reads, or [None] for the link that
    encloses its own target.  [delta] is [target_line - hint_line]: the arrow
    carries the sign, so the number is the absolute line distance. *)
let label ~(delta : int) ~(side : side) : string option =
  match Ordering.of_int (Int.compare delta 0) with
  | Greater -> Some (sprintf "↓%d" delta)
  | Less -> Some (sprintf "↑%d" (-delta))
  | Equal ->
    (match side with
     | Before -> Some "←"
     | After -> Some "→"
     | Enclosing -> None)
;;

(** {2 Eligible targets}

    See {!page-"feature-inlay-hints-link-direction".eligible}. *)

(** The position a link points {e inside the current note}, if it points at one
    at all.  A cross-note target, an unresolved one, and a whole-note
    self-link ([Curr_file], [Note]) alike have no direction to show. *)
let intra_note_target ~(rel_path : string) (target : Oystermark.Vault.Index.resolution)
  : Djot.Textloc.t option
  =
  let same path = String.equal path rel_path in
  let loc =
    match target with
    | Ok (Oystermark.Vault.Index.Anchor { note_path; anchor }) when same note_path ->
      Some anchor.loc
    | Ok (Anchor _ | Note _ | Asset _) | Error _ -> None
  in
  match loc with
  | Some loc when not (Djot.Textloc.is_none loc) -> Some loc
  | _ -> None
;;

(** {2 End-to-end} *)

(** A single hint: a 0-based UTF-16 position and the glyph to show there. *)
type hint =
  { line : int
  ; character : int
  ; label : string
  }
[@@deriving sexp, equal, compare]

(** Direction hints for the links of [content] (at [rel_path], in a vault with
    [index]) that fall within the line range
    [\[range_start_line, range_end_line)].

    The range filters the {e link}, never the target: a link on screen pointing
    far off it is exactly the case the hint exists for. *)
let hints
      ~(index : Oystermark.Vault.Index.t)
      ~(rel_path : string)
      ~(content : string)
      ~(range_start_line : int)
      ~(range_end_line : int)
      ()
  : hint list
  =
  Trace_core.with_span ~__FILE__ ~__LINE__ "link_direction_hints"
  @@ fun _sp ->
  let doc = Lsp_util.parse_doc content in
  let result =
    Link_collect.collect_links ~index ~rel_path doc
    |> List.filter_map ~f:(fun (l : Link_collect.located_link) ->
      match intra_note_target ~rel_path l.destination with
      | None -> None
      | Some target_loc ->
        (* The hint sits just past the link's last byte, and the delta is
           measured from there: an arrow means what it says at the place it
           appears, even for a link that spans lines. *)
        let line, character =
          Lsp_util.position_of_byte_offset content (l.last_byte + 1)
        in
        if line < range_start_line || line >= range_end_line
        then None
        else (
          let target_first_byte = Djot.Textloc.first_byte target_loc in
          let target_line, _ =
            Lsp_util.position_of_byte_offset content target_first_byte
          in
          let side =
            side
              ~link_first_byte:l.first_byte
              ~link_last_byte:l.last_byte
              ~target_first_byte
          in
          label ~delta:(target_line - line) ~side
          |> Option.map ~f:(fun label -> { line; character; label })))
    |> List.stable_sort ~compare:(fun a b ->
      [%compare: int * int] (a.line, a.character) (b.line, b.character))
  in
  Trace_core.add_data_to_span _sp [ "num_hints", `Int (List.length result) ];
  result
;;

(** {1:test Test} *)

let%test_module "label" =
  (module struct
    (* The label table of
       {!page-"feature-inlay-hints-link-direction".label}, read straight. *)
    let%expect_test "every case" =
      let show ~delta ~side:s =
        printf
          "delta %-4d %-10s -> %s\n"
          delta
          (Sexp.to_string [%sexp (s : side)])
          (Option.value (label ~delta ~side:s) ~default:"<none>")
      in
      show ~delta:12 ~side:After;
      show ~delta:1 ~side:After;
      show ~delta:(-45) ~side:Before;
      show ~delta:(-1) ~side:Before;
      show ~delta:0 ~side:After;
      show ~delta:0 ~side:Before;
      show ~delta:0 ~side:Enclosing;
      [%expect
        {|
        delta 12   After      -> ↓12
        delta 1    After      -> ↓1
        delta -45  Before     -> ↑45
        delta -1   Before     -> ↑1
        delta 0    After      -> →
        delta 0    Before     -> ←
        delta 0    Enclosing  -> <none>
        |}]
    ;;

    (* The sign lives in the arrow; a label never carries a minus. *)
    let%expect_test "distance is absolute" =
      let has_minus d =
        Option.value_map (label ~delta:d ~side:After) ~default:false ~f:(fun s ->
          String.is_substring s ~substring:"-")
      in
      printf "%b %b\n" (has_minus (-45)) (has_minus 45);
      [%expect {| false false |}]
    ;;

    let%expect_test "side is decided by byte order" =
      let show ~target_first_byte =
        printf
          "%d -> %s\n"
          target_first_byte
          (Sexp.to_string
             [%sexp
               (side ~link_first_byte:10 ~link_last_byte:20 ~target_first_byte : side)])
      in
      show ~target_first_byte:3;
      show ~target_first_byte:10;
      show ~target_first_byte:15;
      show ~target_first_byte:20;
      show ~target_first_byte:21;
      [%expect
        {|
        3 -> Before
        10 -> Enclosing
        15 -> Enclosing
        20 -> Enclosing
        21 -> After
        |}]
    ;;
  end)
;;

let%test_module "hints" =
  (module struct
    let show ?(range_start_line = 0) ?(range_end_line = 1000) ~rel_path files =
      let index, _docs = Find_references.For_test.make_vault files in
      let content = List.Assoc.find_exn files ~equal:String.equal rel_path in
      hints ~index ~rel_path ~content ~range_start_line ~range_end_line ()
      |> List.iter ~f:(fun h -> printf "(%d,%d) %s\n" h.line h.character h.label)
    ;;

    (* The map-of-content case: a list of [ [[#…]] ] tokens the arrows turn
       into an ordering.  Lines: 0-2 the list, 4 [# Alpha two], 9 [# Baz]. *)
    let moc =
      [ ( "moc.md"
        , "- [[#alpha-two]]\n\
           - [[#baz]]\n\n\
           # Alpha two\n\n\
           {#para}\n\
           Body\n\n\
           # Baz\n\n\
           Back to [[#alpha-two]] and [[#para]].\n" )
      ]
    ;;

    let%expect_test "forward and backward, with distances" =
      show ~rel_path:"moc.md" moc;
      [%expect
        {|
        (0,16) ↓3
        (1,10) ↓7
        (10,22) ↑7
        (10,36) ↑5
        |}]
    ;;

    (* Two links on one line each get their own hint, beside their own link —
       the whole reason placement is adjacent rather than end-of-line.
       See {!page-"feature-inlay-hints-link-direction".placement}. *)
    let%expect_test "two links on one line" =
      show
        ~rel_path:"two.md"
        [ "two.md", "# Top\n\nSee [[#bottom]] and [[#Top]] here.\n\n# Bottom\n" ];
      [%expect
        {|
        (2,15) ↓2
        (2,28) ↑2
        |}]
    ;;

    (* Same-line targets are reachable only through inline attribute anchors,
       and are the [←]/[→] case: no distance, the arrow says it. *)
    let%expect_test "same line: left and right" =
      show
        ~rel_path:"inline.md"
        [ "inline.md", "# I\n\nSee [[#kt]] then [key]{#kt} then [[#kt]] again.\n" ];
      [%expect
        {|
        (2,11) →
        (2,40) ←
        |}]
    ;;

    (* Everything with no direction to show is silent rather than empty.
       See {!page-"feature-inlay-hints-link-direction".eligible}. *)
    let%expect_test "links with no direction" =
      show
        ~rel_path:"quiet.md"
        [ ( "quiet.md"
          , "# Q\n\n\
             Cross-note [[other]] and [[other#Heading]].\n\
             Unresolved [[nope]] and [[#no-such-heading]].\n\
             Whole-note self [[quiet]].\n\
             Markdown [x](other) and [y](nope).\n" )
        ; "other.md", "# Heading\n\nBody.\n"
        ];
      [%expect {| |}]
    ;;

    (* Every spelling of an intra-note link against every anchor kind.  Markdown
       links and embeds resolve the same way as wikilinks, and the note's own
       name is just the long spelling of [#], so within a line — one anchor
       kind, its four spellings — the four hints must carry the same arrow.  A
       spelling that resolves differently for one anchor kind shows up as the
       odd label out.

       Lines: 0 heading, 1 block attribute, 2 inline attribute; targets on 4, 6,
       9. *)
    let%expect_test "spelling x anchor kind" =
      show
        ~rel_path:"forms.md"
        [ ( "forms.md"
          , "[[#Target]] [[forms#Target]] [t](#Target) ![[#Target]]\n\
             [[#para]] [[forms#para]] [b](#para) ![[#para]]\n\
             [[#kt]] [[forms#kt]] [a](#kt) ![[#kt]]\n\n\
             # Target\n\n\
             {#para}\n\
             Body\n\n\
             The [key]{#kt} term.\n" )
        ];
      [%expect
        {|
        (0,11) ↓4
        (0,28) ↓4
        (0,41) ↓4
        (0,54) ↓4
        (1,9) ↓5
        (1,24) ↓5
        (1,35) ↓5
        (1,46) ↓5
        (2,7) ↓7
        (2,20) ↓7
        (2,29) ↓7
        (2,38) ↓7
        |}]
    ;;

    (* The range filters the link, not the target: the point of the hint is a
       target that is somewhere else. *)
    let%expect_test "range filters the link only" =
      show ~range_start_line:0 ~range_end_line:1 ~rel_path:"moc.md" moc;
      [%expect {| (0,16) ↓3 |}]
    ;;

    (* Positions are UTF-16, so a link preceded by non-ASCII text still gets
       its hint where the link really ends.
       See {!page-"feature-utf16-positions"}. *)
    let%expect_test "hint column is UTF-16" =
      show ~rel_path:"utf.md" [ "utf.md", "# U\n\n日本 [[#U]] tail.\n\n# End\n" ];
      [%expect {| (2,9) ↑2 |}]
    ;;
  end)
;;
