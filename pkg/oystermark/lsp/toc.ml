(** Table of contents: finding the regions, generating their content, and the
    staleness check.

    Everything here is a function of the document text alone — no vault, no
    index — because a TOC describes the buffer in front of the writer. The
    server turns the results into diagnostics and code actions.

    A region is found by the parser, never by reading the text a second time:
    {!scan_doc} matches {!Cmarkit.Block.Ext_div} on the same AST {!entries}
    takes its headings from. Recognizing fences here instead would be a second
    implementation of div syntax, free to drift from the one that decides what
    the note actually {e is} — and it did, in the first version of this file:
    a [::: toc] inside a code block was a region, and a nested one closed the
    wrong fence.

    Spec: {!page-"feature-toc"}. *)

open Core

(** {1:fences Fences}

    A region is a {e div} — the vault's own fenced-div extension, not raw
    HTML — with the class {!div_class}.  See {!page-"feature-toc".region}. *)

let div_class = "toc"
let close_fence = ":::"

(** The opening line a generated region carries. *)
let open_line = ":::" ^ " " ^ div_class

(** {1:regions Regions} *)

(** A source span: a 0-based line number and the byte range it covers. *)
type span =
  { line : int
  ; first_byte : int
  ; last_byte : int
  }
[@@deriving sexp, equal, compare]

(** A well-formed region: a [::: toc] div the parser closed with a fence.

    [first_byte]/[last_byte] span the whole div, fence lines included;
    [body_first_byte]/[body_last_byte] span only what an update rewrites: from
    the start of the line after the opening fence to the start of the closing
    fence's line. The body is empty (the two coincide) when the fences sit on
    consecutive lines. *)
type region =
  { start_line : int
  ; end_line : int
  ; first_byte : int
  ; last_byte : int
  ; open_fence : span (** The [::: toc] fence itself, class included. *)
  ; body_first_byte : int
  ; body_last_byte : int
  }
[@@deriving sexp, equal, compare]

(** What a pass over the document's divs found.

    There is no {e stray closing fence} case, unlike a paired-comment scheme:
    a lone [:::] closes some other div and is none of this feature's
    business.  A [::: toc] the parser closed at the end of the document
    instead — {!Cmarkit.Block.Div.closing_fence} is [None] — is kept: a region
    that swallows the rest of the file is not one to rewrite silently. *)
type scan =
  { regions : region list
  ; unterminated : span list (** Opening fences the document's end closed. *)
  }
[@@deriving sexp, equal, compare]

(** The div's opening fence and class as one span — what a diagnostic points
    at, and what tells the body which line to start on. *)
let open_fence_span (d : Cmarkit.Block.Div.t) : span option =
  let textloc node = Cmarkit.Meta.textloc (snd node) in
  let fence = textloc (Cmarkit.Block.Div.opening_fence d) in
  match Cmarkit.Block.Div.class' d with
  | None -> None
  | Some class' ->
    let class' = textloc class' in
    if Cmarkit.Textloc.is_none fence || Cmarkit.Textloc.is_none class'
    then None
    else
      Some
        { line = fst (Cmarkit.Textloc.first_line fence) - 1
        ; first_byte = Cmarkit.Textloc.first_byte fence
        ; last_byte = Cmarkit.Textloc.last_byte class' + 1
        }
;;

(** Whether [d] is a region: a div whose class is exactly {!div_class}.  The
    class comes from the parser, so [::::toc], an indented fence and a fence
    nested in a list item are all recognized, and [::: toc] written inside a
    code block is not a div at all. *)
let is_toc (d : Cmarkit.Block.Div.t) : bool =
  match Cmarkit.Block.Div.class' d with
  | Some (name, _) -> String.equal name div_class
  | None -> false
;;

(** The [::: toc] divs of [doc], in document order.

    Recognition is the parser's, not a second reading of the text: the same
    fold the rest of the vault uses, matching {!Cmarkit.Block.Ext_div}.  A
    [::: toc] nested inside another one is body text rather than a region of
    its own — the fold stops at the outer div, exactly as the parser nests
    them.  See {!page-"feature-toc".region}. *)
let scan_doc (doc : Cmarkit.Doc.t) ~(content : string) : scan =
  let folder =
    Cmarkit.Folder.make
      ~block:(fun _f (regions, unterminated) (b : Cmarkit.Block.t) ->
        match b with
        | Cmarkit.Block.Ext_div (d, meta) when is_toc d ->
          let div = Cmarkit.Meta.textloc meta in
          (match open_fence_span d, Cmarkit.Textloc.is_none div with
           | None, _ | _, true -> Cmarkit.Folder.ret (regions, unterminated)
           | Some open_fence, false ->
             (* Stop here either way: a [::: toc] inside this one is content. *)
             Cmarkit.Folder.ret
               (match Cmarkit.Block.Div.closing_fence d with
                | None -> regions, open_fence :: unterminated
                | Some closing ->
                  let closing = Cmarkit.Meta.textloc (snd closing) in
                  let end_line, close_line_start = Cmarkit.Textloc.first_line closing in
                  let region =
                    { start_line = open_fence.line
                    ; end_line = end_line - 1
                    ; first_byte = Cmarkit.Textloc.first_byte div
                    ; last_byte = Cmarkit.Textloc.last_byte div + 1
                    ; open_fence
                    ; body_first_byte =
                        Lsp_util.line_start_byte content ~line:(open_fence.line + 1)
                    ; body_last_byte = close_line_start
                    }
                  in
                  region :: regions, unterminated))
        | _ -> Cmarkit.Folder.default)
      ~inline:(fun _f acc _i -> Cmarkit.Folder.ret acc)
      ~inline_ext_default:(fun _f acc _i -> acc)
      ~block_ext_default:(fun _f acc _b -> acc)
      ()
  in
  let regions, unterminated = Cmarkit.Folder.fold_doc folder ([], []) doc in
  { regions = List.rev regions; unterminated = List.rev unterminated }
;;

let body (content : string) (r : region) : string =
  String.sub content ~pos:r.body_first_byte ~len:(r.body_last_byte - r.body_first_byte)
;;

(** {1:generation Generation} *)

(** One heading, reduced to what a TOC entry needs. *)
type entry =
  { text : string
  ; level : int
  ; slug : string
  }
[@@deriving sexp, equal, compare]

(** Backslash-escape what would otherwise close the link text early. *)
let escape_link_text (s : string) : string =
  String.concat_map s ~f:(fun c ->
    match c with
    | '[' | ']' | '\\' -> String.of_char_list [ '\\'; c ]
    | c -> String.of_char c)
;;

(** The headings of [content], minus the ones a TOC should not describe:
    empty ones, and any that fall inside a region — a TOC that sits under a
    heading must not list itself.  See {!page-"feature-toc".generation}. *)
let entries ~(doc : Cmarkit.Doc.t) ~(regions : region list) : entry list =
  let inside first_byte =
    List.exists regions ~f:(fun r ->
      r.body_first_byte <= first_byte && first_byte < r.body_last_byte)
  in
  let module Index = Oystermark.Vault.Index in
  let file_stat : Index.file_stat =
    { rel_path = "__toc__.md"; birthtime = None; mtime = None }
  in
  Index.Entry.of_doc_exn file_stat doc
  |> Index.Entry.headings
  |> List.filter_map ~f:(fun (h, loc) ->
    let located_inside = inside (Cmarkit.Textloc.first_byte loc) in
    if located_inside || String.is_empty (String.strip h.text)
    then None
    else Some { text = h.text; level = h.level; slug = h.slug })
;;

(** The Markdown list, newline-terminated, or [""] when there is nothing to
    list.  Indentation counts the {e distinct levels present}, so a note whose
    headings start at [##] is flush left and a skipped level costs no
    indentation.  See {!page-"feature-toc".generation}. *)
let render (entries : entry list) : string =
  match entries with
  | [] -> ""
  | _ :: _ ->
    let levels =
      List.map entries ~f:(fun e -> e.level) |> List.dedup_and_sort ~compare:Int.compare
    in
    let depth level =
      List.findi levels ~f:(fun _ l -> Int.equal l level)
      |> Option.value_map ~default:0 ~f:fst
    in
    List.map entries ~f:(fun e ->
      sprintf
        "%s- [%s](#%s)"
        (String.make (2 * depth e.level) ' ')
        (escape_link_text e.text)
        e.slug)
    |> String.concat ~sep:"\n"
    |> fun s -> s ^ "\n"
;;

(** What one parse of [content] yields: the document, its regions, and the
    TOC they should all hold — every region in a document holds the same one,
    since it is a function of the headings, not of where the region sits.

    The four entry points below each parse exactly once, and the parse is the
    only reading of the text: {!scan_doc} takes its divs from the same AST
    that {!entries} takes its headings from. *)
let read (content : string) : scan * string =
  (* [layout:true] is what gives the fences their locations: without it the
     div is still a div, but its delimiters carry none.  See
     {!page-"feature-toc".region}. *)
  let doc = Lsp_util.parse_doc ~layout:true content in
  let s = scan_doc doc ~content in
  s, render (entries ~doc ~regions:s.regions)
;;

let generate (content : string) : string = snd (read content)

(** {1:staleness Staleness} *)

(** Strip trailing whitespace per line and drop leading and trailing blank
    lines — the only differences a stale check forgives.  See
    {!page-"feature-toc".staleness}. *)
let normalize (s : string) : string =
  let trim_front = List.drop_while ~f:String.is_empty in
  String.split_lines s
  |> List.map ~f:String.rstrip
  |> trim_front
  |> List.rev
  |> trim_front
  |> List.rev
  |> String.concat ~sep:"\n"
;;

let is_stale ~(generated : string) ~(body : string) : bool =
  not (String.equal (normalize generated) (normalize body))
;;

(** {1:diagnostics Diagnostics} *)

type diagnostic =
  { first_byte : int
  ; last_byte : int
  ; message : string
  }
[@@deriving sexp, equal, compare]

let diagnostics (content : string) : diagnostic list =
  let s, generated = read content in
  match s.regions, s.unterminated with
  | [], [] -> []
  | _ ->
    let stale =
      List.filter_map s.regions ~f:(fun r ->
        if is_stale ~generated ~body:(body content r)
        then
          Some
            { first_byte = r.open_fence.first_byte
            ; last_byte = r.open_fence.last_byte
            ; message = "table of contents is out of date"
            }
        else None)
    in
    let unterminated =
      List.map s.unterminated ~f:(fun (m : span) ->
        { first_byte = m.first_byte
        ; last_byte = m.last_byte
        ; message =
            "unterminated table of contents: missing a closing " ^ close_fence ^ " fence"
        })
    in
    List.sort (stale @ unterminated) ~compare:(fun a b ->
      match Int.compare a.first_byte b.first_byte with
      | 0 -> Int.compare a.last_byte b.last_byte
      | c -> c)
;;

(** {1:edits Edits} *)

(** A replacement of a byte span, in the same shape {!Rename} uses, so the
    test harness can apply it. *)
type edit =
  { first_byte : int
  ; last_byte : int
  ; new_text : string
  }
[@@deriving sexp, equal, compare]

(** The region containing [line], fence lines included. *)
let region_at (s : scan) ~(line : int) : region option =
  List.find s.regions ~f:(fun r -> r.start_line <= line && line <= r.end_line)
;;

(** The edit that inserts a fresh region at the start of [line], or [None]
    when [line] is already inside one.  See {!page-"feature-toc".actions}. *)
let insertion (content : string) ~(line : int) : edit option =
  let s, generated = read content in
  match region_at s ~line with
  | Some _ -> None
  | None ->
    let at = Lsp_util.line_start_byte content ~line in
    Some
      { first_byte = at
      ; last_byte = at
      ; new_text = String.concat [ open_line; "\n"; generated; close_fence; "\n" ]
      }
;;

(** The edit that rewrites the body of the first {e stale} region the byte
    range touches.  Fence lines are outside the replaced span, so an update
    cannot lose the region.  See {!page-"feature-toc".actions}. *)
let update (content : string) ~(first_byte : int) ~(last_byte : int) : edit option =
  let s, generated = read content in
  match s.regions with
  | [] -> None
  | _ :: _ ->
    List.find s.regions ~f:(fun r ->
      r.first_byte <= last_byte
      && first_byte <= r.last_byte
      && is_stale ~generated ~body:(body content r))
    |> Option.map ~f:(fun r ->
      { first_byte = r.body_first_byte
      ; last_byte = r.body_last_byte
      ; new_text = generated
      })
;;

(* Tests
   ======

   Spec: {!page-"feature-toc"}. *)

let%test_module "scan" =
  (module struct
    let show content = print_s [%sexp (fst (read content) : scan)]

    let%expect_test "no toc div" =
      show "# H\n\nbody\n";
      [%expect {| ((regions ()) (unterminated ())) |}]
    ;;

    let%expect_test "well-formed region" =
      show "::: toc\n- [H](#h)\n:::\n";
      [%expect
        {|
        ((regions
          (((start_line 0) (end_line 2) (first_byte 0) (last_byte 21)
            (open_fence ((line 0) (first_byte 0) (last_byte 7))) (body_first_byte 8)
            (body_last_byte 18))))
         (unterminated ()))
        |}]
    ;;

    let%expect_test "empty body: fences on consecutive lines" =
      show "::: toc\n:::\n";
      [%expect
        {|
        ((regions
          (((start_line 0) (end_line 1) (first_byte 0) (last_byte 11)
            (open_fence ((line 0) (first_byte 0) (last_byte 7))) (body_first_byte 8)
            (body_last_byte 8))))
         (unterminated ()))
        |}]
    ;;

    let%expect_test "surrounding whitespace on a fence line" =
      show "  ::: toc  \n  :::\n";
      [%expect
        {|
        ((regions
          (((start_line 0) (end_line 1) (first_byte 2) (last_byte 17)
            (open_fence ((line 0) (first_byte 2) (last_byte 9))) (body_first_byte 12)
            (body_last_byte 12))))
         (unterminated ()))
        |}]
    ;;

    let%expect_test "unterminated opening fence" =
      show "::: toc\n- [H](#h)\n";
      [%expect
        {| ((regions ()) (unterminated (((line 0) (first_byte 0) (last_byte 7))))) |}]
    ;;

    (* A lone closing fence belongs to some other div: this feature says
       nothing about it.  See {!page-"feature-toc".region}. *)
    let%expect_test "a closing fence with no toc div above is not ours" =
      show "::: warning\ncontent\n:::\n";
      [%expect {| ((regions ()) (unterminated ())) |}]
    ;;

    (* Unlike a paired-comment scheme, a nested opening fence is content: the
       parser reads it as a nested div, and the first sufficient closing fence
       ends the outer one. *)
    let%expect_test "a nested opening fence does not start a region" =
      show "::: toc\n::: toc\n:::\n";
      [%expect
        {| ((regions ()) (unterminated (((line 0) (first_byte 0) (last_byte 7))))) |}]
    ;;

    let%expect_test "a shorter closing fence does not close a longer one" =
      show ":::: toc\n:::\n::::\n";
      [%expect
        {| ((regions ()) (unterminated (((line 0) (first_byte 0) (last_byte 8))))) |}]
    ;;

    let%expect_test "two independent regions" =
      show "::: toc\n:::\nmid\n::: toc\n:::\n";
      [%expect
        {|
        ((regions
          (((start_line 0) (end_line 1) (first_byte 0) (last_byte 11)
            (open_fence ((line 0) (first_byte 0) (last_byte 7))) (body_first_byte 8)
            (body_last_byte 8))
           ((start_line 3) (end_line 4) (first_byte 16) (last_byte 27)
            (open_fence ((line 3) (first_byte 16) (last_byte 23)))
            (body_first_byte 24) (body_last_byte 24))))
         (unterminated ()))
        |}]
    ;;

    (* Scanning does not parse, so a fence inside a code block still
       delimits.  See {!page-"feature-toc".region}. *)
    let%expect_test "a fence inside a code block is still a fence" =
      show "```\n::: toc\n```\n:::\n";
      [%expect {| ((regions ()) (unterminated ())) |}]
    ;;
  end)
;;

let%test_module "generate" =
  (module struct
    let show content = print_string (generate content)

    let%expect_test "nesting" =
      show "# Alpha\n\n## Beta\n\n### Gamma\n\n## Delta\n";
      [%expect
        {|
        - [Alpha](#alpha)
          - [Beta](#beta)
            - [Gamma](#gamma)
          - [Delta](#delta)
        |}]
    ;;

    let%expect_test "document starting at level 2 is flush left" =
      show "## Beta\n\n### Gamma\n";
      [%expect
        {|
        - [Beta](#beta)
          - [Gamma](#gamma)
        |}]
    ;;

    let%expect_test "a skipped level costs one step, not two" =
      show "## Beta\n\n#### Delta\n";
      [%expect
        {|
        - [Beta](#beta)
          - [Delta](#delta)
        |}]
    ;;

    let%expect_test "duplicate heading texts get deduped slugs" =
      show "# Same\n\n# Same\n";
      [%expect
        {|
        - [Same](#same)
        - [Same](#same-1)
        |}]
    ;;

    (* An authored [ {#id} ] wins over the derived slug, so the TOC links to
       the identifier the author will keep. *)
    let%expect_test "explicit attribute id" =
      show "{#intro}\n# Introduction\n";
      [%expect {| - [Introduction](#intro) |}]
    ;;

    let%expect_test "brackets in a heading are escaped" =
      show "# See [note]\n";
      [%expect {| - [See \[note\]](#see-note) |}]
    ;;

    let%expect_test "no headings: empty body" =
      show "just prose\n";
      [%expect {| |}]
    ;;

    (* A div's body is ordinary block content, so a heading written inside a
       region really is a heading — and would otherwise be listed by the very
       TOC it sits in. *)
    let%expect_test "headings inside a region are not listed" =
      show "# Alpha\n\n::: toc\n\n# Inside\n\n:::\n\n## Beta\n";
      [%expect
        {|
        - [Alpha](#alpha)
          - [Beta](#beta)
        |}]
    ;;
  end)
;;

let%test_module "staleness" =
  (module struct
    let stale content =
      let s, generated = read content in
      List.map s.regions ~f:(fun r -> is_stale ~generated ~body:(body content r))
      |> List.iter ~f:(printf "%b\n")
    ;;

    let region body = sprintf "# Alpha\n\n::: toc\n%s:::\n" body

    let%expect_test "matching body is fresh" =
      stale (region "- [Alpha](#alpha)\n");
      [%expect {| false |}]
    ;;

    let%expect_test "trailing whitespace and blank lines are forgiven" =
      stale (region "\n- [Alpha](#alpha)   \n\n");
      [%expect {| false |}]
    ;;

    let%expect_test "edited link text is stale" =
      stale (region "- [Renamed](#alpha)\n");
      [%expect {| true |}]
    ;;

    let%expect_test "missing heading is stale" =
      stale "# Alpha\n\n::: toc\n- [Alpha](#alpha)\n:::\n\n## Beta\n";
      [%expect {| true |}]
    ;;

    let%expect_test "empty region in a note without headings is fresh" =
      stale "::: toc\n:::\n";
      [%expect {| false |}]
    ;;
  end)
;;

let%test_module "diagnostics" =
  (module struct
    let show content =
      List.iter (diagnostics content) ~f:(fun d -> print_s [%sexp (d : diagnostic)])
    ;;

    let%expect_test "no toc div: nothing" =
      show "# Alpha\n";
      [%expect {| |}]
    ;;

    let%expect_test "fresh region: nothing" =
      show "# Alpha\n\n::: toc\n- [Alpha](#alpha)\n:::\n";
      [%expect {| |}]
    ;;

    let%expect_test "stale region is reported on the opening fence line" =
      show "# Alpha\n\n::: toc\n:::\n";
      [%expect
        {| ((first_byte 9) (last_byte 16) (message "table of contents is out of date")) |}]
    ;;

    let%expect_test "unterminated opening fence" =
      show "::: toc\ntext\n";
      [%expect
        {|
        ((first_byte 0) (last_byte 7)
         (message "unterminated table of contents: missing a closing ::: fence"))
        |}]
    ;;

    (* A div of another class is not this feature's business, closing fence
       and all. *)
    let%expect_test "another div: nothing" =
      show "::: warning\ntext\n:::\n";
      [%expect {| |}]
    ;;
  end)
;;

let%test_module "edits" =
  (module struct
    let apply (content : string) (e : edit) : string =
      String.sub content ~pos:0 ~len:e.first_byte
      ^ e.new_text
      ^ String.subo content ~pos:e.last_byte
    ;;

    let show (content : string) (e : edit option) =
      match e with
      | None -> print_endline "<none>"
      | Some e -> print_string (apply content e)
    ;;

    let%expect_test "insert at the cursor's line" =
      let content = "# Alpha\n\nprose\n\n## Beta\n" in
      show content (insertion content ~line:2);
      [%expect
        {|
        # Alpha

        ::: toc
        - [Alpha](#alpha)
          - [Beta](#beta)
        :::
        prose

        ## Beta
        |}]
    ;;

    let%expect_test "insert in a note without headings leaves an empty region" =
      let content = "prose\n" in
      show content (insertion content ~line:0);
      [%expect
        {|
        ::: toc
        :::
        prose
        |}]
    ;;

    let%expect_test "no insertion inside a region" =
      let content = "::: toc\n- [x](#x)\n:::\n" in
      List.iter [ 0; 1; 2 ] ~f:(fun line -> show content (insertion content ~line));
      [%expect
        {|
        <none>
        <none>
        <none>
        |}]
    ;;

    let%expect_test "update rewrites the body and keeps the fences" =
      let content = "# Alpha\n\n::: toc\nstale\n:::\n\n## Beta\n" in
      show content (update content ~first_byte:9 ~last_byte:9);
      [%expect
        {|
        # Alpha

        ::: toc
        - [Alpha](#alpha)
          - [Beta](#beta)
        :::

        ## Beta
        |}]
    ;;

    let%expect_test "a fresh region has no update" =
      let content = "# Alpha\n\n::: toc\n- [Alpha](#alpha)\n:::\n" in
      show content (update content ~first_byte:9 ~last_byte:9);
      [%expect {| <none> |}]
    ;;

    let%expect_test "a range outside every region has no update" =
      let content = "# Alpha\n\n::: toc\nstale\n:::\n" in
      show content (update content ~first_byte:0 ~last_byte:3);
      [%expect {| <none> |}]
    ;;
  end)
;;
