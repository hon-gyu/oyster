(** How many links land on a note and on each of its headings, and where that
    number belongs.

    Rendered as a lens above the line: see
    {!page-"feature-codelens-reference-counts"}.  Counting over the vault is
    {!Find_references}'s; what is here is which lines are worth counting for,
    and what the number is called once counted. *)

open Core

(** {1:implementation Implementation} *)

(** What a count is about: the note as a whole, or one heading in it. *)
type target =
  | File
  | Heading of { slug : string }
[@@deriving sexp, equal, compare]

(** What points at one line, and where the number goes.  [end_character] is
    the byte length of the line, where an inlay hint sits; a lens uses the line
    alone.

    The references are carried, not just counted: a lens hands them to the
    client so that clicking it can show them, and re-scanning the vault to
    answer a click the count already scanned for would be a second answer that
    could disagree with the first.  See
    {!page-"feature-codelens-reference-counts".click}.

    [rel_path] is the note the count is {e for}, which is what tells a
    reference in it apart from one from elsewhere.  See
    {!page-"feature-codelens-reference-counts".self}. *)
type entry =
  { line : int
  ; end_character : int
  ; rel_path : string
  ; refs : Find_references.reference list
  ; target : target
  }
[@@deriving sexp, equal, compare]

let count (e : entry) : int = List.length e.refs

(** The entry's references split into the ones written in the note itself and
    the ones written elsewhere. *)
let split (e : entry) : Find_references.reference list * Find_references.reference list =
  List.partition_tf e.refs ~f:(fun (r : Find_references.reference) ->
    String.equal r.rel_path e.rel_path)
;;

(** The headings of [content] within the line range [\[range_start_line,
    range_end_line)], as [(line, end_character, slug)] triples.

    Both which lines are headings and what each one's slug is come from
    {!Anchors}, i.e. from the parser: a [#] inside a fenced code block gets no
    lens, and a heading with an authored [ \{#id\} ] is counted under the id
    references actually name.  See {!page-"feature-index"}. *)
let headings_in_range
      ~(content : string)
      ~(range_start_line : int)
      ~(range_end_line : int)
  : (int * int * string) list
  =
  let lines = Array.of_list (String.split_lines content) in
  Anchors.of_content content
  |> List.filter_map ~f:(fun (a : Anchors.t) ->
    match a.value with
    | Caret _ | Attr _ -> None
    | Heading heading ->
      if a.first_line < range_start_line || a.first_line >= range_end_line
      then None
      else (
        let end_char =
          if a.first_line < Array.length lines
          then String.length lines.(a.first_line)
          else 0
        in
        Some (a.first_line, end_char, heading.slug)))
;;

(** Every count worth showing for [rel_path], in line order: the whole-note
    count at line 0, then one per heading that something points at.

    A count of zero produces no entry.  Both renderings would rather say
    nothing than annotate every heading in the vault with a nought — and the
    zero case is the common one.  [docs] is the pre-resolved vault.

    Links the server generated are not counted unless [count_toc_links] says
    to: a [::: toc] region names every heading in the note, and counting those
    would put [1 reference] on every heading of a note that has a table of
    contents — the column of numbers the zero rule exists to prevent.  See
    {!page-"feature-codelens-reference-counts".self}. *)
let entries
      ~(index : Oystermark.Vault.Index.t)
      ~(docs : (string * Cmarkit.Doc.t) list)
      ~(rel_path : string)
      ~(content : string)
      ~(range_start_line : int)
      ~(range_end_line : int)
      ~(count_toc_links : bool)
  : entry list
  =
  let authored (target : Find_references.target) : Find_references.reference list =
    Find_references.scan_vault ~index ~docs target
    |> List.filter ~f:(fun (r : Find_references.reference) ->
      count_toc_links || not r.in_toc)
  in
  let file =
    if range_start_line <= 0 && range_end_line > 0
    then (
      match authored { path = rel_path; address = None } with
      | [] -> []
      | refs -> [ { line = 0; end_character = 0; rel_path; refs; target = File } ])
    else []
  in
  let headings =
    headings_in_range ~content ~range_start_line ~range_end_line
    |> List.filter_map ~f:(fun (line, end_character, slug) ->
      match authored { path = rel_path; address = Some (Heading slug) } with
      | [] -> None
      | refs -> Some { line; end_character; rel_path; refs; target = Heading { slug } })
  in
  file @ headings
;;

(** {2 Wording} *)

(** The lens title: what every other language server writing this feature
    says, near enough — [3 references] on a heading, and [12 backlinks] for the
    note, which is the word a note-taker uses for the same thing.

    Links the note makes to itself are counted apart and named in full:
    [3 backlinks, 1 in-note link].  "Backlink" means a link from somewhere
    else, and a note that mentions its own heading has not been pointed at by
    anything.  Naming both halves is what makes the two numbers read as two
    kinds of thing that add up, rather than as a total and a part of it — and
    it leaves each half free to appear alone.

    A lens keeps its own noun across both halves — a heading says
    [3 references, 1 in-note reference] — except that the note's in-note half
    is a {e link}: [1 in-note backlink] would contradict itself.  See
    {!page-"feature-codelens-reference-counts".wording}. *)
let lens_title (e : entry) : string =
  let self, elsewhere = split e in
  let count n (one, many) =
    if n = 0 then None else Some (sprintf "%d %s" n (if n = 1 then one else many))
  in
  let elsewhere =
    count
      (List.length elsewhere)
      (match e.target with
       | File -> "backlink", "backlinks"
       | Heading _ -> "reference", "references")
  in
  let self =
    count
      (List.length self)
      (match e.target with
       | File -> "in-note link", "in-note links"
       | Heading _ -> "in-note reference", "in-note references")
  in
  String.concat ~sep:", " (List.filter_opt [ elsewhere; self ])
;;

(** {1:test Test} *)

let%test_module "reference_counts" =
  (module struct
    let files =
      [ ( "note-a.md"
        , "# Alpha\n\n## Section One\n\nBody text ^block1\n\n## Untouched\n\nEnd.\n" )
      ; "note-b.md", "# Beta\n\nLink to [[note-a]] here.\n"
      ; ( "note-c.md"
        , "# Gamma\n\nSee [[note-a#Section One]].\n\nAlso [[note-a#^block1]].\n" )
      ]
    ;;

    let index, docs = Find_references.For_test.make_vault files

    let show ?(range_start_line = 0) ?(range_end_line = 100) rel_path =
      let content = List.Assoc.find_exn files ~equal:String.equal rel_path in
      entries
        ~index
        ~docs
        ~rel_path
        ~content
        ~range_start_line
        ~range_end_line
        ~count_toc_links:false
      |> List.iter ~f:(fun e -> printf "line %d: %s\n" e.line (lens_title e))
    ;;

    (* Both wordings of the same counts, side by side.  [## Untouched] is
       absent: nothing points at it, so neither rendering says anything. *)
    let%expect_test "counts and their two wordings" =
      show "note-a.md";
      [%expect
        {|
        line 0: 3 backlinks
        line 2: 1 reference
        |}]
    ;;

    let%expect_test "a note nothing points at" =
      show "note-b.md";
      [%expect {| |}]
    ;;

    (* The range is the client's visible window: line 0 outside it takes the
       whole-note count with it. *)
    let%expect_test "partial range" =
      show ~range_start_line:2 ~range_end_line:5 "note-a.md";
      [%expect {| line 2: 1 reference |}]
    ;;

    let%expect_test "singular and plural" =
      let show_both count =
        let refs =
          List.init count ~f:(fun i ->
            { Find_references.rel_path = "x.md"
            ; first_byte = i
            ; last_byte = i
            ; in_toc = false
            })
        in
        let e =
          { line = 0; end_character = 0; rel_path = "note-a.md"; refs; target = File }
        in
        let h = { e with target = Heading { slug = "s" } } in
        printf "%d: %s / %s\n" count (lens_title e) (lens_title h)
      in
      show_both 1;
      show_both 2;
      [%expect
        {|
        1: 1 backlink / 1 reference
        2: 2 backlinks / 2 references
        |}]
    ;;
  end)
;;

(* A note that links to itself, by hand and through a [::: toc] region.  See
   {!page-"feature-codelens-reference-counts".self}. *)
let%test_module "in-note references" =
  (module struct
    let files =
      [ ( "toc-note.md"
        , "# Alpha\n\n\
           ::: toc\n\
           - [Alpha](#alpha)\n\
           - [Method](#method)\n\
           :::\n\n\
           ## Method\n\n\
           See [[#Method]] below.\n" )
      ; "outside.md", "# Out\n\n[[toc-note]] and [[toc-note#Method]].\n"
      ; "solo.md", "# Solo\n\n## Part\n\nBack to [[#Part]].\n"
      ]
    ;;

    let index, docs = Find_references.For_test.make_vault files

    let show ?(count_toc_links = false) rel_path =
      let content = List.Assoc.find_exn files ~equal:String.equal rel_path in
      entries
        ~index
        ~docs
        ~rel_path
        ~content
        ~range_start_line:0
        ~range_end_line:100
        ~count_toc_links
      |> List.iter ~f:(fun e -> printf "line %d: %s\n" e.line (lens_title e))
    ;;

    (* [# Alpha] gets no lens: the TOC entry naming it is the only link there
       is, and a generated link is not a reference.  Without that rule every
       heading of a note with a TOC would carry [1 reference]. *)
    let%expect_test "a toc names every heading and counts for none" =
      show "toc-note.md";
      [%expect
        {|
        line 0: 2 backlinks, 1 in-note link
        line 7: 1 reference, 1 in-note reference
        |}]
    ;;

    (* Nothing outside points here, so the in-note half is the whole title:
       naming what it counts is what lets it stand alone. *)
    let%expect_test "only in-note links" =
      show "solo.md";
      [%expect
        {|
        line 0: 1 in-note link
        line 2: 1 in-note reference
        |}]
    ;;

    (* Asked for, a TOC entry is counted as what it is: a link the note makes
       to itself, in the in-note half.  [# Alpha], which only the TOC names,
       gets its lens back — which is the reading this is off by default. *)
    let%expect_test "counting them is a setting" =
      show ~count_toc_links:true "toc-note.md";
      [%expect
        {|
        line 0: 2 backlinks, 3 in-note links
        line 0: 1 in-note reference
        line 7: 1 reference, 2 in-note references
        |}]
    ;;

    (* A note with no TOC does not notice the setting. *)
    let%expect_test "no region, no difference" =
      show ~count_toc_links:true "solo.md";
      [%expect
        {|
        line 0: 1 in-note link
        line 2: 1 in-note reference
        |}]
    ;;
  end)
;;
