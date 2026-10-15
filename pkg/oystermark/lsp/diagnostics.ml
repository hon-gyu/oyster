(** Diagnostics: report unresolved links, embeds, and images as warnings.

    Spec: {!page-"feature-diagnostics"}. *)

open Core

(** {1:implementation Implementation} *)

(** A single diagnostic for an unresolved link. *)
type diagnostic =
  { first_byte : int
  ; last_byte : int
  ; message : string
  }
[@@deriving sexp, equal, compare]

(** Every anchor id in [doc] with its byte range, headings and attributes
    alike, since they share one namespace.  Occurrences without a location
    ([Textloc.none]) are dropped.
    See {!page-"feature-diagnostics".duplicate_ids}. *)
let collect_anchor_occurrences (doc : Oystermark.Parse.t) : (string * (int * int)) list =
  let module Index = Oystermark.Vault.Index in
  let file_stat : Index.file_stat =
    { rel_path = "__diagnostics__.md"; birthtime = None; mtime = None }
  in
  Index.Entry.of_doc_exn file_stat doc
  |> Index.Entry.anchors
  |> List.map ~f:(fun anchor ->
    ( Oystermark.Note.Anchor.Address.id (Oystermark.Note.Anchor.address anchor.definition)
    , (Djot.Textloc.first_byte anchor.loc, Djot.Textloc.last_byte anchor.loc) ))
;;

(** Diagnostics for anchor ids that occur more than once in [doc]: every
    located occurrence of a duplicated id is reported.
    See {!page-"feature-diagnostics".duplicate_ids}. *)
let duplicate_id_diagnostics (doc : Oystermark.Parse.t) : diagnostic list =
  collect_anchor_occurrences doc
  |> String.Map.of_alist_multi
  |> Map.fold ~init:[] ~f:(fun ~key:id ~data:ranges acc ->
    if List.length ranges > 1
    then
      List.fold ranges ~init:acc ~f:(fun acc (first_byte, last_byte) ->
        { first_byte; last_byte; message = "duplicate anchor id: " ^ id } :: acc)
    else acc)
;;

(** Undefined footnote references and duplicate footnote definitions in
    [doc]. Every definition of a duplicated label is reported.
    See {!page-"feature-footnotes".diagnostics}. *)
let footnote_diagnostics (doc : Oystermark.Parse.t) : diagnostic list =
  let doc = doc in
  let range loc =
    Option.some_if
      (not (Djot.Textloc.is_none loc))
      (Djot.Textloc.first_byte loc, Djot.Textloc.last_byte loc)
  in
  let defs = Oystermark.Parse.Common.footnote_defs doc in
  let folder =
    Djot.Folder.make
      ~inline:(fun _ acc (Node (_, _, inline) as node) ->
        match inline with
        | FootnoteReference label when Option.is_none (Djot.Doc.footnote doc label) ->
          (match range (Djot.Doc.textloc doc node) with
           | Some (first_byte, last_byte) ->
             Djot.Folder.ret
               ({ first_byte; last_byte; message = "undefined footnote: ^" ^ label }
                :: acc)
           | None -> Djot.Folder.default)
        | _ -> Djot.Folder.default)
      ()
  in
  let undefined = Oystermark.Parse.fold_doc folder [] doc in
  (* A definition's location starts at its [[^]; the label follows. *)
  let label_range node label =
    Option.map
      (range (Djot.Doc.textloc doc node))
      ~f:(fun (first_byte, _) -> first_byte + 2, first_byte + 2 + String.length label - 1)
  in
  let duplicate =
    List.filter_map defs ~f:(fun (Node (_, _, def) as node) ->
      match def with
      | FootnoteDef (label, _) ->
        Option.map (label_range node label) ~f:(fun r ->
          Djot.Kernel.Ast.normalize_label label, (label, r))
      | _ -> None)
    |> String.Map.of_alist_multi
    |> Map.data
    |> List.concat_map ~f:(function
      | [] | [ _ ] -> []
      | occurrences ->
        List.map occurrences ~f:(fun (label, (first_byte, last_byte)) ->
          { first_byte; last_byte; message = "duplicate footnote definition: ^" ^ label }))
  in
  undefined @ duplicate
;;

(** Compute diagnostics for unresolved links, duplicate anchor ids and
    footnote errors in [content] at [rel_path] within a vault [index].

    See {!page-"feature-diagnostics".resolution_check},
    {!page-"feature-diagnostics".duplicate_ids} and
    {!page-"feature-footnotes".diagnostics}. *)
let compute
      ?(config : Lsp_config.t = Lsp_config.default)
      ~(index : Oystermark.Vault.Index.t)
      ~(rel_path : string)
      ~(content : string)
      ()
  : diagnostic list
  =
  Trace_core.with_span ~__FILE__ ~__LINE__ "diagnostics.compute"
  @@ fun _sp ->
  Trace_core.add_data_to_span _sp [ "rel_path", `String rel_path ];
  let doc = Lsp_util.parse_doc content in
  let links = Link_collect.collect_links ~index ~rel_path doc in
  let diagnostics =
    List.filter_map links ~f:(fun (ll : Link_collect.located_link) ->
      let target = ll.destination in
      (* A link is unresolved when: the target file doesn't exist, OR the file
         exists but the heading/block fragment wasn't found (resolve falls back
         to Note/File/Curr_file instead of Heading/Block/Curr_heading/Curr_block).
         See {!page-"feature-diagnostics".resolution_check}. *)
      let is_unresolved =
        match target with
        | Error (Oystermark.Vault.Index.Missing_anchor _) ->
          Lsp_config.equal_fragment_behavior config.diag_unresolved_fragment Strict
        | Error
            Oystermark.Vault.Index.(
              Missing_path | Invalid_query _ | No_match | Ambiguous _) -> true
        | Ok _ -> false
      in
      (* A query link's failure is the author's to fix, so it says why. See
         {!page-"feature-diagnostics".resolution_check}. *)
      let reason =
        match target with
        | Error e when Oystermark.Vault.Index.is_query_error e ->
          ": " ^ Oystermark.Vault.Index.string_of_resolution_error e
        | Ok _ | Error _ -> ""
      in
      if is_unresolved
      then (
        let target_str =
          match ll.reference.target with
          | Some t -> t
          | None -> ""
        in
        let fragment_str =
          Option.value_map
            ll.reference.fragment
            ~default:""
            ~f:Oystermark.Note.Link.Ref.string_of_fragment
        in
        let category =
          match ll.kind with
          | Link_collect.Link -> "link"
          | Embed -> "embed"
          | Image -> "image"
        in
        Some
          { first_byte = ll.first_byte
          ; last_byte = ll.last_byte
          ; message = "unresolved " ^ category ^ ": " ^ target_str ^ fragment_str ^ reason
          })
      else None)
  in
  let all = diagnostics @ duplicate_id_diagnostics doc @ footnote_diagnostics doc in
  let sorted =
    List.sort all ~compare:(fun a b ->
      match Int.compare a.first_byte b.first_byte with
      | 0 -> Int.compare a.last_byte b.last_byte
      | c -> c)
  in
  Trace_core.add_data_to_span _sp [ "num_diagnostics", `Int (List.length sorted) ];
  sorted
;;

(** {1:test Test} *)

let%test_module "compute" =
  (module struct
    let make_index (files : (string * string) list) : Oystermark.Vault.Index.t =
      let md_docs =
        List.filter_map files ~f:(fun (rel_path, content) ->
          if String.is_suffix rel_path ~suffix:".md"
          then (
            let doc = Oystermark.Parse.of_string content in
            Some (rel_path, doc))
          else None)
      in
      let other_files =
        List.filter_map files ~f:(fun (p, _) ->
          if not (String.is_suffix p ~suffix:".md") then Some p else None)
      in
      Oystermark.Vault.build_index ~md_docs ~other_files ()
    ;;

    let files =
      [ "note-a.md", "# Alpha\n\n## Section One\n\n{#block1}\nBody text\n"
      ; "note-b.md", "# Beta\n\nLink to [[note-a]] here.\n"
      ; "image.png", ""
      ]
    ;;

    let index = make_index files

    let show ~rel_path ~content : unit =
      let (diags : diagnostic list) = compute ~index ~rel_path ~content () in
      List.iter diags ~f:(fun d -> print_s [%sexp (d : diagnostic)])
    ;;

    let%expect_test "resolved link produces no diagnostic" =
      show ~rel_path:"note-b.md" ~content:"Link to [[note-a]] here.";
      [%expect {| |}]
    ;;

    let%expect_test "unresolved link produces diagnostic" =
      show ~rel_path:"note-b.md" ~content:"See [[nonexistent]] here.";
      [%expect
        {| ((first_byte 4) (last_byte 18) (message "unresolved link: nonexistent")) |}]
    ;;

    let%expect_test "unresolved heading fragment" =
      show ~rel_path:"note-b.md" ~content:"See [[note-a#Missing Heading]].";
      [%expect {| |}]
    ;;

    let%expect_test "unresolved attribute id" =
      show ~rel_path:"note-b.md" ~content:"See [[note-a#noblock]].";
      [%expect {| |}]
    ;;

    let%expect_test "empty document" =
      show ~rel_path:"note-b.md" ~content:"";
      [%expect {| |}]
    ;;

    let%expect_test "self-referencing heading resolved" =
      show ~rel_path:"note-a.md" ~content:"# Alpha\n\nSee [[#Alpha]].";
      [%expect {| |}]
    ;;

    let%expect_test "self-referencing heading unresolved" =
      show ~rel_path:"note-a.md" ~content:"# Alpha\n\nSee [[#Missing]].";
      [%expect {| |}]
    ;;

    let%expect_test "mixed resolved and unresolved" =
      show
        ~rel_path:"note-b.md"
        ~content:"[[note-a]] and [[missing]] and [[note-a#Section One]]";
      [%expect
        {| ((first_byte 15) (last_byte 25) (message "unresolved link: missing")) |}]
    ;;

    let%expect_test "markdown link unresolved" =
      show ~rel_path:"note-b.md" ~content:"see [text](nowhere) here";
      [%expect {| ((first_byte 4) (last_byte 18) (message "unresolved link: nowhere")) |}]
    ;;

    let%expect_test "external link skipped" =
      show ~rel_path:"note-b.md" ~content:"see [text](https://example.com) here";
      [%expect {| |}]
    ;;

    (* Frontmatter: the reported byte range is full-file-relative, see
       {!Oystermark.Parse.of_string}. [[missing]] here starts at byte 21 (after
       the 20-byte "---\ntitle: T\n---\n" + "See "). *)
    let%expect_test "link range is full-file-relative under frontmatter" =
      show ~rel_path:"note-b.md" ~content:"---\ntitle: T\n---\nSee [[missing]].";
      [%expect
        {| ((first_byte 21) (last_byte 31) (message "unresolved link: missing")) |}]
    ;;

    let%expect_test "embed wikilink unresolved" =
      show ~rel_path:"note-b.md" ~content:"see ![[missing.png]] here";
      [%expect
        {| ((first_byte 4) (last_byte 19) (message "unresolved image: missing.png")) |}]
    ;;

    let%expect_test "note embed unresolved" =
      show ~rel_path:"note-b.md" ~content:"see ![[missing-note]] here";
      [%expect
        {| ((first_byte 4) (last_byte 20) (message "unresolved embed: missing-note")) |}]
    ;;

    let%expect_test "markdown image unresolved" =
      show ~rel_path:"note-b.md" ~content:"see ![alt](missing.png) here";
      [%expect
        {| ((first_byte 4) (last_byte 22) (message "unresolved image: missing.png")) |}]
    ;;

    let%expect_test "resolved image produces no diagnostic" =
      show ~rel_path:"note-b.md" ~content:"see ![[image.png]] here";
      [%expect {| |}]
    ;;

    (* Query links. See {!page-"feature-diagnostics".resolution_check} and
       {!page-"query-link".resolution}. *)
    let%expect_test "query link failures carry their reason" =
      let index =
        make_index
          [ "a.md", "---\nid: a\ngroup: x\n---\n# A\n"
          ; "b.md", "---\nid: b\ngroup: x\n---\n# B\n"
          ]
      in
      compute
        ~index
        ~rel_path:"c.md"
        ~content:"[[?id==a]] [[?id==z]] [[?group==x]] [[?id=]] [[?id==a#Nope]]"
        ()
      |> List.iter ~f:(fun d -> print_s [%sexp (d : diagnostic)]);
      [%expect
        {|
        ((first_byte 11) (last_byte 20)
         (message "unresolved link: ?id==z: no note matches the query"))
        ((first_byte 22) (last_byte 34)
         (message
          "unresolved link: ?group==x: the query matches 2 notes: a.md, b.md"))
        ((first_byte 36) (last_byte 43)
         (message
          "unresolved link: ?id=: invalid query: expected a comparison operator after `id`"))
        |}]
    ;;

    (* Duplicate anchor ids. See {!page-"feature-diagnostics".duplicate_ids}. *)

    let%expect_test "unique attribute id: no diagnostic" =
      show ~rel_path:"note-a.md" ~content:"# H\n\nThe [key]{#k} span.\n";
      [%expect {| |}]
    ;;

    let%expect_test "duplicate attribute ids: every occurrence flagged" =
      show ~rel_path:"note-a.md" ~content:"# H\n\nOne [a]{#dup} two [b]{#dup}.\n";
      [%expect
        {|
        ((first_byte 9) (last_byte 17) (message "duplicate anchor id: dup"))
        ((first_byte 23) (last_byte 31) (message "duplicate anchor id: dup"))
        |}]
    ;;

    (* Cross-kind collision: a heading whose slug equals a hand-written attr id. *)
    let%expect_test "heading slug vs attribute id collision" =
      show ~rel_path:"note-a.md" ~content:"# Intro\n\nSee [x]{#Intro} here.\n";
      [%expect
        {|
        ((first_byte 0) (last_byte 6) (message "duplicate anchor id: Intro"))
        ((first_byte 13) (last_byte 23) (message "duplicate anchor id: Intro"))
        |}]
    ;;

    let%expect_test "distinct ids: no diagnostic" =
      show ~rel_path:"note-a.md" ~content:"# H\n\nOne [a]{#x} two [b]{#y}.\n";
      [%expect {| |}]
    ;;

    (* Footnotes. See {!page-"feature-footnotes".diagnostics}. *)

    let%expect_test "defined footnote: no diagnostic" =
      show ~rel_path:"note-a.md" ~content:"See[^a] and[^a].\n\n[^a]: One.\n";
      [%expect {| |}]
    ;;

    let%expect_test "undefined footnote, case-sensitive" =
      show ~rel_path:"note-a.md" ~content:"See[^ghost] and[^A].\n\n[^a]: One.\n";
      [%expect
        {|
        ((first_byte 3) (last_byte 10) (message "undefined footnote: ^ghost"))
        ((first_byte 15) (last_byte 18) (message "undefined footnote: ^A"))
        |}]
    ;;

    let%expect_test "duplicate footnote definition: every definition flagged" =
      show ~rel_path:"note-a.md" ~content:"See[^a].\n\n[^a]: One.\n\n[^a]: Two.\n";
      [%expect
        {|
        ((first_byte 12) (last_byte 12)
         (message "duplicate footnote definition: ^a"))
        ((first_byte 24) (last_byte 24)
         (message "duplicate footnote definition: ^a"))
        |}]
    ;;

    let%expect_test "undefined reference inside a shadowed definition" =
      show ~rel_path:"note-a.md" ~content:"See[^a].\n\n[^a]: One[^x].\n\n[^a]: Two.\n";
      [%expect
        {|
        ((first_byte 12) (last_byte 12)
         (message "duplicate footnote definition: ^a"))
        ((first_byte 19) (last_byte 22) (message "undefined footnote: ^x"))
        ((first_byte 28) (last_byte 28)
         (message "duplicate footnote definition: ^a"))
        |}]
    ;;

    let%expect_test "unused definition: no diagnostic" =
      show ~rel_path:"note-a.md" ~content:"Text.\n\n[^a]: One.\n";
      [%expect {| |}]
    ;;

    let%expect_test "reference in code is not a reference" =
      show ~rel_path:"note-a.md" ~content:"`[^x]` and \\[^y\\].\n\n```\n[^z]\n```\n";
      [%expect {| |}]
    ;;
  end)
;;
