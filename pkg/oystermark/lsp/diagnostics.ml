(** Diagnostics: report unresolved links as warnings.

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

(** Collect every anchor id in [doc] with its byte range, across the three
    anchor kinds (heading slug, caret block id, attribute id).  Occurrences
    without a location ([Textloc.none]) are dropped.
    See {!page-"feature-diagnostics".duplicate_ids}. *)
let collect_anchor_occurrences (doc : Cmarkit.Doc.t) : (string * (int * int)) list =
  let range_of_loc (loc : Cmarkit.Textloc.t option) : (int * int) option =
    match loc with
    | Some tl when not (Cmarkit.Textloc.is_none tl) ->
      Some (Cmarkit.Textloc.first_byte tl, Cmarkit.Textloc.last_byte tl)
    | _ -> None
  in
  let headings =
    Oystermark.Vault.Index.extract_headings doc
    |> List.filter_map ~f:(fun (h : Oystermark.Vault.Index.heading_entry) ->
      Option.map (range_of_loc h.loc) ~f:(fun r -> h.slug, r))
  in
  let blocks =
    Oystermark.Vault.Index.extract_block_ids doc
    |> List.filter_map ~f:(fun (b : Oystermark.Vault.Index.block_entry) ->
      Option.map (range_of_loc b.loc) ~f:(fun r -> b.id, r))
  in
  let attrs =
    Oystermark.Vault.Index.extract_attr_ids doc
    |> List.filter_map ~f:(fun (a : Oystermark.Vault.Index.attr_entry) ->
      Option.map (range_of_loc a.loc) ~f:(fun r -> a.id, r))
  in
  headings @ blocks @ attrs
;;

(** Diagnostics for anchor ids that occur more than once in [doc]: every
    located occurrence of a duplicated id is reported.
    See {!page-"feature-diagnostics".duplicate_ids}. *)
let duplicate_id_diagnostics (doc : Cmarkit.Doc.t) : diagnostic list =
  collect_anchor_occurrences doc
  |> String.Map.of_alist_multi
  |> Map.fold ~init:[] ~f:(fun ~key:id ~data:ranges acc ->
    if List.length ranges > 1
    then
      List.fold ranges ~init:acc ~f:(fun acc (first_byte, last_byte) ->
        { first_byte; last_byte; message = "duplicate anchor id: " ^ id } :: acc)
    else acc)
;;

(** Compute diagnostics for unresolved links and duplicate anchor ids in
    [content] at [rel_path] within a vault [index].

    See {!page-"feature-diagnostics".resolution_check} and
    {!page-"feature-diagnostics".duplicate_ids}. *)
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
  let links = Link_collect.collect_links doc in
  let diagnostics =
    List.filter_map links ~f:(fun (ll : Link_collect.located_link) ->
      let target = Oystermark.Vault.Resolve.resolve ll.link_ref rel_path index in
      (* A link is unresolved when: the target file doesn't exist, OR the file
         exists but the heading/block fragment wasn't found (resolve falls back
         to Note/File/Curr_file instead of Heading/Block/Curr_heading/Curr_block).
         See {!page-"feature-diagnostics".resolution_check}. *)
      let is_unresolved =
        match target, ll.link_ref.fragment with
        | Oystermark.Vault.Resolve.Unresolved, _ -> true
        | (Note _ | File _), Some _ ->
          Lsp_config.equal_fragment_behavior config.diag_unresolved_fragment Strict
        | Curr_file, Some _ ->
          Lsp_config.equal_fragment_behavior config.diag_unresolved_fragment Strict
        | _ -> false
      in
      if is_unresolved
      then (
        let target_str =
          match ll.link_ref.target with
          | Some t -> t
          | None -> ""
        in
        let fragment_str =
          match ll.link_ref.fragment with
          | Some (Oystermark.Vault.Link_ref.Heading h) -> "#" ^ String.concat ~sep:"#" h
          | Some (Block_ref b) -> "#^" ^ b
          | None -> ""
        in
        Some
          { first_byte = ll.first_byte
          ; last_byte = ll.last_byte
          ; message = "unresolved link: " ^ target_str ^ fragment_str
          })
      else None)
  in
  let all = diagnostics @ duplicate_id_diagnostics doc in
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
      Oystermark.Vault.build_index ~md_docs ~other_files ~dirs:[]
    ;;

    let files =
      [ "note-a.md", "# Alpha\n\n## Section One\n\nBody text ^block1\n"
      ; "note-b.md", "# Beta\n\nLink to [[note-a]] here.\n"
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

    let%expect_test "unresolved block id" =
      show ~rel_path:"note-b.md" ~content:"See [[note-a#^noblock]].";
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

    (* Frontmatter: the reported byte range is full-file-relative — the parser
       blanks rather than strips the frontmatter, so a link after frontmatter is
       located in the original file, not the stripped body. [[missing]] here
       starts at byte 21 (after the 20-byte "---\ntitle: T\n---\n" + "See ").
       See {!Oystermark.Parse.Frontmatter.blank_frontmatter}. *)
    let%expect_test "link range is full-file-relative under frontmatter" =
      show ~rel_path:"note-b.md" ~content:"---\ntitle: T\n---\nSee [[missing]].";
      [%expect
        {| ((first_byte 21) (last_byte 31) (message "unresolved link: missing")) |}]
    ;;

    let%expect_test "embed wikilink unresolved" =
      show ~rel_path:"note-b.md" ~content:"see ![[missing.png]] here";
      [%expect
        {| ((first_byte 4) (last_byte 19) (message "unresolved link: missing.png")) |}]
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
      show ~rel_path:"note-a.md" ~content:"# Intro\n\nSee [x]{#intro} here.\n";
      [%expect
        {|
        ((first_byte 0) (last_byte 6) (message "duplicate anchor id: intro"))
        ((first_byte 13) (last_byte 23) (message "duplicate anchor id: intro"))
        |}]
    ;;

    let%expect_test "distinct ids: no diagnostic" =
      show ~rel_path:"note-a.md" ~content:"# H\n\nOne [a]{#x} two [b]{#y}.\n";
      [%expect {| |}]
    ;;
  end)
;;
