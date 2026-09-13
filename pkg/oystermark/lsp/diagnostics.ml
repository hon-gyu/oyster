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

(** Slugs of headings whose identifier was {e written} rather than derived —
    the [ {#id} ] line above the heading.

    Such an id reaches the collection below twice: once as the heading's slug,
    which the parser resolves from the attribute, and once as the attribute
    line {!Oystermark.Vault.Index.Note.val-anchors} sees.  One authored anchor
    is not a collision, so the heading occurrence is dropped and the attribute
    line — the better place to jump to — is kept.  A genuine collision between
    the same id written twice is still two attribute occurrences.
    See {!page-"feature-diagnostics".duplicate_ids}. *)
let mirrored_heading_slugs (doc : Cmarkit.Doc.t) : String.Set.t =
  let folder =
    Cmarkit.Folder.make
      ~block:(fun _f acc (b : Cmarkit.Block.t) ->
        match b with
        | Cmarkit.Block.Heading (h, _meta) ->
          (match Cmarkit.Block.Heading.id h with
           | Some (`Id id) -> Cmarkit.Folder.ret (Set.add acc id)
           | Some (`Auto _) | None -> Cmarkit.Folder.default)
        | _ -> Cmarkit.Folder.default)
      ~inline:(fun _f acc _i -> Cmarkit.Folder.ret acc)
      ~inline_ext_default:(fun _f acc _i -> acc)
      ~block_ext_default:(fun _f acc _b -> acc)
      ()
  in
  Cmarkit.Folder.fold_doc folder String.Set.empty doc
;;

(** Collect every anchor id in [doc] with its byte range, across the three
    anchor kinds (heading slug, caret block id, attribute id).  Occurrences
    without a location ([Textloc.none]) are dropped.
    See {!page-"feature-diagnostics".duplicate_ids}. *)
let collect_anchor_occurrences (doc : Cmarkit.Doc.t) : (string * (int * int)) list =
  let mirrored = mirrored_heading_slugs doc in
  let module Index = Oystermark.Vault.Index in
  let file_stat : Index.file_stat =
    { rel_path = "__diagnostics__.md"; birthtime = None; mtime = None }
  in
  Index.Note.of_doc_exn file_stat doc
  |> Index.Note.anchors
  |> List.filter_map ~f:(fun anchor ->
    let id =
      match anchor.value with
      | Index.Heading h when Set.mem mirrored h.slug -> None
      | Index.Heading h -> Some h.slug
      | Index.Caret id | Index.Attr { id; _ } -> Some id
    in
    Option.map id ~f:(fun id ->
      id, (Cmarkit.Textloc.first_byte anchor.loc, Cmarkit.Textloc.last_byte anchor.loc)))
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
        | Error Oystermark.Vault.Index.Missing_path -> true
        | Ok _ -> false
      in
      if is_unresolved
      then (
        let target_str =
          match ll.reference.target with
          | Some t -> t
          | None -> ""
        in
        let fragment_str =
          match ll.reference.fragment with
          | Some (Oystermark.Vault.Link_ref.Hash_path h) -> "#" ^ String.concat ~sep:"#" h
          | Some (Caret_id b) -> "#^" ^ b
          | None -> ""
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
          ; message = "unresolved " ^ category ^ ": " ^ target_str ^ fragment_str
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
      Oystermark.Vault.build_index ~md_docs ~other_files ()
    ;;

    let files =
      [ "note-a.md", "# Alpha\n\n## Section One\n\nBody text ^block1\n"
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
