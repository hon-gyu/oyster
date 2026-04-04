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

(** Compute diagnostics for unresolved links in [content] at [rel_path]
    within a vault [index].

    See {!page-"feature-diagnostics".resolution_check}. *)
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
  Trace_core.add_data_to_span _sp [ "num_diagnostics", `Int (List.length diagnostics) ];
  diagnostics
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

    let%expect_test "embed wikilink unresolved" =
      show ~rel_path:"note-b.md" ~content:"see ![[missing.png]] here";
      [%expect
        {| ((first_byte 4) (last_byte 19) (message "unresolved link: missing.png")) |}]
    ;;
  end)
;;
