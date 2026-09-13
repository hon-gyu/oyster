(** Shared link-detection utilities: collect links with byte ranges from a
    parsed document.

    Used by {!Go_to_definition} and {!Diagnostics}.
    See {!page-"feature-go-to-definition".link_detection}.

    {@meta[
      ai-disclosure: autonomous
    ]}
*)

open Core

(** How the source syntax consumes its target. This lets diagnostics
    distinguish navigation links from transclusions and media. *)
type kind =
  | Link
  | Embed
  | Image
[@@deriving sexp, equal, compare]

let is_image_target target =
  let target = String.lowercase target in
  List.exists [ ".png"; ".jpg"; ".jpeg"; ".gif"; ".svg"; ".webp" ] ~f:(fun ext ->
    String.is_suffix target ~suffix:ext)
;;

(** A link found in the AST together with its byte range.
    [first_byte] and [last_byte] are 0-based absolute byte positions. *)
type located_link =
  { source : string
  ; destination : Oystermark.Vault.Index.resolution
  ; reference : Oystermark.Note.Link.Ref.t
  ; kind : kind
  ; first_byte : int
  ; last_byte : int
  }

(** Walk a parsed document's AST and collect all links (wikilinks and markdown
    links/images) together with their byte ranges from [Cmarkit.Meta.textloc].

    Requires the document to have been parsed with [~locs:true] so that
    text locations are available on AST nodes. *)
let collect_links ~(index : Vault.Index.t) ~(rel_path : string) (doc : Cmarkit.Doc.t)
  : located_link list
  =
  Trace_core.with_span ~__FILE__ ~__LINE__ "collect_links"
  @@ fun span ->
  let module Index = Oystermark.Vault.Index in
  let file_stat : Index.file_stat = { rel_path; birthtime = None; mtime = None } in
  let note = Index.Entry.of_doc_exn file_stat doc in
  let links =
    Index.Entry.links note
    |> List.map ~f:(fun link ->
      let resolution = Index.resolve index rel_path link.reference in
      let kind =
        match link.kind, resolution with
        | Index.Link.Link, _ -> Link
        | Embed, Ok (Index.Note _ | Anchor _) -> Embed
        | Embed, Ok (Index.Asset _) -> Image
        | Embed, Error _ ->
          (match link.reference.target with
           | Some target when is_image_target target -> Image
           | _ -> Embed)
      in
      { source = rel_path
      ; destination = resolution
      ; reference = link.reference
      ; kind
      ; first_byte = Cmarkit.Textloc.first_byte link.loc
      ; last_byte = Cmarkit.Textloc.last_byte link.loc
      })
  in
  Trace_core.add_data_to_span span [ "num_links", `Int (List.length links) ];
  links
;;

(** Find the link whose byte range contains [offset].
    Returns the {!located_link} if found. *)
let find_at_offset (links : located_link list) (offset : int)
  : Oystermark.Note.Link.Ref.t option
  =
  Trace_core.with_span ~__FILE__ ~__LINE__ "find_link_ref_at_offset"
  @@ fun _sp ->
  let result =
    List.find_map links ~f:(fun ll ->
      if ll.first_byte <= offset && offset <= ll.last_byte
      then Some ll.reference
      else None)
  in
  Trace_core.add_data_to_span
    _sp
    [ "offset", `Int offset; "found", `Bool (Option.is_some result) ];
  result
;;

(** {1:test Test} *)

let%test_module "collect_links" =
  (module struct
    let show text =
      let doc = Lsp_util.parse_doc text in
      let index =
        Oystermark.Vault.build_index ~md_docs:[ "test.md", doc ] ~other_files:[] ()
      in
      let links = collect_links ~index ~rel_path:"test.md" doc in
      List.iter links ~f:(fun ll ->
        printf
          "[%d-%d] %s\n"
          ll.first_byte
          ll.last_byte
          (Sexp.to_string (Oystermark.Note.Link.Ref.sexp_of_t ll.reference)))
    ;;

    let%expect_test "wikilink" =
      show "see [[Note]] here";
      [%expect {| [4-11] ((target(Note))(fragment())) |}]
    ;;

    let%expect_test "embed wikilink" =
      show "see ![[Image.png]] here";
      [%expect {| [4-17] ((target(Image.png))(fragment())) |}]
    ;;

    let%expect_test "wikilink with fragment" =
      show "go to [[Note#Heading]] now";
      [%expect {| [6-21] ((target(Note))(fragment((Hash_path(Heading))))) |}]
    ;;

    let%expect_test "markdown link" =
      show "see [text](other) here";
      [%expect {| [4-16] ((target(other))(fragment())) |}]
    ;;

    let%expect_test "external link ignored" =
      show "[text](https://example.com)";
      [%expect {| |}]
    ;;

    let%expect_test "two wikilinks" =
      show "[[A]] and [[B]]";
      [%expect
        {|
        [0-4] ((target(A))(fragment()))
        [10-14] ((target(B))(fragment()))
        |}]
    ;;

    let%expect_test "image link" =
      show "see ![alt](img.png) here";
      [%expect {| [4-18] ((target(img.png))(fragment())) |}]
    ;;
  end)
;;

let%test_module "find_at_offset" =
  (module struct
    let find text offset =
      let doc = Lsp_util.parse_doc text in
      let index =
        Oystermark.Vault.build_index ~md_docs:[ "test.md", doc ] ~other_files:[] ()
      in
      let links = collect_links ~index ~rel_path:"test.md" doc in
      find_at_offset links offset
    ;;

    let show text offset =
      match find text offset with
      | None -> print_endline "<none>"
      | Some lr -> print_s (Oystermark.Note.Link.Ref.sexp_of_t lr)
    ;;

    let%expect_test "cursor on wikilink target" =
      show "see [[Note]] here" 6;
      [%expect {| ((target (Note)) (fragment ())) |}]
    ;;

    let%expect_test "cursor on opening brackets" =
      show "see [[Note]] here" 4;
      [%expect {| ((target (Note)) (fragment ())) |}]
    ;;

    let%expect_test "cursor on closing brackets" =
      show "see [[Note]] here" 11;
      [%expect {| ((target (Note)) (fragment ())) |}]
    ;;

    let%expect_test "cursor outside" =
      show "see [[Note]] here" 2;
      [%expect {| <none> |}]
    ;;

    let%expect_test "cursor after link" =
      show "see [[Note]] here" 13;
      [%expect {| <none> |}]
    ;;

    let%expect_test "markdown link" =
      show "[text](other)" 8;
      [%expect {| ((target (other)) (fragment ())) |}]
    ;;

    let%expect_test "external link ignored" =
      show "[text](https://example.com)" 10;
      [%expect {| <none> |}]
    ;;
  end)
;;
