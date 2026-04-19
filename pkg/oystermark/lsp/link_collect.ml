(** Shared link-detection utilities: collect links with byte ranges from a
    parsed document.

    Used by {!Go_to_definition} and {!Diagnostics}.
    See {!page-"feature-go-to-definition".link_detection}. *)

open Core

(** A link found in the AST together with its byte range.
    [first_byte] and [last_byte] are 0-based absolute byte positions. *)
type located_link =
  { link_ref : Oystermark.Vault.Link_ref.t
  ; first_byte : int
  ; last_byte : int
  }

(** Walk a parsed document's AST and collect all links (wikilinks and markdown
    links/images) together with their byte ranges from [Cmarkit.Meta.textloc].

    Requires the document to have been parsed with [~locs:true] so that
    text locations are available on AST nodes. *)
let collect_links (doc : Cmarkit.Doc.t) : located_link list =
  Trace_core.with_span ~__FILE__ ~__LINE__ "collect_links"
  @@ fun _sp ->
  let try_add_link acc link_ref loc =
    if Cmarkit.Textloc.is_none loc
    then acc
    else
      { link_ref
      ; first_byte = Cmarkit.Textloc.first_byte loc
      ; last_byte = Cmarkit.Textloc.last_byte loc
      }
      :: acc
  in
  let folder =
    Cmarkit.Folder.make
      ~inline_ext_default:(fun _f acc i ->
        match i with
        | Oystermark.Parse.Wikilink.Ext_wikilink (wl, meta) ->
          let link_ref = Oystermark.Vault.Link_ref.of_wikilink wl in
          try_add_link acc link_ref (Cmarkit.Meta.textloc meta)
        | _ -> acc)
      ~block_ext_default:(fun f acc b ->
        match b with
        | Oystermark.Parse.Struct.Ext_keyed_list_item ({ label }, inner)
        | Oystermark.Parse.Struct.Ext_keyed_block ({ label }, inner) ->
          let acc = Cmarkit.Folder.fold_inline f acc label in
          Cmarkit.Folder.fold_block f acc inner
        | _ -> acc)
      ~inline:(fun _f acc i ->
        match i with
        | Cmarkit.Inline.Link (link, meta) | Cmarkit.Inline.Image (link, meta) ->
          let loc = Cmarkit.Meta.textloc meta in
          let ref_ = Cmarkit.Inline.Link.reference link in
          (match Oystermark.Vault.Link_ref.of_cmark_reference ref_ with
           | Some link_ref -> Cmarkit.Folder.ret (try_add_link acc link_ref loc)
           | None -> Cmarkit.Folder.default)
        | _ -> Cmarkit.Folder.default)
      ()
  in
  let links = List.rev (Cmarkit.Folder.fold_doc folder [] doc) in
  Trace_core.add_data_to_span _sp [ "num_links", `Int (List.length links) ];
  links
;;

(** Find the link whose byte range contains [offset].
    Returns the {!located_link} if found. *)
let find_at_offset (links : located_link list) (offset : int)
  : Oystermark.Vault.Link_ref.t option
  =
  Trace_core.with_span ~__FILE__ ~__LINE__ "find_link_ref_at_offset"
  @@ fun _sp ->
  let result =
    List.find_map links ~f:(fun ll ->
      if ll.first_byte <= offset && offset <= ll.last_byte then Some ll.link_ref else None)
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
      let links = collect_links doc in
      List.iter links ~f:(fun ll ->
        printf
          "[%d-%d] %s\n"
          ll.first_byte
          ll.last_byte
          (Sexp.to_string (Oystermark.Vault.Link_ref.sexp_of_t ll.link_ref)))
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
      [%expect {| [6-21] ((target(Note))(fragment((Heading(Heading))))) |}]
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
      let links = collect_links doc in
      find_at_offset links offset
    ;;

    let show text offset =
      match find text offset with
      | None -> print_endline "<none>"
      | Some lr -> print_s (Oystermark.Vault.Link_ref.sexp_of_t lr)
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
