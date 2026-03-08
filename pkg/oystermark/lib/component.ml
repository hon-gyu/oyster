(** UI components.

  Usage:
  - Used by {Pipeline} to add components as HTML blocks using [backend_block].
  - Used in {Vault.render_vault}'s last step to render non-body content (WIP).
*)

open Core

type html = string
type doc_component = string * Cmarkit.Doc.t -> html
type vault_component = Vault.t -> html

let strip_md_ext (path : string) : string =
  match String.chop_suffix path ~suffix:".md" with
  | Some p -> p
  | None -> path
;;

let spf = Printf.sprintf

(** Intermediate TOC tree: a list of entries, each either a leaf file or a
    directory containing a subtree. *)
type toc_entry =
  | Leaf of
      { name : string
      ; path : string
      }
  | Dir of
      { name : string
      ; children : toc_entry list
      }

(** Build a [toc_entry list] from a flat list of relative paths.
    Paths are grouped by directory, producing nested entries for shared prefixes. *)
let build_toc_entries (paths : string list) : toc_entry list =
  let rec build (entries : (string list * string) list) : toc_entry list =
    let (by_head : (string * (string list * string)) list list) =
      List.filter_map entries ~f:(fun (segs, path) ->
        match segs with
        | [] -> None
        | hd :: tl -> Some (hd, (tl, path)))
      |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
      |> List.group ~break:(fun (a, _) (b, _) -> not (String.equal a b))
    in
    List.filter_map by_head ~f:(function
      | [] -> None
      | [ (name, ([], path)) ] -> Some (Leaf { name; path })
      | (name, _) :: _ as group ->
        let children = List.map group ~f:snd in
        Some (Dir { name; children = build children }))
  in
  build (List.map paths ~f:(fun p -> String.split p ~on:'/', p))
;;

(** Render a table of contents as a nested [<ul>] tree from a list of relative paths.
    Paths are grouped by directory, producing nested lists for shared prefixes.
    Markdown file names are stripped of their extension.
    [dir_href_f] maps a directory name to an optional href; if [None], no anchor
    is added to the directory entry. *)
let toc_html
      ?(dir_href_f = fun dir -> Some (dir ^ "/index"))
      ?(leaf_href_f : (string -> string) option)
      ?(collapsible = false)
      ?(collapsed_by_default = false)
      (paths : string list)
  : html
  =
  let leaf_href (path : string) : string =
    match leaf_href_f with
    | Some f -> f path
    | None -> strip_md_ext path
  in
  let render_dir_label (dir : string) : string =
    match dir_href_f dir with
    | None -> dir
    | Some href -> spf {|<a href="%s">%s</a>|} href dir
  in
  let rec render_entries (entries : toc_entry list) : html =
    let items =
      List.map entries ~f:(fun entry ->
        match entry with
        | Leaf { name; path } ->
          let href = leaf_href path in
          spf {|<li><a href="%s">%s</a></li>|} href (strip_md_ext name)
        | Dir { name; children } ->
          let subtree = render_entries children in
          let label = render_dir_label name in
          if collapsible
          then (
            let open_attr = if collapsed_by_default then "" else " open" in
            spf
              {|<li style="list-style: none"><details%s><summary>%s</summary>%s</details></li>|}
              open_attr
              label
              subtree)
          else spf "<li>%s\n%s</li>" label subtree)
    in
    "<ul>\n" ^ String.concat ~sep:"\n" items ^ "\n</ul>"
  in
  render_entries (build_toc_entries paths)
;;

let%expect_test "toc_html" =
  let paths = [ "x/y/z.md"; "x/y/t.md"; "a.jpg"; "x/q.md" ] in
  print_endline (toc_html ~dir_href_f:(fun (_ : string) -> None) paths);
  [%expect
    {|
    <ul>
    <li><a href="a.jpg">a.jpg</a></li>
    <li>x
    <ul>
    <li><a href="x/q">q</a></li>
    <li>y
    <ul>
    <li><a href="x/y/t">t</a></li>
    <li><a href="x/y/z">z</a></li>
    </ul></li>
    </ul></li>
    </ul>
    |}];
  print_endline (toc_html paths);
  [%expect
    {|
    <ul>
    <li><a href="a.jpg">a.jpg</a></li>
    <li><a href="x/index">x</a>
    <ul>
    <li><a href="x/q">q</a></li>
    <li><a href="y/index">y</a>
    <ul>
    <li><a href="x/y/t">t</a></li>
    <li><a href="x/y/z">z</a></li>
    </ul></li>
    </ul></li>
    </ul>
    |}];
  print_endline (toc_html ~collapsible:true ~collapsed_by_default:false paths);
  [%expect
    {|
    <ul>
    <li><a href="a.jpg">a.jpg</a></li>
    <li style="list-style: none"><details open><summary><a href="x/index">x</a></summary><ul>
    <li><a href="x/q">q</a></li>
    <li style="list-style: none"><details open><summary><a href="y/index">y</a></summary><ul>
    <li><a href="x/y/t">t</a></li>
    <li><a href="x/y/z">z</a></li>
    </ul></details></li>
    </ul></details></li>
    </ul>
    |}]
;;

(** Render a table of contents as a [Cmarkit.Block.t] unordered list from a list
    of relative paths. Leaf entries become wikilinks; directories become nested
    sub-lists with either plain text or wikilink labels.

    [path_prefix] is prepended to each entry's path for wikilink resolution.
    [dir_link] when [true] renders directory labels as wikilinks to
    [dir/index]; when [false] renders them as plain text. *)
let toc_cmark_list
      ?(path_prefix : string = "")
      ?(dir_link : bool = false)
      (paths : string list)
  : Cmarkit.Block.t
  =
  let m : Cmarkit.Meta.t = Cmarkit.Meta.none in
  let text (s : string) : Cmarkit.Inline.t = Cmarkit.Inline.Text (s, m) in
  let list_item (block : Cmarkit.Block.t) : Cmarkit.Block.List_item.t Cmarkit.node =
    Cmarkit.Block.List_item.make block, m
  in
  let ul (items : Cmarkit.Block.List_item.t Cmarkit.node list) : Cmarkit.Block.t =
    Cmarkit.Block.List (Cmarkit.Block.List'.make (`Unordered '-') items, m)
  in
  let para (inline : Cmarkit.Inline.t) : Cmarkit.Block.t =
    Cmarkit.Block.Paragraph (Cmarkit.Block.Paragraph.make inline, m)
  in
  let make_leaf_wl ~(full_path : string) ~(display : string option) : Cmarkit.Inline.t =
    let target : string option = Some (strip_md_ext full_path) in
    let note_path : string =
      if String.is_suffix full_path ~suffix:".md"
      then full_path
      else full_path ^ "/index.md"
    in
    let resolved_target : Vault.Resolve.target = Note { path = note_path } in
    Vault.Resolve.make_wikilink
      ~target
      ~fragment:None
      ~display
      ~embed:false
      ~resolved_target
  in
  let rec render_entries ~(prefix : string) (entries : toc_entry list) : Cmarkit.Block.t =
    let items : Cmarkit.Block.List_item.t Cmarkit.node list =
      List.map entries ~f:(fun entry ->
        match entry with
        | Leaf { name; path } ->
          let full_path : string = path_prefix ^ path in
          let display : string option =
            if String.is_empty path_prefix && String.is_empty prefix
            then None
            else Some (strip_md_ext name)
          in
          list_item (para (make_leaf_wl ~full_path ~display))
        | Dir { name; children } ->
          let dir_path : string =
            if String.is_empty prefix then name else prefix ^ "/" ^ name
          in
          let label : Cmarkit.Inline.t =
            if dir_link
            then make_leaf_wl ~full_path:(path_prefix ^ dir_path) ~display:(Some name)
            else text name
          in
          let sub_list : Cmarkit.Block.t = render_entries ~prefix:dir_path children in
          let content : Cmarkit.Block.t =
            Cmarkit.Block.Blocks ([ para label; sub_list ], m)
          in
          list_item content)
    in
    ul items
  in
  render_entries ~prefix:"" (build_toc_entries paths)
;;

let%expect_test "toc_cmark_list" =
  let paths = [ "x/y/z.md"; "x/y/t.md"; "a.jpg"; "x/q.md" ] in
  let block = toc_cmark_list paths in
  let doc = Cmarkit.Doc.make block in
  print_endline (Parse.commonmark_of_doc doc);
  [%expect
    {|
    - [[a.jpg]]
    - x
      - [[x/q|q]]
      - y
        - [[x/y/t|t]]
        - [[x/y/z|z]]
    |}]
;;

let%expect_test "toc_cmark_list with path_prefix" =
  let paths = [ "y/z.md"; "y/t.md"; "q.md" ] in
  let block = toc_cmark_list ~path_prefix:"x/" paths in
  let doc = Cmarkit.Doc.make block in
  print_endline (Parse.commonmark_of_doc doc);
  [%expect
    {|
    - [[x/q|q]]
    - y
      - [[x/y/t|t]]
      - [[x/y/z|z]]
    |}]
;;

(** Create title element for each doc based on their note name.
    Special handling:
    - for home.md, should be `Home`
    - for dir/index.md, should be `dir`
*)
let title_of_path (rel_path : string) : string =
  let basename : string = Filename.basename rel_path in
  let name : string = strip_md_ext basename in
  match name with
  | "home" -> "Home"
  | "index" ->
    let dir : string = Filename.dirname rel_path in
    Filename.basename dir
  | _ -> name
;;

let title (ctx : Vault.t) : html list =
  List.map ctx.docs ~f:(fun (rel_path, _doc) -> title_of_path rel_path)
;;

module Backlink = struct
  (** Path that a resolved target points to. *)
  let path_of_resolved (target : Vault.Resolve.target) : string option =
    match target with
    | Vault.Resolve.Note { path } -> Some path
    | Vault.Resolve.File { path } -> Some path
    | Vault.Resolve.Heading { path; _ } -> Some path
    | Vault.Resolve.Block { path; _ } -> Some path
    | Vault.Resolve.Curr_file
    | Vault.Resolve.Curr_heading _
    | Vault.Resolve.Curr_block _
    | Vault.Resolve.Unresolved -> None
  ;;

  (** Check whether an inline tree contains a resolved link to [target_path]. *)
  let inline_links_to (target_path : string) (inline : Cmarkit.Inline.t) : bool =
    let folder =
      Cmarkit.Folder.make
        ~inline:(fun _f acc (i : Cmarkit.Inline.t) ->
          if acc
          then Cmarkit.Folder.ret true
          else (
            match i with
            | Cmarkit.Inline.Link (_, meta) | Cmarkit.Inline.Image (_, meta) ->
              (match Cmarkit.Meta.find Vault.Resolve.resolved_key meta with
               | Some resolved ->
                 (match path_of_resolved resolved with
                  | Some p when String.equal p target_path -> Cmarkit.Folder.ret true
                  | _ -> Cmarkit.Folder.default)
               | None -> Cmarkit.Folder.default)
            | _ -> Cmarkit.Folder.default))
        ~inline_ext_default:(fun _f acc i ->
          if acc
          then acc
          else (
            match i with
            | Parse.Wikilink.Ext_wikilink (_, meta) ->
              (match Cmarkit.Meta.find Vault.Resolve.resolved_key meta with
               | Some resolved ->
                 (match path_of_resolved resolved with
                  | Some p when String.equal p target_path -> true
                  | _ -> acc)
               | None -> acc)
            | _ -> acc))
        ~block_ext_default:(fun _f acc _b -> acc)
        ()
    in
    Cmarkit.Folder.fold_inline folder false inline
  ;;

  (** Check whether a block contains a resolved link to [target_path]. *)
  let block_links_to (target_path : string) (block : Cmarkit.Block.t) : bool =
    let folder =
      Cmarkit.Folder.make
        ~inline:(fun _f acc (i : Cmarkit.Inline.t) ->
          if acc
          then Cmarkit.Folder.ret true
          else (
            match i with
            | Cmarkit.Inline.Link (_, meta) | Cmarkit.Inline.Image (_, meta) ->
              (match Cmarkit.Meta.find Vault.Resolve.resolved_key meta with
               | Some resolved ->
                 (match path_of_resolved resolved with
                  | Some p when String.equal p target_path -> Cmarkit.Folder.ret true
                  | _ -> Cmarkit.Folder.default)
               | None -> Cmarkit.Folder.default)
            | _ -> Cmarkit.Folder.default))
        ~inline_ext_default:(fun _f acc i ->
          if acc
          then acc
          else (
            match i with
            | Parse.Wikilink.Ext_wikilink (_, meta) ->
              (match Cmarkit.Meta.find Vault.Resolve.resolved_key meta with
               | Some resolved ->
                 (match path_of_resolved resolved with
                  | Some p when String.equal p target_path -> true
                  | _ -> acc)
               | None -> acc)
            | _ -> acc))
        ~block_ext_default:(fun _f acc _b -> acc)
        ()
    in
    Cmarkit.Folder.fold_block folder false block
  ;;

  (** Render a single block to HTML using the oystermark renderer. *)
  let render_block (block : Cmarkit.Block.t) : string =
    let doc : Cmarkit.Doc.t = Cmarkit.Doc.make block in
    Html.of_doc ~backend_blocks:true ~safe:false doc
  ;;

  (** Extract the minimal leaf blocks from [doc] that contain a link to
      [target_path], rendered as HTML strings. Descends into container blocks
      (lists, block quotes, splicing) to find the innermost paragraph or heading
      that mentions the target. *)
  let extract_backlink_blocks (target_path : string) (doc : Cmarkit.Doc.t) : string list =
    let rec collect (b : Cmarkit.Block.t) : string list =
      match b with
      | Cmarkit.Block.Paragraph (p, _meta) ->
        let inline : Cmarkit.Inline.t = Cmarkit.Block.Paragraph.inline p in
        if inline_links_to target_path inline then [ render_block b ] else []
      | Cmarkit.Block.Heading (h, _meta) ->
        let inline : Cmarkit.Inline.t = Cmarkit.Block.Heading.inline h in
        if inline_links_to target_path inline then [ render_block b ] else []
      | Cmarkit.Block.List (l, _meta) ->
        let items : Cmarkit.Block.List_item.t Cmarkit.node list =
          Cmarkit.Block.List'.items l
        in
        List.concat_map items ~f:(fun (item, _item_meta) ->
          collect (Cmarkit.Block.List_item.block item))
      | Cmarkit.Block.Block_quote (bq, _meta) ->
        collect (Cmarkit.Block.Block_quote.block bq)
      | Cmarkit.Block.Blocks (blocks, _meta) -> List.concat_map blocks ~f:collect
      | _ -> []
    in
    collect (Cmarkit.Doc.block doc)
  ;;

  (** Render backlinks for [rel_path]: links grouped by source file under
      [<details>] elements. Each item shows the minimum containing block
      rendered as HTML. *)
  let backlinks (rel_path : string) : vault_component =
    fun (vault : Vault.t) ->
    let sources : (string * string list) list =
      List.filter_map vault.docs ~f:(fun (src_path, doc) ->
        let blocks : string list = extract_backlink_blocks rel_path doc in
        match blocks with
        | [] -> None
        | _ -> Some (src_path, blocks))
      |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
    in
    match sources with
    | [] -> ""
    | groups ->
      let items : string list =
        List.map groups ~f:(fun (src_path, blocks) ->
          let src_href : string = Html.note_url_path src_path in
          let src_name : string = strip_md_ext (Filename.basename src_path) in
          let block_items : string =
            List.map blocks ~f:(fun block_html ->
              spf {|<li class="backlink-context">%s</li>|} block_html)
            |> String.concat ~sep:"\n"
          in
          spf
            {|<li style="list-style: none"><details open><summary><a href="%s">%s</a></summary><ul>
  %s
  </ul></details></li>|}
            src_href
            src_name
            block_items)
      in
      spf
        {|<div class="backlinks"><h2>Backlinks</h2>
  <ul>
  %s
  </ul>
  </div>|}
        (String.concat ~sep:"\n" items)
  ;;

  module For_test = struct
    (** Build a resolved wikilink inline pointing to [path]. *)
    let make_test_wikilink (path : string) : Cmarkit.Inline.t =
      Vault.Resolve.make_wikilink
        ~target:(Some path)
        ~fragment:None
        ~display:None
        ~embed:false
        ~resolved_target:(Vault.Resolve.Note { path = path ^ ".md" })
    ;;

    (** Build a paragraph block from an inline. *)
    let make_para (inline : Cmarkit.Inline.t) : Cmarkit.Block.t =
      Cmarkit.Block.Paragraph (Cmarkit.Block.Paragraph.make inline, Cmarkit.Meta.none)
    ;;

    (** Build a doc from a list of blocks. *)
    let make_doc (blocks : Cmarkit.Block.t list) : Cmarkit.Doc.t =
      Cmarkit.Doc.make (Cmarkit.Block.Blocks (blocks, Cmarkit.Meta.none))
    ;;

    (** Build a single-item unordered list from a block. *)
    let make_list_item (block : Cmarkit.Block.t) : Cmarkit.Block.t =
      let item : Cmarkit.Block.List_item.t = Cmarkit.Block.List_item.make block in
      Cmarkit.Block.List
        ( Cmarkit.Block.List'.make (`Unordered '-') [ item, Cmarkit.Meta.none ]
        , Cmarkit.Meta.none )
    ;;

    (** Splice inlines together with space between. *)
    let inlines (parts : Cmarkit.Inline.t list) : Cmarkit.Inline.t =
      Cmarkit.Inline.Inlines (parts, Cmarkit.Meta.none)
    ;;

    let text (s : string) : Cmarkit.Inline.t = Cmarkit.Inline.Text (s, Cmarkit.Meta.none)

    let%expect_test "extract_backlink_blocks: paragraph with link" =
      let wl : Cmarkit.Inline.t = make_test_wikilink "A" in
      let para : Cmarkit.Block.t =
        make_para (inlines [ text "see "; wl; text " here" ])
      in
      let doc : Cmarkit.Doc.t = make_doc [ para ] in
      let blocks : string list = extract_backlink_blocks "A.md" doc in
      List.iter blocks ~f:print_string;
      [%expect {| <p>see <a href="/A/">A</a> here</p> |}]
    ;;

    let%expect_test "extract_backlink_blocks: no match" =
      let wl : Cmarkit.Inline.t = make_test_wikilink "B" in
      let doc : Cmarkit.Doc.t = make_doc [ make_para (inlines [ text "see "; wl ]) ] in
      let blocks : string list = extract_backlink_blocks "A.md" doc in
      Printf.printf "count: %d" (List.length blocks);
      [%expect {| count: 0 |}]
    ;;

    let%expect_test "extract_backlink_blocks: list item extracts only matching paragraph" =
      let wl : Cmarkit.Inline.t = make_test_wikilink "A" in
      let para1 : Cmarkit.Block.t = make_para (text "unrelated text") in
      let para2 : Cmarkit.Block.t = make_para (inlines [ text "this mentions "; wl ]) in
      let list_block : Cmarkit.Block.t =
        make_list_item (Cmarkit.Block.Blocks ([ para1; para2 ], Cmarkit.Meta.none))
      in
      let doc : Cmarkit.Doc.t = make_doc [ list_block ] in
      let blocks : string list = extract_backlink_blocks "A.md" doc in
      List.iter blocks ~f:print_string;
      [%expect {| <p>this mentions <a href="/A/">A</a></p> |}]
    ;;

    let%expect_test "extract_backlink_blocks: multiple list items, only matching ones" =
      let wl : Cmarkit.Inline.t = make_test_wikilink "A" in
      let item1 : Cmarkit.Block.List_item.t =
        Cmarkit.Block.List_item.make (make_para (text "no link here"))
      in
      let item2 : Cmarkit.Block.List_item.t =
        Cmarkit.Block.List_item.make (make_para (inlines [ text "links to "; wl ]))
      in
      let item3 : Cmarkit.Block.List_item.t =
        Cmarkit.Block.List_item.make (make_para (text "also unrelated"))
      in
      let list_block : Cmarkit.Block.t =
        Cmarkit.Block.List
          ( Cmarkit.Block.List'.make
              (`Unordered '-')
              [ item1, Cmarkit.Meta.none
              ; item2, Cmarkit.Meta.none
              ; item3, Cmarkit.Meta.none
              ]
          , Cmarkit.Meta.none )
      in
      let doc : Cmarkit.Doc.t = make_doc [ list_block ] in
      let blocks : string list = extract_backlink_blocks "A.md" doc in
      List.iter blocks ~f:print_string;
      [%expect {| <p>links to <a href="/A/">A</a></p> |}]
    ;;

    let%expect_test "extract_backlink_blocks: nested blockquote" =
      let wl : Cmarkit.Inline.t = make_test_wikilink "A" in
      let para : Cmarkit.Block.t = make_para (inlines [ text "quoted "; wl ]) in
      let bq : Cmarkit.Block.t =
        Cmarkit.Block.Block_quote (Cmarkit.Block.Block_quote.make para, Cmarkit.Meta.none)
      in
      let doc : Cmarkit.Doc.t = make_doc [ bq ] in
      let blocks : string list = extract_backlink_blocks "A.md" doc in
      List.iter blocks ~f:print_string;
      [%expect {| <p>quoted <a href="/A/">A</a></p> |}]
    ;;
  end
end

(** Generate breadcrumb navigation HTML for a page given its URL path.
    Always includes a Home link. Adds intermediate directory links as ancestors.
    Does not include the current page itself.
    Example: url_path="/foo/bar/" → Home / foo *)
let nav_of_url_path ?(home_path = "home.md") (url_path : string) : html =
  let sep : string = {|<span class="sep">/</span>|} in
  let home_href = Html.note_url_path home_path in
  let home : string = {%string|<a href="%{home_href}">Home</a>|} in
  match url_path with
  | "/" -> ""
  | p when String.equal p home_href -> ""
  | _ ->
    let trimmed : string =
      url_path
      |> String.chop_prefix_exn ~prefix:"/"
      |> String.chop_suffix_exn ~suffix:"/"
    in
    let parts : string list =
      String.split trimmed ~on:'/'
      |> List.filter ~f:(fun s -> not (String.is_empty s))
    in
    (* Drop the last segment (the current page) *)
    let ancestors : string list = List.take parts (List.length parts - 1) in
    let crumbs : string list =
      List.mapi ancestors ~f:(fun i name ->
        let href : string =
          "/" ^ String.concat ~sep:"/" (List.take parts (i + 1)) ^ "/"
        in
        spf {|<a href="%s">%s</a>|} href name)
    in
    spf
      {|<nav class="breadcrumb">%s</nav>|}
      (String.concat ~sep (home :: crumbs))
;;

let backlinks = Backlink.backlinks
