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
      ?(collapsible = false)
      ?(collapsed_by_default = false)
      (paths : string list)
  : html
  =
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
          let href = strip_md_ext path in
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
    Vault.Resolve.make_wikilink ~target ~fragment:None ~display ~embed:false ~resolved_target
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
            then
              make_leaf_wl
                ~full_path:(path_prefix ^ dir_path)
                ~display:(Some name)
            else text name
          in
          let sub_list : Cmarkit.Block.t =
            render_entries ~prefix:dir_path children
          in
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

(** Path that a resolved target points to. *)
let path_of_resolved (target : Vault.Resolve.target) : string option =
  match target with
  | Vault.Resolve.Note { path } -> Some path
  | Vault.Resolve.File { path } -> Some path
  | Vault.Resolve.Heading { path; _ } -> Some path
  | Vault.Resolve.Block { path; _ } -> Some path
  | Vault.Resolve.Curr_file | Vault.Resolve.Curr_heading _ | Vault.Resolve.Curr_block _
  | Vault.Resolve.Unresolved -> None
;;

(** Extract all resolved outgoing links from a doc as [(target_path, link_text)] pairs. *)
let extract_outgoing_links (doc : Cmarkit.Doc.t) : (string * string) list =
  let folder =
    Cmarkit.Folder.make
      ~inline:(fun _f acc (i : Cmarkit.Inline.t) ->
        match i with
        | Cmarkit.Inline.Link (link, meta) | Cmarkit.Inline.Image (link, meta) ->
          (match Cmarkit.Meta.find Vault.Resolve.resolved_key meta with
           | Some resolved ->
             (match path_of_resolved resolved with
              | Some path ->
                let text : string =
                  Parse.inline_to_plain_text (Cmarkit.Inline.Link.text link)
                in
                Cmarkit.Folder.ret ((path, text) :: acc)
              | None -> Cmarkit.Folder.default)
           | None -> Cmarkit.Folder.default)
        | _ -> Cmarkit.Folder.default)
      ~inline_ext_default:(fun _f acc i ->
        match i with
        | Parse.Wikilink.Ext_wikilink (wl, meta) ->
          (match Cmarkit.Meta.find Vault.Resolve.resolved_key meta with
           | Some resolved ->
             (match path_of_resolved resolved with
              | Some path ->
                let text : string = Parse.Wikilink.to_plain_text wl in
                (path, text) :: acc
              | None -> acc)
           | None -> acc)
        | _ -> acc)
      ~block_ext_default:(fun _f acc _b -> acc)
      ()
  in
  Cmarkit.Folder.fold_doc folder [] doc
;;

(** Render backlinks for [rel_path]: links grouped by source file under
    [<details>] elements. Each item shows the link's plain text. *)
let backlinks (rel_path : string) : vault_component =
  fun (vault : Vault.t) ->
  (* Collect (src_path, link_text list) for all docs that link to rel_path *)
  let sources : (string * string list) list =
    List.filter_map vault.docs ~f:(fun (src_path, doc) ->
      let links : (string * string) list = extract_outgoing_links doc in
      let matching_texts : string list =
        List.filter_map links ~f:(fun (target, text) ->
          if String.equal target rel_path then Some text else None)
      in
      match matching_texts with
      | [] -> None
      | texts -> Some (src_path, texts))
    |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  in
  match sources with
  | [] -> ""
  | groups ->
    let items : string list =
      List.map groups ~f:(fun (src_path, texts) ->
        let src_href : string = Html.note_url_path src_path in
        let src_name : string = strip_md_ext (Filename.basename src_path) in
        let link_items : string =
          List.map texts ~f:(fun text -> spf "<li>%s</li>" text)
          |> String.concat ~sep:"\n"
        in
        spf
          {|<li style="list-style: none"><details><summary><a href="%s">%s</a></summary><ul>
%s
</ul></details></li>|}
          src_href
          src_name
          link_items)
    in
    "<ul>\n" ^ String.concat ~sep:"\n" items ^ "\n</ul>"
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
