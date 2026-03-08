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
    of relative paths. Leaf entries become links; directories become plain text
    with a nested sub-list.

    [path_prefix] is prepended to each leaf's file path and wikilink target so
    that resolution produces the correct href, while display text uses only the
    short leaf name.  For example, [toc_cmark_list ~path_prefix:"sub/" ["a.md"]]
    generates a wikilink targeting ["sub/a"] that displays as ["a"]. *)
let toc_cmark_list ?(path_prefix : string = "") (paths : string list) : Cmarkit.Block.t =
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
  let rec render_entries (entries : toc_entry list) : Cmarkit.Block.t =
    let items : Cmarkit.Block.List_item.t Cmarkit.node list =
      List.map entries ~f:(fun entry ->
        match entry with
        | Leaf { name; path } ->
          let full_path : string = path_prefix ^ path in
          let target : string option = Some (strip_md_ext full_path) in
          let display : string option =
            if String.is_empty path_prefix then None else Some (strip_md_ext name)
          in
          let resolved_target : Vault.Resolve.target = File { path = full_path } in
          let wl =
            Vault.Resolve.make_wikilink
              ~target
              ~fragment:None
              ~display
              ~embed:false
              ~resolved_target
          in
          list_item (para wl)
        | Dir { name; children } ->
          let sub_list : Cmarkit.Block.t = render_entries children in
          let content : Cmarkit.Block.t =
            Cmarkit.Block.Blocks ([ para (text name); sub_list ], m)
          in
          list_item content)
    in
    ul items
  in
  render_entries (build_toc_entries paths)
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
      - [[x/q]]
      - y
        - [[x/y/t]]
        - [[x/y/z]]
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

(** Extract all file paths that a resolved doc links to. *)
let extract_outgoing_paths (doc : Cmarkit.Doc.t) : string list =
  let folder =
    Cmarkit.Folder.make
      ~inline:(fun _f acc (i : Cmarkit.Inline.t) ->
        let target_of_meta (meta : Cmarkit.Meta.t) : string option =
          match Cmarkit.Meta.find Vault.Resolve.resolved_key meta with
          | Some (Vault.Resolve.File { path }) -> Some path
          | Some (Vault.Resolve.Heading { path; _ }) -> Some path
          | Some (Vault.Resolve.Block { path; _ }) -> Some path
          | _ -> None
        in
        match i with
        | Cmarkit.Inline.Link (_, meta) | Cmarkit.Inline.Image (_, meta) ->
          (match target_of_meta meta with
           | Some path -> Cmarkit.Folder.ret (path :: acc)
           | None -> Cmarkit.Folder.default)
        | _ -> Cmarkit.Folder.default)
      ~inline_ext_default:(fun _f acc i ->
        match i with
        | Parse.Wikilink.Ext_wikilink (_, meta) ->
          (match Cmarkit.Meta.find Vault.Resolve.resolved_key meta with
           | Some (Vault.Resolve.File { path }) -> path :: acc
           | Some (Vault.Resolve.Heading { path; _ }) -> path :: acc
           | Some (Vault.Resolve.Block { path; _ }) -> path :: acc
           | _ -> acc)
        | _ -> acc)
      ~block_ext_default:(fun _f acc _b -> acc)
      ()
  in
  Cmarkit.Folder.fold_doc folder [] doc |> List.dedup_and_sort ~compare:String.compare
;;

(** Render backlinks for [rel_path]: a [<ul>] of all vault docs that link to it. *)
let backlinks (rel_path : string) : vault_component =
  fun (vault : Vault.t) ->
  let linking_paths =
    List.filter_map vault.docs ~f:(fun (src_path, doc) ->
      let targets = extract_outgoing_paths doc in
      if List.mem targets rel_path ~equal:String.equal then Some src_path else None)
  in
  match linking_paths with
  | [] -> ""
  | paths -> toc_html (List.sort paths ~compare:String.compare)
;;
