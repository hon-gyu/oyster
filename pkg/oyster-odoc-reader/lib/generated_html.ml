module Soup = Soup

type note =
  { path : string
  ; body : string
  }

type page =
  { html_path : string
  ; note_path : string
  ; header : string
  ; preamble : string
  ; content : string
  }

let ( let* ) result f = Result.bind result f

let suffix ~affix s =
  let n = String.length s
  and m = String.length affix in
  n >= m && String.equal (String.sub s (n - m) m) affix
;;

let chop_suffix ~affix s = String.sub s 0 (String.length s - String.length affix)

let rec files dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.sort compare
  |> List.concat_map (fun name ->
    let path = Filename.concat dir name in
    if Sys.is_directory path then files path else [ path ])
;;

let relative ~to_:root path =
  let prefix =
    if suffix ~affix:Filename.dir_sep root then root else root ^ Filename.dir_sep
  in
  if
    String.length path >= String.length prefix
    && String.equal (String.sub path 0 (String.length prefix)) prefix
  then String.sub path (String.length prefix) (String.length path - String.length prefix)
  else path
;;

let json_string field json =
  match Yojson.Safe.Util.member field json with
  | `String s -> Ok s
  | _ -> Error (Printf.sprintf "missing JSON string field %S" field)
;;

let current_kind json =
  match Yojson.Safe.Util.member "breadcrumbs" json with
  | `List crumbs ->
    (match List.rev crumbs with
     | `Assoc fields :: _ ->
       (match List.assoc_opt "kind" fields with
        | Some (`String kind) -> Some kind
        | _ -> None)
     | _ -> None)
  | _ -> None
;;

let page_of_file ~root file =
  try
    let json = Yojson.Safe.from_file file in
    let* header = json_string "header" json in
    let* preamble = json_string "preamble" json in
    let* content = json_string "content" json in
    let rel_json = relative ~to_:root file in
    let html_path = chop_suffix ~affix:".json" rel_json in
    let raw_note = chop_suffix ~affix:".html" html_path in
    let note_path =
      if
        Filename.basename raw_note = "index"
        && current_kind json <> Some "page"
        && current_kind json <> Some "leaf-page"
      then Filename.dirname raw_note
      else raw_note
    in
    let header =
      match current_kind json with
      | Some "page" | Some "leaf-page" -> header
      | _ -> ""
    in
    Ok { html_path; note_path; header; preamble; content }
  with
  | Yojson.Json_error message -> Error (file ^ ": " ^ message)
  | Sys_error message -> Error message
  | Invalid_argument _ -> Error (file ^ ": unexpected odoc JSON filename")
;;

let normalize_path path =
  let rec loop acc = function
    | [] -> List.rev acc
    | "" :: rest | "." :: rest -> loop acc rest
    | ".." :: rest ->
      loop
        (match acc with
         | [] -> []
         | _ :: acc -> acc)
        rest
    | part :: rest -> loop (part :: acc) rest
  in
  String.split_on_char '/' path |> loop [] |> String.concat "/"
;;

let split_fragment href =
  match String.index_opt href '#' with
  | None -> href, ""
  | Some i -> String.sub href 0 i, String.sub href (i + 1) (String.length href - i - 1)
;;

let percent_decode s =
  let hex = function
    | '0' .. '9' as c -> Some (Char.code c - Char.code '0')
    | 'a' .. 'f' as c -> Some (10 + Char.code c - Char.code 'a')
    | 'A' .. 'F' as c -> Some (10 + Char.code c - Char.code 'A')
    | _ -> None
  in
  let b = Buffer.create (String.length s) in
  let rec loop i =
    if i < String.length s
    then
      if i + 2 < String.length s && Char.equal s.[i] '%'
      then (
        match hex s.[i + 1], hex s.[i + 2] with
        | Some high, Some low ->
          Buffer.add_char b (Char.chr ((high * 16) + low));
          loop (i + 3)
        | _ ->
          Buffer.add_char b s.[i];
          loop (i + 1))
      else (
        Buffer.add_char b s.[i];
        loop (i + 1))
  in
  loop 0;
  Buffer.contents b
;;

let external_href href =
  String.starts_with ~prefix:"//" href
  ||
  match String.index_opt href ':' with
  | None -> false
  | Some colon ->
    (match String.index_opt href '/' with
     | None -> true
     | Some slash -> colon < slash)
;;

type render_state =
  { page : page
  ; notes_by_html : (string * string) list
  }

let target st href =
  if external_href href
  then `External href
  else (
    let path, fragment = split_fragment href in
    let path = percent_decode path
    and fragment = percent_decode fragment in
    let html_path =
      if String.equal path ""
      then st.page.html_path
      else normalize_path (Filename.concat (Filename.dirname st.page.html_path) path)
    in
    let note_path =
      match List.assoc_opt html_path st.notes_by_html with
      | Some path -> path
      | None ->
        if suffix ~affix:"/index.html" html_path
        then chop_suffix ~affix:"/index.html" html_path
        else if suffix ~affix:".html" html_path
        then chop_suffix ~affix:".html" html_path
        else html_path
    in
    let note_path = if String.equal note_path st.page.note_path then "" else note_path in
    let fragment = Address.anchor fragment in
    let target =
      if String.equal fragment "" then note_path else note_path ^ "#" ^ fragment
    in
    `Internal target)
;;

let classes node = Soup.classes node
let has_class name node = List.mem name (classes node)

let fence content =
  let longest = ref 0
  and run = ref 0 in
  String.iter
    (fun c ->
       if Char.equal c '`'
       then (
         incr run;
         longest := max !longest !run)
       else run := 0)
    content;
  String.make (max 3 (!longest + 1)) '`'
;;

let code_fence ?(info = "") content =
  let f = fence content in
  Printf.sprintf "%s%s\n%s\n%s\n\n" f info content f
;;

let inline_ticks content =
  let longest = ref 0
  and run = ref 0 in
  String.iter
    (fun c ->
       if Char.equal c '`'
       then (
         incr run;
         longest := max !longest !run)
       else run := 0)
    content;
  String.make (max 1 (!longest + 1)) '`'
;;

let rec inline st node =
  match Soup.element node with
  | None -> Option.value ~default:"" (Soup.leaf_text node)
  | Some element ->
    let contents () =
      Soup.children element |> Soup.to_list |> List.map (inline st) |> String.concat ""
    in
    (match Soup.name element with
     | "a" when has_class "anchor" element -> ""
     | "a" ->
       let label = Soup.texts element |> String.concat "" in
       (match Soup.attribute "href" element with
        | None -> label
        | Some href ->
          (match target st href with
           | `External href -> Printf.sprintf "[%s](%s)" label href
           | `Internal href ->
             if String.equal href label
             then Printf.sprintf "[[%s]]" href
             else Printf.sprintf "[[%s|%s]]" href label))
     | "code" ->
       let code = contents () in
       let ticks = inline_ticks code in
       ticks ^ code ^ ticks
     | "em" | "i" -> "*" ^ contents () ^ "*"
     | "strong" | "b" -> "**" ^ contents () ^ "**"
     | "sup" -> "^" ^ contents () ^ "^"
     | "sub" -> "~" ^ contents () ^ "~"
     | "br" -> "\\\n"
     | "img" ->
       let alt = Option.value ~default:"" (Soup.attribute "alt" element) in
       let src = Option.value ~default:"" (Soup.attribute "src" element) in
       Printf.sprintf "![%s](%s)" alt src
     | _ -> contents ())
;;

let indented prefix s =
  String.split_on_char '\n' s
  |> List.mapi (fun i line ->
    if i = 0 || String.equal line "" then line else prefix ^ line)
  |> String.concat "\n"
;;

let rec blocks ?(indent = "") st node =
  match Soup.element node with
  | None -> Option.value ~default:"" (Soup.leaf_text node)
  | Some element ->
    let children () =
      Soup.children element
      |> Soup.to_list
      |> List.map (blocks ~indent st)
      |> String.concat ""
    in
    (match Soup.name element with
     | "p" -> indent ^ inline st (Soup.coerce element) ^ "\n\n"
     | ("h1" | "h2" | "h3" | "h4" | "h5" | "h6") as tag ->
       let level = int_of_string (String.sub tag 1 1) in
       let anchor =
         match Soup.id element with
         | None -> ""
         | Some id -> Address.attribute id ^ "\n"
       in
       anchor
       ^ String.make (min 6 level) '#'
       ^ " "
       ^ inline st (Soup.coerce element)
       ^ "\n\n"
     | "ul" | "ol" ->
       Soup.children element
       |> Soup.elements
       |> Soup.to_list
       |> List.mapi (fun i item ->
         let marker =
           if Soup.name element = "ol" then string_of_int (i + 1) ^ "." else "-"
         in
         let body = blocks ~indent:(indent ^ "  ") st (Soup.coerce item) |> String.trim in
         indent ^ marker ^ " " ^ indented (indent ^ "  ") body ^ "\n\n")
       |> String.concat ""
       |> fun s -> s ^ "\n"
     | "li" -> children ()
     | "pre" -> code_fence (Soup.texts element |> String.concat "")
     | "blockquote" ->
       children ()
       |> String.trim
       |> String.split_on_char '\n'
       |> List.map (fun line -> "> " ^ line ^ "\n")
       |> String.concat ""
       |> fun s -> s ^ "\n"
     | "dl" ->
       let pending = ref None in
       Soup.children element
       |> Soup.elements
       |> Soup.to_list
       |> List.map (fun child ->
         match Soup.name child with
         | "dt" ->
           pending := Some (inline st (Soup.coerce child));
           ""
         | "dd" ->
           let label = Option.value ~default:"" !pending in
           pending := None;
           Printf.sprintf "- %s: %s\n" label (String.trim (blocks st (Soup.coerce child)))
         | _ -> blocks st (Soup.coerce child))
       |> String.concat ""
       |> fun s -> s ^ "\n"
     | "table" -> table st element
     | "hr" -> "---\n\n"
     | "div" when has_class "odoc-spec" element -> spec st element
     | "details" -> children ()
     | "summary" ->
       code_fence ~info:"ocaml" (Soup.texts element |> String.concat "" |> String.trim)
     | "span" when has_class "comment-delim" element -> ""
     | "code" | "a" | "em" | "i" | "strong" | "b" | "sup" | "sub" | "span" | "img" ->
       inline st (Soup.coerce element)
     | _ -> children ())

and table st element =
  let rows = Soup.select "tr" element |> Soup.to_list in
  let row node =
    Soup.children node
    |> Soup.elements
    |> Soup.to_list
    |> List.filter (fun cell -> Soup.name cell = "th" || Soup.name cell = "td")
    |> List.map (fun cell -> inline st (Soup.coerce cell) |> String.trim)
  in
  match List.map row rows with
  | [] -> ""
  | header :: rest ->
    let line cells = "| " ^ String.concat " | " cells ^ " |\n" in
    line header
    ^ line (List.map (fun _ -> "---") header)
    ^ String.concat "" (List.map line rest)
    ^ "\n"

and spec st wrapper =
  match Soup.select_one "> .spec" wrapper with
  | None ->
    Soup.children wrapper |> Soup.to_list |> List.map (blocks st) |> String.concat ""
  | Some declaration ->
    let rec code node =
      match Soup.element node with
      | None -> Option.value ~default:"" (Soup.leaf_text node)
      | Some element when has_class "def-doc" element || has_class "comment-delim" element
        -> ""
      | Some element when Soup.name element = "a" && has_class "anchor" element -> ""
      | Some element when Soup.name element = "li" && has_class "def" element ->
        let text =
          match Soup.select_one "> code" element with
          | None -> ""
          | Some code_node -> Soup.texts code_node |> String.concat ""
        in
        "\n  " ^ text
      | Some element when Soup.name element = "ol" || Soup.name element = "ul" ->
        (Soup.children element |> Soup.to_list |> List.map code |> String.concat "")
        ^ "\n"
      | Some element ->
        Soup.children element |> Soup.to_list |> List.map code |> String.concat ""
    in
    let declaration_code = code (Soup.coerce declaration) |> String.trim in
    let anchor =
      match Soup.id declaration with
      | None -> ""
      | Some id -> Address.attribute id ^ "\n"
    in
    let refs =
      Soup.select "a[href]" declaration
      |> Soup.to_list
      |> List.filter (fun link -> not (has_class "anchor" link))
      |> List.filter_map (fun link ->
        match Soup.attribute "href" link with
        | None -> None
        | Some href ->
          (match target st href with
           | `External _ -> None
           | `Internal target -> Some target))
      |> List.sort_uniq compare
    in
    let refs =
      match refs with
      | [] -> ""
      | refs ->
        "Refs: " ^ String.concat " " (List.map (Printf.sprintf "[[%s]]") refs) ^ "\n\n"
    in
    let members =
      Soup.select "li.def" declaration
      |> Soup.to_list
      |> List.map (fun member ->
        let member_anchor =
          match Soup.id member with
          | None -> ""
          | Some id -> Address.attribute id ^ "\n"
        in
        let label =
          match Soup.select_one "> code" member with
          | None -> ""
          | Some code -> Soup.texts code |> String.concat "" |> String.trim
        in
        let doc =
          match Soup.select_one "> .def-doc" member with
          | None -> "\n\n"
          | Some doc -> ": " ^ String.trim (blocks st (Soup.coerce doc)) ^ "\n\n"
        in
        member_anchor ^ "- `" ^ label ^ "`" ^ doc)
      |> String.concat ""
    in
    let declaration_doc =
      match Soup.select_one "> .spec-doc" wrapper with
      | None -> ""
      | Some doc -> blocks st (Soup.coerce doc)
    in
    anchor ^ code_fence ~info:"ocaml" declaration_code ^ refs ^ members ^ declaration_doc
;;

let render_page notes_by_html page =
  let st = { page; notes_by_html } in
  let render fragment =
    let soup = Soup.parse fragment in
    Soup.children soup |> Soup.to_list |> List.map (blocks st) |> String.concat ""
  in
  { path = page.note_path
  ; body = render page.header ^ render page.preamble ^ render page.content
  }
;;

let of_directory dir =
  let json_files = files dir |> List.filter (suffix ~affix:".html.json") in
  let rec read acc = function
    | [] -> Ok (List.rev acc)
    | file :: rest ->
      (match page_of_file ~root:dir file with
       | Error _ as error -> error
       | Ok page -> read (page :: acc) rest)
  in
  match read [] json_files with
  | Error _ as error -> error
  | Ok pages ->
    let notes_by_html = List.map (fun page -> page.html_path, page.note_path) pages in
    Ok (List.map (render_page notes_by_html) pages)
;;

let rec remove_tree path =
  if Sys.file_exists path
  then
    if Sys.is_directory path
    then (
      Sys.readdir path |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path)
    else Unix.unlink path
;;

let generate file =
  let dir = Filename.temp_dir "oyster-odoc-reader-" "" in
  Fun.protect
    ~finally:(fun () -> remove_tree dir)
    (fun () ->
       let file = Fpath.to_string file in
       let argv = [| "odoc"; "html-generate"; "--as-json"; "-o"; dir; file |] in
       try
         let pid = Unix.create_process "odoc" argv Unix.stdin Unix.stdout Unix.stderr in
         match snd (Unix.waitpid [] pid) with
         | Unix.WEXITED 0 -> of_directory dir
         | Unix.WEXITED n ->
           Error (Printf.sprintf "%s: odoc exited with status %d" file n)
         | Unix.WSIGNALED n ->
           Error (Printf.sprintf "%s: odoc was killed by signal %d" file n)
         | Unix.WSTOPPED n ->
           Error (Printf.sprintf "%s: odoc stopped on signal %d" file n)
       with
       | Unix.Unix_error (error, function_, argument) ->
         Error
           (Printf.sprintf
              "%s: %s(%s): %s"
              file
              function_
              argument
              (Unix.error_message error)))
;;
