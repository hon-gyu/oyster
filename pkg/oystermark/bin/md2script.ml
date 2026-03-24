(** Convert between OysterMark documents and jupytext percent-format Python scripts.

    The percent format delimits cells with [# %%] markers:
    - [# %%] for code cells (Python)
    - [# %% [markdown]] for markdown cells (each line prefixed with [# ])

    Frontmatter [oyster.pyproject] maps to PEP 723 inline script metadata. *)

open Core
module Attribute = Parse.Attribute
module Frontmatter = Parse.Frontmatter

(* Helpers
==================== *)

let is_python_lang (lang : string) : bool =
  let lang' = String.lowercase lang in
  String.equal lang' "python" || String.equal lang' "py"
;;

(** Classify a block as a Python code block or not. *)
let python_code_block_content (b : Cmarkit.Block.t) : string option =
  match b with
  | Cmarkit.Block.Code_block (cb, meta) ->
    let cb_info = Cmarkit.Meta.find Attribute.meta_key meta in
    (match cb_info with
     | Some { lang; _ } when is_python_lang lang ->
       let content =
         List.map (Cmarkit.Block.Code_block.code cb) ~f:Cmarkit.Block_line.to_string
         |> String.concat ~sep:"\n"
       in
       Some content
     | _ -> None)
  | _ -> None
;;

(** Prefix every line of [s] with [# ].
    Empty lines become just [#]. *)
let comment_lines (s : string) : string =
  String.split_lines s
  |> List.map ~f:(fun line -> if String.is_empty line then "#" else "# " ^ line)
  |> String.concat ~sep:"\n"
;;

(** Strip [# ] prefix from every line.
    Lines that are just [#] become empty lines. *)
let uncomment_lines (s : string) : string =
  String.split_lines s
  |> List.map ~f:(fun line ->
    if String.equal (String.rstrip line) "#"
    then ""
    else if String.is_prefix line ~prefix:"# "
    then String.chop_prefix_exn line ~prefix:"# "
    else line)
  |> String.concat ~sep:"\n"
;;

(* PEP 723 inline script metadata
==================== *)

(** Render [oyster.pyproject] config as a PEP 723 inline script metadata block.

    {v
    # /// script
    # requires-python = ">=3.13"
    # dependencies = ["numpy", "pandas"]
    # ///
    v} *)
let pep723_of_config (config : Yaml.value) : string option =
  (* TODO: maybe we should use a proper TOML parser *)
  match Yaml.Util.find "pyproject" config with
  | Ok (Some (`O fields)) ->
    let lines = Buffer.create 64 in
    Buffer.add_string lines "# /// script\n";
    (match List.Assoc.find fields ~equal:String.equal "version" with
     | Some (`Float v) ->
       let ver =
         if Float.is_integer v then sprintf "%d" (Float.to_int v) else sprintf "%g" v
       in
       Buffer.add_string lines (sprintf "# requires-python = \">=%s\"\n" ver)
     | Some (`String s) ->
       Buffer.add_string lines (sprintf "# requires-python = \">=%s\"\n" s)
     | _ -> ());
    (match List.Assoc.find fields ~equal:String.equal "dependencies" with
     | Some (`A deps) ->
       let dep_strs =
         List.filter_map deps ~f:(function
           | `String s -> Some (sprintf "\"%s\"" s)
           | _ -> None)
       in
       Buffer.add_string
         lines
         (sprintf "# dependencies = [%s]\n" (String.concat ~sep:", " dep_strs))
     | _ -> ());
    Buffer.add_string lines "# ///";
    Some (Buffer.contents lines)
  | _ -> None
;;

(** Parse a PEP 723 inline script metadata block back to [oyster.pyproject] YAML. *)
let config_of_pep723 (block : string) : Yaml.value =
  let lines = String.split_lines block in
  let fields = ref [] in
  List.iter lines ~f:(fun line ->
    let line = String.strip line in
    (* Strip leading "# " *)
    let line =
      if String.is_prefix line ~prefix:"# "
      then String.chop_prefix_exn line ~prefix:"# "
      else line
    in
    if String.is_prefix line ~prefix:"requires-python"
    then (
      (* requires-python = ">=3.13" → version: "3.13" *)
      match String.lsplit2 line ~on:'=' with
      | Some (_, rhs) ->
        let rhs = String.strip rhs in
        let ver =
          String.strip ~drop:(fun c -> Char.equal c '"') rhs
          |> String.chop_prefix_if_exists ~prefix:">="
        in
        fields := ("version", `String ver) :: !fields
      | None -> ())
    else if String.is_prefix line ~prefix:"dependencies"
    then (
      match String.lsplit2 line ~on:'=' with
      | Some (_, rhs) ->
        let rhs = String.strip rhs in
        (* Parse ["dep1", "dep2"] *)
        let inner =
          String.strip ~drop:(fun c -> Char.equal c '[' || Char.equal c ']') rhs
        in
        let deps =
          String.split inner ~on:','
          |> List.filter_map ~f:(fun s ->
            let s = String.strip s in
            if String.is_empty s
            then None
            else Some (`String (String.strip ~drop:(fun c -> Char.equal c '"') s)))
        in
        fields := ("dependencies", `A deps) :: !fields
      | None -> ()));
  `O (List.rev !fields)
;;

(* Encode: Doc → percent-format script
==================== *)

(** Segment a document's top-level blocks into alternating
    markdown-cell / code-cell groups. *)
type segment =
  | Markdown_cell of Cmarkit.Block.t list
  | Code_cell of string (** Python code content *)

(** Walk top-level blocks and group them into segments.
    Python code blocks become [Code_cell]; everything else accumulates
    into [Markdown_cell] groups. *)
let segment_blocks (blocks : Cmarkit.Block.t list) : segment list =
  let flush_md acc md_acc =
    match md_acc with
    | [] -> acc
    | bs -> Markdown_cell (List.rev bs) :: acc
  in
  let acc, md_acc =
    List.fold blocks ~init:([], []) ~f:(fun (acc, md_acc) block ->
      match python_code_block_content block with
      | Some content ->
        let acc = flush_md acc md_acc in
        Code_cell content :: acc, []
      | None -> acc, block :: md_acc)
  in
  flush_md acc md_acc |> List.rev
;;

(** Render a list of blocks as CommonMark by wrapping them in a temporary doc. *)
let blocks_to_commonmark (blocks : Cmarkit.Block.t list) : string =
  let block =
    match blocks with
    | [ b ] -> b
    | bs -> Cmarkit.Block.Blocks (bs, Cmarkit.Meta.none)
  in
  let doc = Cmarkit.Doc.make block in
  Cmarkit_commonmark.of_doc doc
;;

(** Encode an OysterMark document to a jupytext percent-format Python script. *)
let encode (doc : Cmarkit.Doc.t) : string =
  let buf = Buffer.create 1024 in
  let first_cell = ref true in
  let add_cell_sep () =
    if !first_cell then first_cell := false else Buffer.add_char buf '\n'
  in
  (* Frontmatter → PEP 723 *)
  let config =
    match Frontmatter.of_doc doc with
    | Some (`O fields) ->
      (match List.Assoc.find fields ~equal:String.equal "oyster" with
       | Some (`O oys_fields) -> `O oys_fields
       | _ -> `O [])
    | _ -> `O []
  in
  (match pep723_of_config config with
   | Some pep ->
     Buffer.add_string buf pep;
     Buffer.add_char buf '\n';
     first_cell := false
   | None -> ());
  (* Collect top-level blocks, skipping frontmatter *)
  let top_blocks =
    match Cmarkit.Doc.block doc with
    | Cmarkit.Block.Blocks (blocks, _) ->
      List.filter blocks ~f:(fun b ->
        match b with
        | Frontmatter.Frontmatter _ -> false
        | _ -> true)
    | Frontmatter.Frontmatter _ -> []
    | b -> [ b ]
  in
  let segments = segment_blocks top_blocks in
  List.iter segments ~f:(fun seg ->
    match seg with
    | Markdown_cell blocks ->
      let md = blocks_to_commonmark blocks |> String.rstrip in
      if not (String.is_empty md)
      then (
        add_cell_sep ();
        Buffer.add_string buf "# %% [markdown]\n";
        Buffer.add_string buf (comment_lines md);
        Buffer.add_char buf '\n')
    | Code_cell content ->
      add_cell_sep ();
      Buffer.add_string buf "# %%\n";
      Buffer.add_string buf content;
      Buffer.add_char buf '\n');
  Buffer.contents buf
;;

(* Decode: percent-format script → Doc
==================== *)

type raw_cell =
  | Raw_markdown of string
  | Raw_code of string
  | Raw_pep723 of string

(** Split a percent-format script into raw cells. *)
let split_cells (script : string) : raw_cell list =
  let lines = String.split_lines script in
  (* First, extract PEP 723 block if present *)
  let pep723, rest =
    match lines with
    | l :: _ when String.is_prefix (String.strip l) ~prefix:"# /// script" ->
      let rec collect_pep acc remaining =
        match remaining with
        | [] -> None, lines (* unclosed → treat all as regular *)
        | l :: tl ->
          if
            String.is_prefix (String.strip l) ~prefix:"# ///"
            && not (String.is_prefix (String.strip l) ~prefix:"# /// script")
          then Some (String.concat ~sep:"\n" (List.rev (l :: acc))), tl
          else collect_pep (l :: acc) tl
      in
      collect_pep [ l ] (List.tl_exn lines)
    | _ -> None, lines
  in
  let cells = ref [] in
  (match pep723 with
   | Some p -> cells := [ Raw_pep723 p ]
   | None -> ());
  (* Split remaining lines by # %% markers *)
  let current_lines = ref [] in
  let current_is_markdown = ref false in
  let flush () =
    match !current_lines with
    | [] -> ()
    | ls ->
      let content = String.concat ~sep:"\n" (List.rev ls) in
      if !current_is_markdown
      then cells := Raw_markdown content :: !cells
      else cells := Raw_code content :: !cells;
      current_lines := [];
      current_is_markdown := false
  in
  let started = ref false in
  List.iter rest ~f:(fun line ->
    let stripped = String.strip line in
    if String.equal stripped "# %% [markdown]"
    then (
      flush ();
      started := true;
      current_is_markdown := true)
    else if String.equal stripped "# %%"
    then (
      flush ();
      started := true;
      current_is_markdown := false)
    else if !started
    then current_lines := line :: !current_lines);
  flush ();
  List.rev !cells
;;

(** Decode a jupytext percent-format Python script to an OysterMark document. *)
let decode (script : string) : Cmarkit.Doc.t =
  let cells = split_cells script in
  let blocks = ref [] in
  let frontmatter = ref None in
  List.iter cells ~f:(fun cell ->
    match cell with
    | Raw_pep723 p ->
      let pyproject = config_of_pep723 p in
      let yaml : Yaml.value = `O [ "oyster", `O [ "pyproject", pyproject ] ] in
      frontmatter := Some yaml
    | Raw_markdown content ->
      let md = uncomment_lines (String.strip content) in
      let doc = Parse.of_string md in
      let inner_blocks =
        match Cmarkit.Doc.block doc with
        | Cmarkit.Block.Blocks (bs, _) -> bs
        | b -> [ b ]
      in
      blocks := List.rev_append inner_blocks !blocks
    | Raw_code content ->
      let content = String.strip content in
      let cb =
        Cmarkit.Block.Code_block.make
          ~info_string:("python", Cmarkit.Meta.none)
          (Cmarkit.Block_line.list_of_string content)
      in
      let meta =
        Cmarkit.Meta.none
        |> Cmarkit.Meta.add
             Attribute.meta_key
             { Attribute.lang = "python"; attribute = None }
      in
      blocks := Cmarkit.Block.Code_block (cb, meta) :: !blocks);
  let all_blocks = List.rev !blocks in
  let all_blocks =
    match !frontmatter with
    | Some fm -> Frontmatter.Frontmatter fm :: all_blocks
    | None -> all_blocks
  in
  let top = Cmarkit.Block.Blocks (all_blocks, Cmarkit.Meta.none) in
  Cmarkit.Doc.make top
;;

(* CLI
==================== *)

let encode_command =
  Command.basic
    ~summary:
      "Convert an OysterMark markdown file to a jupytext percent-format Python script"
    (let%map_open.Command file = anon ("FILE" %: Filename_unix.arg_type) in
     fun () ->
       let content = In_channel.read_all file in
       let doc = Parse.of_string content in
       print_string (encode doc))
;;

let decode_command =
  Command.basic
    ~summary:
      "Convert a jupytext percent-format Python script to an OysterMark markdown file"
    (let%map_open.Command file = anon ("FILE" %: Filename_unix.arg_type) in
     fun () ->
       let content = In_channel.read_all file in
       let doc = decode content in
       print_string (Parse.commonmark_of_doc doc))
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"Convert between OysterMark markdown and jupytext percent-format scripts"
       [ "encode", encode_command; "decode", decode_command ])
;;
