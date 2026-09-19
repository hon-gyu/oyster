(** {1 Vault-wide rename planning}

  This describes byte edits and file
  moves; clients decide how to apply or encode them.

  {@meta[
  ai-disclosure: autonomous
  ]}
*)

open Core
module Index = Index

(** {2 Rename target} *)

type target =
  { path : string
  ; address : Note.Anchor.Address.t option (** [None] is the note itself. *)
  }
[@@deriving sexp, equal]

type edit =
  { rel_path : string
  ; first_byte : int
  ; last_byte : int (** Exclusive. *)
  ; new_text : string
  }
[@@deriving sexp, equal, compare]

type change =
  { edits : edit list (** Addressed by pre-move paths. *)
  ; moves : (string * string) list
    (** Files or directories to move, source to destination, after the edits. *)
  }
[@@deriving sexp, equal]

let valid_note_name s =
  (not (String.is_empty s))
  && (not (String.exists s ~f:(fun c -> Char.equal c '/' || Char.equal c '\\')))
  && not (String.equal s "." || String.equal s "..")
;;

let valid_id s =
  (not (String.is_empty s))
  && String.for_all s ~f:(fun c ->
    Char.is_alphanum c || Char.equal c '-' || Char.equal c '_')
;;

let renamed_note_path ~path ~new_name =
  let new_name =
    if String.is_suffix new_name ~suffix:".md" then new_name else new_name ^ ".md"
  in
  match Index.Path.dirname path with
  | None -> new_name
  | Some dir -> dir ^ "/" ^ new_name
;;

let destination_path (_, _, resolution) =
  Result.ok resolution |> Option.map ~f:Index.target_path
;;

let matches { path; address } ((_, _, resolution) as link) =
  let same_path =
    destination_path link |> Option.value_map ~default:false ~f:(String.equal path)
  in
  same_path
  &&
  match address, resolution with
  | None, Ok _ -> true
  | Some address, Ok (Index.Anchor { anchor; _ }) ->
    Note.Anchor.Address.equal address (Note.Anchor.address anchor.definition)
  | _ -> false
;;

(** {2 Link destination edits} *)

let destination_bounds slice =
  match String.substr_index slice ~pattern:"[[" with
  | Some open_pos ->
    let start = open_pos + 2 in
    let finish =
      String.substr_index ~pos:start slice ~pattern:"]]"
      |> Option.value ~default:(String.length slice)
    in
    let finish =
      String.index_from slice start '|'
      |> Option.filter ~f:(fun p -> p < finish)
      |> Option.value ~default:finish
    in
    Some (`Wikilink, start, finish)
  | None ->
    String.substr_index slice ~pattern:"]("
    |> Option.map ~f:(fun open_pos ->
      let start = open_pos + 2 in
      let rec finish i =
        if
          i >= String.length slice
          || Char.equal slice.[i] ')'
          || Char.is_whitespace slice.[i]
        then i
        else finish (i + 1)
      in
      `Markdown, start, finish start)
;;

(** The link's style, the byte offset of its destination, and the destination
    as written. *)
let authored_destination ~read_file source (link : Index.Link.t) =
  let first_byte = Cmarkit.Textloc.first_byte link.loc in
  let last_byte = Cmarkit.Textloc.last_byte link.loc in
  read_file source
  |> Option.bind ~f:(fun content ->
    let len = last_byte - first_byte + 1 in
    if first_byte < 0 || len <= 0 || first_byte + len > String.length content
    then None
    else (
      let slice = String.sub content ~pos:first_byte ~len in
      destination_bounds slice
      |> Option.map ~f:(fun (style, start, stop) ->
        style, first_byte + start, String.sub slice ~pos:start ~len:(stop - start))))
;;

let encode style text =
  match style with
  | `Wikilink -> text
  | `Markdown -> String.substr_replace_all text ~pattern:" " ~with_:"%20"
;;

let reference_edit ~read_file { address; _ } ~new_name (source, (link : Index.Link.t), _) =
  authored_destination ~read_file source link
  |> Option.bind ~f:(fun (style, destination_first, destination) ->
    String.index destination '#'
    |> Option.map ~f:(fun hash ->
      let marker_length =
        match address with
        | Some (Note.Anchor.Address.Caret _) -> 1
        | None | Some (Heading _ | Attr _) -> 0
      in
      { rel_path = source
      ; first_byte = destination_first + hash + 1 + marker_length
      ; last_byte = destination_first + String.length destination
      ; new_text = encode style new_name
      }))
;;

(** {2 Move edits} *)

let moved_path moves path =
  List.find_map moves ~f:(fun (src, dst) ->
    if String.equal path src
    then Some dst
    else
      String.chop_prefix path ~prefix:(src ^ "/")
      |> Option.map ~f:(fun rest -> dst ^ "/" ^ rest))
  |> Option.value ~default:path
;;

let within ~dir path = String.equal path dir || String.is_prefix path ~prefix:(dir ^ "/")

let check_moves index moves =
  let paths =
    List.map (Index.notes index) ~f:Index.Entry.path
    @ List.map (Index.assets index) ~f:Index.Asset.path
  in
  let check_one (src, dst) =
    match Index.Path.of_string src, Index.Path.of_string dst with
    | Error _, _ -> Error (sprintf "invalid source path: %s" src)
    | _, Error _ -> Error (sprintf "invalid destination path: %s" dst)
    | Ok _, Ok _ ->
      if String.equal src dst
      then Error (sprintf "source and destination are the same: %s" src)
      else if within ~dir:src dst
      then Error (sprintf "cannot move %s into itself" src)
      else if not (List.exists paths ~f:(within ~dir:src))
      then Error (sprintf "nothing indexed at %s" src)
      else Ok ()
  in
  let moved, kept =
    List.partition_tf paths ~f:(fun path ->
      List.exists moves ~f:(fun (src, _) -> within ~dir:src path))
  in
  let open Result.Let_syntax in
  let%bind () = List.map moves ~f:check_one |> Result.all_unit in
  let%bind () =
    match
      List.find_a_dup (List.map moved ~f:(moved_path moves)) ~compare:String.compare
    with
    | Some path -> Error (sprintf "two files would move to %s" path)
    | None -> Ok ()
  in
  match
    List.find kept ~f:(fun path ->
      List.exists moves ~f:(fun (_, dst) -> within ~dir:dst path))
  with
  | Some path -> Error (sprintf "destination already exists: %s" path)
  | None -> Ok ()
;;

let relative_path ~source path =
  let components p = String.split p ~on:'/' in
  let rec drop_common a b =
    match a, b with
    | x :: a', y :: b' when String.equal x y -> drop_common a' b'
    | _ -> a, b
  in
  let ups, downs =
    drop_common
      (Option.value_map (Index.Path.dirname source) ~default:[] ~f:components)
      (components path)
  in
  match ups with
  | [] -> "./" ^ String.concat downs ~sep:"/"
  | _ -> String.concat (List.map ups ~f:(Fn.const "..") @ downs) ~sep:"/"
;;

(** Destinations for [target] written the way [authored] was, most faithful
    first: relative if it was relative, otherwise as many trailing path
    components as it had, growing to the full vault path. The [.md] extension
    is kept only if it was written. *)
let destination_candidates ~authored ~source ~target =
  let written path =
    if String.is_suffix authored ~suffix:".md"
    then path
    else Option.value (String.chop_suffix path ~suffix:".md") ~default:path
  in
  let components = String.split target ~on:'/' in
  let n = List.length components in
  let trailing =
    List.range
      (Int.min n (List.length (String.split authored ~on:'/')))
      n
      ~stop:`inclusive
    |> List.map ~f:(fun k ->
      written (String.concat (List.drop components (n - k)) ~sep:"/"))
  in
  let is_relative =
    String.is_prefix authored ~prefix:"./" || String.is_prefix authored ~prefix:"../"
  in
  if is_relative then written (relative_path ~source target) :: trailing else trailing
;;

let resolves_to ~index ~source ~target reference =
  match Index.resolve index source reference with
  | Ok resolved -> String.equal (Index.target_path resolved) target
  | Error _ -> false
;;

let move_edit
      ~read_file
      ~moved_index
      ~moves
      ((target : Index.target), ({ source; link } : Index.backlink))
  =
  let new_source = moved_path moves source in
  let new_target = moved_path moves (Index.target_path target) in
  let resolves reference =
    resolves_to ~index:moved_index ~source:new_source ~target:new_target reference
  in
  if resolves link.reference
  then None
  else
    authored_destination ~read_file source link
    |> Option.bind ~f:(fun (style, destination_first, destination) ->
      let target_stop =
        Option.value (String.index destination '#') ~default:(String.length destination)
      in
      let authored =
        match style with
        | `Wikilink -> String.prefix destination target_stop
        | `Markdown ->
          Note.Link.Ref.percent_decode (String.prefix destination target_stop)
      in
      if String.is_empty authored
      then None
      else (
        let candidates =
          destination_candidates ~authored ~source:new_source ~target:new_target
        in
        let text =
          List.find candidates ~f:(fun text ->
            resolves { link.reference with target = Some text })
          |> Option.value ~default:(List.last_exn candidates)
        in
        Some
          { rel_path = source
          ; first_byte = destination_first
          ; last_byte = destination_first + target_stop
          ; new_text = encode style text
          }))
;;

(** {2 Definition edits} *)

let line_bounds content line =
  let rec loop pos current =
    if current = line
    then (
      let stop =
        Option.value (String.index_from content pos '\n') ~default:(String.length content)
      in
      Some (pos, stop))
    else (
      match String.index_from content pos '\n' with
      | None -> None
      | Some newline -> loop (newline + 1) (current + 1))
  in
  loop 0 0
;;

let attr_id_offset ~(id : string) line =
  let is_id_char c = Char.is_alphanum c || Char.equal c '-' || Char.equal c '_' in
  let rec scan i =
    if i >= String.length line
    then None
    else if Char.equal line.[i] '{'
    then (
      match String.index_from line i '}' with
      | None -> None
      | Some close ->
        let body = String.sub line ~pos:(i + 1) ~len:(close - i - 1) in
        (match Cmarkit.Attribute.of_string body with
         | Some attr
           when Option.value_map
                  (Cmarkit.Attribute.id attr)
                  ~default:false
                  ~f:(String.equal id) ->
           let pattern = "#" ^ id in
           let rec seek from =
             match String.substr_index body ~pos:from ~pattern with
             | None -> scan (close + 1)
             | Some p ->
               let after = p + String.length pattern in
               if after >= String.length body || not (is_id_char body.[after])
               then Some (i + 1 + p + 1)
               else seek (p + 1)
           in
           seek 0
         | _ -> scan (close + 1)))
    else scan (i + 1)
  in
  scan 0
;;

let definition_edit ~index ~read_file { path; address } ~new_name =
  Option.bind address ~f:(fun (address : Note.Anchor.Address.t) ->
    Index.find_note index path
    |> Option.bind ~f:(fun note ->
      List.find (Index.Entry.anchors note) ~f:(fun anchor ->
        Note.Anchor.Address.equal address (Note.Anchor.address anchor.definition)))
    |> Option.bind ~f:(fun (anchor : Index.Anchor.t) ->
      read_file path
      |> Option.bind ~f:(fun content ->
        let source_line =
          match address with
          | Caret _ -> fst (Cmarkit.Textloc.last_line anchor.loc) - 1
          | Heading _ | Attr _ -> fst (Cmarkit.Textloc.first_line anchor.loc) - 1
        in
        line_bounds content source_line
        |> Option.bind ~f:(fun (start, stop) ->
          let line = String.sub content ~pos:start ~len:(stop - start) in
          match address with
          | Heading _ ->
            let hashes =
              String.length line
              - String.length (String.lstrip line ~drop:(Char.equal '#'))
            in
            let rec skip i =
              if i < String.length line && Char.equal line.[i] ' '
              then skip (i + 1)
              else i
            in
            let text_start = skip hashes in
            let text_stop =
              String.substr_index line ~pos:text_start ~pattern:" {"
              |> Option.value ~default:(String.length line)
            in
            Some
              { rel_path = path
              ; first_byte = start + text_start
              ; last_byte = start + text_stop
              ; new_text = new_name
              }
          | Caret id ->
            String.substr_index line ~pattern:("^" ^ id)
            |> Option.map ~f:(fun pos ->
              { rel_path = path
              ; first_byte = start + pos + 1
              ; last_byte = start + pos + 1 + String.length id
              ; new_text = new_name
              })
          | Attr id ->
            attr_id_offset ~id line
            |> Option.map ~f:(fun pos ->
              { rel_path = path
              ; first_byte = start + pos
              ; last_byte = start + pos + String.length id
              ; new_text = new_name
              })))))
;;

(** {2 Change planning} *)

let resolved_links_of_docs index docs =
  List.concat_map docs ~f:(fun (source, doc) ->
    let file_stat =
      Index.find_note index source
      |> Option.value_map
           ~default:
             ({ rel_path = source; birthtime = None; mtime = None } : Index.file_stat)
           ~f:Index.Entry.file_stat
    in
    Index.Entry.of_doc_exn file_stat doc
    |> Index.Entry.links
    |> List.map ~f:(fun link -> source, link, Index.resolve index source link.reference))
;;

(** Move notes, assets, or directories of them, and rewrite every resolved link
    that would otherwise stop resolving to what it resolves to now: links into
    moved files, relative links out of them, and links a move would capture.

    Links that still resolve are left alone. *)
let plan_moves ~index ~read_file moves =
  Result.map (check_moves index moves) ~f:(fun () ->
    let moved_index = Index.map_paths index ~f:(moved_path moves) in
    let edits =
      Index.all_backlinks index
      |> List.filter_map ~f:(move_edit ~read_file ~moved_index ~moves)
      |> List.sort ~compare:compare_edit
    in
    { edits; moves })
;;

let plan ~index ~docs ~read_file ({ path; address } as target) ~new_name =
  let valid =
    match address with
    | None -> valid_note_name new_name
    | Some (Note.Anchor.Address.Heading _) ->
      not (String.is_empty (String.strip new_name))
    | Some (Caret _ | Attr _) -> valid_id new_name
  in
  if not valid
  then Error "invalid new name"
  else (
    match address with
    | None -> plan_moves ~index ~read_file [ path, renamed_note_path ~path ~new_name ]
    | Some _ ->
      let edits =
        resolved_links_of_docs index docs
        |> List.filter ~f:(matches target)
        |> List.filter_map ~f:(reference_edit ~read_file target ~new_name)
      in
      let definition =
        Option.to_list (definition_edit ~index ~read_file target ~new_name)
      in
      Ok { edits = List.sort (definition @ edits) ~compare:compare_edit; moves = [] })
;;

module For_test = struct
  type vault =
    { files : (string * string) list
    ; docs : (string * Cmarkit.Doc.t) list
    ; index : Index.t
    }

  let stat rel_path : Index.file_stat = { rel_path; birthtime = None; mtime = None }

  let vault files =
    let docs =
      List.filter_map files ~f:(fun (path, content) ->
        Option.some_if
          (String.is_suffix path ~suffix:".md")
          (path, Parse.of_string ~locs:true content))
    in
    let index =
      List.fold files ~init:Index.empty ~f:(fun index (path, _) ->
        match List.Assoc.find docs ~equal:String.equal path with
        | Some doc -> Index.set_note index (Index.Entry.of_doc_exn (stat path) doc)
        | None -> Index.set_asset index (Index.Asset.create (stat path)))
    in
    { files; docs; index }
  ;;

  let read_file { files; _ } path = List.Assoc.find files ~equal:String.equal path

  (** Print the files [change] rewrites or moves, as they are afterwards. *)
  let show_change { files; _ } = function
    | Error error -> printf "error: %s\n" error
    | Ok { edits; moves } ->
      List.iter files ~f:(fun (path, content) ->
        let edits = List.filter edits ~f:(fun e -> String.equal e.rel_path path) in
        let new_path = moved_path moves path in
        if not (List.is_empty edits && String.equal path new_path)
        then (
          let content =
            List.sort edits ~compare:(fun a b -> Int.descending a.first_byte b.first_byte)
            |> List.fold ~init:content ~f:(fun content e ->
              String.prefix content e.first_byte
              ^ e.new_text
              ^ String.drop_prefix content e.last_byte)
          in
          if String.equal path new_path
          then printf "%s\n" path
          else printf "%s -> %s\n" path new_path;
          if not (List.is_empty edits) then printf "  %s" content))
  ;;
end

let%expect_test "plan note and heading renames" =
  let open For_test in
  let v =
    vault
      [ "a.md", "# Alpha\n\nBody\n"; "b.md", "[[a]] [[a#Alpha|label]] [x](a.md#Alpha)\n" ]
  in
  let show target new_name =
    plan ~index:v.index ~docs:v.docs ~read_file:(read_file v) target ~new_name
    |> show_change v
  in
  show { path = "a.md"; address = None } "renamed";
  [%expect
    {|
    a.md -> renamed.md
    b.md
      [[renamed]] [[renamed#Alpha|label]] [x](renamed.md#Alpha)
    |}];
  show { path = "a.md"; address = Some (Heading "alpha") } "New title";
  [%expect
    {|
    a.md
      # New title

    Body
    b.md
      [[a]] [[a#New title|label]] [x](a.md#New%20title)
    |}]
;;

let%expect_test "plan directory moves" =
  let open For_test in
  let v =
    vault
      [ "top.md", "[[a]] [[d/a#A]] [x](d/a.md) ![[d/img.png]] [r](./d/b.md)\n"
      ; "d/a.md", "# A\n\n[[b]] [up](../top.md) [sib](./b.md) [[d/b]]\n"
      ; "d/b.md", "B\n"
      ; "d/img.png", ""
      ]
  in
  let show moves =
    plan_moves ~index:v.index ~read_file:(read_file v) moves |> show_change v
  in
  (* Same parent: bare and relative-within links survive. *)
  show [ "d", "e" ];
  [%expect
    {|
    top.md
      [[a]] [[e/a#A]] [x](e/a.md) ![[e/img.png]] [r](./e/b.md)
    d/a.md -> e/a.md
      # A

    [[b]] [up](../top.md) [sib](./b.md) [[e/b]]
    d/b.md -> e/b.md
    d/img.png -> e/img.png
    |}];
  (* Deeper: relative links out of the directory climb further. *)
  show [ "d", "x/y/d" ];
  [%expect
    {|
    d/a.md -> x/y/d/a.md
      # A

    [[b]] [up](../../../top.md) [sib](./b.md) [[d/b]]
    d/b.md -> x/y/d/b.md
    d/img.png -> x/y/d/img.png
    |}]
;;

let%expect_test "moves keep links that a move would capture" =
  let open For_test in
  let v = vault [ "a.md", "A\n"; "x/b.md", "B\n"; "src.md", "[[b]] [[a]]\n" ] in
  plan
    ~index:v.index
    ~docs:v.docs
    ~read_file:(read_file v)
    { path = "a.md"; address = None }
    ~new_name:"b"
  |> show_change v;
  [%expect
    {|
    a.md -> b.md
    src.md
      [[x/b]] [[b]]
    |}]
;;

let%expect_test "invalid moves" =
  let open For_test in
  let v = vault [ "a.md", "A\n"; "b.md", "B\n"; "d/c.md", "C\n"; "e/c.md", "C\n" ] in
  let show moves =
    plan_moves ~index:v.index ~read_file:(read_file v) moves |> show_change v
  in
  show [ "a.md", "b.md" ];
  show [ "d", "e" ];
  show [ "d", "d/sub" ];
  show [ "missing", "elsewhere" ];
  show [ "d/", "f" ];
  show [ "a.md", "c.md"; "b.md", "c.md" ];
  [%expect
    {|
    error: destination already exists: b.md
    error: destination already exists: e/c.md
    error: cannot move d into itself
    error: nothing indexed at missing
    error: invalid source path: d/
    error: two files would move to c.md
    |}]
;;
