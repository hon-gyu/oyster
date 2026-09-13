(** Hover: show a preview of the link target's content.

    Spec: {!page-"feature-hover"}. *)

open Core

(** {1:implementation Implementation} *)

(** {2 Content extraction} *)

(** Number of lines in [s], counting a final line that lacks a trailing
    newline.  The empty string has no lines. *)
let count_lines (s : string) : int =
  if String.is_empty s
  then 0
  else
    String.count s ~f:(Char.equal '\n') + if String.is_suffix s ~suffix:"\n" then 0 else 1
;;

(** Render [n] bytes for human consumption: [B], [KB] or [MB]. *)
let human_bytes (n : int) : string =
  if n < 1024
  then sprintf "%d B" n
  else if n < 1024 * 1024
  then sprintf "%d KB" ((n + 512) / 1024)
  else sprintf "%d MB" ((n + (512 * 1024)) / (1024 * 1024))
;;

(** {2 Non-text targets}

    A link can resolve to a file that is not markdown.  Its bytes are not
    text, so they are described rather than shown: putting them in the
    hover would be unreadable and, for a binary file, invalid UTF-8 in the
    response.  See {!page-"feature-hover".images} and
    {!page-"feature-hover".binary}. *)

module Media = struct
  (** The image formats {!Link_collect.is_image_target} recognises, each with
      the label a description line uses and the MIME type a [data:] URI
      needs. *)
  let by_extension =
    [ ".png", ("PNG", "image/png")
    ; ".jpg", ("JPEG", "image/jpeg")
    ; ".jpeg", ("JPEG", "image/jpeg")
    ; ".gif", ("GIF", "image/gif")
    ; ".svg", ("SVG", "image/svg+xml")
    ; ".webp", ("WebP", "image/webp")
    ]
  ;;

  (** [(label, mime)] for [path]'s extension, or [None] if it names no image
      format we know. *)
  let of_path (path : string) : (string * string) option =
    let lower = String.lowercase path in
    List.find_map by_extension ~f:(fun (ext, info) ->
      if String.is_suffix lower ~suffix:ext then Some info else None)
  ;;

  (** {3 Dimensions}

      Read from the container header rather than by decoding: PNG's [IHDR],
      GIF's logical screen descriptor, JPEG's first [SOFn] frame header.
      Every accessor is bounds-checked, so a truncated or damaged file
      yields [None] and the description simply omits the field. *)

  let byte (s : string) (i : int) : int option =
    if i >= 0 && i < String.length s then Some (Char.to_int s.[i]) else None
  ;;

  (** Big-endian 16-bit integer at [i]. *)
  let be16 (s : string) (i : int) : int option =
    match byte s i, byte s (i + 1) with
    | Some hi, Some lo -> Some ((hi * 256) + lo)
    | _ -> None
  ;;

  (** Big-endian 32-bit integer at [i].  Widths fit in OCaml's 63-bit int. *)
  let be32 (s : string) (i : int) : int option =
    match be16 s i, be16 s (i + 2) with
    | Some hi, Some lo -> Some ((hi * 65536) + lo)
    | _ -> None
  ;;

  (** Little-endian 16-bit integer at [i]. *)
  let le16 (s : string) (i : int) : int option =
    match byte s i, byte s (i + 1) with
    | Some lo, Some hi -> Some ((hi * 256) + lo)
    | _ -> None
  ;;

  let png_signature = "\137PNG\r\n\026\n"

  (** [IHDR] is required to be the first chunk, so width and height sit at
      fixed offsets 16 and 20. *)
  let png_dimensions (s : string) : (int * int) option =
    if not (String.is_prefix s ~prefix:png_signature)
    then None
    else (
      match be32 s 16, be32 s 20 with
      | Some w, Some h -> Some (w, h)
      | _ -> None)
  ;;

  (** The logical screen descriptor follows the six-byte signature, little
      endian — the one format here that is. *)
  let gif_dimensions (s : string) : (int * int) option =
    if not (String.is_prefix s ~prefix:"GIF87a" || String.is_prefix s ~prefix:"GIF89a")
    then None
    else (
      match le16 s 6, le16 s 8 with
      | Some w, Some h -> Some (w, h)
      | _ -> None)
  ;;

  (** Walk the marker segments from [SOI] to the first frame header.  A JPEG
      keeps its size in [SOFn], which is not at a fixed offset: the tables and
      metadata before it vary in number and length.

      [0xC4] ([DHT]), [0xC8] ([JPG]) and [0xCC] ([DAC]) fall in the [SOFn]
      range numerically but are not frame headers.  [0xD0]–[0xD9] and [0x01]
      stand alone with no length field.  Any number of [0xFF] fill bytes may
      pad the gap before a marker. *)
  let jpeg_dimensions (s : string) : (int * int) option =
    let is_sof m =
      m >= 0xC0 && m <= 0xCF && (not (m = 0xC4)) && (not (m = 0xC8)) && not (m = 0xCC)
    in
    let standalone m = (m >= 0xD0 && m <= 0xD9) || m = 0x01 in
    if not (String.is_prefix s ~prefix:"\255\216")
    then None
    else (
      (* [i] is at the [0xFF] introducing a marker. *)
      let rec walk i =
        match byte s i with
        | Some 0xFF ->
          (match byte s (i + 1) with
           (* Fill: the marker is further along. *)
           | Some 0xFF -> walk (i + 1)
           | Some m when standalone m -> walk (i + 2)
           | Some m when is_sof m ->
             (* Segment: length(2) precision(1) height(2) width(2). *)
             (match be16 s (i + 5), be16 s (i + 7) with
              | Some h, Some w -> Some (w, h)
              | _ -> None)
           | Some _ ->
             (match be16 s (i + 2) with
              (* A length below 2 does not advance; stop rather than loop. *)
              | Some len when len >= 2 -> walk (i + 2 + len)
              | _ -> None)
           | None -> None)
        | _ -> None
      in
      walk 2)
  ;;

  (** Dimensions of [content], dispatched on its {e own} magic bytes rather
      than on the extension: a file misnamed [.png] is described by what it
      is, and one whose format we do not measure (SVG, WebP) simply has no
      dimensions to report. *)
  let dimensions (content : string) : (int * int) option =
    List.find_map [ png_dimensions; gif_dimensions; jpeg_dimensions ] ~f:(fun f ->
      f content)
  ;;

  (** {3 Binary detection} *)

  (** How much of a file the NUL scan looks at.  Enough to catch a header,
      cheap enough to run on every hover. *)
  let sniff_bytes = 8192

  (** Whether [content] looks like something other than text.  The NUL test is
      what [grep] and [git] use: crude, one scan, and biased towards calling a
      file text — a misjudged file is truncated as usual rather than lost.
      See {!page-"feature-hover".binary}. *)
  let looks_binary (content : string) : bool =
    String.exists (String.prefix content sniff_bytes) ~f:(Char.equal '\000')
  ;;

  (** {3 Description} *)

  (** The line naming what the file is: format, dimensions when readable, and
      size.  [label] is [None] for a file that is not a known image. *)
  let describe ?(label : string option) (content : string) : string =
    let fields =
      Option.to_list label
      @ (match dimensions content with
         | Some (w, h) -> [ sprintf "%d×%d" w h ]
         | None -> [])
      @ [ human_bytes (String.length content) ]
    in
    String.concat ~sep:" · " fields
  ;;

  (** The body of an image hover: the description, then the image itself when
      {!Lsp_lib.Config.t.hover_image_preview} is on and the file is within
      {!Lsp_lib.Config.t.hover_image_max_bytes}.  Over the cap, the description
      stands alone and says why.  See {!page-"feature-hover".preview}. *)
  let image_body ~(config : Lsp_config.t) ~(path : string) ~(mime : string) ~label content
    : string
    =
    let description = describe ~label content in
    let size = String.length content in
    if not config.hover_image_preview
    then description
    else if size > config.hover_image_max_bytes
    then
      sprintf
        "%s\n\n*(preview omitted: %s exceeds the %s limit)*"
        description
        (human_bytes size)
        (human_bytes config.hover_image_max_bytes)
    else
      sprintf
        "%s\n\n![%s](data:%s;base64,%s)"
        description
        path
        mime
        (Base64.encode_string content)
  ;;
end

(** Truncate [s] to at most [max_chars] bytes, snapping to the previous
    newline to avoid cutting mid-word, and appending a notice reporting
    how much of the content is shown.

    The notice measures in {e lines}: that is the unit a reader
    perceives, and the cut lands on a line boundary so the count is
    exact.  The percentage is derived from the very same counts, so the
    two figures can never disagree.

    When one or zero lines are hidden the line counts carry no
    information (the remainder is a single long line, e.g. a wide table
    row), so the notice switches wholesale to bytes rather than mixing
    units.

    The percentage is clamped to \[1, 99\] and rounded down: truncation
    did happen, so the reader is never shown [0%] or [100%].

    Returns [s] unchanged if it is already short enough.

    See {!page-"feature-hover".truncation}. *)
let truncate ~max_chars (s : string) : string =
  if String.length s <= max_chars
  then s
  else (
    (* Snap back to the last newline before the cut point. *)
    let cut =
      match String.rindex_exn (String.prefix s max_chars) '\n' with
      | pos -> pos
      | exception Not_found_s _ -> max_chars
    in
    let shown = String.prefix s cut in
    let pct ~shown ~total = Int.max 1 (Int.min 99 (shown * 100 / Int.max 1 total)) in
    let total_lines = count_lines s in
    let shown_lines = count_lines shown in
    let notice =
      if total_lines - shown_lines <= 1
      then
        sprintf
          "*(truncated: showing %s of %s, %d%%)*"
          (human_bytes (String.length shown))
          (human_bytes (String.length s))
          (pct ~shown:(String.length shown) ~total:(String.length s))
      else
        sprintf
          "*(truncated: showing %d of %d lines, %d%%)*"
          shown_lines
          total_lines
          (pct ~shown:shown_lines ~total:total_lines)
    in
    shown ^ "\n\n" ^ notice)
;;

(** The source text of the blocks [address] names in [content], or [None].

    The blocks are chosen as for embedding ({!Oystermark.Note.read}) and shown
    as written in the file ({!Oystermark.Note.source_text}). See
    {!page-"feature-hover"}. *)
let read_address (address : Oystermark.Note.Address.t) (content : string) : string option =
  let doc = Lsp_util.parse_doc content in
  Oystermark.Note.read [ Cmarkit.Doc.block doc ] address
  |> Oystermark.Note.source_text content
;;

(** {2 Formatting} *)

(** What a resolved target contributed, and whether the hover budget applies
    to it. *)
type body =
  | Text of string (** Note content, subject to {!Lsp_lib.Config.t.hover_max_chars}. *)
  | Fixed of string
  (** A description of a non-text file, already bounded — and, with a preview
      attached, one that truncation would turn into a broken image rather
      than a shorter one.  See {!page-"feature-hover".images}. *)

(** Build the hover string: path header, separator, then body.
    If [body] is empty, shows [*(empty)*] instead. *)
let format_hover ~(path : string) (body : string) : string =
  let header = "*Path*:" ^ path in
  if String.is_empty (String.strip body)
  then header ^ "\n\n*(empty)*"
  else header ^ "\n\n" ^ body
;;

(** {2 Main computation} *)

(** Compute hover content for the link at the given position.

    Returns [(markdown_string, first_byte, last_byte)] or [None] if
    there is no recognisable, readable link at the cursor.

    See {!page-"feature-hover"}. *)
let hover
      ?(config : Lsp_config.t = Lsp_config.default)
      ~(index : Oystermark.Vault.Index.t)
      ~(rel_path : string)
      ~(content : string)
      ~(line : int)
      ~(character : int)
      ~(read_file : string -> string option)
      ()
  : (string * int * int) option
  =
  Trace_core.with_span ~__FILE__ ~__LINE__ "hover"
  @@ fun _sp ->
  Trace_core.add_data_to_span
    _sp
    [ "rel_path", `String rel_path; "line", `Int line; "character", `Int character ];
  let offset = Lsp_util.byte_offset_of_position content ~line ~character in
  let doc = Lsp_util.parse_doc content in
  let links = Link_collect.collect_links ~index ~rel_path doc in
  match Link_collect.find_at_offset links offset with
  | None -> None
  | Some link_ref ->
    let ll =
      List.find_exn links ~f:(fun ll -> ll.first_byte <= offset && offset <= ll.last_byte)
    in
    let target = Oystermark.Vault.Index.resolve index rel_path link_ref in
    (* An image, or any other file whose bytes are not text, is described
       rather than shown — fragment and all, since there is nothing inside
       one for a fragment to name.  See {!page-"feature-hover".images}. *)
    let image_hover path =
      match read_file path with
      | None -> None
      | Some file_content ->
        let label, mime = Option.value_exn (Media.of_path path) in
        Some (path, Fixed (Media.image_body ~config ~path ~mime ~label file_content))
    in
    let file_hover path =
      match read_file path with
      | None -> None
      | Some file_content when Media.looks_binary file_content ->
        Some (path, Fixed (Media.describe ~label:"Binary file" file_content))
      | Some file_content ->
        let body =
          (* The index may be older than [file_content] (for example, with unsaved
             edits), so resolve the fragment against [file_content]. *)
          match link_ref.fragment with
          | None -> file_content
          | Some fragment ->
            Oystermark.Note.Link_ref.resolve_fragment
              (Oystermark.Note.Anchor.of_doc (Lsp_util.parse_doc file_content))
              fragment
            |> Option.bind ~f:(fun (anchor : Oystermark.Note.Anchor.t) ->
              read_address (Oystermark.Note.Anchor.address anchor.value) file_content)
            |> Option.value ~default:file_content
        in
        Some (path, Text body)
    in
    let result_opt =
      match target with
      | Error Missing_path -> None
      (* A fragment naming nothing still leaves its file identified, so hover
         shows that file: an image is described, a note falls back to its full
         content.  See {!page-"feature-hover"} edge cases. *)
      | (Ok (Oystermark.Vault.Index.Asset path) | Error (Missing_anchor path))
        when Option.is_some (Media.of_path path) -> image_hover path
      | Error (Missing_anchor path) -> file_hover path
      | Ok (Note path | Asset path) -> file_hover path
      | Ok (Anchor { note_path = path; anchor }) ->
        let target_content =
          if String.equal path rel_path then Some content else read_file path
        in
        (match target_content with
         | None -> None
         | Some file_content ->
           let body =
             Option.value
               (read_address (Oystermark.Note.Anchor.address anchor.value) file_content)
               ~default:file_content
           in
           Some (path, Text body))
    in
    (* The budget applies to the body only: the path header is short,
       always useful, and must survive however small the budget is. *)
    Option.map result_opt ~f:(fun (path, body) ->
      let body =
        match body with
        | Text s -> truncate ~max_chars:config.hover_max_chars s
        | Fixed s -> s
      in
      let text = format_hover ~path body in
      Trace_core.add_data_to_span _sp [ "content_bytes", `Int (String.length text) ];
      text, ll.first_byte, ll.last_byte)
;;

(** {1:test Test} *)

let%test_module "truncate" =
  (module struct
    let%expect_test "short string unchanged" =
      print_string (truncate ~max_chars:100 "hello\nworld");
      [%expect
        {|
        hello
        world
        |}]
    ;;

    let%expect_test "truncates at newline, reporting lines and percentage" =
      let s = "line one\nline two\nline three" in
      print_string (truncate ~max_chars:15 s);
      [%expect
        {|
        line one

        *(truncated: showing 1 of 3 lines, 33%)* |}]
    ;;

    (* The remainder is a single long line, so line counts would say
       "1 of 2, 50%" while hiding almost everything.  Report bytes. *)
    let%expect_test "single long remainder reports bytes" =
      let s = "head\n" ^ String.make 4000 'x' in
      print_string (truncate ~max_chars:100 s);
      [%expect
        {|
        head

        *(truncated: showing 4 B of 4 KB, 1%)* |}]
    ;;

    (* No newline at all: nothing to snap back to, cut at the budget. *)
    let%expect_test "no newline reports bytes" =
      print_string (truncate ~max_chars:10 (String.make 40 'y'));
      [%expect
        {|
        yyyyyyyyyy

        *(truncated: showing 10 B of 40 B, 25%)* |}]
    ;;

    (* Truncation happened, so neither 0% nor 100% may be reported:
       a sliver of a huge note is 1%, and all-but-a-sliver is 99%. *)
    let%expect_test "percentage is clamped away from the extremes" =
      let notice s = List.last_exn (String.split_lines s) in
      let many =
        List.init 500 ~f:(fun i -> sprintf "line %d" i) |> String.concat ~sep:"\n"
      in
      print_endline (notice (truncate ~max_chars:20 many));
      let one_line = String.make 10_000 'z' in
      print_endline (notice (truncate ~max_chars:9_999 one_line));
      [%expect
        {|
        *(truncated: showing 2 of 500 lines, 1%)*
        *(truncated: showing 10 KB of 10 KB, 99%)*
        |}]
    ;;
  end)
;;

let%test_module "read_address: heading" =
  (module struct
    let content =
      "# Title\n\nIntro.\n\n## Section One\n\nBody one.\n\n## Section Two\n\nBody two.\n"
    ;;

    let show ~slug content =
      print_string
        (Option.value (read_address (Heading slug) content) ~default:"<not found>")
    ;;

    let%expect_test "extracts first section" =
      show ~slug:"section-one" content;
      [%expect
        {|
        ## Section One

        Body one.
         |}]
    ;;

    let%expect_test "top-level heading stops at next h1" =
      show ~slug:"title" content;
      [%expect
        {|
        # Title

        Intro.

        ## Section One

        Body one.

        ## Section Two

        Body two.
        |}]
    ;;
  end)
;;

let%test_module "read_address: caret" =
  (module struct
    let content = "First para.\n\nSecond para ^abc\n\nThird para.\n"

    let%expect_test "finds block" =
      print_s [%sexp (read_address (Caret "abc") content : string option)];
      [%expect {| ("Second para ^abc") |}]
    ;;

    let%expect_test "missing block returns None" =
      print_s [%sexp (read_address (Caret "nope") content : string option)];
      [%expect {| () |}]
    ;;
  end)
;;

let%test_module "format_hover" =
  (module struct
    let%expect_test "with content" =
      print_string (format_hover ~path:"dir/note.md" "# Hello\n\nWorld.\n");
      [%expect
        {|
        *Path*:dir/note.md

        # Hello

        World.
        |}]
    ;;

    let%expect_test "empty content" =
      print_string (format_hover ~path:"dir/empty.md" "");
      [%expect
        {|
        *Path*:dir/empty.md

        *(empty)*
        |}]
    ;;

    let%expect_test "whitespace-only content" =
      print_string (format_hover ~path:"dir/blank.md" "  \n\n  ");
      [%expect
        {|
        *Path*:dir/blank.md

        *(empty)*
        |}]
    ;;
  end)
;;

(** Synthetic image headers, shared by the {!Media} tests and the hover tests
    below.  Only the header is real: dimensions are read from it, and nothing
    downstream decodes the pixels. *)
module For_test = struct
  let be32 (n : int) : string =
    String.init 4 ~f:(fun i -> Char.of_int_exn ((n lsr (8 * (3 - i))) land 0xFF))
  ;;

  let be16 (n : int) : string =
    String.init 2 ~f:(fun i -> Char.of_int_exn ((n lsr (8 * (1 - i))) land 0xFF))
  ;;

  let le16 (n : int) : string =
    String.init 2 ~f:(fun i -> Char.of_int_exn ((n lsr (8 * i)) land 0xFF))
  ;;

  (** Signature, then an [IHDR] chunk carrying the size. *)
  let png ~w ~h = "\137PNG\r\n\026\n" ^ "\000\000\000\r" ^ "IHDR" ^ be32 w ^ be32 h

  (** Signature, then the logical screen descriptor. *)
  let gif ~w ~h = "GIF89a" ^ le16 w ^ le16 h ^ "\247\000\000"

  (** [SOI], an [APP0] the walk must skip over, then [SOF0]. *)
  let jpeg ~w ~h =
    "\255\216"
    ^ ("\255\224" ^ be16 6 ^ "JFIF")
    ^ "\255\192"
    ^ be16 11
    ^ "\008"
    ^ be16 h
    ^ be16 w
    ^ "\001\001\017\000"
  ;;
end

let%test_module "Media" =
  (module struct
    (** Spec: {!page-"feature-hover".images}, {!page-"feature-hover".binary}. *)

    open For_test

    let%expect_test "extension decides the format label and MIME type" =
      List.iter
        [ "a.png"; "b/C.JPG"; "c.jpeg"; "d.gif"; "e.svg"; "f.webp"; "g.md" ]
        ~f:(fun p ->
          printf
            "%s -> %s\n"
            p
            (Sexp.to_string [%sexp (Media.of_path p : (string * string) option)]));
      [%expect
        {|
        a.png -> ((PNG image/png))
        b/C.JPG -> ((JPEG image/jpeg))
        c.jpeg -> ((JPEG image/jpeg))
        d.gif -> ((GIF image/gif))
        e.svg -> ((SVG image/svg+xml))
        f.webp -> ((WebP image/webp))
        g.md -> ()
        |}]
    ;;

    let%expect_test "dimensions come from the header of each container" =
      let show s = print_s [%sexp (Media.dimensions s : (int * int) option)] in
      show (png ~w:1024 ~h:768);
      show (gif ~w:16 ~h:9);
      show (jpeg ~w:4032 ~h:3024);
      [%expect
        {|
        ((1024 768))
        ((16 9))
        ((4032 3024))
        |}]
    ;;

    (* Dispatch is on the magic bytes, so a format we do not measure and a
       file too short to hold its own header both simply have no dimensions
       — never a guess. *)
    let%expect_test "unreadable dimensions are absent, not wrong" =
      let show s = print_s [%sexp (Media.dimensions s : (int * int) option)] in
      show "<svg width=\"10\" height=\"10\"/>";
      show "";
      show (String.prefix (png ~w:8 ~h:8) 18);
      show ("\255\216" ^ "\255\224" ^ For_test.be16 1);
      [%expect
        {|
        ()
        ()
        ()
        ()
        |}]
    ;;

    let%expect_test "description names format, size, and dimensions if known" =
      print_endline (Media.describe ~label:"PNG" (png ~w:4 ~h:3));
      print_endline (Media.describe ~label:"SVG" (String.make 12_000 'x'));
      print_endline (Media.describe ~label:"Binary file" (String.make 2048 '\000'));
      print_endline (Media.describe ~label:"PNG" "");
      [%expect
        {|
        PNG · 4×3 · 24 B
        SVG · 12 KB
        Binary file · 2 KB
        PNG · 0 B
        |}]
    ;;

    let%expect_test "binary detection is the NUL test" =
      let show s = printf "%b\n" (Media.looks_binary s) in
      show "plain text\n";
      show "";
      show "%PDF-1.4\n\000\000";
      (* A NUL past the sniff window is not looked for: the scan is bounded. *)
      show (String.make (Media.sniff_bytes + 10) 'x' ^ "\000");
      [%expect
        {|
        false
        false
        true
        false
        |}]
    ;;
  end)
;;

let%test_module "hover" =
  (module struct
    let files =
      [ ( "note-a.md"
        , "# Alpha\n\n## Section One\n\nBody text. ^block1\n\n## Section Two\n\nMore.\n" )
      ; "note-b.md", "# Beta\n\nSee [[note-a]].\n"
      ; "note-c.md", "# Gamma\n\nSee [[note-a#Section One]].\n"
      ; "note-d.md", "# Delta\n\nSee [[note-a#^block1]].\n"
      ; "note-e.md", "# Epsilon\n\nSelf [[#Epsilon]].\n"
      ; "empty.md", ""
      ; "note-f.md", "# Zeta\n\nSee [[empty]].\n"
      ; "note-g.md", "# Eta\n\nThe [key term]{#kt} matters here.\n"
      ; "note-h.md", "# Theta\n\nSee [[note-g#kt]].\n"
      ; "pic.png", For_test.png ~w:1024 ~h:768
      ; "logo.svg", "<svg xmlns=\"http://www.w3.org/2000/svg\"/>"
      ; "paper.pdf", "%PDF-1.4\n\000\000binary\n"
      ]
    ;;

    let make_index files =
      let md_docs =
        List.filter_map files ~f:(fun (rel_path, content) ->
          if String.is_suffix rel_path ~suffix:".md"
          then Some (rel_path, Oystermark.Parse.of_string content)
          else None)
      in
      let other_files =
        List.filter_map files ~f:(fun (p, _) ->
          if String.is_suffix p ~suffix:".md" then None else Some p)
      in
      Oystermark.Vault.build_index ~md_docs ~other_files ()
    ;;

    let index = make_index files
    let read_file rp = List.Assoc.find files ~equal:String.equal rp

    let show ~rel_path ~content ~line ~character =
      match hover ~index ~rel_path ~content ~line ~character ~read_file () with
      | None -> print_endline "<none>"
      | Some (text, fb, lb) -> printf "[%d-%d]\n%s\n" fb lb text
    ;;

    let%expect_test "plain note link" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-b.md" in
      show ~rel_path:"note-b.md" ~content ~line:2 ~character:8;
      [%expect
        {|
        [12-21]
        *Path*:note-a.md

        # Alpha

        ## Section One

        Body text. ^block1

        ## Section Two

        More.
        |}]
    ;;

    let%expect_test "heading fragment" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-c.md" in
      show ~rel_path:"note-c.md" ~content ~line:2 ~character:8;
      [%expect
        {|
        [13-34]
        *Path*:note-a.md

        ## Section One

        Body text. ^block1
        |}]
    ;;

    let%expect_test "block fragment" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-d.md" in
      show ~rel_path:"note-d.md" ~content ~line:2 ~character:8;
      [%expect
        {|
        [13-30]
        *Path*:note-a.md

        Body text. ^block1
        |}]
    ;;

    let%expect_test "self-referencing heading" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-e.md" in
      show ~rel_path:"note-e.md" ~content ~line:2 ~character:8;
      [%expect
        {|
        [16-27]
        *Path*:note-e.md

        # Epsilon

        Self [[#Epsilon]].
        |}]
    ;;

    (* Hovering a link to an inline attribute anchor shows the containing
       block. See {!page-"feature-attribute-anchors"}. *)
    let%expect_test "attribute-anchor fragment" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-h.md" in
      show ~rel_path:"note-h.md" ~content ~line:2 ~character:8;
      [%expect
        {|
        [13-25]
        *Path*:note-g.md

        The [key term]{#kt} matters here.
        |}]
    ;;

    let%expect_test "empty target note" =
      let content = List.Assoc.find_exn files ~equal:String.equal "note-f.md" in
      show ~rel_path:"note-f.md" ~content ~line:2 ~character:8;
      [%expect
        {|
        [12-20]
        *Path*:empty.md

        *(empty)*
        |}]
    ;;

    let%expect_test "unresolved link returns none" =
      let content = "See [[missing]]." in
      show ~rel_path:"note-b.md" ~content ~line:0 ~character:6;
      [%expect {| <none> |}]
    ;;

    (* The note exists and the heading does not: the file still identifies
       what to show, so hover falls back to its full content, as
       go-to-definition does.  See {!page-"feature-hover"} edge cases. *)
    let%expect_test "a heading that names nothing falls back to the file" =
      let content = "See [[note-a#No Such Heading]]." in
      show ~rel_path:"note-b.md" ~content ~line:0 ~character:8;
      [%expect
        {|
        [4-29]
        *Path*:note-a.md

        # Alpha

        ## Section One

        Body text. ^block1

        ## Section Two

        More.
        |}]
    ;;

    let%expect_test "cursor outside link returns none" =
      let content = "See [[note-a]]." in
      show ~rel_path:"note-b.md" ~content ~line:0 ~character:0;
      [%expect {| <none> |}]
    ;;

    (** {2 Images and other non-text files}

        Spec: {!page-"feature-hover".images}, {!page-"feature-hover".binary}. *)

    let show_with config ~content ~line ~character =
      match
        hover ~config ~index ~rel_path:"note-b.md" ~content ~line ~character ~read_file ()
      with
      | None -> print_endline "<none>"
      | Some (text, _, _) -> print_string text
    ;;

    let%expect_test "an image is described, not dumped" =
      show ~rel_path:"note-b.md" ~content:"See ![[pic.png]]." ~line:0 ~character:8;
      [%expect
        {|
        [4-15]
        *Path*:pic.png

        PNG · 1024×768 · 24 B
        |}]
    ;;

    (* A markdown image reaches the same branch as a wikilink embed. *)
    let%expect_test "markdown image syntax" =
      show ~rel_path:"note-b.md" ~content:"See ![alt](pic.png)." ~line:0 ~character:12;
      [%expect
        {|
        [4-18]
        *Path*:pic.png

        PNG · 1024×768 · 24 B
        |}]
    ;;

    (* There is nothing inside an image for a fragment to name, so it is
       ignored rather than being a reason to show nothing. *)
    let%expect_test "a fragment on an image is ignored" =
      show ~rel_path:"note-b.md" ~content:"See ![[pic.png#top]]." ~line:0 ~character:8;
      [%expect
        {|
        [4-19]
        *Path*:pic.png

        PNG · 1024×768 · 24 B
        |}]
    ;;

    (* A format whose dimensions we do not read keeps every other field. *)
    let%expect_test "svg has no dimensions to report" =
      show ~rel_path:"note-b.md" ~content:"See ![[logo.svg]]." ~line:0 ~character:8;
      [%expect
        {|
        [4-16]
        *Path*:logo.svg

        SVG · 41 B
        |}]
    ;;

    (* Not an image, not markdown: the bytes would be invalid UTF-8 in the
       response, so they are described instead. *)
    let%expect_test "binary file is described" =
      show ~rel_path:"note-b.md" ~content:"See [[paper.pdf]]." ~line:0 ~character:8;
      [%expect
        {|
        [4-16]
        *Path*:paper.pdf

        Binary file · 18 B
        |}]
    ;;

    let%expect_test "preview embeds a data URI when turned on" =
      let config = { Lsp_config.default with hover_image_preview = true } in
      show_with config ~content:"See ![[pic.png]]." ~line:0 ~character:8;
      [%expect
        {|
        *Path*:pic.png

        PNG · 1024×768 · 24 B

        ![pic.png](data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAABAAAAAMA)
        |}]
    ;;

    (* Over the cap the description still stands: it is the part that is
       readable everywhere, and it is what makes the fallback legible. *)
    let%expect_test "an image over the cap keeps its description" =
      let config =
        { Lsp_config.default with hover_image_preview = true; hover_image_max_bytes = 16 }
      in
      show_with config ~content:"See ![[pic.png]]." ~line:0 ~character:8;
      [%expect
        {|
        *Path*:pic.png

        PNG · 1024×768 · 24 B

        *(preview omitted: 24 B exceeds the 16 B limit)*
        |}]
    ;;

    (* [hover_max_chars] governs note content only: applying it to a data URI
       would produce a broken image rather than a shorter one. *)
    let%expect_test "the text budget does not truncate an image hover" =
      let config =
        { Lsp_config.default with hover_max_chars = 10; hover_image_preview = true }
      in
      show_with config ~content:"See ![[pic.png]]." ~line:0 ~character:8;
      [%expect
        {|
        *Path*:pic.png

        PNG · 1024×768 · 24 B

        ![pic.png](data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAABAAAAAMA)
        |}]
    ;;

    let%expect_test "truncation" =
      let config = { Lsp_config.default with hover_max_chars = 30 } in
      let content = List.Assoc.find_exn files ~equal:String.equal "note-b.md" in
      (match
         hover
           ~config
           ~index
           ~rel_path:"note-b.md"
           ~content
           ~line:2
           ~character:8
           ~read_file
           ()
       with
       | None -> print_endline "<none>"
       | Some (text, _, _) -> print_string text);
      [%expect
        {|
        *Path*:note-a.md

        # Alpha

        ## Section One


        *(truncated: showing 3 of 9 lines, 33%)*
        |}]
    ;;
  end)
;;
