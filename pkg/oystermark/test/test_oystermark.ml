open Oystermark

let printf = Printf.printf
let sprintf = Printf.sprintf

(* --- helpers --- *)

let frag_equal a b =
  match a, b with
  | None, None -> true
  | Some (Wikilink.Heading a), Some (Wikilink.Heading b) -> a = b
  | Some (Wikilink.Block_ref a), Some (Wikilink.Block_ref b) -> a = b
  | _ -> false
;;

let frag_to_string = function
  | None -> "None"
  | Some (Wikilink.Heading hs) -> sprintf "Heading [%s]" (String.concat "; " hs)
  | Some (Wikilink.Block_ref s) -> sprintf "Block_ref %s" s
;;

let opt_to_string = function
  | None -> "None"
  | Some s -> s
;;

(* --- Wikilink.parse_content tests --- *)

let test_parse name ~embed input expected_target expected_fragment expected_display =
  let w = Wikilink.parse_content ~embed input in
  let pass = ref true in
  if w.target <> expected_target
  then (
    printf
      "FAIL [%s]: target = %s, expected %s\n"
      name
      (opt_to_string w.target)
      (opt_to_string expected_target);
    pass := false);
  if not (frag_equal w.fragment expected_fragment)
  then (
    printf
      "FAIL [%s]: fragment = %s, expected %s\n"
      name
      (frag_to_string w.fragment)
      (frag_to_string expected_fragment);
    pass := false);
  if w.display <> expected_display
  then (
    printf
      "FAIL [%s]: display = %s, expected %s\n"
      name
      (opt_to_string w.display)
      (opt_to_string expected_display);
    pass := false);
  if w.embed <> embed
  then (
    printf "FAIL [%s]: embed = %b, expected %b\n" name w.embed embed;
    pass := false);
  if !pass then printf "OK   [%s]\n" name
;;

let test_parse_content () =
  test_parse "basic note" ~embed:false "Note" (Some "Note") None None;
  test_parse "note with ext" ~embed:false "Note.md" (Some "Note.md") None None;
  test_parse "dir path" ~embed:false "dir/Note" (Some "dir/Note") None None;
  test_parse
    "display text"
    ~embed:false
    "Note|custom text"
    (Some "Note")
    None
    (Some "custom text");
  test_parse
    "heading"
    ~embed:false
    "Note#Heading"
    (Some "Note")
    (Some (Wikilink.Heading [ "Heading" ]))
    None;
  test_parse
    "nested heading"
    ~embed:false
    "Note#H1#H2"
    (Some "Note")
    (Some (Wikilink.Heading [ "H1"; "H2" ]))
    None;
  test_parse
    "current note heading"
    ~embed:false
    "#Heading"
    None
    (Some (Wikilink.Heading [ "Heading" ]))
    None;
  test_parse
    "block ref"
    ~embed:false
    "Note#^blockid"
    (Some "Note")
    (Some (Wikilink.Block_ref "blockid"))
    None;
  test_parse
    "block ref with hyphen"
    ~embed:false
    "Note#^block-id"
    (Some "Note")
    (Some (Wikilink.Block_ref "block-id"))
    None;
  test_parse
    "invalid block id (underscore)"
    ~embed:false
    "Note#^block_id"
    (Some "Note")
    (Some (Wikilink.Heading [ "^block_id" ]))
    None;
  test_parse
    "block ref current note"
    ~embed:false
    "#^blockid"
    None
    (Some (Wikilink.Block_ref "blockid"))
    None;
  test_parse
    "heading with display"
    ~embed:false
    "#H1#H2|text"
    None
    (Some (Wikilink.Heading [ "H1"; "H2" ]))
    (Some "text");
  test_parse "embed" ~embed:true "Note" (Some "Note") None None;
  test_parse
    "hash collapse"
    ~embed:false
    "##A###B"
    None
    (Some (Wikilink.Heading [ "A"; "B" ]))
    None;
  test_parse
    "heading then block ref literal"
    ~embed:false
    "Note#H1#^blockid"
    (Some "Note")
    (Some (Wikilink.Heading [ "H1"; "^blockid" ]))
    None;
  test_parse "empty target with hash" ~embed:false "#" None None None;
  test_parse "empty target" ~embed:false "" None None None
;;

(* --- Wikilink.scan tests --- *)

let count_wikilinks inlines =
  List.length
    (List.filter
       (function
         | Wikilink.Ext_wikilink _ -> true
         | _ -> false)
       inlines)
;;

let test_scan () =
  let meta = Cmarkit.Meta.none in
  (* No wikilinks *)
  let r = Wikilink.scan "hello world" meta in
  if Option.is_some r
  then printf "FAIL [scan no wikilinks]: expected None\n"
  else printf "OK   [scan no wikilinks]\n";
  (* Single wikilink *)
  (match Wikilink.scan "before [[Note]] after" meta with
   | None -> printf "FAIL [scan single]: expected Some\n"
   | Some inlines ->
     let n = count_wikilinks inlines in
     if n = 1
     then printf "OK   [scan single]\n"
     else printf "FAIL [scan single]: %d wikilinks, expected 1\n" n);
  (* Multiple wikilinks *)
  (match Wikilink.scan "[[A]] and [[B]]" meta with
   | None -> printf "FAIL [scan multiple]: expected Some\n"
   | Some inlines ->
     let n = count_wikilinks inlines in
     if n = 2
     then printf "OK   [scan multiple]\n"
     else printf "FAIL [scan multiple]: %d wikilinks, expected 2\n" n);
  (* Embed *)
  (match Wikilink.scan "![[image.png]]" meta with
   | None -> printf "FAIL [scan embed]: expected Some\n"
   | Some inlines ->
     (match inlines with
      | [ Wikilink.Ext_wikilink (w, _) ] ->
        if w.embed
        then printf "OK   [scan embed]\n"
        else printf "FAIL [scan embed]: embed=false\n"
      | _ -> printf "FAIL [scan embed]: unexpected structure\n"));
  (* Unclosed *)
  let r = Wikilink.scan "[[unclosed" meta in
  if Option.is_some r
  then printf "FAIL [scan unclosed]: expected None\n"
  else printf "OK   [scan unclosed]\n"
;;

(* --- Block_id tests --- *)

let test_block_id () =
  let test name input expected =
    let result = Block_id.extract_trailing input in
    let pass =
      match result, expected with
      | None, None -> true
      | Some (before, id), Some (exp_before, exp_id) -> before = exp_before && id = exp_id
      | _ -> false
    in
    if pass
    then printf "OK   [block_id %s]\n" name
    else (
      let show = function
        | None -> "None"
        | Some (b, i) -> sprintf "(%s, %s)" b i
      in
      printf
        "FAIL [block_id %s]: got %s, expected %s\n"
        name
        (show result)
        (show expected))
  in
  test "basic" "Some text ^blockid" (Some ("Some text", "blockid"));
  test "with hyphen" "Text ^block-id" (Some ("Text", "block-id"));
  test "no block id" "Just text" None;
  test "invalid (underscore)" "Text ^block_id" None;
  test "at start" "^blockid" (Some ("", "blockid"));
  test "trailing space" "Text ^blockid  " (Some ("Text", "blockid"))
;;

(* --- Integration: of_string mapper --- *)

let rec has_wikilink = function
  | Wikilink.Ext_wikilink _ -> true
  | Cmarkit.Inline.Inlines (inlines, _) -> List.exists has_wikilink inlines
  | _ -> false
;;

let rec collect_inline_from_block = function
  | Cmarkit.Block.Paragraph (p, _meta) -> [ Cmarkit.Block.Paragraph.inline p ]
  | Cmarkit.Block.Blocks (bs, _) -> List.concat_map collect_inline_from_block bs
  | _ -> []
;;

let test_integration () =
  (* Wikilink in paragraph *)
  let doc = Oystermark.of_string "Hello [[Note]] world" in
  let inlines = collect_inline_from_block (Cmarkit.Doc.block doc) in
  let found = List.exists has_wikilink inlines in
  if found
  then printf "OK   [integration wikilink]\n"
  else printf "FAIL [integration wikilink]: no wikilink in parsed doc\n";
  (* Block ID in paragraph *)
  let doc = Oystermark.of_string "Some paragraph ^myblock" in
  let block = Cmarkit.Doc.block doc in
  match block with
  | Cmarkit.Block.Paragraph (_, meta) ->
    (match Cmarkit.Meta.find Block_id.meta_key meta with
     | Some id ->
       if id = "myblock"
       then printf "OK   [integration block_id]\n"
       else printf "FAIL [integration block_id]: id=%s\n" id
     | None -> printf "FAIL [integration block_id]: no meta key\n")
  | _ -> printf "FAIL [integration block_id]: not a paragraph\n"
;;

let () =
  printf "=== Wikilink.parse_content ===\n";
  test_parse_content ();
  printf "\n=== Wikilink.scan ===\n";
  test_scan ();
  printf "\n=== Block_id ===\n";
  test_block_id ();
  printf "\n=== Integration ===\n";
  test_integration ()
;;
