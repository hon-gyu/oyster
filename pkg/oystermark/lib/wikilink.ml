open Core

type fragment =
  | Heading of string list
  | Block_ref of string

type t = {
  target : string option;
  fragment : fragment option;
  display : string option;
  embed : bool;
}

type Cmarkit.Inline.t += Ext_wikilink of t Cmarkit.node

let meta_key : unit Cmarkit.Meta.key = Cmarkit.Meta.key ()

let is_valid_block_id s =
  String.length s > 0
  && String.for_all s ~f:(fun c ->
       Char.is_alphanum c || Char.equal c '-')
  && Char.is_alphanum (String.get s 0)

let parse_content ~(embed:bool) (content : string) : t =
  (* Split on first unescaped | *)
  let ref_part, display =
    match String.lsplit2 content ~on:'|' with
    | Some (r, d) -> (r, Some d)
    | None -> (content, None)
  in
  (* Split on first # *)
  let target, fragment =
    match String.lsplit2 ref_part ~on:'#' with
    | None ->
      let target = if String.is_empty ref_part then None else Some ref_part in
      (target, None)
    | Some (t, frag_str) ->
      let target = if String.is_empty t then None else Some t in
      let fragment =
        if String.is_empty frag_str then
          None
        else if String.is_prefix frag_str ~prefix:"^" then
          let candidate = String.drop_prefix frag_str 1 in
          if is_valid_block_id candidate then
            Some (Block_ref candidate)
          else
            (* Treat as heading with literal ^ *)
            let parts =
              String.split frag_str ~on:'#'
              |> List.filter ~f:(fun s -> not (String.is_empty s))
            in
            if List.is_empty parts then None else Some (Heading parts)
        else
          let parts =
            String.split frag_str ~on:'#'
            |> List.filter ~f:(fun s -> not (String.is_empty s))
          in
          if List.is_empty parts then None else Some (Heading parts)
      in
      (target, fragment)
  in
  { target; fragment; display; embed }

(* Find the index of substring [needle] in [haystack] starting at [from]. *)
let find_substring haystack ~needle ~from =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if from + nlen > hlen then None
  else
    let rec loop i =
      if i + nlen > hlen then None
      else if String.is_substring_at haystack ~pos:i ~substring:needle then
        Some i
      else loop (i + 1)
    in
    loop from

let is_escaped s pos =
  pos > 0 && Char.equal (String.get s (pos - 1)) '\\'

let scan s meta =
  let len = String.length s in
  if len < 4 then None (* minimum: [[x]] *)
  else
    let rec find_links acc pos =
      match find_substring s ~needle:"[[" ~from:pos with
      | None ->
        if List.is_empty acc then None
        else begin
          let tail =
            if pos < len then
              [ Cmarkit.Inline.Text (String.drop_prefix s pos, meta) ]
            else []
          in
          Some (List.rev acc @ tail)
        end
      | Some open_pos ->
        (* Check for embed (!) and escaping *)
        let embed, start_pos =
          if open_pos > 0
             && Char.equal (String.get s (open_pos - 1)) '!'
             && not (is_escaped s (open_pos - 1))
          then (true, open_pos - 1)
          else (false, open_pos)
        in
        (* Check if the [[ itself is escaped *)
        if is_escaped s (if embed then open_pos - 1 else open_pos) then
          find_links acc (open_pos + 2)
        else begin
          let content_start = open_pos + 2 in
          match find_substring s ~needle:"]]" ~from:content_start with
          | None ->
            if List.is_empty acc then None
            else begin
              let tail = [ Cmarkit.Inline.Text (String.drop_prefix s pos, meta) ] in
              Some (List.rev acc @ tail)
            end
          | Some close_pos ->
            let content = String.sub s ~pos:content_start ~len:(close_pos - content_start) in
            let wikilink = parse_content ~embed content in
            let wl_meta = Cmarkit.Meta.tag meta_key (Cmarkit.Meta.make ()) in
            let before =
              if start_pos > pos then
                [ Cmarkit.Inline.Text (String.sub s ~pos ~len:(start_pos - pos), meta) ]
              else []
            in
            let wl_node = Ext_wikilink (wikilink, wl_meta) in
            let acc = wl_node :: (List.rev before @ acc) in
            find_links acc (close_pos + 2)
        end
    in
    find_links [] 0
