(** Hierarchical document symbols for headings and anchors.

    Spec: {!page-"feature-document-outline"}. *)

open Core

type kind =
  | Heading of int
  | Block_id
  | Attribute_id
[@@deriving sexp, equal, compare]

type symbol =
  { name : string
  ; kind : kind
  ; first_byte : int
  ; last_byte : int
  ; selection_first_byte : int
  ; selection_last_byte : int
  ; children : symbol list
  }
[@@deriving sexp, equal, compare]

type event =
  { first_byte : int
  ; last_byte : int
  ; name : string
  ; kind : kind
  }

type node =
  { event : event
  ; mutable section_last_byte : int
  ; mutable children_rev : node list
  }

let event_of_loc ~name ~kind = function
  | None -> None
  | Some loc when Cmarkit.Textloc.is_none loc -> None
  | Some loc ->
    Some
      { first_byte = Cmarkit.Textloc.first_byte loc
      ; last_byte = Cmarkit.Textloc.last_byte loc + 1
      ; name
      ; kind
      }
;;

let events (entry : Oystermark.Vault.Index.file_entry) =
  let headings =
    List.filter_map entry.headings ~f:(fun h ->
      event_of_loc ~name:h.text ~kind:(Heading h.level) h.loc)
  in
  let blocks =
    List.filter_map entry.blocks ~f:(fun b ->
      event_of_loc ~name:("^" ^ b.id) ~kind:Block_id b.loc)
  in
  let attrs =
    List.filter_map entry.attrs ~f:(fun a ->
      event_of_loc ~name:("#" ^ a.id) ~kind:Attribute_id a.loc)
  in
  List.sort (headings @ blocks @ attrs) ~compare:(fun a b ->
    match Int.compare a.first_byte b.first_byte with
    | 0 ->
      (match a.kind, b.kind with
       | Heading _, (Block_id | Attribute_id) -> -1
       | (Block_id | Attribute_id), Heading _ -> 1
       | _ -> Int.compare a.last_byte b.last_byte)
    | c -> c)
;;

let symbols ~(entry : Oystermark.Vault.Index.file_entry) ~(content_length : int)
  : symbol list
  =
  let roots_rev = ref [] in
  let heading_stack : (int * node) list ref = ref [] in
  let close_until level next_byte =
    let rec loop = function
      | (open_level, node) :: rest when open_level >= level ->
        node.section_last_byte <- next_byte;
        loop rest
      | stack -> stack
    in
    heading_stack := loop !heading_stack
  in
  let attach node =
    match !heading_stack with
    | (_, parent) :: _ -> parent.children_rev <- node :: parent.children_rev
    | [] -> roots_rev := node :: !roots_rev
  in
  List.iter (events entry) ~f:(fun event ->
    match event.kind with
    | Heading level ->
      close_until level event.first_byte;
      let node =
        { event; section_last_byte = content_length; children_rev = [] }
      in
      attach node;
      heading_stack := (level, node) :: !heading_stack
    | Block_id | Attribute_id ->
      attach { event; section_last_byte = event.last_byte; children_rev = [] });
  List.iter !heading_stack ~f:(fun (_, node) ->
    node.section_last_byte <- content_length);
  let rec freeze node =
    { name = node.event.name
    ; kind = node.event.kind
    ; first_byte = node.event.first_byte
    ; last_byte = node.section_last_byte
    ; selection_first_byte = node.event.first_byte
    ; selection_last_byte = node.event.last_byte
    ; children = List.rev_map node.children_rev ~f:freeze
    }
  in
  List.rev_map !roots_rev ~f:freeze
;;

let document_outline ~(index : Oystermark.Vault.Index.t) ~rel_path ~content =
  List.find index.files ~f:(fun entry -> String.equal entry.rel_path rel_path)
  |> Option.value_map ~default:[] ~f:(fun entry ->
    symbols ~entry ~content_length:(String.length content))
;;

module For_test = struct
  let document_outline = document_outline
end

