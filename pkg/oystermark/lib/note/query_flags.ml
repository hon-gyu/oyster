open Core

type t =
  { under : string option
  ; direct : bool
  ; kind : string option
  ; lang : string option
  ; attr_id : string option
  ; caret_id : string option
  ; key : string option
  ; nth : int option
  }

let to_query (flags : t) : (Query.t, string) Result.t =
  let open Result.Let_syntax in
  let%map kind =
    match flags.kind with
    | Some name when not (List.mem Node.kinds name ~equal:String.equal) ->
      Error
        (sprintf "unknown kind %s; kinds: %s" name (String.concat ~sep:", " Node.kinds))
    | kind -> Ok kind
  in
  (* Attribute and caret identifiers share the [id] property. *)
  let id = Option.first_some flags.attr_id flags.caret_id in
  let where =
    List.filter_opt
      [ Option.map kind ~f:Query.is
      ; Option.map flags.lang ~f:(fun lang -> Query.Prop ("lang", Eq, String lang))
      ; Option.map id ~f:(fun id -> Query.Prop ("id", Eq, String id))
      ; Option.map flags.key ~f:(fun key -> Query.Prop ("key", Eq, String key))
      ]
  in
  (* The flag is 1-based, the step is 0-based. *)
  let nth = Option.map flags.nth ~f:(fun n -> n - 1) in
  let scope =
    match flags.under with
    | None -> Query.empty
    | Some heading -> Query.section ~exact:false [ heading ] Query.empty
  in
  (* [direct] keeps the search to what the scope holds itself. *)
  if flags.direct then Query.child ~where ?nth scope else Query.descend ~where ?nth scope
;;
