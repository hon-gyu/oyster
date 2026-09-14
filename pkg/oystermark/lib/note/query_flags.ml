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
  let scope : Query.t =
    match flags.under with
    | None -> Query.Sugar.descendants_or_self
    | Some heading ->
      Query.Sugar.descendants_or_self
      @ [ Query.Filter (Named (Heading (Parse.Common.heading_id_of_text heading)))
        ; Query.Section { nested = not flags.direct }
        ]
      @ Query.Sugar.descendants_or_self
  in
  let filter make value = Option.map value ~f:(fun v -> Query.Filter (make v)) in
  scope
  @ List.filter_opt
      [ filter Query.Sugar.is kind
      ; filter (fun lang -> Query.Prop ("info", Eq, String lang)) flags.lang
      ; filter (fun id -> Query.Named (Attr id)) flags.attr_id
      ; filter (fun id -> Query.Named (Caret id)) flags.caret_id
      ; filter (fun key -> Query.Prop ("key", Eq, String key)) flags.key
      ; Option.map flags.nth ~f:(fun n -> Query.Nth n)
      ]
;;
