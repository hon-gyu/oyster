open Core
open Common_

(** Every link that does not resolve, as [path:line:column: severity: message].
    A query link's failure is an error and any other is a warning; see
    {!page-"query-link".resolution}. *)
let diagnostics (index : Vault.Index.t) : (bool * string) list =
  links index
  |> List.filter_map ~f:(fun (source, (link : Note.Link.t), resolution) ->
    match resolution with
    | Ok _ -> None
    | Error error ->
      let line, line_start = Djot.Textloc.first_line link.loc in
      let column = Djot.Textloc.first_byte link.loc - line_start + 1 in
      let is_error = Vault.Index.is_query_error error in
      Some
        ( is_error
        , sprintf
            "%s:%d:%d: %s: unresolved %s: %s%s: %s"
            source
            line
            column
            (if is_error then "error" else "warning")
            (match link.kind with
             | Link -> "link"
             | Embed -> "embed")
            (Option.value link.reference.target ~default:"")
            (Option.value_map
               link.reference.fragment
               ~default:""
               ~f:Note.Link.Ref.string_of_fragment)
            (Vault.Index.string_of_resolution_error error) ))
;;

let command : Command.t =
  Command.basic
    ~summary:"Report links that do not resolve; exit 1 if a query link fails"
    (let%map_open.Command root = vault_param in
     fun () ->
       let diagnostics = diagnostics (load_vault root).index in
       List.iter diagnostics ~f:(fun (_, line) -> print_endline line);
       if List.exists diagnostics ~f:fst then exit 1)
;;
