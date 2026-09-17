open Query_common_

val to_string : t -> string

(** The steps [text] describes, or why it cannot be read. *)
val of_string : string -> (t, string) Result.t

val step_to_string : step -> string
val pred_to_string : pred -> string
