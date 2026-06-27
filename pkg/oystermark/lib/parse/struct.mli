open Cmarkit

(** The colon-keyed restructuring pass. Re-exported from {!Cmarkit.Struct};
    keyed nodes are the single {!Cmarkit.Block.Ext_keyed} constructor. *)

val sexp_of_block : Common.block_sexp
val rewrite_doc : ?paragraph_inline_value:bool -> Cmarkit.Doc.t -> Cmarkit.Doc.t

module For_test : sig
  val count_keyed : Cmarkit.Doc.t -> int
  val doc_of_string : ?paragraph_inline_value:bool -> string -> Cmarkit.Doc.t
  val pp_doc_sexp : Cmarkit.Doc.t -> unit
  val pp_doc_debug : Cmarkit.Doc.t -> unit

  type example =
    { name : string
    ; content : string
    }

  val examples : example list
end
