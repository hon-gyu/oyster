module Attribute = Parse.Attribute
module Frontmatter = Parse.Frontmatter

type cell =
  { id : int
    (** Unique code block id. Most of the time it will be the order of appearance in code blocks in the document *)
  ; lang : string option
  ; info : Attribute.t
  ; content : string
  }

type output =
  { id : int
  ; res : [ `Html of string | `Markdown of string | `Error of string ]
  }

type exec_ctx =
  { config : Yaml.value
  ; inputs : cell list
  }

type exec_result = output list
type executor = exec_ctx -> exec_result

let todo () = failwith "TODO"

let extract_exec_ctx ?(fm_key : string option = None) (doc : Cmarkit.Doc.t) : exec_ctx =
  todo ()
;;

(* uv
==================== *)

type uv_config =
  { version : float
  ; dependencies : string list
  }

let default_uv_config = { version = 3.13; dependencies = [] }
let uv (default_config : uv_config) : executor = todo ()
