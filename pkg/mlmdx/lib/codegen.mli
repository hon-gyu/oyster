type origin =
  { byte : int
  ; line : int
  }

type split =
  { prelude : string
  ; body : string
  ; body_origin : origin
  }

val split_initial_prelude : string -> split

val of_string : file:string -> string -> Parsetree.structure
