(** The flags of [oyster block]: one syntax for a {!Query.t}. *)

type t =
  { under : string option
    (** Only blocks in the section of the heading with this id or text. The
        value is converted to an id with {!Parse.Common.heading_id_of_text}. *)
  ; direct : bool (** With [under], stop the section at the first subheading. *)
  ; kind : string option (** A kind name, see {!Node.kinds}. *)
  ; lang : string option (** Only code blocks whose info string is exactly this. *)
  ; attr_id : string option (** Only the block a link to [ #id ] names. *)
  ; caret_id : string option (** Only the block a link to [ #^id ] names. *)
  ; key : string option (** Only keyed nodes and list items with this key. *)
  ; nth : int option (** Of the blocks the other flags keep, only the [n]th. *)
  }

(** The query [flags] stand for, or an error naming an unknown kind:
    - the scope: {!Query.Sugar.descendants_or_self}, or with [under h] that,
      then [Filter (Named (Heading h)); Section { nested = not direct }], then
      {!Query.Sugar.descendants_or_self} again;
    - then a [Filter] for each of [kind] ({!Query.Sugar.is}), [lang] ([Prop ("info", Eq, _)]),
      [attr_id] and [caret_id] ([Named]), and [key] ([Prop ("key", Eq, _)]);
    - then [Nth] for [nth]. *)
val to_query : t -> (Query.t, string) Result.t
