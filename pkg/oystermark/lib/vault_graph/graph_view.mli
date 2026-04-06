open Common

(** Graph view: visual rendering of a {!Vault_graph.t}.

    Produces JSON or self-contained HTML with an interactive
    force-directed graph widget (d3-force). *)

(** Serialize the graph to JSON.

    Format:
    {v
    { "nodes": [ {"id": <path>, "title": ..., "tags": [...], "folder": ...} ],
      "edges": [ {"source": <path>, "target": <path>} ] }
    v}

    Vertices are collapsed to note-level: only [Note] vertices appear as nodes,
    and edges are deduplicated to note→note. *)
val to_json : t -> string

(** Embeddable widget HTML fragment (style + container + scripts).
    Suitable for inlining into an existing page via an [=html] code block. *)
val to_widget_html : ?config:Config.Home_graph_view.t -> t -> string

(** Produce a self-contained HTML page with an interactive graph widget.
    The JSON data is inlined; no external fetches required. *)
val to_html : t -> string
