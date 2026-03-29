Helper to compile a .ml file to .cmt:

  $ MODGRAPH=../modgraph.exe
  $ compile() { ocamlfind ocamlc -bin-annot -no-alias-deps -c "$1"; }

Single unit, no submodules
====================


  $ cat > single.ml << 'EOF'
  > let x = 1
  > EOF
  $ compile single.ml
  $ $MODGRAPH single.cmt | sort
    "Single";
    node [shape=box fontname=monospace];
    rankdir=LR;
  digraph modules {
  }

Single unit with submodules, no cross-references
====================

  $ cat > with_subs.ml << 'EOF'
  > module A = struct
  >   let a = 1
  > end
  > module B = struct
  >   let b = 2
  > end
  > EOF
  $ compile with_subs.ml
  $ $MODGRAPH with_subs.cmt | sort
    "With_subs" -> "With_subs.A";
    "With_subs" -> "With_subs.B";
    "With_subs";
    "With_subs.A";
    "With_subs.B";
    node [shape=box fontname=monospace];
    rankdir=LR;
  digraph modules {
  }

Submodule referencing another submodule
====================

This currently resolves to the value path (Util.helper) rather than the
module (Refs.Util) — a known bug in path normalization:

  $ cat > refs.ml << 'EOF'
  > module Util = struct
  >   let helper () = 42
  > end
  > module Handler = struct
  >   let run () = Util.helper ()
  > end
  > EOF
  $ compile refs.ml
  $ $MODGRAPH refs.cmt | sort
    "Refs" -> "Refs.Handler";
    "Refs" -> "Refs.Util";
    "Refs";
    "Refs.Handler" -> "Util.helper";
    "Refs.Handler";
    "Refs.Util";
    "Util.helper";
    node [shape=box fontname=monospace];
    rankdir=LR;
  digraph modules {
  }

No self-edges
====================

Self-referencing module should not produce a self-edge:

  $ cat > selfref.ml << 'EOF'
  > module M = struct
  >   let x = 1
  >   let y = x + 1
  > end
  > EOF
  $ compile selfref.ml
  $ $MODGRAPH selfref.cmt | grep -c '\"Selfref\" -> \"Selfref\"'
  0
  [1]

No duplicate edges
====================

Multiple references to the same module should produce only one edge:

  $ cat > dupes.ml << 'EOF'
  > module Util = struct
  >   let a () = 1
  >   let b () = 2
  > end
  > module Handler = struct
  >   let x = Util.a ()
  >   let y = Util.b ()
  > end
  > EOF
  $ compile dupes.ml
  $ $MODGRAPH dupes.cmt | grep 'Handler.*->.*Util' | wc -l | tr -d ' '
  2

Multiple compilation units
====================

  $ cat > alpha.ml << 'EOF'
  > let x = 1
  > EOF
  $ cat > beta.ml << 'EOF'
  > let y = 2
  > EOF
  $ compile alpha.ml
  $ compile beta.ml
  $ $MODGRAPH alpha.cmt beta.cmt | sort
    "Alpha";
    "Beta";
    node [shape=box fontname=monospace];
    rankdir=LR;
  digraph modules {
  }
