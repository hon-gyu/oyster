# TikZ Examples

## Simple Arrow

```tikz
\begin{tikzpicture}
    \node (A) at (0,0) {A};
    \node (B) at (2,0) {B};
    \draw[->] (A) -- (B);
\end{tikzpicture}
```

## Graph

```tikz
\begin{tikzpicture}[
    node distance=2cm,
    every node/.style={circle, draw, minimum size=1cm}
]
    \node (1) {1};
    \node (2) [right of=1] {2};
    \node (3) [below of=1] {3};
    \node (4) [right of=3] {4};

    \draw[->] (1) -- (2);
    \draw[->] (1) -- (3);
    \draw[->] (2) -- (4);
    \draw[->] (3) -- (4);
\end{tikzpicture}
```

## Tree Structure

```tikz
\begin{tikzpicture}[
    level distance=1.5cm,
    level 1/.style={sibling distance=3cm},
    level 2/.style={sibling distance=1.5cm}
]
    \node {Root}
        child {node {Left}
            child {node {L1}}
            child {node {L2}}
        }
        child {node {Right}
            child {node {R1}}
            child {node {R2}}
        };
\end{tikzpicture}
```
