# Quiver Examples

## Simple Commutative Diagram

```quiver
\begin{tikzcd}
    A \arrow[r, "f"] \arrow[d, "g"] & B \arrow[d, "h"] \\
    C \arrow[r, "k"] & D
\end{tikzcd}
```

## Category Theory Diagram

```quiver
\begin{tikzcd}
    X \times Y \arrow[r, "\pi_1"] \arrow[d, "\pi_2"] & X \arrow[d, "f"] \\
    Y \arrow[r, "g"] & Z
\end{tikzcd}
```

## Functors

```quiver
\begin{tikzcd}
    \mathcal{C} \arrow[r, "F", bend left] \arrow[r, "G"', bend right] & \mathcal{D}
\end{tikzcd}
```

## Exact Sequence

```quiver
\begin{tikzcd}
    0 \arrow[r] & A \arrow[r, "\alpha"] & B \arrow[r, "\beta"] & C \arrow[r] & 0
\end{tikzcd}
```

## Without tikzcd wrapper

```quiver
A \arrow[r, "f"] & B \arrow[r, "g"] & C
```
