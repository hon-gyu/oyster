# Math in Mixed Content

## Lists with Math

1. First item with $x^2$
2. Second item with $\sqrt{2}$
3. Pythagorean theorem: $a^2 + b^2 = c^2$
4. Quadratic: $ax^2 + bx + c = 0$

Unordered list:
- Circle area: $A = \pi r^2$
- Sphere volume: $V = \frac{4}{3}\pi r^3$
- Cylinder volume: $V = \pi r^2 h$

## Tables with Math

| Formula | Description | Domain |
|---------|-------------|--------|
| $a^2 + b^2 = c^2$ | Pythagorean theorem | Right triangles |
| $\pi r^2$ | Area of circle | $r > 0$ |
| $e^{i\theta} = \cos\theta + i\sin\theta$ | Euler's formula | All $\theta$ |

## Code and Math Together

Here's some Python code to calculate factorial:

```python
def factorial(n):
    if n == 0:
        return 1
    return n * factorial(n - 1)
```

And the mathematical definition: $n! = \prod_{i=1}^{n} i$

## Inline Code vs Math

- This is inline code: `x = 5`
- This is inline math: $x = 5$
- They render differently!

## Blockquotes with Math

> The derivative of $x^2$ is $2x$.
>
> This follows from the power rule: $\frac{d}{dx} x^n = nx^{n-1}$

## Multiple Math Blocks in Sequence

First equation:

$$
f(x) = x^2 + 2x + 1
$$

Second equation:

$$
g(x) = \frac{1}{x}
$$

Their composition:

$$
f(g(x)) = \frac{1}{x^2} + \frac{2}{x} + 1
$$

## Math in Headings

### The Function $f(x) = x^2$

This section discusses the function $f(x) = x^2$ and its properties.

## Complex Expression

The probability density function of a normal distribution:

$$
f(x) = \frac{1}{\sigma\sqrt{2\pi}} e^{-\frac{1}{2}\left(\frac{x-\mu}{\sigma}\right)^2}
$$

Where:
- $\mu$ is the mean
- $\sigma$ is the standard deviation
- $\sigma^2$ is the variance
