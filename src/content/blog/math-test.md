---
title: "Testing LaTeX Math Support"
description: "A demonstration of LaTeX math rendering in Astro"
date: 2024-03-20
tags: ["math", "latex", "astro"]
---

# Testing LaTeX Math Support

This post demonstrates how to use LaTeX math in your Astro blog posts.

## Inline Math

You can write inline math using single dollar signs: $E = mc^2$ or $a^2 + b^2 = c^2$.

## Display Math

For display math (centered equations), use double dollar signs:

$$
\frac{d}{dx}e^x = e^x
$$

$$
\begin{align*}
\nabla \times \mathbf{E} &= -\frac{\partial \mathbf{B}}{\partial t} \\
\nabla \times \mathbf{B} &= \mu_0\left(\mathbf{J} + \epsilon_0\frac{\partial \mathbf{E}}{\partial t}\right)
\end{align*}
$$

## Matrix Example

Here's a matrix example:

$$
\begin{bmatrix}
a & b \\
c & d
\end{bmatrix}
\begin{bmatrix}
x \\
y
\end{bmatrix} =
\begin{bmatrix}
ax + by \\
cx + dy
\end{bmatrix}
$$

## Integration Example

And here's an integration example:

$$
\int_{-\infty}^{\infty} e^{-x^2} dx = \sqrt{\pi}
$$

You can use any LaTeX math commands supported by KaTeX. For more information, check out the [KaTeX documentation](https://katex.org/docs/supported.html). 