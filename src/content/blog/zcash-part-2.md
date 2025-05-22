---
title: "Private Money: Part 2"
description: "Investigating Project Tachyon"
date: 2024-05-20
tags: ["crypto", "pcs"]
---

**⚠️ Warning:** Mathematics ahead. I assume a solid understanding of high-school math and willingness to bear with me :)


# Some Mathematics Prerequisites

## Musical Motivation

In the 18th century, composer **Johann Sebastian Bach** wrote music that continues to be admired for its elegance and structure. Despite lacking formal mathematical training, Bach often composed with a precision that feels inherently mathematical. One striking example is his *Crab Canon* from *The Musical Offering*:

<div style="display: flex; justify-content: center;">
  <iframe src="https://www.youtube.com/embed/xUHQ2ybTejU" frameborder="0" allowfullscreen style="width: 100%; aspect-ratio: 16/9; max-width: 560px;"></iframe>
</div>

*Bach's Crab Canon from the Musical Offering is a fascinating example of mathematical music. When played forward and backward simultaneously, it creates a perfect palindrome — a musical Möbius strip where the end connects seamlessly to the beginning. This topological structure, where a one-sided surface is created by twisting and joining a strip, mirrors how the canon's melody can be read in both directions while maintaining musical coherence.*

The connection between musical expression and hidden structure has fascinated people for a long time. One of the key ideas here is the [**Circle of Fifths**](https://en.wikipedia.org/wiki/Circle_of_fifths) — a diagram that lays out musical notes so that closely related keys sit next to each other in a loop. It's a handy way to make sense of harmony, and its circular shape hints at something deeper going on beneath the sound: a kind of pattern that music follows, even when we're not consciously aware of it.


<div style="display: flex; flex-direction: column; align-items: center; justify-content: center; text-align: center; gap: 0.5em;">
  <img src="https://upload.wikimedia.org/score/r/n/rn1zaakvsmp2icu895k7e1obrhuy0lw/rn1zaakv.png" alt="Circle of fifths clockwise within one octave" />

  <audio controls>
    <source src="https://upload.wikimedia.org/score/r/n/rn1zaakvsmp2icu895k7e1obrhuy0lw/rn1zaakv.mp3" type="audio/mpeg">
    Your browser does not support the audio element.
  </audio>

  <p><em>Circle of fifths clockwise within one octave. Source: <a href="https://en.wikipedia.org/wiki/Circle_of_fifths">Wikipedia</a>.</em></p>
</div>

Building on this, modern visualizations let us see musical motion as something geometric. In the animation below, a major seventh progression moves across the surface of an umbilic torus — a looping, twisted shape that shows how harmony can circle around while still shifting forward. Like Bach's Canon, it turns music into more than just a line of notes — it becomes a kind of movement through space, shaped by patterns and rules we can start to recognize, even if we can't name them yet.

<div style="display: flex; justify-content: center;">
  <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/9/91/The_circle_of_fifths_on_umbilic_torus_surface.gif/500px-The_circle_of_fifths_on_umbilic_torus_surface.gif" alt="Major 7th progression on umbilic torus surface" style="max-width: 100%; height: auto;" />
</div>

*This animation shows a major seventh progression traced on an umbilic torus surface — a higher-dimensional visualization of the circle of fifths. The smooth rotation through harmonic space represents tonal motion as continuous geometry, revealing deep symmetries between pitch, interval, and curvature.*

These patterns aren't just beautiful — there's clearly something deeper going on under the surface. To really make sense of it, we'll need a new kind of language — one that helps us talk about how music moves, transforms, and loops back on itself. That's where we're headed next: group theory.

## From Loops to Logic

You notice that music "moves" from one key to another in a structured way. To describe these moves precisely, we need a language for how actions combine. That’s where group theory comes in.

A **group** is a set equipped with a rule for combining elements — an operation — that behaves predictably: there's a way to combine any two elements (closure), the way elements are grouped doesn't matter (associativity), there's an identity element that does nothing (identity), and every element has an inverse that undoes it (inverses).

A simple example of a group is a finite cyclic group $\mathbb{Z} / n \mathbb{Z}$, the integers modulo $n$ which consists of the set

$$
    {0, 1, 2, \ldots, n - 1}
$$

with the action being addition modulo $n$. For example, in $\mathbb{Z} / 12 \mathbb{Z}$, $12 \cong 0$ so $7 + 6 = 1 \mod 12$. Check out [Wikipedia](https://en.wikipedia.org/wiki/Group_(mathematics)#:~:text=Definition%20and%20illustration) for the mathematical definition of a group.

It turns out that cyclic groups are also very useful for cryptography. Suppose that you start with $0$ in $\mathbb{Z} / 12 \mathbb{Z}$ and keep adding 5. Then you get the sequence:

$$
    0, 5, 10, 3, 8, 1, \ldots
$$

Eventually, you reach every number in the set. Now, if I asked you to tell how many times you have to add 5 to get the number 8, would you be able to tell me? 

This is the [Discrete Logarithm Problem](https://en.wikipedia.org/wiki/Discrete_logarithm). Given a generator (like 5) and a result (like 8), the challenge is to figure out how many times the generator was applied. In this example, the answer is 4, since $5 \times 4 = 20 \cong 8 \mod 12$.

In cryptography, we often write cyclic groups using **multiplicative notation**, where repeated application of a generator $g$ is written as:

$$
g^0, \; g^1, \; g^2, \; \dots, \; g^{n - 1}
$$

This gives all elements of the group if $g$ is a generator.

The **discrete logarithm problem** asks:
Given $g$ and $h = g^x$, find $x$.

For small numbers, this is easy to solve by trial. But in large cyclic groups — especially those built from primes with hundreds of digits — the problem becomes extremely hard. For small numbers, this is easy to solve by trial. But in large cyclic groups (like those used in Ethereum's BLS signatures, where the modulus is a 381-bit prime) the problem becomes practically impossible to reverse, and that's exactly what makes it secure against [classical computers](https://www.cs.umd.edu/~amchilds/teaching/w08/l02.pdf).