---
title: "Private Money: Part 2"
description: "Investigating Project Tachyon: Preliminaries"
date: 2024-05-21T00:00:00Z
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

You notice that music "moves" from one key to another in a structured way. To describe these moves precisely, we need a language for how actions combine. That's where group theory comes in.

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

For small numbers, this is easy to solve by trial. But in large cyclic groups (especially those built from primes with hundreds of digits) the problem becomes extremely hard. For small numbers, this is easy to solve by trial. But in large cyclic groups (like those used in Ethereum's BLS signatures, where the modulus is a 381-bit prime) the problem becomes practically impossible to reverse, and that's exactly what makes it secure against [classical computers](https://www.cs.umd.edu/~amchilds/teaching/w08/l02.pdf).

# Pedersen Vector Commitment Scheme

Finally, we have the language to talk about tools that rely on group structure to ensure both **hiding** and **binding**: the two essential properties of a cryptographic commitment. One of the simplest and most elegant examples is the **Pedersen commitment**.

## Hiding and Binding

Before going further, it's worth pausing to explain what we mean by *hiding* and *binding*.

* **Hiding** means the commitment doesn't reveal any information about the underlying message. Even if someone sees the commitment, they can't figure out what value was committed — because it's masked using randomness.

* **Binding** means that once you've committed to a value, you can't later change your mind. That is, you can't open the same commitment to a different value. This ensures the commitment is fixed and can't be altered after the fact.

In short: hiding protects privacy; binding ensures integrity.

## The Pedersen Commitment

Let $G$ be a cyclic group of prime order $q$, with generators $g$ and $h$ such that no one knows the discrete logarithm between them. To commit to a value $m \in \mathbb{Z}_q$, choose a random blinding factor $r \in \mathbb{Z}_q$ and compute:

$$
\text{Com}(m, r) = g^m h^r
$$

This is a commitment to $m$ that is:

* **Perfectly hiding**: because $r$ is chosen at random, the output reveals nothing about $m$
* **Computationally binding**: under the discrete log assumption, it's infeasible to find two different pairs $(m, r)$ and $(m', r')$ that yield the same commitment

Additionally, Pedersen commitments are **homomorphic**:

$$
\text{Com}(m_1, r_1) \cdot \text{Com}(m_2, r_2) = \text{Com}(m_1 + m_2, r_1 + r_2)
$$

This means commitments can be added without opening them, a property useful in many protocols.

## From Commitments to Vector Commitments

To commit to a whole vector $\mathbf{m} = (m_1, m_2, \dots, m_n)$, we extend the idea by using $n$ independent generators $g_1, g_2, \dots, g_n \in G$, and a single blinding base $h$. The commitment is:

$$
\text{Com}(\mathbf{m}, r) = g_1^{m_1} \cdot g_2^{m_2} \cdots g_n^{m_n} \cdot h^r
$$

This compactly binds the entire vector $\mathbf{m}$ into a single group element. It maintains the same properties:

* **Hiding**, because the random $r$ masks the entire vector
* **Binding**, assuming the generators $g_1, \dots, g_n$ are independent and the discrete log relationships between them are unknown

With additional structure, such commitments can also support **position-wise openings**, allowing you to reveal just one component $m_i$ and prove that it was committed to without revealing the rest.

## Algebraic Properties

What makes Pedersen (vector) commitments especially powerful is their algebraic structure:

* **Linearity**: Commitments respect linear combinations:

  $$
  \text{Com}(\mathbf{m}, r) \cdot \text{Com}(\mathbf{m}', r') = \text{Com}(\mathbf{m} + \mathbf{m}', r + r')
  $$

  where vector addition is component-wise.

* **Scalability**: You can aggregate commitments across multiple vectors:

  $$
  \prod_{i=1}^k \text{Com}(\mathbf{m}^{(i)}, r_i) = \text{Com}\left(\sum_{i=1}^k \mathbf{m}^{(i)}, \sum_{i=1}^k r_i\right)
  $$

* **Inner-product compatibility**: Because exponentiation distributes over sums, Pedersen commitments can be used inside inner-product arguments (like in Bulletproofs or IPA schemes), where both prover and verifier can manipulate commitments algebraically without knowing the underlying messages.

* **Non-interactive opening proofs**: Given $\mathbf{m}$ and $r$, it's trivial to open the commitment and prove correctness. Zero-knowledge variants can be layered on top if needed.

These algebraic properties make Pedersen commitments a favorite building block in privacy-preserving protocols, SNARK-friendly constructions, and succinct proofs of integrity over large datasets.