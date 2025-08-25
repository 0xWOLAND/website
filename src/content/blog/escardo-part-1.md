---
title: "Escardó’s Exhaustive Search: Part 1"
description: "Infinite sets that admit fast exhaustive search"
date: 2024-07-25
tags: ["math", "pl theory"]
---

# Introduction

I've recently read [this](https://math.andrej.com/2007/09/28/seemingly-impossible-functional-programs/) old blog post by Andrej Bauer about Martin Escardo's [*Infinite sets that admit fast exhaustive search*](https://martinescardo.github.io/papers/exhaustive.pdf). At first, it seems pretty ridiculous! How is it possible to decide a problem that is embedded in an infinite topological space? However, using some nice tricks from functional programming and higher-level computability, we can achieve this and even explore some other unexpected consequences. 


> 

Any finite set is immediately exhaustible[^1], but the interesting thing is that certain infinite sets can be exhaustible. 

## Notation
The simple types are $\sigma, \tau := o | \tau | \sigma \times \tau | \rightarrow \tau$. Let's dig into this a bit more.
- $o$ the Booleans which in Haskell are `Bool = True | False` 
- $\iota$ (natural numbers), written as `Int`. 
- Product type $\sigma \times \tau$ is the Cartesian product, which is `IntMap a` $: \tau \times$ `(Maybe a)`
- Function type $\sigma \rightarrow \tau$
    - $\iota \rightarrow o$ = predicates on the naturals
    - $o \rightarrow o$ = boolean functions $\neg, \land, \lor$, etc.

In my code, I represent this type as 
```haskell
Oracle a = Int -> a   
```

Using these primitives, we can construct increasingly more complex types as: 

Base case (level 0): 
- $o$
- $\iota$

Level 1: 
- $o \times o$ = pair of booleans `(True, False)`
- $\iota \times \iota$ = pair of naturals `(3, 7)`
- $o \rightarrow o$ = boolean functions `{NOT, AND True, OR False, id, ...}`
- $\iota \rightarrow o$ = predicates on naturals `{isEven, isPrime, λn.n>5}`
- $\iota \rightarrow \iota$ = arithmetic functions, `{successor, λn.2*n, λn.n²}`


In my code, I use 

```haskell
type Oracle a = Int -> a        -- ι → a (where a could be o, ι, etc.)
type Pred a = Oracle a -> Bool  -- (ι → a) → o  
type Prefix = IntMap Bool        -- finite partial assigments  
```
- The domain is a Cantor space $\mathbb{B}^\mathbb{N}$ (all boolean streams)
- A predicate `p` is a higher-type functional $p: (\mathbb{B}^\mathbb{N}) \rightarrow \mathbb{B}$
- a prefix `asg` is a finite partial map $\alpha : S \rightarrow \{0, 1\}$ which is used to define the [cylinder sets](https://en.wikipedia.org/wiki/Cylinder_set) used to define the product topology on the Cantor space. 
### Scott Domains
For each type $\sigma$, there is a **Scott domain** $D_\sigma$ of partial functionals of the that same type $\sigma$ defined by lifting the type $\sigma$ to contain the `undefined` type with all of the properties you would expect (e.g. ordering, etc.). In Haskell, this looks like an `Oracle a` type with potential `Need` exceptions. This lets us capture the behavior of undefined/non-terminating computation, which we use to create *partial* oracles. 


### Total functionals $T_\sigma \subseteq D_\sigma$
A functional is **total** if it maps total inputs to total outputs (so it never produces a `Nothing` type from non-`Nothing`)

```haskell
evalP :: Pred a -> Prefix a -> Either Int Bool
evalP p asg = case unsafePerformIO (try (evaluate (p (oracle undefined asg)))) of
  Left (Need i) -> Left i    -- Partial: needs position i
  Right b -> Right b         -- Total on this prefix
```
So then we can evaluate $P(f^\perp)$ where $f^\perp$ is a partial oracle depending on what the domain is. 

# Constructive Selection Functional for Cantor Space

Now consider the following code: 
```haskell
escardo :: Pred -> Maybe Oracle
escardo p = fmap extend (go IM.empty) where
  extend asg i = IM.findWithDefault False i asg
  go asg = case evalP p asg of
    Right True  -> Just asg
    Right False -> Nothing
    Left i -> case go (IM.insert i False asg) of
      Just a  -> Just a
      Nothing -> go (IM.insert i True asg)
```


[^1]: A set $K$ is *exhaustible* if for any decidable predicate $p$, there is a deterministic algorithm to determine whether all elements of $K$ satisfy $p$. Formally, we can write that for a functional type $(C \rightarrow \{0, 1\}) \rightarrow \{0, 1\}$ with $K \subseteq C$. The input is a predicate $p: C \rightarrow \{0, 1\}$ and the output is $p(x)$ holds for all $x \in K$. 

[^2]: A set $K$ is *searchable* if there is a computable functional $\epsilon_K : (D \rightarrow B) \rightarrow D$ such that for every $p$ defined on $K$, $\epsilon_k (p) \in K$ and $p(x) = $ `True` for some $x \in K$ implies that $p(\epsilon_K (p)) = $ `True`. Thus, searchability $\implies$ exhaustibility. 
