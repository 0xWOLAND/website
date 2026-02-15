---
title: "Private Money: Part 4"
description: "On Monero's FCMP++ Upgrade"
date: 2025-09-28
tags: crypto, zk
---

**Series Index**  
- [Part 1](https://bhargav.wtf/blog/zcash-part-1/): State-of-the-art private money protocols  
- [Part 2](https://bhargav.wtf/blog/zcash-part-2/): Project Tachyon preliminaries  
- [Part 3](https://bhargav.wtf/blog/zcash-part-3/): Set non-inclusion accumulators  

---

# Introduction

In the [first blog](https://bhargav.wtf/blog/zcash-part-1/) post in this series, I discussed the potential vulnerabilities of the active Monero ring signature protocol and how this sort of heuristic security can lead to _flooding attacks_ and _statistical tracing_. But since then, the Monero community proposed the "FCMPs+SA+L" to defend against such spam attacks. By ensuring that verification cost grows very slowly compared to spam size (using logarithmic-size proofs with recursive composition) and using full-set membership (through the initial FCMP proposal), FCMP++ promises to address the DoS-style spam vectors. 

> In contrast to relying on ad-hoc sampling of decoys, FCMP++ uses bulletproofs to create provable non-inclusion guarantees -- every spend comes with a succinct proof that it hasn't been spent already without requiring global scans or probabilistic mixing.


This is a __substantial change__ from the active Monero protocol. FCMP++ abandons this heuristic model entirely. Instead of sampling decoys, every spend is proven against the entire accumulator of outputs. This proof is _succinct_ and _compositional_ because of the recursive structure of curve trees and bulletproof-style arguments. But now the trust assumptions change: instead of relying on statistical obfuscation, FCMP++ security is based on the algebraic properties of [elliptic curves](https://en.wikipedia.org/wiki/Elliptic-curve_cryptography).

# A Toy Example of the FCMP++ Tree

Suppose that Monero has only $8$ outputs (in reality this would be in the millions). Let them be $o_1, o_2, \cdots, o_8$. At a high-level, the FCMP++ would look like this:

## Commitments for each output
Let $C_i$ be the commitment for $o_i$, which is used to hide the value of $o_i$ but has some algebraic properties. 

## Construct a Merkle (Curve) tree of commitments
The commitments are paired and combined in a Merkle-tree

<iframe class="quiver-embed" src="https://q.uiver.app/#q=WzAsMTUsWzQsMCwiUj1mKE5fezIsMX0sIE5fezIsMn0pIl0sWzIsMSwiTl97MiwxfT1mKE5fezEsMX0sIE5fezEsMn0pIl0sWzYsMSwiTl97MiwyfT1mKE5fezEsM30sIE5fezEsNH0pIl0sWzEsMiwiTl97MSwxfT1mKENfMSwgQ18yKSJdLFszLDIsIk5fezEsMn09ZihDXzMsIENfNCkiXSxbNSwyLCJOX3sxLDN9PWYoQ181LCBDXzYpIl0sWzcsMiwiTl97MSw0fT1mKENfNSwgQ182KSJdLFswLDMsIkNfMSJdLFsxLDMsIkNfMiJdLFsyLDMsIkNfMyJdLFszLDMsIkNfNCJdLFs0LDMsIkNfNSJdLFs1LDMsIkNfNiJdLFs2LDMsIkNfNyJdLFs3LDMsIkNfOCJdLFs3LDNdLFs4LDNdLFszLDFdLFs5LDRdLFsxMCw0XSxbMTEsNV0sWzEyLDVdLFsxMyw2XSxbMTQsNl0sWzEsMF0sWzIsMF0sWzQsMV0sWzUsMl0sWzYsMl1d&embed" width="100%" style="border-radius: 8px; border: none; pointer-events: none;"></iframe>

_Where $f$ usually is a [hash-to-curve](https://datatracker.ietf.org/doc/rfc9380/) function_.

## Membership proofs
To prove that $o_3$ has been spent for example, you generate a succinct membership proof that its commitment $C_3$ is included under the root $R$, by providing the path up the tree $C_3 \to N_{1,2} \to N_{2,1} \to R$. This convinces the verifier that $C_3$ is indeed part of the global accumulator without revealing which leaf you control. 

But, membership itself is not sufficient: we have to also prove that $o_3$ has not been spent yet. In FCMP++, the aforementioned "+L" (Linkability) component of the proposal addresses this. With the membership proof, the spender publishes a unique linking tag $L$ derived from their secret key. Consensus nodes maintain a set of all seen tags such that any repeat indicates a double spend event. Zcash actively employs nullifiers in the same way that Monero proposes for their upgrade.

The distinguishing characteristic for FCMP++ is the underlying machinery. Membership in the curve tree is checked inside of a __generalized bulletproof__ using inner-product arguments and the linkability tag is bound to the spend in a parallel proof. 

Zcash uses [Halo2](https://github.com/zcash/halo2) for this, which is a general-purpose proof system where gadgets can be reused inside a single circuit, making proofs modular and easier to audit.

Generalized Bulletproofs are not a universal circuit SNARK like in Halo2: __you can’t just add a constraint to a global circuit__. Instead, each property you want to prove (membership via a Merkle path, spend authorization with linkability, balance, or range) must be encoded as a specialized algebraic argument expressed through inner-product relations. These arguments are then delicately composed into a single aggregated proof, where Bulletproof's logarithmic-size folding tricks excel at compressing many homogeneous instances (e.g. multiple range proofs) but don't natively support heterogeneous ones. As a result, while "gadgets" exist in the sense of reusable argument templates, they are not modular plug-ins that can be audited independently: each must be hand-designed to fit the aggregation framework.

# More on Merkle-esque Trees

Monero's FCMP++ proposal is built on _curve trees_, which are [a shallow Merkle tree where the leaves and internal nodes are points over an elliptic curve](https://research.protocol.ai/publications/curve-trees-practical-and-transparent-zero-knowledge-accumulators/campanelli2022d.pdf#:~:text=the%20random%20oracle%20model%20,level%20we%20use%20an%20appropriately). Instead of hashing bitstrings, a curve tree uses commitments and alternating elliptic-curve cycles to build an accumulator. The prover then re-randomizes the commitments along the path and proves membership using a generalized Bulletproof and elliptic-curve divisors. This is very reminiscent of the incremental Merkle tree used in Zcash's Orchard pool, where note commitments are appended to a fixed-depth tree  and each spend proves membership in that tree.

Both schemes have to prevent double spends and do so through similar mechanisms. In Orchard, each note derives a nullifier, which are required to be unique via consensus rules. FCMP++ achieves the same thing with a linking tag: a ZK circuit that emits a tag derived from the spender's secret key and includes a commitment $R$ to the randomness used to blind it. In both cases, a deterministic value tied to the note or output is recorded on‑chain, and the consensus layer rejects any repeat of that value, thereby preserving unspentness.

Project Tachyon, Zcash's newest upgrade moves away from this sort of tree accumulator. Zcash currently uses a Merkle tree of note commitments and nullifiers which naturally fit in with Halo2, but the Tachyon upgrade plans to insert nullifiers into a new accumulator that supports efficient set-membership/non-membership proofs in a more succinct/cleaner hash-chain-style accumulator. This removes the necessity for [wallets to maintain Merkle-path witnesses and allows on-chain proofs to shrink even more](https://seanbowe.com/blog/tachyon-scaling-zcash-oblivious-synchronization/#:~:text=services%20from%20learning%20sensitive%20information,This%20can%20be%20achieved) since now __validators only need to check that a nullifier hasn't appeared recently.__ Monero's newest upgrade adopts an algebraic variant of a Merkle-tree approach that is currently being deprecated in favor of a leaner, more succinct accumulator in Tachyon [^4]. 

[^4]: There is a derivation and implementation of the accumulator in Project Tachyon in [my previous blog](https://bhargav.wtf/blog/zcash-part-3/).

# Ecosystem Hurdles

> Bulletproofs in Monero are experimental infrastructure with limited tooling in the broader ZK community, while PLONKish proof systems are well-understood and matured with complete compilers, DSLs, and even zkVMs. They are far less error-prone, easier to adopt, and therefore more auditable and secure in practice.

Developer experience isn't just a convenience factor but has direct security implications. Particularly for Bulletproofs which require __bespoke inner-product arguments for every new property__, and composing these arguments requires even more delicate care. With limited high-level tooling, the risk of subtle implementation flaws is higher. 

Multiple critical exploits have been discovered in Monero security audits (documented [here](http://suyash67.github.io/homepage/assets/pdfs/bulletproofs_plus_audit_report_v1.1.pdf) and [here](https://blog.quarkslab.com/resources/2018-10-22-audit-monero-bulletproof/18-06-439-REP-monero-bulletproof-sec-assessment.pdf)), even in their more restricted implementations of Bulletproofs from a few years ago. To Monero's credit, these vulnerabilities were identified through rigorous auditing processes. However, they demonstrate how challenging these cryptographic systems are to properly constrain—even in limited use cases. This naturally raises questions about the security implications of the generalized Bulletproofs that FCMP++ requires.

PLONKish systems have matured into an entire developer stack: there's a full suite of DSLs, circuit libraries, recursive provers, and zkVMs that make them stable and production-grade. High-level languages like Circom, Noir, and Leo compile directly into PLONK circuits, with libraries like `gnark` and `jellyfish` providing ready-made gadgets. Recursive provers and zkVMs[^2] like [SP1](http://succinct.xyz/) further expand the ecosystem.

PLONK secures billions of dollars in zkRollups such as Polygon zkEVM, zkSync, Scroll, and Aztec, with public gadgets subject to constant audits and scrutiny. Halo2, one of many PLONK-based protocols, benefits from this ecosystem: its modular circuits enable easy sharing of gadgets and optimizations. This breadth of reuse and review makes PLONKish systems established and less experimental in practice, while Bulletproofs, and by extension FCMP++, remain far more isolated within the broader developer ecosystem.

# Final Thoughts
With FCMP++, Monero is finally confronting some of the long-standing vulnerabilities in its anonymity model. By replacing ring signatures and heuristic decoy sampling with a global curve tree accumulator and full-chain membership proofs, Monero moves closer to the stronger, cryptographic security model that systems like Zcash pioneered. This is a significant shift both structurally for Monero but broadly for the definition of privacy for the industry: statistical obfuscation isn't enough. Explicit, algebraic guarantees are necessary.

But Monero's approach is complex: FCMP++ requires composing heterogeneous Bulletproofs with divisor arithmetic and custom gadgets, all within a monolithic proof system. 

This is __untested territory__. Monero is building a one-off proving system for their uses that no other production-grade ecosystem shares in ZK. This is a bold but risky project: on one hand it addresses critical weaknesses in Monero's current design but on the other it does so by venturing into complex and less established terrain. I'm curious to see how this pans out..


[^2]: zkVMs lower the barrier to entry by letting developers write ordinary Rust (or any other RISCV language) programs that compile automatically into PLONK-style circuits, eliminating the need to hand-craft constraints. This expands the ecosystem and driving demand for PLONKish proof systems as the standard.