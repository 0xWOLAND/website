---
title: "Private Money: Part 3"
description: "Investigating Project Tachyon: Set Non-Inclusion Accumulator"
date: 2024-05-26
tags: ["crypto", "pcs"]
---

# Background

<div style="background-color: #f8f9fa; padding: 1rem; border-radius: 8px; margin: 1rem 0;">
  <div style="display: flex; justify-content: center;">
    <img src="/images/zcash/double_spending.png" alt="Double Spending" style="width: 50%; height: auto;" />
  </div>
</div>

**Double-spending** is when someone tries to use the same funds more than once. It's a fundamental problem in finance that appears in various guises. In traditional banking, this looks like **check kiting**—manipulating the float time between accounts to cover overdrafts. A notorious example is [Najeeb Khan's $180M fraud](https://apnews.com/article/bank-fraud-classic-cars-keybank-elkhart-d0c9a4a2a66fb88a832a613a8560c49c), where he exploited bank timing windows to fund a lavish lifestyle at the expense of clients.

In crypto, Ethereum Classic suffered [multiple 51% attacks in 2020](https://www.coinbase.com/blog/coinbases-perspective-on-the-recent-ethereum-classic-etc-double-spend) where attackers rewrote transaction history and double-spent over $9M. This showed that public blockchains without secure consensus can be vulnerable too.

Zcash presents a more complex challenge: its shielded transactions reveal nothing about sender, receiver, or value. To prevent double-spending while preserving privacy, Zcash enforces two cryptographic constraints:
1. **Private inclusion proof**: The note must be in the note-commitment Merkle tree.
2. **Public non-inclusion proof**: The note's **nullifier** must not be in the nullifier set.

## Why Zcash uses a public non-inclusion check

<div style="background-color: #f8f9fa; padding: 1rem; border-radius: 8px; margin: 1rem 0;">
  <div style="display: flex; justify-content: center;">
    <img src="/images/zcash/trees.png" alt="Image of Note-Commitment Tree and Noninclusion Set" style="width: 50%; height: auto;" />
  </div>
</div>

When a new shielded note is created, only its **commitment** is revealed. This is appended to the global note-commitment Merkle tree, while the note's actual value and recipient stay private. Because the tree is append-only and doesn't track spending, a second tag—the **nullifier**—ensures each note is spent at most once.

A **nullifier** is a deterministic, unlinkable fingerprint of a note. It's computed using a [pseudorandom function](https://crypto.stanford.edu/pbc/notes/crypto/prf.html) (PRF) keyed by the note's secret. Only the owner can derive the nullifier, and each note has exactly one[^1].

[^1]: [In Orchard](https://zcash.github.io/orchard/design/nullifiers.html?highlight=nullifier#nullifiers), the nullifier is computed as a PRF fo the note's two randomizers $\rho, \phi$, the owner's nullifier-deriving key $nk$, and the commitment $cm$. 

> *"A transaction is not valid if it would have added a nullifier to the nullifier set that already exists in the set."*

The nullifier is publicly revealed and checked against the nullifier set, which is updated each block along with the Merkle tree. This allows the network to reject double-spends while learning nothing about the note itself.

## Private membership proof

Zcash uses [zk-SNARKs](https://z.cash/learn/what-are-zk-snarks/) to enforce both constraints—commitment inclusion and nullifier consistency—without revealing which note is spent. Each proof is ~1–2 kB, verifiable in milliseconds, and works even for light clients[^2].

### 1. The private link created inside the zk-SNARK

**Merkle inclusion**:  
The prover supplies the note commitment $cm$ and a Merkle authentication path $\pi$ as private witness data. The circuit enforces:
$$
\text{MerkleRoot}(cm, \pi) = \rho_t
$$
where $\rho_t$ is the **anchor**, a public input representing the Merkle root at block height $t$ [^3]. This confirms that $cm$ appears somewhere in the historical note-commitment tree.

**Nullifier computation**:  
In the same circuit, the prover recomputes the nullifier using:
$$
nf = \text{PRF}_{nk}(\rho_t, \psi, cm)
$$
where $(nk, \psi)$ are secrets derived from the note. The output nullifier $nf$ is made public and bound to the same anchor and commitment.

This internal linkage ensures that the SNARK proves consistency between the revealed nullifier and the private note it was derived from—without revealing anything about the note itself.

### 2. How the on-chain check works

Each transaction includes the anchor $\rho_t$ and the nullifier $nf$ in the public input of the proof. Once the SNARK verifies, every full node checks:
- That the nullifier $nf$ is **not already in** the nullifier set $N(t)$.
- If the check passes, the block updates:
  $$
  N(t+1) = N(t) \cup \{nf\}
  $$

This ensures that every nullifier appears at most once—enforcing one-time spendability.

### 3. Why the two links suffice

- **Existence**: The Merkle equation certifies that a real commitment $cm$ already sits in the tree whose root is $\rho_t$.
- **Uniqueness**: The PRF binds a single nullifier $nf$ to that $cm$, and consensus allows each $nf$ to appear only once.

Hence, any valid transaction must:
- Reference some existing note (via inclusion of $cm$),
- And cannot reuse that note (since its $nf$ would already be in $N$).

The inclusion proof and the non-inclusion check are **mathematically fixed** through the shared variables $(cm, \rho_t, nf)$—inside the SNARK and on-chain.

[^2]: A [light client](https://ethereum.org/en/developers/docs/nodes-and-clients/light-clients/) verifies state transitions using succinct proofs instead of downloading the full chain. This allows secure operation on low-resource devices.

[^3]: Each block in Zcash has a note-commitment tree of height. The genesis block is height 0, and each subsequent block increments this height by 1.

# Limitations of Merkle Trees

Incremental Merkle trees[^4] are the classic way Zcash records shielded notes[^5]. They have a fixed depth $d$, so the ledger can accept at most $2^d$ commitments before a migration is needed. Every new note becomes a fresh leaf, and the tree's collision-resistant hashing lets a prover later show inclusion with a $d$-hash path. That path is constant-size and efficient, but the tree's *state grows forever*, and wallets must track every new leaf to keep their stored paths current.

[^4]: An incremental Merkle tree is a binary tree that supports efficient, append-only updates: each new element is added as the next available leaf, and only the hashes along its path to the root are recomputed. This allows the Merkle root to evolve over time without rebuilding the whole tree, enabling short inclusion proofs that stay constant in size.

[^5]: Zcash uses incremental Merkle trees to maintain a commitment tree of all shielded notes. As each note is created, its commitment is appended to the next empty leaf. Internal nodes are updated on-the-fly, and the Merkle root evolves incrementally. Inclusion proofs are short (one hash per level), and the current root is used as a public anchor in each transaction. This enables privacy-preserving spending proofs without revealing which note is spent.


### Sharding helps, but doesn't solve the problem

A natural next step is to **shard** the Merkle tree. Instead of one monolith, the ledger maintains many sub-trees (e.g., $2^{32}$-leaf trees). When a shard fills, a new one is opened, and a small **root-of-roots tree** tracks the current shard roots. A note's inclusion path now includes:
- A short *intra-shard* Merkle path
- One additional hash to reach the root-of-roots

This design keeps proof sizes small and **reduces wallet update overhead**: clients only need to track changes in shards that contain their notes [^6].

But even this is a **temporary fix**. The root-of-roots is itself incremental and will eventually fill. Historical paths must still be updated as long as the shard is active. State pruning remains difficult because **every note ever minted must remain accessible**. As a result, ledger size and wallet sync bandwidth **grow linearly with protocol lifetime**.

[^6]: Sharding solves UX problems around note detection and spendability. See [this forum post](https://forum.zcashcommunity.com/t/improving-ux-with-detection-keys/46372/14) for details on the tradeoffs and design.

### Why a set non-inclusion accumulator changes everything

A **set non-inclusion accumulator** breaks the scaling wall imposed by Merkle trees.

Instead of embedding every commitment into a massive hash tree, all notes are folded into a **constant-size accumulator value** $A_t$. Every insertion is a **succinct polynomial-commitment update**, and old accumulator states can be discarded—because an IVC (incremental verifiable computation) chain certifies correctness across updates.

The magic lies in the accumulator's recursive structure: each update witnesses that a *vector* of notes was inserted without including a particular element $x$. The non-membership claim is upheld step-by-step, proving that each polynomial inserted lacked $x$ as a root—meaning $x$ was not present. This transforms the problem into one of recursive algebra, not storage.

At the end, proving **non-inclusion** ("this nullifier was never inserted up to $A_t$") requires only checking that a single polynomial (the one folded into the accumulator) **does** have $x$ as a root. This final step is **constant-time** and easily SNARK-friendly.

* The accumulator's size **never grows**, no matter how many notes are inserted
* Wallet witnesses **never need refreshing**—history is abstracted away
* There's **no depth cap** or leaf index to exhaust
* The ledger stays **light and syncs fast**—forever
* Each IVC step is just a few hashes and group ops—efficient even onchain
* You don't need to track historical state prior to the last $k$ epochs—as long as all proofs spanning that range have been generated, the earlier accumulator data can be safely discarded

In short: **shielded anonymity can grow indefinitely**, with no migrations, no proof bloat, and no path rewrites. This is the foundation for **Project Tachyon's accumulator design**—a system where privacy doesn't get more expensive as it scales.

--- 

# Implementation 

The accumulator starts in a trivial state $A_0 = \langle(1,0,0,\ldots), G\rangle$ and is updated every time we insert a **vector of field elements**
$\mathbf a_i = (a_{i,1},\dots,a_{i,k}) \subset \mathbb F$.
After many updates we want to show, for *every* step in a chosen range $[j,\,m)$, that the inserted vector **never contained** some element $v$.
Instead of storing all past vectors, we fold them into a **single curve-point accumulator** whose size never grows.


### 1. Insertion: folding one vector of roots

For the current step we first build its *vanishing polynomial*

$$
a_i(X)=\prod_{r\in\mathbf a_i}(X-r),
$$

then make a Pedersen commitment $P_i$ to the coefficient vector.
One Fiat–Shamir challenge $h=H(A_i,P_i)$ lets us hop to the next state

$$
A_{i+1}=[h]\,A_i + P_i .
$$

```rust
pub fn insert(roots: &[Fr], a_prev: G1Affine, r: Fr) -> Result<State> {
    let poly   = poly_from_roots(roots);           //  a_i(X)
    let p_i    = commit(&poly.coeffs, r)?;         //  P_i
    let h      = hash_points_to_fr(&a_prev, &p_i); //  H(A_i,P_i)
    let next   = a_prev * h + p_i;                 //  A_{i+1}
    Ok(State { Accumulator: next.into_affine(), Commitment: p_i })
}
```

*Key point:* no matter how many vectors we add, `Accumulator` stays a *single* point.


### 2. Proving that a fresh value $v$ was **not** among today's roots

The verifier will accept only if the polynomial we committed **doesn't vanish** at $v$.

1. Evaluate once: $\alpha = a_i(v)$.
   If $\alpha=0$ the proof must abort (v was a root).

2. Shift the commitment so the **shifted polynomial *does* vanish at $v$**:
   $P'_i = P_i - [\alpha]G_0$
3. Use a second challenge $h' = H(S_i, P'_i)$ to hop the non-membership accumulator.

```rust
pub fn check_non_membership(
        roots: &[Fr], v: Fr, r: Fr, s_prev: G1Affine) -> Result<State>
{
    let poly   = poly_from_roots(roots);
    let alpha  = evaluate_poly(&poly.coeffs, v);     // α = a_i(v)
    assert!(!alpha.is_zero());                       // must be non-root

    let p_i    = commit(&poly.coeffs, r)?;           // P_i
    let p_ip   = p_i - POINTS[0] * alpha;            // P'_i
    let h_p    = hash_points_to_fr(&s_prev, &p_ip);  // h′ = H(S_i,P′_i)
    let next   = s_prev * h_p + p_ip;                // S_{i+1}
    Ok(State { Accumulator: next.into_affine(), Commitment: p_i })
}
```

Because $a_i(v)\neq0$, the *shifted* commitment now hides a polynomial whose **only** root at $v$ is the one we artificially created—exactly the witness we need for non-membership.

### 3. Non-membership across a **range** with IVC

The HackMD text says: *"Create a recursive (IVC) proof whose base case is $(A_j,S_j)$... and repeat the above step for every $i \ge j$."*

* In the circuit we start from snapshot $(A_j,S_j)$.
* At each iteration we witness $(P_i, \alpha_i)$ and apply the `check_non_membership` hop.
* The accumulator moves from $(A_i,S_i)$ to $(A_{i+1},S_{i+1})$.
* Only the **current** state crosses the IVC boundary; earlier data are discarded.

After $m-j$ hops the verifier sees $(A_m,S_m)$.
If `S_m` opens *to zero at $v$*, then—by induction—every intermediate polynomial was proven to evaluate *non-zero* at $v$.  Hence $v$ never appeared in any root vector $\mathbf a_j$.

### 4. Why this matters for Zcash-style nullifier checks

* **O(1) state:** `Accumulator` is one point $\rightarrow$ no Merkle depth cap.
* **Forget history:** once the IVC proof exists, we can delete every old vector.
* **Cheap per block:** each hop is a couple of hashes and group operations
* **Scalable non-membership:** perfect for showing a nullifier *never* appeared, without keeping the nullifier set on-chain.

In practice, replacing Zcash's append-only Merkle trees with this accumulator would remove anchor leakage and tree maintenance, while still proving that a nullifier is uniquexactly the promise of Project Tachyon's non-inclusion accumulator.


---

# Conclusion

A vector-commitment accumulator gives Zcash the one feature Merkle trees cannot: **constant-size state and proofs**, no matter how many shielded notes exist. That single property is *critical* for long-term scalability, eliminating depth caps, tree migrations, and anchor-leakage trade-offs.

Real-world deployment still has open questions—metadata privacy, lightweight witness updates, etc. Check out my implementations of this accumulator below. 

| Prototype     | Idea                                                    | Code                                                                                                                                                       |
| ------------- | ------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------- |
| PCS-only      | accumulator + polynomial commitment                     | [https://github.com/0xWOLAND/set-noninclusion](https://github.com/0xWOLAND/set-noninclusion)                                                               |
| Folded proofs | same accumulator, block-by-block recursion with the zkVM SP1| [https://github.com/0xWOLAND/sp1-noninclusion](https://github.com/0xWOLAND/sp1-noninclusion) built with [SP1](https://www.succinct.xyz/) |

For the full design sketch and code prototypes, see [Sean Bowe's HackMD](https://hackmd.io/@dJO3Nbl4RTirkR2uDM6eOA/BJOnrTEj1x).


# Appendix 

The vector-commitment accumulator guarantees **existence** (the value really was inserted) and **uniqueness** (no two different vectors can open to the same commitment at the same index) for two independent reasons:

#### Existence in the IVC non-membership chain

For non-inclusion, each IVC step asserts $p_t(z)\neq0$.  Because the polynomial $p_t$ encodes *exactly* the vector inserted at step $t$, the statement "$z$ was not in ${\bf v}_t$" is true **iff** the evaluation is non-zero.  The final recursive proof therefore certifies that *for every step in the range*, $z$ was not a coordinate of any inserted vector.  That is an *existential* statement about the entire history, achieved with only $O(1)$ verifier work.

#### Uniqueness of a nullifier-style opening

If we swap the vector commitment for one that derives a nullifier‐like output (e.g. include the index $j$ and value $v_j$ in a PRF), the uniqueness follows the same logic: the PRF output is a function of a **single** valid opening, and the binding of the commitment ensures no second, distinct opening can produce the same output without violating discrete-log or collision-resistance.

In short, the algebraic binding of the commitment bases **anchors existence**, while their linear independence **enforces uniqueness**—properties that survive each IVC update and give the accumulator the same double-spend resistance Merkle nullifiers provide, but with constant-size state and proofs.
