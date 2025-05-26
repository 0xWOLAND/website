---
title: "Private Money: Part 3"
description: "Investigating Project Tachyon: Set Non-Inclusion Accumulator"
date: 2024-05-22
tags: ["crypto", "pcs"]
---

# Background

<div style="display: flex; justify-content: center;">
  <img src="/images/zcash/double_spending.png" alt="Double Spending" style="width: 50%; height: auto;" />
</div>

**Double-spending** is when someone tries to use the same funds more than once. It's a fundamental problem in finance that shows up in different ways. In traditional banking, this appears as **check kiting**: writing checks between accounts to exploit the time it takes for checks to clear. One notorious case involved [Najeeb Khan's $180M scheme](https://apnews.com/article/bank-fraud-classic-cars-keybank-elkhart-d0c9a4a2a66fb88a832a613a8560c49c), where he bounced funds across banks to cover overdrafts, spending millions on luxury assets before the fraud collapsed and harmed thousands of clients.

In crypto, Ethereum Classic suffered multiple 51% attacks in 2020 when attackers gained majority hash power, rewrote history, and [double-spent over $9M (~1.26 million ETC total)](https://www.coinbase.com/blog/coinbases-perspective-on-the-recent-ethereum-classic-etc-double-spend). These incidents showed that without consensus integrity, even public blockchains are vulnerable to double-spending when transaction histories can be rewritten.

Zcash faces a harder challenge because its shielded transactions hide all note details. To prevent double-spending without sacrificing privacy, it enforces two tests: privately prove the note's commitment is in the Merkle tree of minted notes, and publicly show the **nullifier** (a cryptographic fingerprint of the note) hasn't been seen before. This ensures that coins are real and only spent once.

## Why Zcash uses a public non-inclsion check


<div style="display: flex; justify-content: center;">
  <img src="/images/zcash/trees.png" alt="Image of Note-Commitment Tree and Noninclusion Set" style="width: 50%; height: auto;" />
</div>

When a new shielded note is created, the transaction discloses *only a commitment* to that note (the commitment is appended to the global note-commitment Merkle tree and later used in the spend proof while the note's value and recipient remain secret). Because the note-commitment tree is *append-only*, [it is not the job of this tree to protect against double-spending](https://zips.z.cash/protocol/protocol.pdf#page=20). Instead, a second, unlinkable tag--the nullifier--is used to ensure that each note is spent at most once.

<div 

A **nullifier** is a deterministic value unique to each note computed using a [pseudorandom function](https://crypto.stanford.edu/pbc/notes/crypto/prf.html) (A topic that is very deep itself!)[^1]. Because the PRF is collision-resistant and keyed, only the legitimate owner can derive the nullifer (and each note has exactly one). 

[^1]: [In Orchard](https://zcash.github.io/orchard/design/nullifiers.html?highlight=nullifier#nullifiers), the nullifier is computed as a PRF fo the note's two randomizers $\rho, \phi$, the owner's nullifier-deriving key $nk$, and the commitment $cm$. 

> *"A transaction is **not valid** if it would have added a nullifier to the nullifier set that already exists in the set."*

Nullifiers are kept in a global nullifier set that is updated together with the note-commitment tree in every block. A public-noninclusion test prevents double-spends while revealing only a random-looking identifier. 

## Private membership proof 

In Zcash, [zk-SNARKS](https://z.cash/learn/what-are-zk-snarks/) let every node ensure the correctness of shielded transactions while learning *nothing* about *which* notes were spent or *how* much they were worth. Proofs are ~1-2 kB and can be verified in miliseconds regardless of how large the hidden witness is ([Vitalik has a great blog on their magic](https://vitalik.eth.limo/general/2021/01/26/snarks.html)). This keeps CPU cost low and allows even light clients [^2] to verify a proof that internally checks tons of elliptic-curve arithmetic, Merkle-hashes, value-balance equations, and much more. 


[^2]: A [light client](https://ethereum.org/en/developers/docs/nodes-and-clients/light-clients/) is a minimal client that verifies the validity of state or transactions using succinct proofs (e.g. zk-SNARKs), without downloading or executing the full chain, enabling secure and efficient verification on low-resource devices.

## How it works

Each transaction includes an **anchor** $\rho_t$, the root of the note-commitment tree at some prior block height $t$ [^3]. Due to the collision-resistance of the PRF, each root uniquely identifies a tree state. 

To spend a note, the prover *privately* supplies the commitment $cm$ and its Merkle path $\pi$ and proves 

$$
  \text{MerkleRoot}(cm, \pi) = \rho_t
$$

Only the root $\rho_t$ is revealed and observers learn that *some* commitment among many others is being spent but not which one. After teh zk-SNARK verifiers, every full node checks that each nullifier, which is computed as 

$$
  nf = \text{PRF}_{nk} (\rho, \phi_t, cm)
$$

output by the proof is absent from teh current nullifier set. 

[^3]: "Each block in a block chain has a block height. The block height of the genesis block is 0, and the block height of each subsequent block in the block chain increments by 1."