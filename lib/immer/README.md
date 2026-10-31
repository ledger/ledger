# immer (vendored)

This directory holds the header-only [immer](https://github.com/arximboldi/immer)
library, version 0.9.1, copied verbatim from `include/immer` (the
`experimental/` tree is omitted).  It is distributed under the Boost Software
License 1.0 (see `LICENSE`).

Ledger uses `immer::map` as the storage of `balance_t` (src/balance.h): a
persistent hash map whose copies share structure, so that the running total
kept for every posting of a register costs O(log K) rather than O(K) per
posting.  `cmake/FindImmer.cmake` prefers a system installation (hint:
`-DIMMER_PATH=...`) and falls back to this copy.
