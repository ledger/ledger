# immer (vendored)

This directory holds the part of the header-only
[immer](https://github.com/arximboldi/immer) library, version 0.9.1, that
`immer/map.hpp` and `immer/map_transient.hpp` include: 28 headers, copied
verbatim from `include/immer` (the vectors, sets, tables and the other heaps are
left out; to update, copy those two headers and follow their `#include
<immer/...>` lines).  It is distributed under the Boost Software License 1.0
(see `LICENSE`).

`champ::empty()` builds the shared empty node in static storage sized for a
node without a collision buffer, which clang's UBSan object-size check reports
on the first balance of a process in optimized builds; `ubsan-suppressions.txt`
at the top level silences that report (every write stays within the storage).

Ledger uses `immer::map` as the storage of `balance_t` (src/balance.h): a
persistent hash map whose copies share structure, so that the running total
kept for every posting of a register costs O(log K) rather than O(K) per
posting.  `cmake/FindImmer.cmake` prefers a system installation (hint:
`-DIMMER_PATH=...`) and falls back to this copy.
