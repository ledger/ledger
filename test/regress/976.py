import ledger

# Regression test for issue #976.
#
# close_journal_files() was calling amount_t::shutdown() followed by
# amount_t::initialize().  shutdown() resets commodity_pool_t::current_pool
# (dropping its refcount) and clears the GMP global temporaries.
# initialize() then creates a *new* pool.
#
# Any commodity_t* raw pointer obtained before the call (including the
# one backing ledger.commodities, captured at Python import time) now
# belongs to the *old* pool and carries a graph_index that is an index
# into the *old* pool's Boost.Graph price-history graph.  When that
# commodity is later passed as the `target` argument to find_price() on
# the *new* pool, vertex(old_index, new_graph) is called with an
# out-of-bounds index.  This corrupts Dijkstra's predecessor/distance
# vectors, ultimately producing a this=0x0 segfault in the ptime
# comparison inside commodity_history_impl_t::find_price.
#
# The fix is to replace only the commodity pool in close_journal_files()
# without touching the GMP temporaries, and to update ledger.commodities
# in py_close_journal_files() so Python code sees the new pool.

# 1. Bare close_journal_files() must not crash.
ledger.close_journal_files()

# 2. Reading a journal, closing, then reading again must not crash.
j1 = ledger.read_journal("test/regress/976.dat")
ledger.close_journal_files()
j2 = ledger.read_journal("test/regress/976.dat")

# 3. ledger.commodities must reflect the new pool after close.
eur = ledger.commodities.find_or_create("EUR")
print(eur.symbol)
