import ledger

# A commodity kept from a pool that Amount.shutdown()/Amount.initialize()
# has replaced carries a graph_index into the old pool's price graph.
# Used as a price target in the new pool, it must find no price, whether
# its index is in range of the new graph (it then names another
# commodity's vertex) or past its end.  See also
# amount-initialize-rebinds-commodities_py.test, which swaps the pool the
# same way.

old_pool = ledger.commodities
in_range = old_pool.find_or_create('EUR')
for i in range(40):
    old_pool.find_or_create('OLD%02d' % i)
out_of_range = old_pool.find_or_create('ZZZ')

ledger.Amount.shutdown()
ledger.Amount.initialize()

ledger.read_journal_from_string("""
P 2024/01/01 AAA 2.00 USD
P 2024/01/01 BBB 3.00 USD
P 2024/01/01 CCC 4.00 USD
P 2024/01/01 DDD 5.00 USD
P 2024/01/01 FFF 6.00 USD
P 2024/01/01 GGG 7.00 USD
""")

usd = ledger.Amount("1 USD").commodity
print(ledger.Amount("1 AAA").value(usd))
print(ledger.Amount("10 USD").value(out_of_range))

# The in-range index names the vertex of one of the new pool's
# commodities (BBB's).  From BBB the source and target vertices are the
# same one, which must not read as "the same commodity"; from every
# other source the walk ends at that vertex, and the amount must not be
# priced in the stale commodity that merely shares its index.
for symbol in ('AAA', 'BBB', 'CCC', 'DDD', 'FFF', 'GGG', 'USD'):
    print(symbol, ledger.Amount("10 " + symbol).value(in_range))
