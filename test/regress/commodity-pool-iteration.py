import ledger

# ledger.commodities behaves like a read-only dict keyed by symbol: iterating
# it yields the symbols, and iteritems() yields (symbol, Commodity) pairs.

ledger.Amount('$1')
ledger.Amount('10 EUR')
ledger.Amount('2 AAPL {$5}')  # annotated: kept in a separate map
comms = ledger.commodities

print('iter %s' % sorted(comms))
print('iterkeys %s' % sorted(comms.iterkeys()))
print('itervalues %s' % sorted(c.symbol for c in comms.itervalues()))
items = dict(comms.iteritems())
print('iteritems %s' % sorted(items))
print('symbols match %s' % all(items[k].symbol == k for k in items))
print('same commodity %s' % (items['EUR'] == comms['EUR']))
