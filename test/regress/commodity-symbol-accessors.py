import ledger

# Commodity.symbol, Commodity.base_symbol and str(Commodity) return
# Python strings: the symbol as written (quoted when it needs quotes) and
# the bare base symbol.  An annotated lot reports its base's symbols.

plain = ledger.commodities.find_or_create('EUR')
quoted = ledger.commodities.find_or_create('A1 B')
lot = ledger.Amount('10 AAPL {$5.00}').commodity

for comm in (plain, quoted, lot):
    print('%s|%s|%s|%s' % (comm.symbol, comm.base_symbol, str(comm), type(comm.symbol).__name__))
