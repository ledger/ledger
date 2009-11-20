import sys

import ledger

print "Welcome to the Ledger.Python demo!"

def assertEqual(pat, candidate):
    if pat != candidate:
        print "FAILED: %s != %s" % (pat, candidate)
        sys.exit(1)

# COMMODITIES

pool = ledger.commodity_pool

usd = pool.find_or_create('$')
eur = pool.find_or_create('EUR')
xcd = pool.find_or_create('XCD')

assertEqual('$', usd.symbol)
assertEqual('$', pool['$'].symbol)

assert not pool.find('CAD')
assert not pool.has_key('CAD')
assert not 'CAD' in pool

# There are a few built-in commodities: null, %, h, m and s
assertEqual([u'', u'$', u'%', u'EUR', u'XCD',
             u'h', u'm', u's'], sorted(pool.keys()))

for symbol in pool.iterkeys(): pass
for commodity in pool.itervalues(): pass

# jww (2009-11-19): Not working: missing conversion from std::pair
#for symbol, commodity in pool.iteritems(): pass
#for symbol, commodity in pool: pass

# This creates a price exchange entry, trading EUR for $0.77 each at the
# current time.
pool.exchange(eur, ledger.Amount('$0.77'))

# AMOUNTS & BALANCES

# When two amounts are multipied or divided, the result carries the commodity
# of the first term.  So, 1 EUR / $0.77 == roughly 1.2987 EUR
amt    = ledger.Amount('$100.12')
market = ((ledger.Amount('1 EUR') / ledger.Amount('$0.77')) * amt)

# An amount's "precision" is a notional thing only.  Since Ledger uses
# rational numbers throughout, and only renders to decimal form for printing
# to the user, the meaning of amt.precision should not be relied on as
# meaningful.  It only controls how much precision unrounded numbers (those
# for which keep_precision is True, and thus that ignore display_precision)
# are rendered into strings.  This is the case, btw, for all uncommoditized
# amounts.
assert not amt.keep_precision
assertEqual(2, amt.precision)
assertEqual(2, amt.display_precision)

assertEqual('$-100.12', str(amt.negated()))  # negate the amount
assertEqual('$0.01', str(amt.inverted()))    # reverse NUM/DEM
assertEqual('$100.12', str(amt.rounded()))   # round it to display precision
assertEqual('$100.12', str(amt.truncated())) # truncate to display precision
assertEqual('$100.00', str(amt.floored()))   # floor it to nearest integral
assertEqual(market, amt.value(eur))          # find present market value
assertEqual('$100.12', str(abs(amt)))        # absolute value
assertEqual('$100.12', str(amt))             # render to a string
assertEqual('100.12', amt.quantity_string()) # render quantity to a string
assertEqual('100.12', str(amt.number()))     # strip away commodity
assertEqual(1, amt.sign())                   # -1, 0 or 1
assert amt.is_nonzero()                      # True if display amount nonzero
assert not amt.is_zero()                     # True if display amount is zero
assert not amt.is_realzero()                 # True only if value is 0/0
assert not amt.is_null()                     # True if uninitialized

assertEqual(100.12, amt.to_double())
assert amt.fits_in_long()
assertEqual(100, amt.to_long())

amt2 = ledger.Amount('$100.12 {140 EUR}')

assert amt2.has_annotation()
assertEqual(amt, amt2.strip_annotations(ledger.KeepDetails()))

# jww (2009-11-19): Not working: missing conversion from optional<amount_t>
#assertEqual(ledger.Amount('20 EUR'), amt.annotation.price)

# VALUES

val = ledger.Value('$100.00')

assert val.is_amount()
assertEqual('$', val.to_amount().commodity.symbol)

# JOURNALS

#journal.find_account('')
#journal.find_or_create_account('')

# ACCOUNTS

#account.name
#account.fullname()
#account.amount
#account.total

# TRANSACTIONS

#txn.payee

# POSTINGS

#post.account

# REPORTING

#journal.collect('-M food')
#journal.collect_accounts('^assets ^liab ^equity')

print 'Demo completed successfully.'
