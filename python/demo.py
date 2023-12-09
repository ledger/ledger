#!/usr/bin/env python3

import sys
from datetime import datetime

# The following literate program will demonstrate, by example, how to use the
# Ledger Python module to access your data and build custom reports using the
# magic of Python.

import ledger

print("Welcome to the Ledger.Python demo!")

# Some quick helper functions to help us assert various types of truth
# throughout the script.

def assertEqual(pat, candidate):
    if pat != candidate:
        raise Exception("FAILED: %s != %s" % (pat, candidate))
        sys.exit(1)

###############################################################################
#
# COMMODITIES
#
# Every amount in Ledger has a commodity, even if it is the "null commodity".
# What's special about commodities are not just their symbol, but how they
# alter the way amounts are displayed.
#
# For example, internally Ledger uses infinite precision rational numbers,
# which have no decimal point.  So how does it know that $1.00 / $0.75 should
# be displayed as $1.33, and not with an infinitely repeating decimal?  It
# does it by consulting the commodity.
#
# Whenever an amount is encountered in your data file, Ledger observes how you
# specified it:
#   - How many digits of precision did you use?
#   - Was the commodity name before or after the amount?
#   - Was the commodity separated from the amount by a space?
#   - Did you use thousands markers (1,000)?
#   - Did you use European-style numbers (1.000,00)?
#
# By tracking this information for each commodity, Ledger knows how you want
# to see the amount in your reports.  This way, dollars can be output as
# $123.56, while stock options could be output as 10.113 AAPL.
#
# Your program can access the known set of commodities using the global
# `ledger.commodities'.  This object behaves like a dict, and support all of
# the non-modifying dict protocol methods.  If you wish to create a new
# commodity without parsing an amount, you can use the method
# `find_or_create':

comms = ledger.commodities

usd = comms.find_or_create('$')
xcd = comms.find_or_create('XCD')

# Tests currency symbols encoded using UCS. For details see #2132.
eur = comms.find_or_create('€') # UCS-2 / UCS-4
xxx = comms.find_or_create('¤') # UCS-1

assert not comms.find('CAD')
assert not comms.has_key('CAD')
assert not 'CAD' in comms

# The above mentioned commodity display attributes can be set using commodity
# display flags.  This is not something you will usually be doing, however, as
# these flags can be inferred correctly from a large enough set of sample
# amounts, such as those found in your data file.  If you live in Europe and
# want all amounts to default to the European-style, set the static variable
# `european_by_default'.

eur.add_flags(ledger.COMMODITY_STYLE_DECIMAL_COMMA)
assert eur.has_flags(ledger.COMMODITY_STYLE_DECIMAL_COMMA)
assert not eur.has_flags(ledger.COMMODITY_STYLE_THOUSANDS)

comms.european_by_default = True

# There are a few built-in commodities: null, %, h, m and s.  Normally you
# don't need to worry about them, but they'll show up if you examine all the
# keys in the commodities dict.

assertEqual([u'', u'$', u'%', u'XCD', u'h', u'm', u's', u'¤', u'€'],
            sorted(comms.keys()))

# All the styles of dict iteration are supported:

for symbol in comms.iterkeys():
    pass
for commodity in comms.itervalues():
    pass
#for symbol, commodity in comms.iteritems():
#    pass
#for symbol, commodity in comms:
#    pass

# Another important thing about commodities is that they remember if they've
# been exchanged for another commodity, and what the conversion rate was on
# that date.  You can record specific conversion rates for any date using the
# `exchange' method.

comms.exchange(eur, ledger.Amount('$0.77')) # Trade 1 € for $0.77
comms.exchange(eur, ledger.Amount('$0.66'), datetime.now())

# For the most part, however, you won't be interacting with commodities
# directly, except maybe to look at their `symbol'.

assertEqual('$', usd.symbol)
assertEqual('$', comms['$'].symbol)

###############################################################################
#
# AMOUNTS & BALANCES
#
# Ledger deals with two basic numerical values: Amount and Balance objects.
# An Amount is an infinite-precision rational with an associated commodity
# (even if it is the null commodity, which is called an "uncommoditized
# amount").  A Balance is a collection of Amounts of differing commodities.
#
# Amounts support all the math operations you might expect of an integer,
# except it carries a commodity.  Let's take dollars for example:

zero  = ledger.Amount("$0")
one   = ledger.Amount("$1")
oneb  = ledger.Amount("$1")
two   = ledger.Amount("$2")
three = ledger.Amount("3")      # uncommoditized

assert one == oneb              # numeric equality, not identity
assert one != two
assert not zero                 # tests if it would *display* as a zero
assert one < two
assert one > zero

# For addition and subtraction, only amounts of the same commodity may be
# used, unless one of the amounts has no commodity at all -- in which case the
# result uses the commodity of the other value.  Adding $10 to 10 €, for
# example, causes an ArithmeticError exception, but adding 10 to $10 gives
# $20.

four = ledger.Amount(two)       # make a copy
four += two
assertEqual(four, two + two)
assertEqual(zero, one - one)

try:
    two += ledger.Amount("20 €")
    assert False
except ArithmeticError:
    pass

# Use `number' to get the uncommoditized version of an Amount

assertEqual(three, (two + one).number())

# Multiplication and division does supports Amounts of different commodities,
# however:
#   - If either amount is uncommoditized, the result carries the commodity of
#     the other amount.
#   - Otherwise, the result always carries the commodity of the first amount.

five = ledger.Amount("5 CAD")

assertEqual(one, two / two)
assertEqual(five, (five * ledger.Amount("$2")) - ledger.Amount("5"))

# An amount's commodity determines the decimal precision it's displayed with.
# However, this "precision" is a notional thing only.  You can tell an amount
# to ignore its display precision by setting `keep_precision' to True.
# (Uncommoditized amounts ignore the value of `keep_precision', and assume it
# is always True).  In this case, Ledger does its best to maintain maximal
# precision by watching how the Amount is used.  That is, 1.01 * 1.01 yields a
# precision of 4.  This tracking is just a best estimate, however, since
# internally Ledger never uses floating-point values.

amt  = ledger.Amount('$100.12')
mini = ledger.Amount('0.00045')

assert not amt.keep_precision

assertEqual(5, mini.precision)
assertEqual(5, mini.display_precision) # display_precision == precision
assertEqual(2, amt.precision)
assertEqual(2, amt.display_precision)

mini *= mini
amt  *= amt

assertEqual(10, mini.precision)
assertEqual(10, mini.display_precision)
assertEqual(4, amt.precision)
assertEqual(2, amt.display_precision)

# There are several other supported math operations:

amt    = ledger.Amount('$100.12')
market = ((ledger.Amount('1 €') / ledger.Amount('$0.77')) * amt)

assertEqual(market, amt.value(eur))            # find present market value

assertEqual('$-100.12', str(amt.negated()))    # negate the amount
assertEqual('$-100.12', str(- amt))            # negate it more simply
assertEqual('$0.01',    str(amt.inverted()))   # reverse NUM/DEM
assertEqual('$100.12',  str(amt.rounded()))    # round it to display precision
assertEqual('$100.12',  str(amt.truncated()))  # truncate to display precision
assertEqual('$100.00',  str(amt.floored()))    # floor it to nearest integral
assertEqual('$100.12',  str(abs(amt)))         # absolute value
assertEqual('$100.12',  str(amt))              # render to a string
assertEqual('100.12',   amt.quantity_string()) # render quantity to a string
assertEqual('100.12',   str(amt.number()))     # strip away commodity
assertEqual(1,          amt.sign())            # -1, 0 or 1
assert amt.is_nonzero()                        # True if display amount nonzero
assert not amt.is_zero()                       # True if display amount is zero
assert not amt.is_realzero()                   # True only if value is 0/0
assert not amt.is_null()                       # True if uninitialized

# Amounts can also be converted the standard floats and integers, although
# this is not recommend since it can lose precision.

assertEqual(100.12, amt.to_double())
assert amt.fits_in_long()       # there is no `fits_in_double'
assertEqual(100, amt.to_long())

# Finally, amounts can be annotated to provide additional information about
# "lots" of a given commodity.  This example shows $100.12 that was purchased
# on 2009/10/01 for 140 €.  Lot information can be accessed through via the
# Amount's `annotation' property.  You can also strip away lot details to get
# the underlying amount.  If you want the total price of any Amount, by
# multiplying by its per-unit lot price, call the `Amount.price' method
# instead of the `Annotation.price' property.

amt2 = ledger.Amount('$100.12 {140 €} [2009/10/01]')

assert amt2.has_annotation()
assertEqual(amt, amt2.strip_annotations())

assertEqual(ledger.Amount('140 €'), amt2.annotation.price)
assertEqual(ledger.Amount('14016,8 €'), amt2.price()) # european amount!

###############################################################################
#
# VALUES
#
# As common as Amounts and Balances are, there is a more prevalent numeric
# type you will encounter when generating reports: Value objects.  A Value is
# a variadic type that can be any of the following types:
#   - Amount
#   - Balance
#   - boolean
#   - integer
#   - datetime
#   - date
#   - string
#   - regex
#   - sequence
#
# The reason for the variadic type is that it supports dynamic self-promotion.
# For example, it is illegal to add two Amounts of different commodities, but
# it is not illegal to add two Value amounts of different commodities.  In the
# former case an exception in raised, but in the latter the Value simply
# promotes itself to a Balance object to make the addition valid.
#
# Values are not used by any of Ledger's data objects (Journal, Transaction,
# Posting or Account), but they are used extensively by value expressions.

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

print('Demo completed successfully.')
