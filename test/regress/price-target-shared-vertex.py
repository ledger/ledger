import ledger

# A price target whose graph vertex holds another commodity is priced as
# that commodity: a commodity retired by a later `alias` (its referent's
# vertex) and an annotated commodity (its base's).  Only the numbers are
# printed: which of the two commodities states the result is the price
# graph's rule for stating results, pinned by alias-after-price.test.

journal = ledger.read_journal_from_string("""
2024/01/01 Buy
    A    10 AAPL {5 Yen}
    B

commodity JPY
    alias Yen

P 2024/01/05 USD 150 JPY
""")

retired = None
for xact in journal.xacts:
    for post in xact.posts:
        if post.amount.has_annotation():
            retired = post.amount.annotation.price.commodity
jpy = ledger.commodities.find('JPY')
lot = ledger.Amount("1 JPY {2 EUR}").commodity


def number(amount):
    return None if amount is None else '%g' % amount.to_double()


print(retired.symbol, retired == jpy)
print(number(ledger.Amount("10 JPY").value(retired)))
print(number(ledger.Amount("2 USD").value(retired)))
print(number(ledger.Amount("2 USD").value(lot)))
print(number(ledger.Amount("10 JPY").value(lot)))
