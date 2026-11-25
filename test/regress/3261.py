import ledger

# Regression test for issue #3261: a commodity conversion needs a commodity
# on each side.  parse_conversion() enforces this for the C directive, but
# the smaller and larger properties set a commodity's conversion links
# directly, so Python could still link a commodity to a bare amount.
# Unreducing an annotated amount through such a link then crashed.

x = ledger.commodities.find_or_create("X")

try:
    x.larger = ledger.Amount("1")
    print("larger accepted a bare amount")
except ValueError as e:
    print("larger:", e)
print(ledger.Amount("10 X {$1}").unreduced())

try:
    x.smaller = ledger.Amount("1")
    print("smaller accepted a bare amount")
except ValueError as e:
    print("smaller:", e)
print(ledger.Amount("10 X").reduced())

# A link to a commodity still works.
x.smaller = ledger.Amount("60 Y")
print(ledger.Amount("2 X").reduced())
