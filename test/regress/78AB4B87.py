from __future__ import print_function

import ledger

eur = ledger.commodities.find_or_create('EUR')

total_eur = ledger.Amount("0.00 EUR")
total_gbp = ledger.Amount("0.00 GBP")
total = ledger.Amount("0.00 EUR")

for post in ledger.read_journal("test/regress/78AB4B87.dat").query("^income:"):
    print(post.amount)
    print(post.amount.commodity)
    if post.amount.commodity == "EUR":
        total_eur += post.amount
    elif post.amount.commodity == "GBP":
        total_gbp += post.amount

    a = post.amount.value(eur)
    if a:
        print("Total is presently: (%s)" % total)
        print("Converted to EUR:   (%s)" % a)
        total += a
        print("Total is now:       (%s)" % total)
    else:
        print("Cannot convert '%s'" % post.amount)
    print()

print(total)
