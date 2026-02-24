import ledger

# Obtain a commodity reference before close_journal_files().  With the old
# code, close_journal_files() called amount_t::shutdown() which destroyed
# commodity_pool_t::current_pool, leaving this pointer dangling and causing
# a segfault when it was used afterward (issue #978).
eur = ledger.commodities.find_or_create('EUR')

ledger.session.close_journal_files()

j = ledger.read_journal_from_string("""
P 2015/01/01 GBP 1.25 EUR

; Establish EUR suffix display style (mirrors file "d" from the original issue)
2015/01/01 Opening balance
    Assets:Cash       100.00 EUR
    Equity:Opening

2015/06/01 Test
    Assets:Bank           100.00 GBP
    Income:Whatever
""")

for post in j.query("^income:"):
    # Calling value(eur) exercises the commodity pointer that survived
    # close_journal_files() -- this crashed before the fix.
    converted = post.amount.value(eur)
    if converted:
        print(converted)
    else:
        print(post.amount)
