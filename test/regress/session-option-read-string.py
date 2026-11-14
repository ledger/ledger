import ledger

# read_journal_from_string applies the session's options, here a
# command-line --lot-matching fifo, to each fresh journal it reads, as
# reading a file does: the sale consumes the oldest lot.
data = """
2024/01/15 Buy First Lot
    Assets:Brokerage         10 AAPL {$150.00} [2024/01/15]
    Assets:Cash          $-1500.00

2024/02/15 Buy Second Lot
    Assets:Brokerage          5 AAPL {$160.00} [2024/02/15]
    Assets:Cash           $-800.00

2024/03/15 Sell Some
    Assets:Brokerage         -8 AAPL @ $170.00
    Assets:Cash
"""

for reading in (1, 2):
    journal = ledger.read_journal_from_string(data)
    for xact in journal.xacts:
        if xact.payee == "Sell Some":
            for post in xact.posts:
                if str(post.account) == "Assets:Brokerage":
                    print("%d %s %s" % (reading, post.account, post.amount))
