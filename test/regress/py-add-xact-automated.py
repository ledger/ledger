# Journal.add_xact, called after parsing has finished (so the journal
# has no parse context), must apply the journal's automated transactions
# and account check directives, as parsing does.

import ledger

journal = ledger.session.read_journal_files()

xact = ledger.Transaction()
xact.date = ledger.parse_date("2026/01/05")
xact.payee = "Grocer"

food = ledger.Posting()
food.account = journal.find_account("Expenses:Food")
food.amount = ledger.Amount("$10.00")
xact.add_post(food)

cash = ledger.Posting()
cash.account = journal.find_account("Assets:Cash")
cash.amount = ledger.Amount("$-10.00")
xact.add_post(cash)

print("added:", journal.add_xact(xact), flush=True)
for post in xact:
    print(post.account.fullname(), post.amount, flush=True)
