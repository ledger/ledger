# Journal.add_xact runs after parsing has finished, so there is no parse
# context.  A failed account check on the added transaction is reported
# without a source location even when the transaction carries a position
# copied from a parsed one: only a parse context makes a position a
# location to cite.

import ledger

journal = ledger.session.read_journal_files()
opening = next(iter(journal))

xact = ledger.Transaction()
xact.date = ledger.parse_date("2026/01/05")
xact.payee = "Grocer"
xact.pos = opening.pos

food = ledger.Posting()
food.account = journal.find_account("Expenses:Food")
food.amount = ledger.Amount("$10.00")
xact.add_post(food)

cash = ledger.Posting()
cash.account = journal.find_account("Assets:Cash")
cash.amount = ledger.Amount("$-10.00")
xact.add_post(cash)

print("position line:", xact.pos.beg_line, flush=True)
print("added:", journal.add_xact(xact), flush=True)
