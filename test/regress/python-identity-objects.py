import ledger

# Account, AccountXData and AutomatedTransaction are identity objects:
# the bindings construct them and hand out references, never copies.
# An account's local name is fixed once it sits in a tree, because the
# parent's child map is keyed by that name and fullname() caches the
# joined path, so Python may read Account.name but not assign it.

session = ledger.Session()
journal = session.read_journal_from_string("""
~ Monthly
    Expenses:Food    $10.00
    Assets:Cash

2024-01-01 Payee
    Expenses:Food    $10.00
    Assets:Cash
""")

food = journal.find_account("Expenses:Food")
try:
    food.name = "Drink"
    print("renamed to %s" % food.name)
except AttributeError:
    print("name is read-only")
print(food.fullname())
print(journal.find_account_re("Food").fullname())

details = food.self_details(True)
print("self_details posts_count=%d" % details.posts_count)
print("xdata self_details posts_count=%d" % food.xdata().self_details.posts_count)

# Assigning an account to a posting passes a reference, not a copy.
cash = journal.find_account("Assets:Cash")
for xact in journal.xacts:
    first = list(xact.posts)[0]
    first.account = cash
    print("reassigned posting to %s" % first.account.fullname())
    first.account = food

for period in journal.period_xacts:
    print("period %s with %d postings" % (period.period_string, len(period)))

account = ledger.Account()
print("Account() name=%r depth=%d" % (account.name, account.depth))

ledger.AutomatedTransaction()
print("AutomatedTransaction() constructed")
