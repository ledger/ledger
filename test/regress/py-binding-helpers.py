import datetime

import ledger

# The Python bindings route C++ errors to Python exception types, supply
# default arguments to value(), and copy C++ sequences into Python lists.
# This pins all three so that sharing one helper for each across the
# binding files changes nothing a script can see.  The RuntimeError
# translators (parse_error, error_count) are pinned by
# py-binding-helpers-error.py, whose test also expects stderr.


def err(label, f):
    try:
        f()
        print(label, "no error")
    except Exception as e:
        print(label, type(e).__name__ + ":", e)


err("amount", lambda: ledger.Amount("1 USD") + ledger.Amount("1 EUR"))
bal = ledger.Balance(ledger.Amount("1 USD"))
bal += ledger.Amount("1 EUR")
err("balance", lambda: bal.to_amount())
err("value", lambda: ledger.Value(True) + ledger.Value(ledger.Amount("1 USD")))

journal = ledger.read_journal_from_string("""
P 2024/01/01 XYZ 2 USD

2024/01/02 Groceries
    Expenses:Food:Groceries    3 XYZ
    Assets:Cash

2024/01/03 Dinner
    Expenses:Food:Dining       4 XYZ
    Assets:Cash

2024/01/04 Bus
    Expenses:Transport         5 XYZ
    Assets:Cash
""")

usd = ledger.commodities.find("USD")
before = datetime.date(2023, 12, 31)
after = datetime.datetime(2024, 6, 1, 12, 0, 0)
for v in (ledger.Amount("3 XYZ"), ledger.Balance(ledger.Amount("3 XYZ")),
          ledger.Value(ledger.Amount("3 XYZ"))):
    print(type(v).__name__, v.value(), v.value(usd), v.value(usd, before),
          v.value(usd, after))
print("no price", ledger.Amount("3 QQQ").value(), ledger.Amount("3 QQQ").value(usd))

keys = ledger.commodities.keys()
print("keys sorted", keys == sorted(keys), [k for k in keys if k in ("USD", "XYZ", "QQQ")])

print([a.fullname() for a in journal.find_accounts_re("Food|Transport")])
print([a.fullname() for a in journal.master.find_accounts_re("Food|Transport")])
print([a.fullname() for a in journal.find_accounts_re("^Nothing$")])

posts = journal.query("expenses")
print([str(v) for v in posts.display_amounts])
