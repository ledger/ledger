import ledger

# Python indexing of query results, transactions and accounts follows the
# sequence protocol: index -len is the first element, -1 the last, and
# len, -len - 1 or any index further out raise IndexError (so every index
# of an empty sequence does).  Ledger used to read past the end of a query
# result for any negative or too-large index, which killed the process,
# refused the valid index -len on transactions and accounts, and hung on
# the most negative index of a transaction.

session = ledger.Session()
j = session.read_journal_from_string("""
2024/01/01 Grocer
    Expenses:Food        $10.00
    Expenses:Drink        $3.00
    Assets:Cash
""")


def probe(label, seq, show):
    n = len(seq)
    print(label, n, [show(seq[i]) for i in range(-n, n)])
    for i in (n, -n - 1, -2**63):
        try:
            seq[i]
            print(label, i, "no error")
        except IndexError as err:
            print(label, i, "IndexError:", err)


q = j.query("")
probe("query", q, lambda p: str(p.amount))
print("last is last iterated:", str(q[-1].amount) == str(list(q)[-1].amount))
del q
probe("empty query", j.query("Nonexistent"), lambda p: str(p.amount))
probe("xact", list(j.xacts)[0], lambda p: str(p.amount))
probe("account", j.find_account("Expenses"), lambda a: a.fullname())
probe("leaf account", j.find_account("Assets:Cash"), lambda a: a.fullname())
