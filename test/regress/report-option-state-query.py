import ledger

journal = ledger.read_journal("test/regress/report-option-state-query_py.test")


def show(query):
    print(query)
    for post in journal.query(query):
        print("  %s %s" % (post.account.fullname(), post.amount))


show("--ignore-diacritics cafe")
show("--account-rewrite Café=Wallet Expenses")
show("Expenses")
show("--account-rewrite Cafe=Wallet Expenses")
show("--now 2020/01/20 --current Expenses")
show("--current Expenses")
