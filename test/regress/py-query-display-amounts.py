import ledger

# Journal.query() exposes, beside each posting, its display amount: the
# amount the matching `ledger reg` would print, after -X/-V conversion,
# revaluation, -M subtotals and --group-by.  The collector records it as
# each posting arrives, and an error while computing it raises, as the
# same error does in `ledger reg`, instead of falling back to the raw
# amount.

journal = ledger.read_journal(__file__.replace(".py", "_py.test"))


def show(query):
    try:
        posts = journal.query(query)
    except RuntimeError as err:
        print("%s: RuntimeError: %s" % (query, str(err).strip().splitlines()[-1]))
        return
    amounts = posts.display_amounts
    print("%s: %d posts, %d amounts" % (query, len(posts), len(amounts)))
    for post, amount in zip(posts, amounts):
        print("  %s %s %s" % (post.date, post.account.fullname(), amount))


show("-X $")
show("-X $ -M")
show("--tail 1 -X $")
show("--group-by payee -X $")
show("--group-by payee -V --no-rounding --display-amount"
     " 'account =~ /Revalued/ ? nosuchfn(amount) : market(amount, value_date, exchange)'")
show("-V --no-rounding --display-amount"
     " 'account =~ /Revalued/ ? nosuchfn(amount) : market(amount, value_date, exchange)'")
show("-X $ Expenses")
