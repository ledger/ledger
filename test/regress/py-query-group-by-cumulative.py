import ledger

# Journal.query() under --group-by-cumulative returns each group's own
# postings, as `reg --group-by-cumulative` reports them.  With --related a
# group skipped the siblings an earlier group had related (5 postings where
# the non-cumulative query returns 7); with -S as well, a posting related
# again appended a second set of sort keys to its first and the sort failed
# an assertion.  With -P (--by-payee) the payee subtotals were freed as soon
# as they were reported, while the result and the next group's chain still
# pointed at them, so the query read freed memory, cumulative or not.

journal = ledger.read_journal(__file__.replace(".py", "_py.test"))


def amount_text(amount):
    return str(amount).replace("\n", " + ")


def show(query):
    posts = journal.query(query)
    amounts = posts.display_amounts
    print("%s: %d posts, %d amounts" % (query, len(posts), len(amounts)))
    for post, amount in zip(posts, amounts):
        print("  %s %s %s %s" % (post.date, post.xact.payee, post.account.fullname(),
                                 amount_text(amount)))


show("--group-by account -r Expenses")
show("--group-by account --group-by-cumulative -r Expenses")
show("--group-by account --group-by-cumulative -r -V Expenses")
show("--group-by account --group-by-cumulative -r -S payee")
show("-P Expenses")
show("--group-by payee --group-by-cumulative -P Expenses")

# The payee subtotals stay alive with the result, while later queries run.
held = journal.query("-P Expenses")
show("Expenses")
print("held: %d posts: %s" % (len(held), ", ".join(
    "%s %s %s" % (post.xact.payee, post.account.fullname(), amount_text(amount))
    for post, amount in zip(held, held.display_amounts))))
