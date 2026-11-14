import ledger

# Journal.query() with --group-by returns the postings of every group, in
# group order, each group run through a report chain of its own as
# `reg --group-by` runs it.  It used to return nothing: the splitter
# cleared its chain after each group, and with it the collector that held
# the results.  With --group-by-cumulative it aborted, because the journal's
# xdata was cleared between groups that share one running total.

journal = ledger.read_journal(__file__.replace(".py", "_py.test"))


def amount_text(amount):
    return str(amount).replace("\n", " + ")


def show(query):
    posts = journal.query(query)
    amounts = posts.display_amounts
    print("%s: %d posts, %d amounts" % (query, len(posts), len(amounts)))
    for post, amount in zip(posts, amounts):
        print("  %s %s %s" % (post.date, post.account.fullname(), amount_text(amount)))


def summary(label, posts):
    print("%s: %d posts: %s" % (label, len(posts), ", ".join(
        "%s %s" % (post.account.fullname(), amount_text(amount))
        for post, amount in zip(posts, posts.display_amounts))))


show("--group-by payee Expenses")
show("--group-by payee -M Expenses")
show("--group-by payee Income")
# --account makes transfer_details create X: temporary accounts that a later
# group's chain adopts from journal->master, so destroying the result must
# release the chains newest first (see group_chains).
show("--group-by payee --account \"'X'\" Expenses")
show("--group-by payee --group-by-cumulative Expenses")

# A grouped result stays usable, synthetic postings included, while later
# queries run; a cumulative one leaves no xdata behind to block them.
held = journal.query("--group-by payee -M Expenses")
held_cumulative = journal.query("--group-by payee --group-by-cumulative Expenses")
show("Expenses")
show("-M Expenses")
summary("held", held)
summary("held cumulative", held_cumulative)
