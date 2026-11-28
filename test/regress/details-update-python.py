# AccountXDataDetails.update(post, gather_all) keeps its two-argument
# Python form; the reference date for the recency counters is today
# (--now when given).

import ledger

journal = ledger.session.read_journal_files()

details = ledger.AccountXDataDetails()
for xact in journal:
    for post in xact:
        details.update(post, True)

print("posts:", details.posts_count, flush=True)
print("this month:", details.posts_this_month_count, flush=True)
print("last 7 days:", details.posts_last_7_count, flush=True)
print("last 30 days:", details.posts_last_30_count, flush=True)
