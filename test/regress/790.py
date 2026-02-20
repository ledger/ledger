import ledger

journal = ledger.read_journal("test/regress/790_py.test")

# This query with -M (monthly) was previously causing a segfault
# because the filter chain (interval_posts) was destroyed before
# the Python code could iterate over the collected results.
posts = journal.query("Expenses:Utilities -M")
count = 0
for post in posts:
    count += 1

print(count)
