import ledger

# POST_EXT_MATCHES was retired: filter_posts no longer flags the postings a
# predicate passes, and nothing in Ledger read that flag.  A script naming
# it now fails loudly with AttributeError.  The other POST_EXT_* report
# state flags stay exported.

print(hasattr(ledger, "POST_EXT_MATCHES"))
for name in ("POST_EXT_RECEIVED", "POST_EXT_HANDLED", "POST_EXT_DISPLAYED",
             "POST_EXT_DIRECT_AMT", "POST_EXT_SORT_CALC", "POST_EXT_COMPOUND",
             "POST_EXT_VISITED", "POST_EXT_CONSIDERED"):
    print(name, hasattr(ledger, name))
