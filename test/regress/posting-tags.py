import ledger

# Posting reaches has_tag/get_tag through JournalItem's bindings: the
# item_t& wrappers dispatch virtually to post_t's overrides, which consult
# the parent transaction when the posting lacks the tag.

journal = ledger.read_journal_from_string("""
2024/01/01 Grocer
    ; XactTag: from-xact
    ; :Shared:
    Expenses:Food        $10.00
        ; PostTag: from-post
        ; Shared: post-wins
    Assets:Cash
""")

for xact in journal:
    for post in xact.posts:
        print(post.account.fullname())
        print(post.has_tag("PostTag"), post.has_tag("XactTag"), post.has_tag("Missing"))
        print(post.get_tag("PostTag"), post.get_tag("XactTag"), post.get_tag("Missing"))
        print(post.has_tag("Shared"), post.get_tag("Shared"))
        print(post.tag("XactTag"))

# A posting with no transaction has only its own (empty) metadata.
detached = ledger.Posting()
print(detached.has_tag("XactTag"), detached.get_tag("XactTag"))

# The mask overloads take a C++ mask_t, which no Python type converts to
# (ledger has no Mask class; a mask Value is not a mask_t), so Python
# cannot reach them on Posting or on JournalItem, before or after.
mask = ledger.mask_value("PostTag")
for call in (lambda: post.has_tag(mask), lambda: post.get_tag(mask),
             lambda: post.has_tag(mask, mask), lambda: post.get_tag(mask, mask)):
    try:
        call()
        print("reached")
    except Exception as e:
        print(type(e).__name__)

# The tag methods are JournalItem's own, not per-class copies.
print("has_tag" in ledger.Posting.__dict__, "get_tag" in ledger.Posting.__dict__)
print(ledger.Posting.has_tag is ledger.JournalItem.has_tag)
