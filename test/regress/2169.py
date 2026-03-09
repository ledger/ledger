import ledger

# Regression test for GitHub issue #2169:
# post.xdata().has_flags(POST_VIRTUAL) always returns False.
#
# The bug had two root causes:
#
# 1. POST_VIRTUAL is a flag on post_t itself (checked via post.has_flags()),
#    not on xdata_t (checked via post.xdata().has_flags()).  Users who called
#    post.xdata().has_flags(POST_VIRTUAL) were checking the wrong flag set;
#    xdata_t uses POST_EXT_* flags for report-time state.
#
# 2. The JournalItem.has_flags() Python binding used supports_flags<> (i.e.
#    supports_flags<uint_least8_t>) as the 'this' type, while item_t actually
#    inherits from supports_flags<uint_least16_t>.  This type mismatch has
#    been corrected to use the right template instantiation.
#
# The fix also adds a post.is_virtual convenience property that directly
# tests the POST_VIRTUAL flag, making the API unambiguous.

session = ledger.Session()
j = session.read_journal_from_string("""
2022-07-24 Budget
    [Budget:Expenses:Gas]    $200.00
    [Budget:Equity]         -$200.00

2022-07-25 Virtual unbalanced
    (Virtual:Tracking)      $100.00
    Expenses:Other          $100.00
    Assets:Cash            -$100.00
""")

results = []
for xact in j:
    for post in xact.posts:
        name = post.account.fullname()
        is_virt = post.is_virtual
        has_flag = post.has_flags(ledger.POST_VIRTUAL)
        results.append((name, is_virt, has_flag))

for name, is_virt, has_flag in results:
    print(f"{name}: is_virtual={is_virt}, has_flags={has_flag}")
