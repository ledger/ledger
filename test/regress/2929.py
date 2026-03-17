import ledger

# Regression test for GitHub issue #2929:
# post.xact.note throws TypeError after migration to std::optional.
#
# The note field on item_t is declared as unqualified `optional<string>`,
# which resolves to boost::optional<string> via `using namespace boost` in
# utils.h.  The std::optional migration (7bff9ea6) replaced the
# boost::optional Python converter with a std::optional converter, but did
# not change the field type, so accessing `note` from Python raised:
#   TypeError: No to_python (by-value) converter found for C++ type:
#     boost::optional<std::string>

session = ledger.Session()
j = session.read_journal_from_string("""
2025/01/15 Test transaction  ; a note
    expenses:food      10 USD
    assets:checking
""")

for xact in j:
    for post in xact:
        print(post.xact.note)
