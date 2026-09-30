# Regression for #3237: set_tag must not expose its C++ map iterator.
import ledger

journal = ledger.read_journal_from_string("""
2026-01-01 Example
    Expenses:Supplies  10 AUD
    Assets:Cash
""")
xact = next(iter(journal))
post = next(iter(xact))

assert post.set_tag("Category", ledger.string_value("supplies"), True) is None
assert post.has_tag("category")
assert str(post.get_tag("CATEGORY")) == "supplies"

assert post.set_tag("CATEGORY", ledger.string_value("ignored"), False) is None
assert str(post.get_tag("Category")) == "supplies"
assert post.set_tag("category", ledger.string_value("office"), True) is None
assert str(post.get_tag("Category")) == "office"

assert post.set_tag("Flag", None, False) is None
assert post.has_tag("flag")
assert post.get_tag("FLAG") is None
assert post.set_tag("Empty", ledger.string_value(""), True) is None
assert post.has_tag("empty")
assert post.get_tag("EMPTY") is None
print("Posting tag assignment passed")

assert xact.set_tag("Project", ledger.string_value("one"), True) is None
assert xact.has_tag("project")
assert str(xact.get_tag("PROJECT")) == "one"
assert post.has_tag("PROJECT")
assert str(post.get_tag("project")) == "one"
assert xact.set_tag("PROJECT", ledger.string_value("ignored"), False) is None
assert str(post.get_tag("Project")) == "one"
assert xact.set_tag("project", ledger.string_value("two"), True) is None
assert str(post.get_tag("PROJECT")) == "two"

assert xact.set_tag("TxnFlag", None, True) is None
assert post.has_tag("txnflag")
assert post.get_tag("TXNFLAG") is None
assert post.set_tag("Project", ledger.string_value("local"), False) is None
assert str(post.get_tag("Project")) == "local"
assert str(xact.get_tag("Project")) == "two"
print("Transaction tag assignment and inheritance passed")

try:
    post.set_tag("TooFew", ledger.string_value("value"))
except TypeError:
    pass
else:
    raise AssertionError("set_tag accepted a missing overwrite argument")
print("Explicit overwrite argument remains required")
