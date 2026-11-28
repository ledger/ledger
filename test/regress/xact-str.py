import re
import ledger

# str() on a Transaction used to return '' (a stub left as a TODO), so
# print(xact) printed a blank line.  With no __str__ binding, Python's
# default falls back to repr(), which names the class.
def mask(s):
    return re.sub(r"0x[0-9a-fA-F]+", "0x", s)

journal = ledger.read_journal("test/regress/xact-str_py.test")
for xact in journal:
    print(mask(str(xact)))
    print(mask(repr(xact)))
print(mask(str(next(iter(journal.query("food"))).xact)))
print(mask(str(ledger.Transaction())))
