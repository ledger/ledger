import ledger

# A Python str reaches C++ as its UTF-8 bytes whatever the width of its
# internal storage (one, two or four bytes per character), an embedded NUL,
# an empty string and a str subclass included; a lone surrogate, which has
# no UTF-8 form, raises UnicodeEncodeError.

for s in ["1 café", "1 €", "1 \U0001D11E"]:
    amount = ledger.Amount(s)
    print(str(amount), str(amount.commodity), len(str(amount.commodity)))

journal = ledger.read_journal_from_string("""
2024/01/01 Café Ünïcode \U0001F600
    Expenses:Café        10 €
    Assets:Cash
""")
for post in journal.query(""):
    print(post.xact.payee, post.account.fullname(), str(post.amount))


class Text(str):
    pass


print(str(ledger.Amount(Text("1 €"))))

value = ledger.Value()
for s in ["a\x00b", ""]:
    value.set_string(s)
    print(repr(value.to_string()), len(value.to_string()))

for s in ["1 \ud800", "x\udcff", "\U0001D11E\ud800"]:
    try:
        ledger.Amount(s)
        print("no error")
    except Exception as e:
        print(type(e).__name__, e)
