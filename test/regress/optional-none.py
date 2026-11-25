import ledger

# Assigning None to an optional property must clear it.  xact.note and
# xact.code are std::optional<string> and post.cost a
# std::optional<amount_t>, all served by the one optional converter family.

session = ledger.Session()
journal = session.read_journal_from_string("""
2024/01/01 (C1) Payee  ; a note
    Expenses:Food      10 USD @ 2 EUR
    Assets:Cash
""")

for xact in journal:
    post = next(iter(xact))
    print(xact.code, xact.note, post.cost)

    xact.code = None
    xact.note = None
    post.cost = None
    print(repr(xact.code), repr(xact.note), repr(post.cost))

    xact.code = 'ABC'
    xact.note = 'n'
    post.cost = ledger.Amount('1 EUR')
    print(xact.code, xact.note, post.cost)

    # A value of the wrong type is still rejected, and the field kept; None
    # is still rejected by a property that is not optional.
    for name, obj, value in (('code', xact, 5), ('cost', post, 'x'), ('payee', xact, None)):
        try:
            setattr(obj, name, value)
            print(name, 'accepted')
        except Exception as e:
            print(name, type(e).__name__)
    print(xact.code, post.cost, xact.payee)
