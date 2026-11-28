import ledger

# A journal that fails to parse raises RuntimeError in Python (the
# error_count translator, whose message is the parser's last error), and
# the parser reports that error on stderr.  py-binding-helpers.py pins the
# ArithmeticError translators; this case lives apart because its test
# expects stderr.


def err(label, f):
    try:
        f()
        print(label, "no error")
    except Exception as e:
        print(label, type(e).__name__ + ":", e)


err("journal", lambda: ledger.read_journal_from_string("""
2024/01/01 Unbalanced
    Expenses:Food    10 USD
    Assets:Cash      -5 USD
"""))
