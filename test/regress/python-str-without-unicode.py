import ledger

# Python 3 reads str() through __str__; the Python 2 __unicode__ protocol
# is gone, and str() still returns the text, non-ASCII included.

for cls in (ledger.Account, ledger.Amount, ledger.Balance, ledger.Commodity, ledger.Value):
    print(cls.__name__, hasattr(cls, "__unicode__"))

journal = ledger.read_journal_from_string("""
2024/01/01 Café
    Dépenses:Café        10.00 €
    Actif:Caisse
""")

amount = ledger.Amount("10.00 €")
print(str(amount))
print(str(ledger.Balance(amount)))
print(str(ledger.Value(amount)))
print(str(ledger.commodities.find("€")))
print(str(journal.find_account("Dépenses:Café")))
