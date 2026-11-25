import ledger

# Guards for P4-012: every optional crossing the Python boundary now uses
# std::optional (one converter family), session_t::journal is a
# std::shared_ptr, and Account.amount()/total() take no expression.

def show(label, thunk):
    try:
        print(label, repr(thunk()))
    except Exception as err:
        print(label, type(err).__name__)

journal = ledger.Session().read_journal_from_string("""
2025/01/15 Noted  ; a note
    Expenses:Food      10 EUR
    Expenses:Food      5 USD
    Assets:Cash

2025/01/16 Plain
    Expenses:Food      1 EUR
    Assets:Cash
""")

eur = ledger.commodities.find('EUR')
usd = ledger.commodities.find('USD')

# Amount.value / Balance.value / Balance.commodity_amount results
show('amount.value()', lambda: ledger.Amount('10 EUR').value())
show('amount.value(usd)', lambda: ledger.Amount('10 EUR').value(usd))
single = ledger.Balance(ledger.Amount('10 EUR'))
show('single.commodity_amount()', lambda: str(single.commodity_amount()))
show('single.commodity_amount(usd)', lambda: single.commodity_amount(usd))
multi = ledger.Balance(ledger.Amount('10 EUR'))
multi += ledger.Amount('5 USD')
show('multi.commodity_amount()', lambda: multi.commodity_amount())
show('multi.commodity_amount(eur)', lambda: str(multi.commodity_amount(eur)))
show('multi.value(eur)', lambda: str(multi.value(eur)))
show('multi.value(eur, date)',
     lambda: str(multi.value(eur, ledger.parse_date('2025/02/01'))))

# item_t::note (optional<string>) read and written through the converter;
# the Journal outlives the Session that read it (shared_ptr holder)
for xact in journal:
    show('note ' + xact.payee, lambda: xact.note)
for xact in journal:
    xact.note = 'set from python'
for xact in journal:
    show('note ' + xact.payee, lambda: xact.note)

# Account.amount()/total() take no expression from Python
food = journal.find_account('Expenses:Food')
show('food.amount()', lambda: str(food.amount()))
show('food.total()', lambda: str(food.total()))
show('food.amount(expr)', lambda: food.amount(ledger.Expr('amount')))
show('food.total(expr)', lambda: food.total(ledger.Expr('amount')))
