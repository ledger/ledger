import ledger

def myvalue(incoming):
    x = ledger.Amount("$123.12")
    print x
    print x * 1
    return x * 1
