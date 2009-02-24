import ledger

def precmd_hello():
    hello = ledger.Value()
    hello.set_string("Well, hello yourself!  This is Ledger, coming to you from Python Land.")
    print hello
    return hello
