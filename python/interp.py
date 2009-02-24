from code import InteractiveConsole

def cmd_python():
    interpreter = InteractiveConsole()
    interpreter.push("from ledger import *")
    interpreter.interact("Welcome to Ledger")
    return True
