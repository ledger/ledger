import sys
import os

from ledger import *

def foo (str):
    print "Hello:", str
def bar (str):
    print "Goodbye:", str

register_option ("hello", "h:", foo)
register_option ("goodbye", "g:", bar)
args = process_arguments (sys.argv[1:])
process_environment (os.environ, "TEST_")

parser = TextualParser ()
register_parser (parser)

journal = Journal ()
parse_journal_file (args[0], journal)

class OutputTransaction (TransactionHandler):
    def __call__ (self, xact):
	print xact.entry.payee

handler = OutputTransaction()
handler = FilterTransactions (handler, "/Checking/")

for entry in journal:
    for xact in entry:
	handler (xact)
