import sys
import os
import time

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
    def __init__ (self):
	self.formatter = Format ("%D %-20P %N")
	TransactionHandler.__init__ (self)
    def __call__ (self, xact):
	print self.formatter.format(xact)

handler = OutputTransaction()
handler = FilterTransactions (handler, "/Checking/")

for entry in journal:
    for xact in entry:
	handler (xact)

span = Interval ("weekly last month")
for date in span:
    print time.strftime ("%c", time.localtime (date))
