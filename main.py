import sys
import os
import time

from ledger import *

def hello (str):
    print "Hello:", str
def goodbye (str):
    print "Goodbye:", str

register_option ("hello", "h:", hello)
register_option ("goodbye", "g:", goodbye)
args = process_arguments (sys.argv[1:])
process_environment (os.environ, "TEST_")

parser = TextualParser ()
register_parser (parser)

journal = Journal ()
parse_journal_file (args[0], journal)

class FormatTransaction (TransactionHandler):
    def __init__ (self, fmt):
	self.formatter = Format (fmt)
	TransactionHandler.__init__ (self)

    def __call__ (self, xact):
	print self.formatter.format(xact)

def foo(d, val):
    return d.xact.amount + val

handler = FormatTransaction("%D %-20P %N %('foo'{$100})")
handler = FilterTransactions (handler, "/Checking/")

expr = parse_value_expr ("a*2")

for entry in journal:
    for xact in entry:
	handler (xact)

for date in Interval ("weekly last month"):
    print time.strftime ("%c", time.localtime (date))
