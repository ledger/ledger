import sys
import os
import time

from ledger import *

def hello (str):
    print "Hello:", str
def goodbye (str):
    print "Goodbye:", str

add_config_option_handlers ()
add_option_handler ("hello", ":", hello)
add_option_handler ("goodbye", ":", goodbye)

args = process_arguments (sys.argv[1:])
process_environment (os.environ, "LEDGER_")

if len (args) > 0:
    config.process_options (args[0], args[1:])

text_parser = TextualParser ()
register_parser (text_parser)

journal = Journal ()
print parse_journal_file (args[0], journal), "entries"

class FormatTransaction (TransactionHandler):
    def __init__ (self, fmt):
	self.formatter = Format (fmt)
	TransactionHandler.__init__ (self)

    def __call__ (self, xact):
	print self.formatter.format(xact)

expr = parse_value_expr ("a*2")

def foo(x, val):
    return x.xact.amount + expr.compute (x) + val

handler = FormatTransaction("%D %-20P %N %('foo'{$100})")
handler = FilterTransactions (handler, "/Checking/")

for entry in journal:
    for xact in entry:
	handler (xact)

for date in Interval ("weekly last month"):
    print time.strftime ("%c", time.localtime (date))
