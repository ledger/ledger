import sys
import os

from ledger import *

def foo (str):
    print "Hello:", str
def bar (str):
    print "Goodbye:", str

register_option ("hello", "h:", foo)
register_option ("goodbye", "g:", bar)
print process_arguments (sys.argv[1:])
process_environment (os.environ, "TEST_")

sys.exit(0)

parser = TextualParser ()
register_parser (parser)

journal = Journal ()
parse_journal_file (sys.argv[1], journal)

print journal[-1].payee
