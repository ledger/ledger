import sys

from ledger import *

parser = TextualParser ()
register_parser (parser)

journal = Journal ()
parse_journal_file (sys.argv[1], journal)

print journal[-1].payee
