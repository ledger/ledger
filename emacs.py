import sys

from ledger import *

def emacs_date (seconds):
    return "(%d %d %d)" % (seconds / 65536, seconds % 65536, 0)

class EmacsFormatTransactions (TransactionHandler):
    last_entry = None
    output     = None

    def __init__ (self):
	self.last_entry = None

	if config.output_file:
	    self.output = open (config.output_file, "w")
	else:
	    self.output = sys.stdout

	TransactionHandler.__init__ (self)

    def __del__ (self):
	if config.output_file:
	    self.output.close ()

    def flush (self):
	self.output.write ("))\n")
	self.output.flush ()

    def write_entry (self, entry):
	self.output.write("%s %s %s %s\n" %
			  (emacs_date (entry.date),
			   (entry.state and "t") or "nil",
			   (entry.code and "\"%s\"" % entry.code) or "nil",
			   (entry.payee and "\"%s\"" % entry.payee) or "nil"))

    def __call__ (self, xact):
	if not transaction_has_xdata (xact) or \
	   not transaction_xdata (xact).dflags & TRANSACTION_DISPLAYED:
	    if self.last_entry is None:
		self.output.write("((")
		self.write_entry (xact.entry)
	    elif xact.entry != self.last_entry:
		self.output.write(")\n (")
		self.write_entry (xact.entry)
	    else:
		self.output.write("\n")
	    self.output.write("  (\"%s\" \"%s\"%s%s)" %
			      (xact.account.fullname (), xact.amount,
			       (xact.cost and " \"%s\"" % xact.cost) or "",
			       (xact.note and " \"%s\"" % xact.note) or ""))
	    self.last_entry = xact.entry
	    transaction_xdata (xact).dflags |= TRANSACTION_DISPLAYED
