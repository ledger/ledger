#!/usr/bin/env python

# Ledger, the command-line accounting tool
#
# Copyright (c) 2003-2004, New Artisans LLC. All rights reserved.
#
# This program is made available under the terms of the BSD Public
# License.  See the LICENSE file included with the distribution for
# details and disclaimer.
#
# This script provides a Python front-end to the ledger library, and
# replicates the functionality of the C++ front-end, main.cc.  It is
# provided as an example, and as a starting point for creating custom
# front-ends based on the Ledger module.  See the documentation for an
# API reference, and how to use this module.

import os
import sys
import string

from ledger import *

# Create the main journal object, into which all entries will be
# recorded.  Once done, the 'journal' may be iterated to yield those
# entries, in the same order as which they appeared in the journal
# file.

journal = Journal ()

# This call registers all of the default command-line options that
# Ledger supports into the option handling mechanism.  Skip this call
# if you wish to do all of your own processing -- in which case simply
# modify the 'config' object however you like.

add_config_option_handlers ()

# Process the command-line arguments, test whether caching should be
# enabled, and then process any option settings from the execution
# environment.  Some historical environment variable names are also
# supported.

args = process_arguments (sys.argv[1:])
config.use_cache = not config.data_file
process_environment (os.environ, "LEDGER_")

if os.environ.has_key ("LEDGER"):
    process_option ("file", os.environ["LEDGER"])
if os.environ.has_key ("PRICE_HIST"):
    process_option ("price-db", os.environ["PRICE_HIST"])
if os.environ.has_key ("PRICE_EXP"):
    process_option ("price-exp", os.environ["PRICE_EXP"])

# If no argument remain, then no command word was given.  Report the
# default help text and exit.

if len (args) == 0:
    option_help ()
    sys.exit (0)

# The command word is in the first argument.  Canonicalize it to a
# unique, simple form that the remaining code can use to find out
# which command was specified.

command = args.pop (0);

if command == "balance" or command == "bal" or command == "b":
    command = "b"
elif command == "register" or command == "reg" or command == "r":
    command = "r"
elif command == "print" or command == "p":
    command = "p"
elif command == "entry":
    command = "e"
elif command == "equity":
    command = "E"
else:
    print "Unrecognized command:", command
    sys.exit (1)

# Create all the parser objects to be used.  They are all registered,
# so that Ledger will try each one in turn whenever it is presented
# with a data file.  They are attempted in reverse order to their
# registry.  Note that Gnucash parsing is only available if the Ledger
# module was built with such support (which requires the xmlparse C
# library).

text_parser = TextualParser ()
bin_parser  = BinaryParser ()
qif_parser  = QifParser ()
gnucash_parser = None
try:
    gnucash_parser = GnucashParser ()
except:
    pass

register_parser (text_parser)
register_parser (bin_parser)
if gnucash_parser:
    register_parser (gnucash_parser)
register_parser (qif_parser)

# Parse all entries from the user specified locations (found in
# 'config') into the journal object we created.  The two parsers given
# as explicit arguments indicate: the parser to be used for standard
# input, and the parser to be used for cache files.

parse_ledger_data (journal, text_parser, bin_parser)

# Now that everything has been correctly parsed (parse_ledger_data
# would have thrown an exception if not), we can take time to further
# process the configuration options.  This changes the configuration a
# bit based on previous option settings, the command word, and the
# remaining arguments.

config.process_options(command, args);

# If the command is "e", use the method journal.derive_entry to create
# a brand new entry based on the arguments given.

new_entry = None
if command == "e":
    new_entry = derive_new_entry (journal, args)
    if new_entry is None:
	sys.exit (1)

# Determine the format string to used, based on the command.

if config.format_string:
    format = config.format_string
elif command == "b":
    format = config.balance_format
elif command == "r":
    format = config.register_format
elif command == "E":
    format = config.equity_format
else:
    format = config.print_format

# The following two classes are responsible for outputing transactions
# and accounts to the user.  There are corresponding C++ versions to
# these, but they rely on I/O streams, which Boost.Python does not
# provide a conversion layer for.

class FormatTransaction (TransactionHandler):
    last_entry = None
    output     = None

    def __init__ (self, fmt):
	try:
	    i = string.index (fmt, '%/')
	    self.formatter  = Format (fmt[: i])
	    self.nformatter = Format (fmt[i + 2 :])
	except ValueError:
	    self.formatter  = Format (fmt)
	    self.nformatter = None

	self.last_entry = None

	if config.output_file:
	    self.output = open(config.output_file, "w")
	else:
	    self.output = sys.stdout

	TransactionHandler.__init__ (self)

    def __del__ (self):
	if config.output_file:
	    self.output.close ()

    def flush (self):
	self.output.flush ()

    def __call__ (self, xact):
	if not transaction_has_xdata (xact) or \
	   not transaction_xdata (xact).dflags & TRANSACTION_DISPLAYED:
	    if self.nformatter is not None and \
	       self.last_entry is not None and \
	       xact.entry == self.last_entry:
		self.output.write(self.nformatter.format(xact))
	    else:
		self.output.write(self.formatter.format(xact))
		self.last_entry = xact.entry

class FormatAccount (AccountHandler):
    output = None

    def __init__ (self, fmt, pred):
	self.formatter = Format (fmt)
	self.predicate = AccountPredicate (pred)

	if config.output_file:
	    self.output = open(config.output_file, "w")
	else:
	    self.output = sys.stdout

	AccountHandler.__init__ (self)

    def __del__ (self):
	if config.output_file:
	    self.output.close ()

    def flush (self):
	self.output.flush ()

    def __call__ (self, account):
	if display_account (account, self.predicate):
	    if not account.parent:
		account_xdata (account).dflags |= ACCOUNT_TO_DISPLAY
	    else:
		self.output.write(self.formatter.format(account))
		account_xdata (account).dflags |= ACCOUNT_DISPLAYED

# Set the final transaction handler: for balances and equity reports,
# it will simply add the value of the transaction to the account's
# xdata, which is used a bit later to report those totals.  For all
# other reports, the transaction data is sent to the configured output
# location (default is sys.stdout).

if command == "b" or command == "E":
    handler = SetAccountValue()
else:
    handler = FormatTransaction(format)

# Chain transaction filters on top of the base handler.  Most of these
# filters customize the output for reporting.  None of this is done
# for balance or equity reports, which don't need it.

if not (command == "b" or command == "E"):
    if config.display_predicate:
	handler = FilterTransactions(handler, config.display_predicate)

    handler = CalcTransactions(handler, config.show_inverted)

    if config.sort_string:
	handler = SortTransactions(handler, config.sort_string)

    if config.show_revalued:
	handler = ChangedValueTransactions(handler, config.show_revalued_only)

    if config.show_collapsed:
	handler = CollapseTransactions(handler);

    if config.show_subtotal:
	handler = SubtotalTransactions(handler)
    elif config.report_interval:
	handler = IntervalTransactions(handler, config.report_interval)
	handler = SortTransactions(handler, "d")
    elif config.days_of_the_week:
	handler = DowTransactions(handler)

# The next two transaction filters are used by all reports.

if config.show_related:
    handler = RelatedTransactions(handler, config.show_all_related)

if config.predicate:
    handler = FilterTransactions(handler, config.predicate)

# Walk the journal's entries, and pass each entry's transaction to the
# handler chain established above.  And although a journal's entries
# can be walked using Python, it is significantly faster to do this
# simple walk in C++, using `walk_entries'.

if 1:
    walk_entries (journal, handler)
else:
    for entry in journal:
	for xact in entry:
	    handler (xact)

# Flush the handlers, causing them to output whatever data is still
# pending.

handler.flush ()

# For the balance and equity reports, the account totals now need to
# be displayed.  This is different from outputting transactions, in
# that we are now outputting account totals to display a summary of
# the transactions that were just walked.

if command == "b":
    acct_formatter = FormatAccount (format, config.display_predicate)
    sum_accounts (journal.master)
    walk_accounts (journal.master, acct_formatter, config.sort_string)
    acct_formatter.flush ()
    #if account_has_xdata(journal.master):
    #  account_xdata(journal.master).value = account_xdata(journal.master).total;
    #
    #  if account_xdata(journal.master).dflags & ACCOUNT_TO_DISPLAY:
    #      print "--------------------"
    #      config.format.format(out, details_t(*journal->master));

#elif command == "E":
#    format_equity acct_formatter(out, config.format, config.nformat,
#				 config.display_predicate);
#    sum_accounts(*journal->master);
#    walk_accounts(*journal->master, acct_formatter, config.sort_string);
#    acct_formatter.flush();

# If the cache is being used, and is dirty, update it now.

if config.use_cache and config.cache_dirty and config.cache_file:
    write_binary_journal(config.cache_file, journal);

# We're done!
