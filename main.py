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
import time

true, false = 1, 0

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

averages = {}
compute_monthly_avg = false

def get_index (xact):
    return time.strftime ("%Y/%m", time.localtime (xact.entry.date))

class ComputeMonthlyAvg (TransactionHandler):
    def __call__ (self, xact):
	global averages
	index = get_index (xact)
	if not averages.has_key(index):
	    averages[index] = [Value (), 0]
	add_transaction_to (xact, averages[index][0])
	averages[index][1] += 1
	TransactionHandler.__call__ (self, xact)

def monthly_avg (details):
    index = get_index (xact)
    return averages[index][0] / averages[index][1]

def show_monthly_averages (arg):
    global compute_monthly_avg
    compute_monthly_avg = true
    config.report_period = "monthly";
    config.total_expr = "@monthly_avg()"

add_option_handler ("monthly-avg", "", show_monthly_averages)

# Process the command-line arguments, test whether caching should be
# enabled, and then process any option settings from the execution
# environment.  Some historical environment variable names are also
# supported.

args = process_arguments (sys.argv[1:])
config.use_cache = not config.data_file
process_environment (os.environ, "LEDGER_")

if os.environ.has_key ("LEDGER"):
    process_option ("file", os.getenv ("LEDGER"))
if os.environ.has_key ("PRICE_HIST"):
    process_option ("price-db", os.getenv ("PRICE_HIST"))
if os.environ.has_key ("PRICE_EXP"):
    process_option ("price-exp", os.getenv ("PRICE_EXP"))

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
elif command == "output":
    command = "w"
elif command == "emacs":
    command = "x"
elif command == "xml":
    command = "X"
elif command == "entry":
    command = "e"
elif command == "equity":
    command = "E"
elif command == "prices":
    command = "P"
elif command == "pricesdb":
    command = "D";
else:
    print "Unrecognized command:", command
    sys.exit (1)

# Create all the parser objects to be used.  They are all registered,
# so that Ledger will try each one in turn whenever it is presented
# with a data file.  They are attempted in reverse order to their
# registry.  Note that Gnucash parsing is only available if the Ledger
# module was built with such support (which requires the xmlparse C
# library).

bin_parser  = BinaryParser ()
gnucash_parser = None
xml_parser = None
try: xml_parser = GnucashParser ()
except: pass
try: gnucash_parser = GnucashParser ()
except: pass
try: ofx_parser = OfxParser ()
except: pass
qif_parser  = QifParser ()
text_parser = TextualParser ()

register_parser (bin_parser)
if xml_parser:
    register_parser (xml_parser)
if gnucash_parser:
    register_parser (gnucash_parser)
if ofx_parser:
    register_parser (ofx_parser)
register_parser (qif_parser)
register_parser (text_parser)

# Parse all entries from the user specified locations (found in
# 'config') into the journal object we created.  The two parsers given
# as explicit arguments indicate: the parser to be used for standard
# input, and the parser to be used for cache files.

parse_ledger_data (journal, bin_parser)

# Now that everything has been correctly parsed (parse_ledger_data
# would have thrown an exception if not), we can take time to further
# process the configuration options.  This changes the configuration a
# bit based on previous option settings, the command word, and the
# remaining arguments.

config.process_options (command, args);

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
elif command == "P":
    min_val = 0
    def vmin(d, val):
	global min_val
	if not min_val or val < min_val:
	    min_val = val
	    return val
	return min_val

    max_val = 0
    def vmax(d, val):
	global max_val
	if not max_val or val > max_val:
	    max_val = val
	    return val
	return max_val

    format = config.prices_format
elif command == "D":
    format = config.pricesdb_format
elif command == "w":
    format = config.write_xact_format
else:
    format = config.print_format

# Configure the output file

if config.output_file:
    out = open (config.output_file, "w")
else:
    out = sys.stdout

# Set the final transaction handler: for balances and equity reports,
# it will simply add the value of the transaction to the account's
# xdata, which is used a bit later to report those totals.  For all
# other reports, the transaction data is sent to the configured output
# location (default is sys.stdout).

if command == "b" or command == "E":
    handler = SetAccountValue ()
elif command == "p" or command == "e":
    handler = FormatEntries (out, format)
elif command == "x":
    handler = FormatEmacsTransactions (out)
elif command == "X":
    handler = FormatXmlEntries (out, config.show_totals)
else:
    handler = FormatTransactions (out, format)

if command == "w":
    write_textual_journal(journal, args, handler, out);
else:
    # Chain transaction filters on top of the base handler.  Most of these
    # filters customize the output for reporting.  None of this is done
    # for balance or equity reports, which don't need it.

    if not (command == "b" or command == "E"):
	if config.head_entries or config.tail_entries:
	    handler = TruncateEntries (handler, config.head_entries,
				       config.tail_entries)

	if config.display_predicate:
	    handler = FilterTransactions (handler, config.display_predicate)

	handler = CalcTransactions (handler)

	#if config.reconcile_balance:
	#  reconcilable = False
	#  if config.reconcile_balance == "<all>"
	#    reconcilable = True
	#  else
	#    target_balance = Value (config.reconcile_balance)
	#
	#  cutoff = time.time ()
	#  # jww (2005-02-15): needs conversion
	#  #if config.reconcile_date:
	#  #  cutoff = parse_date (config.reconcile_date)
	#
	#  handler = ReconcileTransactions (handler, target_balance,
	#                                   cutoff, reconcilable)

	if config.sort_string:
	    handler = SortTransactions (handler, config.sort_string)

	if config.show_revalued:
	    handler = ChangedValueTransactions (handler,
						config.show_revalued_only)

	if config.show_collapsed:
	    handler = CollapseTransactions (handler);

    if config.show_subtotal and not (command == "b" or command == "E"):
	handler = SubtotalTransactions (handler)

    if config.days_of_the_week:
	handler = DowTransactions (handler)
    elif config.by_payee:
	handler = ByPayeeTransactions (handler)

    if config.report_period:
	handler = IntervalTransactions (handler, config.report_period,
					config.report_period_sort)
	handler = SortTransactions (handler, "d")

    if compute_monthly_avg:
	handler = ComputeMonthlyAvg (handler)

    # The next set of transaction filters are used by all reports.

    if config.show_inverted:
	handler = InvertTransactions (handler)

    if config.show_related:
	handler = RelatedTransactions (handler, config.show_all_related)

    if config.predicate:
	handler = FilterTransactions (handler, config.predicate)

    if config.budget_flags:
	handler = BudgetTransactions (handler, config.budget_flags)
	handler.add_period_entries (journal)
    elif config.forecast_limit:
	handler = ForecastTransactions (handler, config.forecast_limit)
	handler.add_period_entries (journal)

    if config.comm_as_payee:
	handler = SetCommAsPayee (handler)

    # Walk the journal's entries, and pass each entry's transaction to the
    # handler chain established above.  And although a journal's entries
    # can be walked using Python, it is significantly faster to do this
    # simple walk in C++, using `walk_entries'.
    #
    # if command == "e":
    #     for xact in new_entry:
    #         handler (xact)
    # else:
    #     for entry in journal:
    #         for xact in entry:
    #             handler (xact)

    if command == "e":
	walk_transactions (new_entry, handler)
    elif command == "P" or command == "D":
	walk_commodities (handler)
    else:
	walk_entries (journal, handler)

    # Flush the handlers, causing them to output whatever data is still
    # pending.

    if command != "P" and command != "D":
	handler.flush ()

# For the balance and equity reports, the account totals now need to
# be displayed.  This is different from outputting transactions, in
# that we are now outputting account totals to display a summary of
# the transactions that were just walked.

if command == "b":
    acct_formatter = FormatAccount (out, format, config.display_predicate)
    sum_accounts (journal.master)
    walk_accounts (journal.master, acct_formatter, config.sort_string)
    acct_formatter.final (journal.master)
    acct_formatter.flush ()

    if account_has_xdata (journal.master):
      xdata = account_xdata (journal.master)
      if not config.show_collapsed and xdata.total:
	out.write("--------------------\n")
	xdata.value = xdata.total
	# jww (2005-02-15): yet to convert
	#acct_formatter.format.format (out, details_t (journal.master))

elif command == "E":
    acct_formatter = FormatEquity (out, format, config.display_predicate)
    sum_accounts (journal.master)
    walk_accounts (journal.master, acct_formatter, config.sort_string)
    acct_formatter.flush ()

# If it were important to clean things up, we would have to clear out
# the accumulated xdata at this point:

#clear_all_xdata ()

# If the cache is being used, and is dirty, update it now.

if config.use_cache and config.cache_dirty and config.cache_file:
    write_binary_journal (config.cache_file, journal);

# We're done!
