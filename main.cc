#include <ledger.h>
#include "acconf.h"
#include "debug.h"
#ifdef USE_BOOST_PYTHON
#include "py_eval.h"
#endif

using namespace ledger;

#include <iostream>
#include <fstream>
#include <sstream>
#include <memory>
#include <algorithm>
#include <exception>
#include <iterator>
#include <string>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>

#ifdef HAVE_UNIX_PIPES
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include "fdstream.hpp"
#endif

#if !defined(DEBUG_LEVEL) || DEBUG_LEVEL <= RELEASE

#define auto_ptr bogus_auto_ptr

// This version of auto_ptr does not delete on deconstruction.
namespace std {
  template <typename T>
  struct bogus_auto_ptr {
    T * ptr;
    bogus_auto_ptr() : ptr(NULL) {}
    explicit bogus_auto_ptr(T * _ptr) : ptr(_ptr) {}
    T& operator*() const throw() {
      return *ptr;
    }
    T * operator->() const throw() {
      return ptr;
    }
    T * get() const throw() { return ptr; }
    T * release() throw() {
      T * tmp = ptr;
      ptr = 0;
      return tmp;
    }
    void reset(T * p = 0) throw() {
      if (p != ptr) {
	delete ptr;
	ptr = p;
      }
    }
  };
}

#endif

item_handler<transaction_t> *
chain_xact_handlers(const std::string&		  command,
		    item_handler<transaction_t> * base_formatter,
		    journal_t *			  journal,
		    account_t *			  master,
		    std::list<item_handler<transaction_t> *>& ptrs)
{
  item_handler<transaction_t> * formatter = NULL;

  ptrs.push_back(formatter = base_formatter);

  // format_transactions write each transaction received to the
  // output stream.
  if (! (command == "b" || command == "E")) {
    // truncate_entries cuts off a certain number of _entries_ from
    // being displayed.  It does not affect calculation.
    if (config.head_entries || config.tail_entries)
      ptrs.push_back(formatter =
		     new truncate_entries(formatter,
					  config.head_entries,
					  config.tail_entries));

    // filter_transactions will only pass through transactions
    // matching the `display_predicate'.
    if (! config.display_predicate.empty())
      ptrs.push_back(formatter =
		     new filter_transactions(formatter,
					     config.display_predicate));

    // calc_transactions computes the running total.  When this
    // appears will determine, for example, whether filtered
    // transactions are included or excluded from the running total.
    ptrs.push_back(formatter = new calc_transactions(formatter));

    // reconcile_transactions will pass through only those
    // transactions which can be reconciled to a given balance
    // (calculated against the transactions which it receives).
    if (! config.reconcile_balance.empty()) {
      bool    reconcilable = false;
      value_t target_balance;
      if (config.reconcile_balance == "<all>")
	reconcilable = true;
      else
	target_balance = value_t(config.reconcile_balance);

      time_t  cutoff = now;
      if (! config.reconcile_date.empty())
	parse_date(config.reconcile_date.c_str(), &cutoff);

      ptrs.push_back(formatter =
		     new reconcile_transactions(formatter, target_balance,
						cutoff, reconcilable));
    }

    // sort_transactions will sort all the transactions it sees, based
    // on the `sort_order' value expression.
    if (! config.sort_string.empty())
      ptrs.push_back(formatter =
		     new sort_transactions(formatter, config.sort_string));

    // changed_value_transactions adds virtual transactions to the
    // list to account for changes in market value of commodities,
    // which otherwise would affect the running total unpredictably.
    if (config.show_revalued)
      ptrs.push_back(formatter =
		     new changed_value_transactions(formatter,
						    config.show_revalued_only));

    // collapse_transactions causes entries with multiple transactions
    // to appear as entries with a subtotaled transaction for each
    // commodity used.
    if (config.show_collapsed)
      ptrs.push_back(formatter = new collapse_transactions(formatter));
  }

  // subtotal_transactions combines all the transactions it receives
  // into one subtotal entry, which has one transaction for each
  // commodity in each account.
  //
  // period_transactions is like subtotal_transactions, but it
  // subtotals according to time periods rather than totalling
  // everything.
  //
  // dow_transactions is like period_transactions, except that it
  // reports all the transactions that fall on each subsequent day
  // of the week.
  if (config.show_subtotal && ! (command == "b" || command == "E"))
    ptrs.push_back(formatter = new subtotal_transactions(formatter));

  if (config.days_of_the_week)
    ptrs.push_back(formatter = new dow_transactions(formatter));
  else if (config.by_payee)
    ptrs.push_back(formatter = new by_payee_transactions(formatter));

  if (! config.report_period.empty()) {
    ptrs.push_back(formatter =
		   new interval_transactions(formatter,
					     config.report_period,
					     config.report_period_sort));
    ptrs.push_back(formatter = new sort_transactions(formatter, "d"));
  }

  // invert_transactions inverts the value of the transactions it
  // receives.
  if (config.show_inverted)
    ptrs.push_back(formatter = new invert_transactions(formatter));

  // related_transactions will pass along all transactions related
  // to the transaction received.  If `show_all_related' is true,
  // then all the entry's transactions are passed; meaning that if
  // one transaction of an entry is to be printed, all the
  // transaction for that entry will be printed.
  if (config.show_related)
    ptrs.push_back(formatter =
		   new related_transactions(formatter,
					    config.show_all_related));

  // This filter_transactions will only pass through transactions
  // matching the `predicate'.
  if (! config.predicate.empty())
    ptrs.push_back(formatter = new filter_transactions(formatter,
						       config.predicate));

  // budget_transactions takes a set of transactions from a data
  // file and uses them to generate "budget transactions" which
  // balance against the reported transactions.
  //
  // forecast_transactions is a lot like budget_transactions, except
  // that it adds entries only for the future, and does not balance
  // them against anything but the future balance.

  if (config.budget_flags) {
    budget_transactions * handler
      = new budget_transactions(formatter, config.budget_flags);
    handler->add_period_entries(journal->period_entries);
    ptrs.push_back(formatter = handler);
  }
  else if (! config.forecast_limit.empty()) {
    forecast_transactions * handler
      = new forecast_transactions(formatter, config.forecast_limit);
    handler->add_period_entries(journal->period_entries);
    ptrs.push_back(formatter = handler);
  }

  if (config.comm_as_payee)
    ptrs.push_back(formatter = new set_comm_as_payee(formatter));

  return formatter;
}

int parse_and_report(int argc, char * argv[], char * envp[])
{
  std::auto_ptr<journal_t> journal(new journal_t);

  // Parse command-line arguments, and those set in the environment

  std::list<std::string> args;
  process_arguments(config_options, argc - 1, argv + 1, false, args);

  if (args.empty()) {
    option_help(std::cerr);
    return 1;
  }
  strings_list::iterator arg = args.begin();

  if (config.cache_file == "<none>")
    config.use_cache = false;
  else
    config.use_cache = config.data_file.empty() && config.price_db.empty();
  DEBUG_PRINT("ledger.config.cache", "1. use_cache = " << config.use_cache);

  process_environment(config_options, envp, "LEDGER_");

#if 1
  // These are here for backwards compatability, but are deprecated.

  if (const char * p = std::getenv("LEDGER"))
    process_option(config_options, "file", p);
  if (const char * p = std::getenv("LEDGER_INIT"))
    process_option(config_options, "init-file", p);
  if (const char * p = std::getenv("PRICE_HIST"))
    process_option(config_options, "price-db", p);
  if (const char * p = std::getenv("PRICE_EXP"))
    process_option(config_options, "price-exp", p);
#endif

  const char * p    = std::getenv("HOME");
  std::string  home = p ? p : "";

  if (config.init_file.empty())
    config.init_file  = home + "/.ledgerrc";
  if (config.price_db.empty())
    config.price_db   = home + "/.pricedb";

  if (config.cache_file.empty())
    config.cache_file = home + "/.ledger-cache";

  if (config.data_file == config.cache_file)
    config.use_cache = false;
  DEBUG_PRINT("ledger.config.cache", "2. use_cache = " << config.use_cache);

  // Read the command word, canonicalize it to its one letter form,
  // then configure the system based on the kind of report to be
  // generated

  std::string command = *arg++;

  if (command == "balance" || command == "bal" || command == "b")
    command = "b";
  else if (command == "register" || command == "reg" || command == "r")
    command = "r";
  else if (command == "print" || command == "p")
    command = "p";
  else if (command == "output")
    command = "w";
  else if (command == "emacs")
    command = "x";
  else if (command == "xml")
    command = "X";
  else if (command == "entry")
    command = "e";
  else if (command == "equity")
    command = "E";
  else if (command == "prices")
    command = "P";
  else if (command == "pricesdb")
    command = "D";
  else
    throw error(std::string("Unrecognized command '") + command + "'");

  // Parse initialization files, ledger data, price database, etc.

  std::auto_ptr<binary_parser_t>  bin_parser(new binary_parser_t);
#ifdef HAVE_XMLPARSE
  std::auto_ptr<xml_parser_t>     xml_parser(new xml_parser_t);
  std::auto_ptr<gnucash_parser_t> gnucash_parser(new gnucash_parser_t);
#endif
#ifdef HAVE_LIBOFX
  std::auto_ptr<ofx_parser_t>     ofx_parser(new ofx_parser_t);
#endif
  std::auto_ptr<qif_parser_t>     qif_parser(new qif_parser_t);
  std::auto_ptr<textual_parser_t> text_parser(new textual_parser_t);

  register_parser(bin_parser.get());
#ifdef HAVE_XMLPARSE
  register_parser(xml_parser.get());
  register_parser(gnucash_parser.get());
#endif
#ifdef HAVE_LIBOFX
  register_parser(ofx_parser.get());
#endif
  register_parser(qif_parser.get());
  register_parser(text_parser.get());

  parse_ledger_data(journal.get(), bin_parser.get(), text_parser.get()
#ifdef HAVE_XMLPARSE
		    , xml_parser.get()
#endif
		    );

  // process the command word and its following arguments

  std::string first_arg;
  if (command == "w") {
    if (arg == args.end())
      throw error("The 'output' command requires a file argument");
    first_arg = *arg++;
  }

  config.process_options(command, arg, args.end());

  std::auto_ptr<entry_t> new_entry;
  if (command == "e") {
    new_entry.reset(derive_new_entry(*journal, arg, args.end()));
    if (! new_entry.get())
      return 1;
  }

  // Configure the output stream

#ifdef HAVE_UNIX_PIPES
  int status, pfd[2];		// Pipe file descriptors
#endif
  std::ostream * out = &std::cout;

  if (! config.output_file.empty()) {
    out = new std::ofstream(config.output_file.c_str());
  }
#ifdef HAVE_UNIX_PIPES
  else if (! config.pager.empty()) {
    status = pipe(pfd);
    if (status == -1)
      throw error("Failed to create pipe");

    status = fork();
    if (status < 0) {
      throw error("Failed to fork child process");
    }
    else if (status == 0) {	// child
      const char *arg0;

      // Duplicate pipe's reading end into stdin
      status = dup2(pfd[0], STDIN_FILENO);
      if (status==-1)
	perror("dup2");

      // Close unuseful file descriptors: the pipe's writing and
      // reading ends (the latter is not needed anymore, after the
      // duplication).
      close(pfd[1]);
      close(pfd[0]);

      // Find command name: its the substring starting right of the
      // rightmost '/' character in the pager pathname.  See manpage
      // for strrchr.
      arg0 = std::strrchr(config.pager.c_str(), '/');
      if (arg0 != NULL)
	arg0++;
      else
	arg0 = config.pager.c_str(); // No slashes in pager.

      execlp(config.pager.c_str(), arg0, (char *)0);
      perror("execl");
      exit(1);
    }
    else {			// parent
      close(pfd[0]);
      out = new boost::fdostream(pfd[1]);
    }
  }
#endif

  // Compile the format strings

  const std::string * format;

  if (! config.format_string.empty())
    format = &config.format_string;
  else if (command == "b")
    format = &config.balance_format;
  else if (command == "r")
    format = &config.register_format;
  else if (command == "E")
    format = &config.equity_format;
  else if (command == "P")
    format = &config.prices_format;
  else if (command == "D")
    format = &config.pricesdb_format;
  else if (command == "w")
    format = &config.write_xact_format;
  else
    format = &config.print_format;

#ifdef USE_BOOST_PYTHON

  // If Python support is compiled, we can easily report minimum and
  // maximum values for each commodity.  There is a line in config.cc
  // which configures the prices report to call these two functions,
  // if Python is available.

  if (command == "P")
    python_eval("\
min_val = 0\n\
def vmin(d, val):\n\
    global min_val\n\
    if not min_val or val < min_val:\n\
	min_val = val\n\
	return val\n\
    return min_val\n\
\n\
max_val = 0\n\
def vmax(d, val):\n\
    global max_val\n\
    if not max_val or val > max_val:\n\
	max_val = val\n\
	return val\n\
    return max_val\n", PY_EVAL_MULTI);

#endif // USE_BOOST_PYTHON

  // Walk the entries based on the report type and the options

  item_handler<transaction_t> *		   formatter;
  std::list<item_handler<transaction_t> *> formatter_ptrs;

  if (command == "b" || command == "E")
    formatter = new set_account_value;
  else if (command == "p" || command == "e")
    formatter = new format_entries(*out, *format);
  else if (command == "x")
    formatter = new format_emacs_transactions(*out);
  else if (command == "X") {
#ifdef HAVE_XMLPARSE
    formatter = new format_xml_entries(*out, config.show_totals);
#else
    throw error("XML support was not compiled into this copy of Ledger");
#endif
  } else
    formatter = new format_transactions(*out, *format);

  if (command == "w") {
    write_textual_journal(*journal, first_arg, *formatter, *out);
  } else {
    formatter = chain_xact_handlers(command, formatter, journal.get(),
				    journal->master, formatter_ptrs);
    if (command == "e")
      walk_transactions(new_entry->transactions, *formatter);
    else if (command == "P" || command == "D")
      walk_commodities(commodity_t::commodities, *formatter);
    else
      walk_entries(journal->entries, *formatter);

    if (command != "P" && command != "D")
      formatter->flush();
  }

  // For the balance and equity reports, output the sum totals.

  if (command == "b") {
    format_account acct_formatter(*out, *format, config.display_predicate);
    sum_accounts(*journal->master);
    walk_accounts(*journal->master, acct_formatter, config.sort_string);
    acct_formatter.flush();

    if (account_has_xdata(*journal->master)) {
      account_xdata_t& xdata = account_xdata(*journal->master);
      if (! config.show_collapsed && xdata.total) {
	*out << "--------------------\n";
	xdata.value = xdata.total;
	acct_formatter.format.format(*out, details_t(*journal->master));
      }
    }
  }
  else if (command == "E") {
    format_equity acct_formatter(*out, *format, config.display_predicate);
    sum_accounts(*journal->master);
    walk_accounts(*journal->master, acct_formatter, config.sort_string);
    acct_formatter.flush();
  }

#if DEBUG_LEVEL >= BETA
  clear_all_xdata();

  if (! config.output_file.empty())
    delete out;

  for (std::list<item_handler<transaction_t> *>::iterator i
	 = formatter_ptrs.begin();
       i != formatter_ptrs.end();
       i++)
    delete *i;
  formatter_ptrs.clear();
#endif

  // Write out the binary cache, if need be

  if (config.use_cache && config.cache_dirty && ! config.cache_file.empty()) {
    std::ofstream stream(config.cache_file.c_str());
    write_binary_journal(stream, journal.get());
  }

#ifdef HAVE_UNIX_PIPES
  if (! config.pager.empty()) {
    delete out;
    close(pfd[1]);

    // Wait for child to finish
    wait(&status);
    if (status & 0xffff != 0)
      throw error("Something went wrong in the pager");
  }
#endif

  return 0;
}

int main(int argc, char * argv[], char * envp[])
{
  std::ios::sync_with_stdio(false);

  try {
    return parse_and_report(argc, argv, envp);
  }
  catch (const std::exception& err) {
    std::cerr << "Error: " << err.what() << std::endl;
    return 1;
  }
  catch (int& val) {
    return val;			// this acts like a std::setjmp
  }
}

// main.cc ends here.
