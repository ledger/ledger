#include <ledger.h>
#include "acconf.h"

using namespace ledger;

#include <iostream>
#include <fstream>
#include <sstream>
#include <memory>
#include <algorithm>
#include <iterator>
#include <string>
#include <cstdlib>
#include <cstring>
#include <ctime>

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
chain_formatters(const std::string& command,
		 item_handler<transaction_t> * base_formatter,
		 std::list<item_handler<transaction_t> *>& ptrs)
{
  item_handler<transaction_t> * formatter = NULL;

  ptrs.push_back(formatter = base_formatter);

  // format_transactions write each transaction received to the
  // output stream.
  if (! (command == "b" || command == "E")) {
    // filter_transactions will only pass through transactions
    // matching the `display_predicate'.
    if (! config.display_predicate.empty())
      ptrs.push_back(formatter =
		     new filter_transactions(formatter,
					     config.display_predicate));

    // calc_transactions computes the running total.  When this
    // appears will determine, for example, whether filtered
    // transactions are included or excluded from the running total.
    ptrs.push_back(formatter =
		   new calc_transactions(formatter, config.show_inverted));

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

    // subtotal_transactions combines all the transactions it receives
    // into one subtotal entry, which has one transaction for each
    // commodity in each account.
    //
    // interval_transactions is like subtotal_transactions, but it
    // subtotals according to time intervals rather than totalling
    // everything.
    //
    // dow_transactions is like interval_transactions, except that it
    // reports all the transactions that fall on each subsequent day
    // of the week.
    if (config.show_subtotal) {
      ptrs.push_back(formatter = new subtotal_transactions(formatter));
    }
    else if (! config.report_interval.empty()) {
      ptrs.push_back(formatter =
		     new interval_transactions(formatter,
					       config.report_interval));
      ptrs.push_back(formatter = new sort_transactions(formatter, "d"));
    }
    else if (config.days_of_the_week) {
      ptrs.push_back(formatter = new dow_transactions(formatter));
    }
  }

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

  config.use_cache = config.data_file.empty();

  process_environment(config_options, envp, "LEDGER_");

#if 1
  // These are here for backwards compatability, but are deprecated.

  if (const char * p = std::getenv("LEDGER"))
    process_option(config_options, "file", p);
  if (const char * p = std::getenv("PRICE_HIST"))
    process_option(config_options, "price-db", p);
  if (const char * p = std::getenv("PRICE_EXP"))
    process_option(config_options, "price-exp", p);
#endif

  if (config.data_file == config.cache_file)
    config.use_cache = false;

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
  else if (command == "entry")
    command = "e";
  else if (command == "equity")
    command = "E";
  else
    throw error(std::string("Unrecognized command '") + command + "'");

  // Parse initialization files, ledger data, price database, etc.

  std::auto_ptr<binary_parser_t>  bin_parser(new binary_parser_t);
#ifdef READ_GNUCASH
  std::auto_ptr<gnucash_parser_t> gnucash_parser(new gnucash_parser_t);
#endif
  std::auto_ptr<qif_parser_t>     qif_parser(new qif_parser_t);
  std::auto_ptr<textual_parser_t> text_parser(new textual_parser_t);

  register_parser(bin_parser.get());
#ifdef READ_GNUCASH
  register_parser(gnucash_parser.get());
#endif
  register_parser(qif_parser.get());
  register_parser(text_parser.get());

  parse_ledger_data(journal.get(), text_parser.get(), bin_parser.get());

  // Process the command word and its following arguments

  config.process_options(command, arg, args.end());

  std::auto_ptr<entry_t> new_entry;
  if (command == "e") {
    new_entry.reset(derive_new_entry(*journal, arg, args.end()));
    if (! new_entry.get())
      return 1;
  }

  // Configure the output stream

  std::ostream * out = &std::cout;
  if (! config.output_file.empty()) {
    if (access(config.output_file.c_str(), W_OK) == -1)
      throw error(std::string("Cannot write output to file '" +
			      config.output_file + "'"));
    else
      out = new std::ofstream(config.output_file.c_str());
  }

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
  else
    format = &config.print_format;

  // Walk the entries based on the report type and the options

  item_handler<transaction_t> * formatter;
  std::list<item_handler<transaction_t> *> formatter_ptrs;

  if (command == "b" || command == "E") {
    formatter = new set_account_value;
    formatter = chain_formatters(command, formatter, formatter_ptrs);
  } else {
    formatter = new format_transactions(*out, *format);
    formatter = chain_formatters(command, formatter, formatter_ptrs);
  }

  if (command == "e")
    walk_transactions(new_entry->transactions, *formatter);
  else
    walk_entries(journal->entries, *formatter);

  formatter->flush();

  // For the balance and equity reports, output the sum totals.

  if (command == "b") {
    format_account acct_formatter(*out, *format, config.display_predicate);
    sum_accounts(*journal->master);
    walk_accounts(*journal->master, acct_formatter, config.sort_string);
    acct_formatter.flush();

    if (account_has_xdata(*journal->master)) {
      account_xdata_t& xdata = account_xdata(*journal->master);
      xdata.value = xdata.total;
      if (xdata.dflags & ACCOUNT_TO_DISPLAY) {
	*out << "--------------------\n";
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
  clear_transactions_xdata();
  clear_accounts_xdata();

  if (! config.output_file.empty())
    delete out;

  for (std::list<item_handler<transaction_t> *>::iterator i
	 = formatter_ptrs.begin();
       i != formatter_ptrs.end();
       i++)
    delete *i;
#endif

  // Write out the binary cache, if need be

  if (config.use_cache && config.cache_dirty &&
      ! config.cache_file.empty()) {
    std::ofstream stream(config.cache_file.c_str());
    write_binary_journal(stream, journal.get(), &journal->sources);
  }

  return 0;
}

int main(int argc, char * argv[], char * envp[])
{
  try {
    return parse_and_report(argc, argv, envp);
  }
  catch (error& err) {
    std::cerr << "Error: " << err.what() << std::endl;
    return 1;
  }
  catch (int& val) {
    return val;			// this acts like a std::setjmp
  }
}

// main.cc ends here.
