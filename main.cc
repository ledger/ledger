#include "ledger.h"
#include "error.h"
#include "valexpr.h"
#include "format.h"
#include "walk.h"
#include "quotes.h"
#include "option.h"
#include "config.h"
#include "timing.h"

using namespace ledger;

#include <iostream>
#include <fstream>
#include <memory>
#include <algorithm>
#include <string>
#include <cstdlib>
#include <cstring>
#include <ctime>

namespace {
  bool cache_dirty = false;

  TIMER_DEF(write_cache,    "writing cache file");
  TIMER_DEF(report_gen,	    "generation of final report");
  TIMER_DEF(handle_options, "configuring based on options");
  TIMER_DEF(parse_files,    "parsing ledger files");
  TIMER_DEF(process_env,    "processing environment");
  TIMER_DEF(process_args,   "processing command-line arguments");
  TIMER_DEF(read_cache,	    "reading cache file");
}

#ifdef NO_CLEANUP

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

#endif // NO_CLEANUP

int main(int argc, char * argv[], char * envp[])
{
  std::auto_ptr<journal_t> journal(new journal_t);

  // Initialize the global configuration object for this run

  std::auto_ptr<config_t> global_config(new config_t);
  config = global_config.get();

  // Parse command-line arguments

  TIMER_START(process_args);

  strings_list args;
  process_arguments(argc, argv, false, args);

  if (args.empty()) {
    option_help(std::cerr);
    return 1;
  }
  strings_list::iterator arg = args.begin();

  TIMER_STOP(process_args);

  bool use_cache = config->files.empty();

  // Process options from the environment

  TIMER_START(process_env);

  process_environment(envp, "LEDGER_");

#if 1
  // These are here until 3.x, for backwards compatability.

  if (const char * p = std::getenv("LEDGER"))
    process_option("file", p);
  if (const char * p = std::getenv("PRICE_HIST"))
    process_option("price-db", p);
  if (const char * p = std::getenv("PRICE_EXP"))
    process_option("price-exp", p);
#endif

  TIMER_STOP(process_env);

  // Parse ledger files

  TIMER_START(parse_files);

  int entry_count = 0;

  try {
    if (! config->init_file.empty())
      if (parse_journal_file(config->init_file, journal.get()))
	throw error("Entries not allowed in initialization file");

    if (use_cache && ! config->cache_file.empty()) {
      journal->sources.clear();		// remove init_file
      entry_count += parse_journal_file(config->cache_file, journal.get());
      journal->sources.pop_front();	// remove cache_file

      strings_list exceptions;
#if 0
      std::set_difference(journal->sources.begin(), journal->sources.end(),
			  config->files.begin(), config->files.end(),
			  exceptions.begin());
#endif

      if (entry_count == 0 || exceptions.size() > 0) {
	journal.reset(new journal_t);
	entry_count = 0;
	cache_dirty = true;
      } else {
	cache_dirty = false;
      }
    }

    if (entry_count == 0)
      for (strings_list::iterator i = config->files.begin();
	   i != config->files.end();
	   i++)
	if (*i == "-") {
	  use_cache = false;
	  entry_count += parse_textual_journal(std::cin, journal.get(),
					       journal->master);
	} else {
	  entry_count += parse_journal_file(*i, journal.get());
	}

    if (! config->price_db.empty())
      if (parse_journal_file(config->price_db, journal.get()))
	throw error("Entries not allowed in price history file");

    for (strings_list::iterator i = config->price_settings.begin();
	 i != config->price_settings.end();
	 i++) {
      std::string conversion = "C ";
      conversion += *i;
      int i = conversion.find('=');
      if (i != -1) {
	conversion[i] = ' ';
	std::istringstream stream(conversion);
	parse_textual_journal(stream, journal.get(), journal->master);
      }
    }
  }
  catch (error& err) {
    std::cerr << "Fatal: " << err.what() << std::endl;
    return 1;
  }

  if (entry_count == 0) {
    std::cerr << "Please specify ledger file(s) using -f option "
	      << "or LEDGER environment variable." << std::endl;
    return 1;
  }

  TIMER_STOP(parse_files);

  // Read the command word, and then check and simplify it

  std::string command = *arg++;

  TIMER_START(handle_options);

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
  else {
    std::cerr << "Error: Unrecognized command '" << command << "'."
	      << std::endl;
    return 1;
  }

  // Process the remaining command-line arguments

  std::auto_ptr<entry_t> new_entry;
  if (command == "e") {
    new_entry.reset(journal->derive_entry(arg, args.end()));
  } else {
    // Treat the remaining command-line arguments as regular
    // expressions, used for refining report results.

    strings_list::iterator i = args.begin();
    for (; i != args.end(); i++)
      if (*i == "--")
	break;

    const std::string pred = regexps_to_predicate(arg, i);
    if (! pred.empty()) {
      if (! config->predicate.empty())
	config->predicate += "&";
      config->predicate += pred;
    }

    if (i != args.end()) {
      const std::string pred = regexps_to_predicate(i, args.end(), false);
      if (! pred.empty()) {
	if (! config->predicate.empty())
	  config->predicate += "&";
	config->predicate += pred;
      }
    }
  }

  // Compile the predicates

  if (config->display_predicate.empty()) {
    if (command == "b") {
      if (! config->show_empty)
	config->display_predicate = "T";

      if (! config->show_expanded && config->predicate.empty()) {
	if (! config->display_predicate.empty())
	  config->display_predicate += "&";
	config->display_predicate += "!n";
      }
    }
    else if (command == "E") {
      config->display_predicate = "a";
    }
  }

  // Compile the sorting criteria

  std::auto_ptr<value_expr_t> sort_order;

  if (! config->sort_string.empty()) {
    try {
      std::istringstream stream(config->sort_string);
      sort_order.reset(parse_value_expr(stream));
      if (stream.peek() != -1) {
	std::ostringstream err;
	err << "Unexpected character '" << char(stream.peek()) << "'";
	throw value_expr_error(err.str());
      }
      else if (! sort_order.get()) {
	std::cerr << "Failed to parse sort criteria!" << std::endl;
	return 1;
      }
    }
    catch (const value_expr_error& err) {
      std::cerr << "Error in sort criteria: " << err.what() << std::endl;
      return 1;
    }
  }

  // Setup the meaning of %t and %T, used in format strings

  try {
#ifdef NO_CLEANUP
    format_t::value_expr = parse_value_expr(config->value_expr);
#else
    format_t::value_expr.reset(parse_value_expr(config->value_expr));
#endif
  }
  catch (const value_expr_error& err) {
    std::cerr << "Error in amount (-t) specifier: " << err.what()
	      << std::endl;
    return 1;
  }

  try {
#ifdef NO_CLEANUP
    format_t::total_expr = parse_value_expr(config->total_expr);
#else
    format_t::total_expr.reset(parse_value_expr(config->total_expr));
#endif
  }
  catch (const value_expr_error& err) {
    std::cerr << "Error in total (-T) specifier: " << err.what()
	      << std::endl;
    return 1;
  }

  // Configure some option depending on the report type

  bool show_all_related = false;

  if (command == "p" || command == "e") {
    config->show_related = show_all_related = true;
    config->show_expanded = true;
  }
  else if (command == "E") {
    config->show_expanded = true;
  }
  else if (config->show_related) {
    if (command == "r")
      config->show_inverted = true;
    else
      show_all_related = true;
  }

  // Compile the format strings

  const char * f;
  if (! config->format_string.empty())
    f = config->format_string.c_str();
  else if (command == "b")
    f = bal_fmt.c_str();
  else if (command == "r")
    f = reg_fmt.c_str();
  else if (command == "E")
    f = equity_fmt.c_str();
  else
    f = print_fmt.c_str();

  std::string first_line_format;
  std::string next_lines_format;

  if (const char * p = std::strstr(f, "%/")) {
    first_line_format = std::string(f, 0, p - f);
    next_lines_format = std::string(p + 2);
  } else {
    first_line_format = next_lines_format = f;
  }

  format_t format(first_line_format);
  format_t nformat(next_lines_format);

  TIMER_STOP(handle_options);

  // Setup a few local and global variables, depending on the config
  // settings.

  std::auto_ptr<std::ostream> output_stream;
  std::auto_ptr<interval_t>   report_interval;
  std::time_t		      interval_begin;

  if (config->download_quotes)
    commodity_t::updater = new quotes_by_script(config->price_db,
						config->pricing_leeway,
						cache_dirty);

  if (! config->output_file.empty())
    output_stream.reset(new std::ofstream(config->output_file.c_str()));

#define OUT() (output_stream.get() ? *output_stream : std::cout)

  if (! config->interval_text.empty()) {
    std::istringstream stream(config->interval_text);
    report_interval.reset(interval_t::parse(stream));
    if (! stream.eof()) {
      std::string word;
      stream >> word;
      if (word == "from") {
	stream >> word;
	if (! parse_date(word.c_str(), &interval_begin)) {
	  std::cerr << "Error in report interval: "
		    << "Could not parse 'from' date"
		    << std::endl;
	  return 1;
	}
      }
    }
  }

  if (! config->date_format.empty())
    format_t::date_format = config->date_format;

  // Walk the entries based on the report type and the options

  TIMER_START(report_gen);

  if (command == "b") {
    std::auto_ptr<item_handler<transaction_t> > formatter;
    formatter.reset(new add_to_account_value);
    if (config->show_related)
      formatter.reset(new related_transactions(formatter.release(),
					       show_all_related));
    formatter.reset(new filter_transactions(formatter.release(),
					    config->predicate));
    walk_entries(journal->entries, *formatter);
    formatter->flush();

    format_account acct_formatter(OUT(), format, config->display_predicate);
    if (config->show_subtotals)
      sum_accounts(journal->master);
    walk_accounts(journal->master, acct_formatter, sort_order.get());
    acct_formatter.flush();

    if (format_account::disp_subaccounts_p(journal->master)) {
      std::string end_format = "--------------------\n";
      format.reset(end_format + f);
      format.format_elements(OUT(), details_t(journal->master));
    }
  }
  else if (command == "E") {
    std::auto_ptr<item_handler<transaction_t> > formatter;
    formatter.reset(new add_to_account_value);
    formatter.reset(new filter_transactions(formatter.release(),
					    config->predicate));
    walk_entries(journal->entries, *formatter);
    formatter->flush();

    format_equity acct_formatter(OUT(), format, nformat,
				 config->display_predicate);
    sum_accounts(journal->master);
    walk_accounts(journal->master, acct_formatter, sort_order.get());
    acct_formatter.flush();
  }
  else if (command == "e") {
    format_transactions formatter(OUT(), format, nformat);
    walk_transactions(new_entry->transactions, formatter);
    formatter.flush();
  }
  else {
    std::auto_ptr<item_handler<transaction_t> > formatter;

    // Stack up all the formatter needed to fulfills the user's
    // requests.  Some of these are order dependent, in terms of
    // whether calc_transactions occurs before or after them.

    // format_transactions write each transaction received to the
    // output stream.
    formatter.reset(new format_transactions(OUT(), format, nformat));

    // sort_transactions will sort all the transactions it sees, based
    // on the `sort_order' value expression.
    if (sort_order.get())
      formatter.reset(new sort_transactions(formatter.release(),
					    sort_order.get()));

    // filter_transactions will only pass through transactions
    // matching the `display_predicate'.
    formatter.reset(new filter_transactions(formatter.release(),
					    config->display_predicate));

    // calc_transactions computes the running total.  When this
    // appears will determine, for example, whether filtered
    // transactions are included or excluded from the running total.
    formatter.reset(new calc_transactions(formatter.release(),
					  config->show_inverted));

    // changed_value_transactions adds virtual transactions to the
    // list to account for changes in market value of commodities,
    // which otherwise would affect the running total unpredictably.
    if (config->show_revalued)
      formatter.reset(new changed_value_transactions(formatter.release(),
						     config->show_revalued_only));

    // collapse_transactions causes entries with multiple transactions
    // to appear as entries with a subtotaled transaction for each
    // commodity used.
    if (! config->show_subtotals)
      formatter.reset(new collapse_transactions(formatter.release()));

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
    if (config->show_expanded)
      formatter.reset(new subtotal_transactions(formatter.release()));
    else if (report_interval.get())
      formatter.reset(new interval_transactions(formatter.release(),
						*report_interval,
						interval_begin));
    else if (config->days_of_the_week)
      formatter.reset(new dow_transactions(formatter.release()));

    // related_transactions will pass along all transactions related
    // to the transaction received.  If `show_all_related' is true,
    // then all the entry's transactions are passed; meaning that if
    // one transaction of an entry is to be printed, all the
    // transaction for that entry will be printed.
    if (config->show_related)
      formatter.reset(new related_transactions(formatter.release(),
					       show_all_related));

    // This filter_transactions will only pass through transactions
    // matching the `predicate'.
    formatter.reset(new filter_transactions(formatter.release(),
					    config->predicate));

    // Once the filters are chained, walk `journal's entries and start
    // feeding each transaction that matches `predicate' to the chain.
    walk_entries(journal->entries, *formatter);
    formatter->flush();

#ifdef DEBUG_ENABLED
    // The transaction display flags (dflags) are not recorded in the
    // binary cache, and only need to be cleared if the transactions
    // are to be displayed a second time.
    clear_display_flags cleanup;
    walk_entries(journal->entries, cleanup);
    cleanup.flush();
#endif
  }

  TIMER_STOP(report_gen);

  // Save the cache, if need be

  TIMER_START(write_cache);

  if (use_cache && cache_dirty && ! config->cache_file.empty()) {
    std::ofstream stream(config->cache_file.c_str());
    write_binary_journal(stream, journal.get(), &journal->sources);
  }

  TIMER_STOP(write_cache);

  return 0;
}

// main.cc ends here.
