#include "ledger.h"
#include "parser.h"
#include "textual.h"
#include "binary.h"
#include "qif.h"
#ifdef READ_GNUCASH
#include "gnucash.h"
#endif
#include "valexpr.h"
#include "format.h"
#include "walk.h"
#include "quotes.h"
#include "option.h"
#include "config.h"
#include "timing.h"
#include "error.h"

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

#ifndef DO_CLEANUP

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

#endif // !DO_CLEANUP

static void
regexps_to_predicate(std::list<std::string>::const_iterator begin,
		     std::list<std::string>::const_iterator end,
		     config_t * config,
		     const bool account_regexp		= false,
		     const bool add_account_short_masks = false)
{
  std::vector<std::string> regexps(2);

  // Treat the remaining command-line arguments as regular
  // expressions, used for refining report results.

  for (std::list<std::string>::const_iterator i = begin;
       i != end;
       i++)
    if ((*i)[0] == '-') {
      if (! regexps[1].empty())
	regexps[1] += "|";
      regexps[1] += (*i).substr(1);
    }
    else if ((*i)[0] == '+') {
      if (! regexps[0].empty())
	regexps[0] += "|";
      regexps[0] += (*i).substr(1);
    }
    else {
      if (! regexps[0].empty())
	regexps[0] += "|";
      regexps[0] += *i;
    }

  for (std::vector<std::string>::const_iterator i = regexps.begin();
       i != regexps.end();
       i++)
    if (! (*i).empty()) {
      if (! config->predicate.empty())
	config->predicate += "&";

      if (i != regexps.begin()) {
	config->predicate += "!";
      }
      else if (add_account_short_masks) {
	if ((*i).find(':') != std::string::npos) {
	  config->show_subtotal = true;
	} else {
	  if (! config->display_predicate.empty())
	    config->display_predicate += "&";
	  else if (! config->show_empty)
	    config->display_predicate += "T&";

	  config->display_predicate += "///(?:";
	  config->display_predicate += *i;
	  config->display_predicate += ")/";
	}
      }

      if (! account_regexp)
	config->predicate += "/";
      config->predicate += "/(?:";
      config->predicate += *i;
      config->predicate += ")/";
    }
}

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

  bool use_cache = config->data_file.empty();

  // Process options from the environment

  TIMER_START(process_env);

  process_environment(envp, "LEDGER_");

#if 1
  // These are here for backwards compatability, but are deprecated.

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

  // Setup the parsers
  std::auto_ptr<binary_parser_t>  bin_parser(new binary_parser_t);
  std::auto_ptr<qif_parser_t>     qif_parser(new qif_parser_t);
  std::auto_ptr<textual_parser_t> text_parser(new textual_parser_t);

  parser_t::parsers.push_back(bin_parser.get());
#ifdef READ_GNUCASH
  parser_t::parsers.push_back(gnucash_parser.get());
#endif
  parser_t::parsers.push_back(qif_parser.get());
  parser_t::parsers.push_back(text_parser.get());

  int entry_count = 0;

  try {
    if (! config->init_file.empty())
      if (parser_t::parse_file(config->init_file, journal.get()))
	throw error("Entries not allowed in initialization file");

    if (use_cache && ! config->cache_file.empty() &&
	! config->data_file.empty()) {
      entry_count += parser_t::parse_file(config->cache_file, journal.get(),
					  NULL, &config->data_file);
      journal->sources.pop_front(); // remove cache_file

      if (entry_count == 0) {
	journal.reset(new journal_t);
	cache_dirty = true;
      } else {
	cache_dirty = false;
      }
    }

    if (entry_count == 0 && ! config->data_file.empty()) {
      account_t * account = NULL;
      if (! config->account.empty())
	account = journal->find_account(config->account);

      if (config->data_file == "-") {
	use_cache = false;
	entry_count += text_parser->parse(std::cin, journal.get(), account);
      } else {
	entry_count += parser_t::parse_file(config->data_file, journal.get(),
					    account);
      }

      if (! config->price_db.empty())
	if (parser_t::parse_file(config->price_db, journal.get()))
	  throw error("Entries not allowed in price history file");
    }

    for (strings_list::iterator i = config->price_settings.begin();
	 i != config->price_settings.end();
	 i++) {
      std::string conversion = "C ";
      conversion += *i;
      int i = conversion.find('=');
      if (i != -1) {
	conversion[i] = ' ';
	std::istringstream stream(conversion);
	text_parser->parse(stream, journal.get(), journal->master);
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

  // Configure some other options depending on report type

  bool show_all_related = false;

  if (command == "p" || command == "e") {
    config->show_related =
    show_all_related	 = true;
  }
  else if (command == "E") {
    config->show_subtotal = true;
  }
  else if (config->show_related) {
    if (command == "r") {
      config->show_inverted = true;
    } else {
      config->show_subtotal = true;
      show_all_related      = true;
    }
  }

  // Process remaining command-line arguments

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

    regexps_to_predicate(arg, i, config, true,
			 command == "b" && ! config->show_subtotal &&
			 config->display_predicate.empty());
    if (i != args.end())
      regexps_to_predicate(i, args.end(), config);
  }

  // Setup default value for the display predicate

  if (config->display_predicate.empty()) {
    if (command == "b") {
      if (! config->show_empty)
	config->display_predicate = "T";
      if (! config->show_subtotal) {
	if (! config->display_predicate.empty())
	  config->display_predicate += "&";
	config->display_predicate += "l<=1";
      }
    }
    else if (command == "E") {
      config->display_predicate = "t";
    }
  }

  // Compile sorting criteria

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

  // Setup the values of %t and %T, used in format strings

  try {
    format_t::value_expr = parse_value_expr(config->value_expr);
  }
  catch (const value_expr_error& err) {
    std::cerr << "Error in amount (-t) specifier: " << err.what()
	      << std::endl;
    return 1;
  }

  try {
    format_t::total_expr = parse_value_expr(config->total_expr);
  }
  catch (const value_expr_error& err) {
    std::cerr << "Error in total (-T) specifier: " << err.what()
	      << std::endl;
    return 1;
  }

  // Setup local and global variables, depending on config settings.

  std::auto_ptr<std::ostream> output_stream;

  interval_t  report_interval;
  std::time_t interval_begin = 0;

  if (config->download_quotes)
    commodity_t::updater = new quotes_by_script(config->price_db,
						config->pricing_leeway,
						cache_dirty);

  if (! config->output_file.empty())
    output_stream.reset(new std::ofstream(config->output_file.c_str()));

#define OUT() (output_stream.get() ? *output_stream : std::cout)

  if (! config->interval_text.empty()) {
    try {
      std::istringstream stream(config->interval_text);
      std::time_t begin = -1, end = -1;

      report_interval = interval_t::parse(stream, &begin, &end);

      if (begin != -1) {
	interval_begin = begin;

	if (! config->predicate.empty())
	  config->predicate += "&";
	char buf[32];
	std::sprintf(buf, "d>=%lu", begin);
	config->predicate += buf;
      }

      if (end != -1) {
	if (! config->predicate.empty())
	  config->predicate += "&";
	char buf[32];
	std::sprintf(buf, "d<%lu", end);
	config->predicate += buf;
      }
    }
    catch (const interval_expr_error& err) {
      std::cerr << "Error in interval (-z) specifier: " << err.what()
		<< std::endl;
      return 1;
    }
  }

  if (! config->date_format.empty())
    format_t::date_format = config->date_format;

#ifdef DEBUG_ENABLED
  DEBUG_PRINT("ledger.main.predicates", "predicate: " << config->predicate);
  DEBUG_PRINT("ledger.main.predicates",
	      "disp-pred: " << config->display_predicate);
#endif

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

  // Walk the entries based on the report type and the options

  TIMER_START(report_gen);

  // Stack up all the formatter needed to fulfills the user's
  // requests.  Some of these are order dependent, in terms of
  // whether calc_transactions occurs before or after them.

  std::auto_ptr<item_handler<transaction_t> > formatter;

  // format_transactions write each transaction received to the
  // output stream.
  if (command == "b" || command == "E") {
#ifdef DEBUG_ENABLED
    if (DEBUG("ledger.balance.items")) {
      formatter.reset(new format_transactions(OUT(), format, nformat));
      formatter.reset(new set_account_value(formatter.release()));
    } else
#endif
    formatter.reset(new set_account_value);
  } else {
    formatter.reset(new format_transactions(OUT(), format, nformat));

    // filter_transactions will only pass through transactions
    // matching the `display_predicate'.
    formatter.reset(new filter_transactions(formatter.release(),
					    config->display_predicate));

    // calc_transactions computes the running total.  When this
    // appears will determine, for example, whether filtered
    // transactions are included or excluded from the running total.
    formatter.reset(new calc_transactions(formatter.release(),
					  config->show_inverted));

    // sort_transactions will sort all the transactions it sees, based
    // on the `sort_order' value expression.
    if (sort_order.get())
      formatter.reset(new sort_transactions(formatter.release(),
					    sort_order.get()));

    // changed_value_transactions adds virtual transactions to the
    // list to account for changes in market value of commodities,
    // which otherwise would affect the running total unpredictably.
    if (config->show_revalued)
      formatter.reset(new changed_value_transactions(formatter.release(),
						     config->show_revalued_only));

    // collapse_transactions causes entries with multiple transactions
    // to appear as entries with a subtotaled transaction for each
    // commodity used.
    if (config->show_collapsed)
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
    if (config->show_subtotal)
      formatter.reset(new subtotal_transactions(formatter.release()));
    else if (report_interval)
      formatter.reset(new interval_transactions(formatter.release(),
						report_interval,
						interval_begin));
    else if (config->days_of_the_week)
      formatter.reset(new dow_transactions(formatter.release()));
  }

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
  if (command == "e")
    walk_transactions(new_entry->transactions, *formatter);
  else
    walk_entries(journal->entries, *formatter);

  formatter->flush();

  // At this point all printing is finished if doing a register
  // report; but if it's a balance or equity report, we've only
  // finished calculating the totals and there is still reporting to
  // be done.

  if (command == "b") {
    format_account acct_formatter(OUT(), format, config->display_predicate);
    sum_accounts(journal->master);
    walk_accounts(journal->master, acct_formatter, sort_order.get());
    acct_formatter.flush();

    if (journal->master->data) {
      ACCT_DATA(journal->master)->value = ACCT_DATA(journal->master)->total;

      if (ACCT_DATA(journal->master)->dflags & ACCOUNT_TO_DISPLAY) {
	std::string end_format = "--------------------\n";
	format.reset(end_format + f);
	format.format_elements(OUT(), details_t(journal->master));
      }
    }
  }
  else if (command == "E") {
    format_equity acct_formatter(OUT(), format, nformat,
				 config->display_predicate);
    sum_accounts(journal->master);
    walk_accounts(journal->master, acct_formatter, sort_order.get());
    acct_formatter.flush();
  }

#ifdef DO_CLEANUP
  // Cleanup the data handlers that might be present on some objects.

  clear_transaction_data xact_cleanup;
  walk_entries(journal->entries, xact_cleanup);

  clear_account_data acct_cleanup;
  walk_accounts(journal->master, acct_cleanup);
#endif

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
