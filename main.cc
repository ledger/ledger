#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <exception>
#include <iterator>
#include <string>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>

#include "acconf.h"

#ifdef HAVE_UNIX_PIPES
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include "fdstream.hpp"
#endif

#include "ledger.h"
#include "timing.h"

using namespace ledger;

namespace {
  TIMER_DEF_(setup);
  TIMER_DEF_(parse);
  TIMER_DEF_(process);
  TIMER_DEF_(walk);
  TIMER_DEF_(cleanup);
  TIMER_DEF_(cache_write);
}

int parse_and_report(int argc, char * argv[], char * envp[])
{
  TIMER_START(setup);

  config_t config;

  std::auto_ptr<journal_t> journal(new journal_t);

  // Configure the terminus for value expressions

  ledger::terminus = now;

  // Parse command-line arguments, and those set in the environment

  std::list<std::string> args;
  config.process_arguments(argc - 1, argv + 1, false, args);

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

  config.process_environment(envp, "LEDGER_");

#if 1
  // These are here for backwards compatability, but are deprecated.

  if (const char * p = std::getenv("LEDGER"))
    config.process_option("file", p);
  if (const char * p = std::getenv("LEDGER_INIT"))
    config.process_option("init-file", p);
  if (const char * p = std::getenv("PRICE_HIST"))
    config.process_option("price-db", p);
  if (const char * p = std::getenv("PRICE_EXP"))
    config.process_option("price-exp", p);
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
  else if (command == "csv") {
    config.register_format = config.csv_register_format;
    command = "r";
  }
  else
    throw error(std::string("Unrecognized command '") + command + "'");

  TIMER_STOP(setup);

  // Parse initialization files, ledger data, price database, etc.

  TIMER_START(parse);

  if (parse_ledger_data(config, journal.get()) == 0)
    throw error("Please specify ledger file using -f"
		" or LEDGER_FILE environment variable.");

  TIMER_STOP(parse);

  // process the command word and its following arguments

  TIMER_START(process);

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
      if (status == -1)
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

  TIMER_STOP(process);

  // Walk the entries based on the report type and the options

  TIMER_START(walk);

  item_handler<transaction_t> *		   formatter;
  std::list<item_handler<transaction_t> *> formatter_ptrs;

  if (command == "b" || command == "E")
    formatter = new set_account_value;
  else if (command == "p" || command == "e")
    formatter = new format_entries(*out, *format);
  else if (command == "x")
    formatter = new format_emacs_transactions(*out);
  else if (command == "X") {
#if defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)
    formatter = new format_xml_entries(*out, config.show_totals);
#else
    throw error("XML support was not compiled into this copy of Ledger");
#endif
  } else
    formatter = new format_transactions(*out, *format);

  if (command == "w") {
    write_textual_journal(*journal, first_arg, *formatter,
			  config.write_hdr_format, *out);
  } else {
    formatter = config.chain_xact_handlers(command, formatter, journal.get(),
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

  TIMER_STOP(walk);

  TIMER_START(cleanup);

#if DEBUG_LEVEL >= BETA
  clear_transaction_xdata xact_cleaner;
  walk_entries(journal->entries, xact_cleaner);

  clear_account_xdata acct_cleaner;
  walk_accounts(*journal->master, acct_cleaner);

  if (! config.output_file.empty())
    delete out;

  for (std::list<item_handler<transaction_t> *>::iterator i
	 = formatter_ptrs.begin();
       i != formatter_ptrs.end();
       i++)
    delete *i;
  formatter_ptrs.clear();
#endif

  TIMER_STOP(cleanup);

  // Write out the binary cache, if need be

  TIMER_START(cache_write);

  if (config.use_cache && config.cache_dirty && ! config.cache_file.empty()) {
    std::ofstream stream(config.cache_file.c_str());
    write_binary_journal(stream, journal.get());
  }

  TIMER_STOP(cache_write);

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
