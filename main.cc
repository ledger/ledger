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

using namespace ledger;

int parse_and_report(config_t& config, int argc, char * argv[], char * envp[])
{
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

  TRACE(main, "Processing options and environment variables");

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

  TRACE(main, std::string("Initialization file is ") + config.init_file);
  TRACE(main, std::string("Price database is ") + config.price_db);
  TRACE(main, std::string("Binary cache is ") + config.cache_file);
  TRACE(main, std::string("Main journal is ") + config.data_file);

  TRACE(main, std::string("Based on option settings, binary cache ") +
	(config.use_cache ? "WILL " : "will NOT ") + "be used");

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
  else if (command == "lisp")
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
  else if (command == "parse") {
    value_auto_ptr expr(ledger::parse_value_expr(*arg));
    if (config.verbose_mode) {
      std::cout << "Value expression tree:" << std::endl;
      ledger::dump_value_expr(std::cout, expr.get());
      std::cout << std::endl;
      std::cout << "Value expression parsed was:" << std::endl;
      ledger::write_value_expr(std::cout, expr.get());
      std::cout << std::endl << std::endl;
      std::cout << "Result of computation: ";
    }
    value_t result = guarded_compute(expr.get());

    if (! config.keep_price || ! config.keep_date || ! config.keep_tag) {
      switch (result.type) {
      case value_t::AMOUNT:
      case value_t::BALANCE:
      case value_t::BALANCE_PAIR:
	result = result.strip_annotations();
	break;
      default:
	break;
      }
    }
    std::cout << result << std::endl;

    return 0;
  }
  else if (command == "expr") {
    // this gets done below...
  }
  else {
    throw new error(std::string("Unrecognized command '") + command + "'");
  }

  // Parse initialization files, ledger data, price database, etc.

  std::auto_ptr<journal_t> journal(new journal_t);

  { TRACE_PUSH(parser, "Parsing journal file");

    if (parse_ledger_data(config, journal.get()) == 0)
      throw new error("Please specify ledger file using -f"
		      " or LEDGER_FILE environment variable.");

    TRACE_POP(parser, "Finished parsing"); }

  // process the command word and its following arguments

  std::string first_arg;
  if (command == "w") {
    if (arg == args.end())
      throw new error("The 'output' command requires a file argument");
    first_arg = *arg++;
  }

  TRACE(options, std::string("Post-processing options ") +
	"for command \"" + command + "\"");

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
      throw new error("Failed to create pipe");

    status = fork();
    if (status < 0) {
      throw new error("Failed to fork child process");
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
      if (arg0)
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

  // Are we handling the parse or expr commands?  Do so now.

  if (command == "expr") {
    value_auto_ptr expr(ledger::parse_value_expr(*arg));
    if (config.verbose_mode) {
      std::cout << "Value expression tree:" << std::endl;
      ledger::dump_value_expr(std::cout, expr.get());
      std::cout << std::endl;
      std::cout << "Value expression parsed was:" << std::endl;
      ledger::write_value_expr(std::cout, expr.get());
      std::cout << std::endl << std::endl;
      std::cout << "Result of computation: ";
    }
    value_t result = guarded_compute(expr.get());

    if (! config.keep_price || ! config.keep_date || ! config.keep_tag) {
      switch (result.type) {
      case value_t::AMOUNT:
      case value_t::BALANCE:
      case value_t::BALANCE_PAIR:
	result = result.strip_annotations();
	break;
      default:
	break;
      }
    }
    std::cout << result << std::endl;

    return 0;
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
  else if (command == "P")
    format = &config.prices_format;
  else if (command == "D")
    format = &config.pricesdb_format;
  else if (command == "w")
    format = &config.write_xact_format;
  else
    format = &config.print_format;

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
#if defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)
    formatter = new format_xml_entries(*out, config.show_totals);
#else
    throw new error("XML support was not compiled into this copy of Ledger");
#endif
  } else
    formatter = new format_transactions(*out, *format);

  if (command == "w") {
    TRACE_PUSH(text_writer, "Writing journal file");
    write_textual_journal(*journal, first_arg, *formatter,
			  config.write_hdr_format, *out);
    TRACE_POP(text_writer, "Finished writing");
  } else {
    TRACE_PUSH(main, "Walking journal entries");

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

    TRACE_POP(main, "Finished entry walk");
  }

  // For the balance and equity reports, output the sum totals.

  if (command == "b") {
    TRACE_PUSH(main, "Walking journal accounts");

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
    TRACE_POP(main, "Finished account walk");
  }
  else if (command == "E") {
    TRACE_PUSH(main, "Walking journal accounts");

    format_equity acct_formatter(*out, *format, config.display_predicate);
    sum_accounts(*journal->master);
    walk_accounts(*journal->master, acct_formatter, config.sort_string);
    acct_formatter.flush();

    TRACE_POP(main, "Finished account walk");
  }

#if DEBUG_LEVEL >= BETA
  { TRACE_PUSH(cleanup, "Cleaning up allocated memory");

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

  TRACE_POP(cleanup, "Finished cleaning"); }
#endif

  // Write out the binary cache, if need be

  if (config.use_cache && config.cache_dirty && ! config.cache_file.empty()) {
    TRACE_PUSH(binary_cache, "Writing journal file");

    std::ofstream stream(config.cache_file.c_str());
    write_binary_journal(stream, journal.get());

    TRACE_POP(binary_cache, "Finished writing");
  }

#ifdef HAVE_UNIX_PIPES
  if (! config.pager.empty()) {
    delete out;
    close(pfd[1]);

    // Wait for child to finish
    wait(&status);
    if (status & 0xffff != 0)
      throw new error("Something went wrong in the pager");
  }
#endif

  return 0;
}

int main(int argc, char * argv[], char * envp[])
{
  try {
#if DEBUG_LEVEL < BETA
    ledger::do_cleanup = false;
#endif
    config_t config;
    TRACE_PUSH(main, "Starting Ledger " PACKAGE_VERSION);
    int status = parse_and_report(config, argc, argv, envp);
    TRACE_POP(main, "Ledger done");
    return status;
  }
  catch (error * err) {
    std::cout.flush();
    // Push a null here since there's no file context
    if (err->context.empty() ||
	! dynamic_cast<xact_context *>(err->context.front()))
      err->context.push_front(new error_context(""));
    err->reveal_context(std::cerr, "Error");
    std::cerr << err->what() << std::endl;
    delete err;
    return 1;
  }
  catch (fatal * err) {
    std::cout.flush();
    // Push a null here since there's no file context
    if (err->context.empty() ||
	! dynamic_cast<xact_context *>(err->context.front()))
      err->context.push_front(new error_context(""));
    err->reveal_context(std::cerr, "Fatal");
    std::cerr << err->what() << std::endl;
    delete err;
    return 1;
  }
  catch (const std::exception& err) {
    std::cout.flush();
    std::cerr << "Error: " << err.what() << std::endl;
    return 1;
  }
  catch (int status) {
    return status;
  }
}

// main.cc ends here.
