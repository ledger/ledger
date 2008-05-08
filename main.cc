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

#include "acconf.h"

#ifdef HAVE_UNIX_PIPES
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include "fdstream.hpp"
#endif

#include "ledger.h"

using namespace ledger;

int parse_and_report(config_t& config, report_t& report,
		     int argc, char * argv[], char * envp[])
{
  // Configure the terminus for value expressions

  ledger::terminus = current_moment;

  // Parse command-line arguments, and those set in the environment

  std::list<string> args;
  process_arguments(ledger::config_options, argc - 1, argv + 1, false, args);

  if (args.empty()) {
    option_help(std::cerr);
    return 1;
  }
  strings_list::iterator arg = args.begin();

  if (config.cache_file == "<none>")
    config.use_cache = false;
  else
    config.use_cache = config.data_file.empty() && config.price_db.empty();
  DEBUG("ledger.config.cache", "1. use_cache = " << config.use_cache);

#if 0
  TRACE(main, "Processing options and environment variables");
#endif

  process_environment(ledger::config_options,
		      const_cast<const char **>(envp), "LEDGER_");

#if 1
  // These are here for backwards compatability, but are deprecated.

  if (const char * p = std::getenv("LEDGER"))
    process_option(ledger::config_options, "file", p);
  if (const char * p = std::getenv("LEDGER_INIT"))
    process_option(ledger::config_options, "init-file", p);
  if (const char * p = std::getenv("PRICE_HIST"))
    process_option(ledger::config_options, "price-db", p);
  if (const char * p = std::getenv("PRICE_EXP"))
    process_option(ledger::config_options, "price-exp", p);
#endif

  const char * p    = std::getenv("HOME");
  string  home = p ? p : "";

  if (config.init_file.empty())
    config.init_file  = home + "/.ledgerrc";
  if (config.price_db.empty())
    config.price_db   = home + "/.pricedb";

  if (config.cache_file.empty())
    config.cache_file = home + "/.ledger-cache";

  if (config.data_file == config.cache_file)
    config.use_cache = false;
  DEBUG("ledger.config.cache", "2. use_cache = " << config.use_cache);

#if 0
  TRACE(main, string("Initialization file is ") + config.init_file);
  TRACE(main, string("Price database is ") + config.price_db);
  TRACE(main, string("Binary cache is ") + config.cache_file);
  TRACE(main, string("Main journal is ") + config.data_file);

  TRACE(main, string("Based on option settings, binary cache ") +
	(config.use_cache ? "WILL " : "will NOT ") + "be used");
#endif

  // Read the command word, canonicalize it to its one letter form,
  // then configure the system based on the kind of report to be
  // generated

  string command = *arg++;

  if (command == "balance" || command == "bal" || command == "b")
    command = "b";
  else if (command == "register" || command == "reg" || command == "r")
    command = "r";
  else if (command == "print" || command == "p")
    command = "p";
  else if (command == "output")
    command = "w";
  else if (command == "dump")
    command = "W";
  else if (command == "emacs" || command == "lisp")
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
  else if (command == "csv")
    command = "c";
  else if (command == "parse") {
    value_expr expr(ledger::parse_value_expr(*arg));

    if (config.verbose_mode) {
      std::cout << "Value expression tree:" << std::endl;
      ledger::dump_value_expr(std::cout, expr.get());
      std::cout << std::endl;
      std::cout << "Value expression parsed was:" << std::endl;
      ledger::print_value_expr(std::cout, expr.get());
      std::cout << std::endl << std::endl;
      std::cout << "Result of computation: ";
    }

    value_t result = guarded_compute(expr.get());
    std::cout << result.strip_annotations() << std::endl;

    return 0;
  }
  else if (command == "expr") {
    // this gets done below...
  }
  else {
    throw new error(string("Unrecognized command '") + command + "'");
  }

  // Parse initialization files, ledger data, price database, etc.

  std::auto_ptr<journal_t> journal(new journal_t);

#if 0
  { TRACE_PUSH(parser, "Parsing journal file");
#endif

    if (parse_ledger_data(config, journal.get()) == 0)
      throw new error("Please specify ledger file using -f"
		      " or LEDGER_FILE environment variable.");

#if 0
    TRACE_POP(parser, "Finished parsing"); }
#endif

  // process the command word and its following arguments

  string first_arg;
  if (command == "w") {
    if (arg != args.end())
      first_arg = *arg++;
  }
  else if (command == "W") {
    if (report.output_file.empty())
      throw new error("The 'dump' command requires use of the --output option");
  }

#if 0
  TRACE(options, string("Post-processing options ") +
	"for command \"" + command + "\"");
#endif

  report.process_options(command, arg, args.end());

#if 0
  // jww (2008-05-08): Is this disabled now?
  // If downloading is to be supported, configure the updater

  if (! commodity_base_t::updater && config.download_quotes)
    commodity_base_t::updater =
      new quotes_by_script(config.price_db, config.pricing_leeway,
			   config.cache_dirty);
#endif

  std::auto_ptr<entry_t> new_entry;
  if (command == "e") {
    if (arg == args.end()) {
      std::cout << "\
The entry command requires at least one argument, so Ledger can intelligently\n\
create a new entry for you.  The possible arguments are:\n\
    DATE  PAYEE  [ACCOUNT] [AMOUNT] [DRAW ACCOUNT]\n\n\
Some things to note:\n\
  - The ACCOUNT is optional; if no account is given, the last account affected\n\
    by PAYEE is used.  If no payee can be found, the generic account 'Expenses'\n\
    is used.\n\
  - The AMOUNT is optional; if not specified, the same amount is used as the\n\
    last time PAYEE was seen, or 0 if not applicable.\n\
  - The AMOUNT does not require a commodity; if none is given, the commodity\n\
    currently contained within ACCOUNT is used, or no commodity at all if\n\
    either: the ACCOUNT was not found, or it contains more than one commodity.\n\
  - Lastly, the DRAW ACCOUNT is optional; if not present, the last account\n\
    drawn from by PAYEE is used, or the 'basket' account (specified with\n\
    'A ACCOUNT' in your Ledger file) if that does not apply, or the generic\n\
    account 'Equity' is used.\n\n\
Here are a few examples, all of which may be equivalent depending on your\n\
Ledger data:\n\
    ledger entry 3/25 chevron\n\
    ledger entry 3/25 chevron 20\n\
    ledger entry 3/25 chevron \\$20\n\
    ledger entry 3/25 chevron gas 20\n\
    ledger entry 3/25 chevron gas \\$20 checking\n\n\
A final note: Ledger never modifies your data!  You are responsible for\n\
appending the output of this command to your Ledger file if you so choose."
		<< std::endl;
      return 1;
    }
    new_entry.reset(derive_new_entry(*journal, arg, args.end()));
    if (! new_entry.get())
      return 1;
  }

  // Configure the output stream

#ifdef HAVE_UNIX_PIPES
  int status, pfd[2];		// Pipe file descriptors
#endif
  std::ostream * out = &std::cout;

  if (! report.output_file.empty()) {
    out = new ofstream(report.output_file);
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
    value_expr expr(ledger::parse_value_expr(*arg));

    if (config.verbose_mode) {
      std::cout << "Value expression tree:" << std::endl;
      ledger::dump_value_expr(std::cout, expr.get());
      std::cout << std::endl;
      std::cout << "Value expression parsed was:" << std::endl;
      ledger::print_value_expr(std::cout, expr.get());
      std::cout << std::endl << std::endl;
      std::cout << "Result of computation: ";
    }

    value_t result = guarded_compute(expr.get());
    std::cout << result.strip_annotations() << std::endl;

    return 0;
  }

  // Compile the format strings

  const string * format;

  if (! report.format_string.empty())
    format = &report.format_string;
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
  else if (command == "X")
    formatter = new format_xml_entries(*out, report.show_totals);
  else if (command == "c")
    formatter = new format_csv_transactions(*out);
  else
    formatter = new format_transactions(*out, *format);

  if (command == "w") {
#if 0
    TRACE_PUSH(text_writer, "Writing journal file");
#endif
    write_textual_journal(*journal, first_arg, *formatter,
			  config.write_hdr_format, *out);
#if 0
    TRACE_POP(text_writer, "Finished writing");
#endif
  }
  else if (command == "W") {
#if 0
    TRACE_PUSH(binary_writer, "Writing binary file");
#endif
    ofstream stream(report.output_file);
    binary::write_journal(stream, journal.get());
#if 0
    TRACE_POP(binary_writer, "Finished writing");
#endif
  }
  else {
#if 0
    TRACE_PUSH(main, "Walking journal entries");
#endif

    formatter = report.chain_xact_handlers(command, formatter, journal.get(),
					   journal->master, formatter_ptrs);
    if (command == "e")
      walk_transactions(new_entry->transactions, *formatter);
    else if (command == "P" || command == "D")
      walk_commodities(amount_t::current_pool->commodities, *formatter);
    else
      walk_entries(journal->entries, *formatter);

    if (command != "P" && command != "D")
      formatter->flush();

#if 0
    TRACE_POP(main, "Finished entry walk");
#endif
  }

  // For the balance and equity reports, output the sum totals.

  if (command == "b") {
#if 0
    TRACE_PUSH(main, "Walking journal accounts");
#endif

    format_account acct_formatter(*out, *format, report.display_predicate);
    sum_accounts(*journal->master);
    walk_accounts(*journal->master, acct_formatter, report.sort_string);
    acct_formatter.flush();

    if (account_has_xdata(*journal->master)) {
      account_xdata_t& xdata = account_xdata(*journal->master);
      if (! report.show_collapsed && xdata.total) {
	*out << "--------------------\n";
	xdata.value = xdata.total;
	acct_formatter.format.format(*out, details_t(*journal->master));
      }
    }
#if 0
    TRACE_POP(main, "Finished account walk");
#endif
  }
  else if (command == "E") {
#if 0
    TRACE_PUSH(main, "Walking journal accounts");
#endif

    format_equity acct_formatter(*out, *format, report.display_predicate);
    sum_accounts(*journal->master);
    walk_accounts(*journal->master, acct_formatter, report.sort_string);
    acct_formatter.flush();

#if 0
    TRACE_POP(main, "Finished account walk");
#endif
  }

#if DEBUG_LEVEL >= BETA
#if 0
  { TRACE_PUSH(cleanup, "Cleaning up allocated memory");
#endif

  clear_transaction_xdata xact_cleaner;
  walk_entries(journal->entries, xact_cleaner);

  clear_account_xdata acct_cleaner;
  walk_accounts(*journal->master, acct_cleaner);

  if (! report.output_file.empty())
    delete out;

  for (std::list<item_handler<transaction_t> *>::iterator i
	 = formatter_ptrs.begin();
       i != formatter_ptrs.end();
       i++)
    delete *i;

#if 0
  TRACE_POP(cleanup, "Finished cleaning"); }
#endif
#endif

  // Write out the binary cache, if need be

  if (config.use_cache && config.cache_dirty &&
      ! config.cache_file.empty()) {
#if 0
    TRACE_PUSH(binary_cache, "Writing journal file");
#endif

    ofstream stream(config.cache_file);
    binary::write_journal(stream, journal.get());

#if 0
    TRACE_POP(binary_cache, "Finished writing");
#endif
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
    report_t report;
    ledger::config = &config;
    ledger::report = &report;
#if 0
    TRACE_PUSH(main, "Ledger starting");
#endif
    int status = parse_and_report(config, report, argc, argv, envp);
#if 0
    TRACE_POP(main, "Ledger done");
#endif
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
