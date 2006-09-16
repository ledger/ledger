#ifdef USE_PCH
#include "pch.h"
#else
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

#include "option.h"
#include "acconf.h"

#ifdef HAVE_UNIX_PIPES
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include "fdstream.hpp"
#endif

#ifdef USE_BOOST_PYTHON
#include "pyledger.h"
#else
#include "ledger.h"
#endif
#include "debug.h"
#endif

using namespace ledger;

static int parse_and_report(report_t * report, int argc, char * argv[],
			    char * envp[])
{
  session_t& session(*report->session);

  // Parse command-line arguments

  std::list<std::string> args;
  process_arguments(argc - 1, argv + 1, false, report, args);

  if (args.empty()) {
#if 0
    help(std::cerr);
#endif
    return 1;
  }
  strings_list::iterator arg = args.begin();

  if (session.cache_file == "<none>")
    session.use_cache = false;
  else
    session.use_cache = session.data_file.empty() && session.price_db.empty();

  DEBUG_PRINT("ledger.session.cache", "1. use_cache = " << session.use_cache);

  // Process environment settings

  TRACE(main, "Processing options and environment settings");

  process_environment(const_cast<const char **>(envp), "LEDGER_", report);

  const char * p = std::getenv("HOME");
  std::string home = p ? p : "";

  if (session.init_file.empty())
    session.init_file  = home + "/.ledgerrc";
  if (session.price_db.empty())
    session.price_db   = home + "/.pricedb";

  if (session.cache_file.empty())
    session.cache_file = home + "/.ledger-cache";

  if (session.data_file == session.cache_file)
    session.use_cache = false;

  DEBUG_PRINT("ledger.session.cache", "2. use_cache = " << session.use_cache);

  TRACE(main, std::string("Initialization file is ") + session.init_file);
  TRACE(main, std::string("Price database is ") + session.price_db);
  TRACE(main, std::string("Binary cache is ") + session.cache_file);
  TRACE(main, std::string("Main journal is ") + session.data_file);

  TRACE(main, std::string("Based on option settings, binary cache ") +
	(session.use_cache ? "WILL " : "will NOT ") + "be used");

  // Read the command word

  std::string verb = *arg++;

  valexpr_t::functor_t * command = NULL;

  if (verb == "register" || verb == "reg" || verb == "r")
    command = new register_command(report);
#if 0
  else if (verb == "balance" || verb == "bal" || verb == "b")
    command = new balance_command;
  else if (verb == "print" || verb == "p")
    command = new print_command;
  else if (verb == "output")
    command = new output_command;
  else if (verb == "dump")
    command = new dump_command;
  else if (verb == "emacs" || verb == "lisp")
    command = new emacs_command;
  else if (verb == "xml")
    command = new xml_command;
  else if (verb == "entry")
    command = new entry_command;
  else if (verb == "equity")
    command = new equity_command;
  else if (verb == "prices")
    command = new prices_command;
  else if (verb == "pricesdb")
    command = new pricesdb_command;
  else if (verb == "csv")
    command = new csv_command;
  else if (verb == "expr")
    command = new expr_command;
#endif
  else if (verb == "parse") {
    valexpr_t expr(*arg);

    if (session.verbose_mode) {
      std::cout << "Value expression tree:" << std::endl;
      expr.dump(std::cout);
      std::cout << std::endl;
      std::cout << "Value expression parsed was:" << std::endl;
      expr.write(std::cout);
      std::cout << std::endl << std::endl;
      std::cout << "Result of calculation: ";
    }

    std::cout << expr.calc(report).strip_annotations() << std::endl;

    return 0;
  }
  else {
    char buf[128];
    std::strcpy(buf, "command_");
    std::strcat(buf, verb.c_str());
    if (valexpr_t::node_t * def = report->lookup(buf))
      command = def->functor_obj();

    if (! command)
      throw new error(std::string("Unrecognized command '") + verb + "'");
  }

  // Parse the initialization file, which can only be textual

  session.read_init();

  // Parse ledger data, price database, etc.

  journal_t * journal = session.read_data(report->account);

#if 0
  // process the command word and its following arguments

  std::string first_arg;
  if (command == "w") {
    if (arg != args.end())
      first_arg = *arg++;
  }
  else if (command == "W") {
    if (report.output_file.empty())
      throw new error("The 'dump' command requires use of the --output option");
  }

  TRACE(options, std::string("Post-processing options ") +
	"for command \"" + command + "\"");

  report.process_options(command, arg, args.end());

  // If downloading is to be supported, configure the updater

  if (! commodity_base_t::updater && session.download_quotes)
    commodity_base_t::updater =
      new quotes_by_script(session.price_db, session.pricing_leeway,
			   session.cache_dirty);

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
#endif

  // Configure the output stream

#ifdef HAVE_UNIX_PIPES
  int status, pfd[2];		// Pipe file descriptors
#endif
  std::ostream * out = &std::cout;

  if (! report->output_file.empty()) {
    out = new std::ofstream(report->output_file.c_str());
  }
#ifdef HAVE_UNIX_PIPES
  else if (! report->pager.empty()) {
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
      arg0 = std::strrchr(report->pager.c_str(), '/');
      if (arg0)
	arg0++;
      else
	arg0 = report->pager.c_str(); // No slashes in pager.

      execlp(report->pager.c_str(), arg0, (char *)0);
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

  if (verb == "expr") {
    valexpr_t expr(*arg);

    if (session.verbose_mode) {
      std::cout << "Value expression tree:" << std::endl;
      expr.dump(std::cout);
      std::cout << std::endl;
      std::cout << "Value expression parsed was:" << std::endl;
      expr.write(std::cout);
      std::cout << std::endl << std::endl;
      std::cout << "Result of calculation: ";
    }

    std::cout << expr.calc(report).strip_annotations() << std::endl;

    return 0;
  }

#if 0
  // Compile the format strings

  const std::string * format;

  if (! report.format_string.empty())
    format = &report.format_string;
  else if (command == "b")
    format = &session.balance_format;
  else if (command == "r")
    format = &session.register_format;
  else if (command == "E")
    format = &session.equity_format;
  else if (command == "P")
    format = &session.prices_format;
  else if (command == "D")
    format = &session.pricesdb_format;
  else if (command == "w")
    format = &session.write_xact_format;
  else
    format = &session.print_format;

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
    TRACE_PUSH(text_writer, "Writing journal file");
    write_textual_journal(*journal, first_arg, *formatter,
			  session.write_hdr_format, *out);
    TRACE_POP(text_writer, "Finished writing");
  }
  else if (command == "W") {
    TRACE_PUSH(binary_writer, "Writing binary file");
    std::ofstream stream(report.output_file.c_str());
    write_binary_journal(stream, journal);
    TRACE_POP(binary_writer, "Finished writing");
  }
  else {
    TRACE_PUSH(main, "Walking journal entries");

    formatter = report.chain_xact_handlers(command, formatter, journal,
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
    TRACE_POP(main, "Finished account walk");
  }
  else if (command == "E") {
    TRACE_PUSH(main, "Walking journal accounts");

    format_equity acct_formatter(*out, *format, report.display_predicate);
    sum_accounts(*journal->master);
    walk_accounts(*journal->master, acct_formatter, report.sort_string);
    acct_formatter.flush();

    TRACE_POP(main, "Finished account walk");
  }

#if DEBUG_LEVEL >= BETA
  { TRACE_PUSH(cleanup, "Cleaning up allocated memory");

#ifdef USE_BOOST_PYTHON
    shutdown_ledger_for_python();
#endif

    for (int i = 0; i < OPTIONS_SIZE; i++)
      delete options[i].handler;

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

    TRACE_POP(cleanup, "Finished cleaning"); }
#endif
#endif

  valexpr_t::scope_t * locals = new valexpr_t::scope_t(report, true);

  std::auto_ptr<repitem_t> items(repitem_t::wrap(&session, report, true));

  locals->args.push_back(out);
  locals->args.push_back(items.get());

  if (command->wants_args)
    for (strings_list::iterator i = args.begin();
	 i != args.end();
	 i++)
      locals->args.push_back(*i);

  value_t temp;
  (*command)(temp, locals);

  // Write out the binary cache, if need be

  if (session.use_cache && session.cache_dirty &&
      ! session.cache_file.empty()) {
    TRACE_PUSH(binary_cache, "Writing journal file");

    std::ofstream stream(session.cache_file.c_str());
    write_binary_journal(stream, journal);

    TRACE_POP(binary_cache, "Finished writing");
  }

#ifdef HAVE_UNIX_PIPES
  if (! report->pager.empty()) {
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
    std::ios::sync_with_stdio(false);

    ledger::tracing_active = true;

#if DEBUG_LEVEL < BETA
    ledger::do_cleanup = false;
#endif
    TRACE_PUSH(main, "Ledger starting");

    ledger::session_t * session = new ledger::session_t;

    session->register_parser(new binary_parser_t);
#if defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)
    session->register_parser(new xml_parser_t);
    session->register_parser(new gnucash_parser_t);
#endif
#ifdef HAVE_LIBOFX
    session->register_parser(new ofx_parser_t);
#endif
    session->register_parser(new qif_parser_t);
    session->register_parser(new textual_parser_t);

    ledger::report_t * report = new ledger::report_t(session);

    int status = parse_and_report(report, argc, argv, envp);

    if (ledger::do_cleanup) {
      delete report;
      delete session;
    }

    TRACE_POP(main, "Ledger done");

    DEBUG_IF("ledger.trace.memory") {
      report_memory(std::cout);
    }

    ledger::tracing_active = false;

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
    ledger::tracing_active = false;
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
    ledger::tracing_active = false;
    return 1;
  }
  catch (const std::exception& err) {
    std::cout.flush();
    std::cerr << "Error: " << err.what() << std::endl;
    ledger::tracing_active = false;
    return 1;
  }
  catch (int status) {
    ledger::tracing_active = false;
    return status;
  }
}

// main.cc ends here.
