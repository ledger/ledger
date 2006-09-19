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

static inline
const std::string& either_or(const std::string& first,
			     const std::string& second)
{
  if (first.empty())
    return second;
  else
    return first;
}

void print_addr(repitem_t * item) {
  std::cout << item << std::endl;
}

static int parse_and_report(report_t * report, int argc, char * argv[],
			    char * envp[])
{
  session_t& session(*report->session);

  // Handle the command-line arguments

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

  // Process the environment settings

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

  // Read the command word and create a command object based on it

  std::string verb = *arg++;

  valexpr_t::functor_t * command = NULL;

  if (verb == "tree")
    command = new dump_command;
  if (verb == "register" || verb == "reg" || verb == "r") {
    command = new format_command
      ("register", either_or(report->format_string,
			     report->session->register_format));
  }
#if 0
  else if (verb == "print" || verb == "p") {
    report->transforms.push_back(new optimize_transform);
    command = new format_command
      ("print", either_or(report->format_string,
			  report->session->print_format));
  }
  else if (verb == "balance" || verb == "bal" || verb == "b") {
    report->transforms.push_back(new accounts_transform);
    command = new format_command
      ("balance", either_or(report->format_string,
			     report->session->balance_format));
  }
  else if (verb == "equity") {
    report->transforms.push_back(new accounts_transform);
    command = new format_command
      ("equity", either_or(report->format_string,
			   report->session->equity_format));
  }
  else if (verb == "entry")
    command = new entry_command;
  else if (verb == "dump")
    command = new dump_command;
  else if (verb == "output")
    command = new output_command;
  else if (verb == "prices")
    command = new prices_command;
  else if (verb == "pricesdb")
    command = new pricesdb_command;
  else if (verb == "xml")
    command = new xml_command;
  else if (verb == "csv")
    command = new csv_command;
  else if (verb == "emacs" || verb == "lisp")
    command = new emacs_command;
#endif
  else if (verb == "expr")
    ;
  else if (verb == "path")
    ;
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

  // Parse the initialization file, which can only be textual; then
  // parse the journal data.

  session.read_init();

  journal_t * journal = session.read_data(report->account);

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

  // Are we handling the expr commands?  Do so now.

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
  else if (verb == "path") {
    std::cout << "Report item selection path:" << std::endl;
    std::auto_ptr<const repitem_t::path_t>
      path(repitem_t::parse_selector(*arg));
    repitem_t::dump_path(std::cout, path.get());

    std::auto_ptr<repitem_t> items(repitem_t::wrap(&session, report, true));
    items->select(path.get(), print_addr);
    return 0;
  }

  // Cleanup memory -- if this is a beta or development build.

#if DEBUG_LEVEL >= BETA
  { TRACE_PUSH(cleanup, "Cleaning up allocated memory");

#ifdef USE_BOOST_PYTHON
    shutdown_ledger_for_python();
#endif

    if (! report->output_file.empty())
      delete out;

    TRACE_POP(cleanup, "Finished cleaning"); }
#endif

  // Create the an argument scope containing the report command's
  // arguments, and then invoke the command.

  valexpr_t::scope_t * locals = new valexpr_t::scope_t(report, true);

  std::auto_ptr<repitem_t> items(repitem_t::wrap(&session, report, true));

  locals->args.push_back(out);
  locals->args.push_back(items.get());

  if (command->wants_args) {
    for (strings_list::iterator i = args.begin();
	 i != args.end();
	 i++)
      locals->args.push_back(*i);
  } else {
    std::string regexps[4];

    // Treat the remaining command-line arguments as regular
    // expressions, used for refining report results.

    int base = 0;
    for (strings_list::iterator i = arg; i != args.end(); i++)
      if ((*i)[0] == '-') {
	if ((*i)[1] == '-') {
	  if (base == 0)
	    base += 2;
	  continue;
	}
	if (! regexps[base + 1].empty())
	  regexps[base + 1] += "|";
	regexps[base + 1] += (*i).substr(1);
      } else {
	if (! regexps[base].empty())
	  regexps[base] += "|";
	regexps[base] += *i;
      }

    if (! regexps[3].empty())
      report->transforms.push_front
	(new remove_transform
	 (std::string("//entry[payee =~ /(") + regexps[3] + ")/]"));

    if (! regexps[2].empty())
      report->transforms.push_front
	(new select_transform
	 (std::string("//entry[payee =~ /(") + regexps[2] + ")/]"));

    if (! regexps[1].empty())
      report->transforms.push_front
	(new remove_transform
	 (std::string("//xact[account =~ /(") + regexps[1] + ")/]"));

    if (! regexps[0].empty())
      report->transforms.push_front
	(new select_transform
	 (std::string("//xact[account =~ /(") + regexps[0] + ")/]"));
  }

  report->apply_transforms(items.get());

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

  // If the user specified a pager, wait for it to exit now

#ifdef HAVE_UNIX_PIPES
  if (report->output_file.empty() && ! report->pager.empty()) {
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
