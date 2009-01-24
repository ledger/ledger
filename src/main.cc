/*
 * Copyright (c) 2003-2009, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of New Artisans LLC nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "session.h"
#include "report.h"
#include "option.h"
#include "help.h"
#if defined(HAVE_BOOST_PYTHON)
#include "pyinterp.h"
#endif

#include "textual.h"
#include "qif.h"
#include "xml.h"
#include "gnucash.h"
#ifdef HAVE_LIBOFX
#include "ofx.h"
#endif

#ifdef HAVE_UNIX_PIPES
#include <sys/types.h>
#include <sys/wait.h>
#include "fdstream.h"
#endif

namespace ledger {
  int read_and_report(ledger::report_t& report,
		      int argc, char * argv[], char * envp[])
  {
    using namespace ledger;

    session_t& session(report.session);

    // Setup global defaults

    optional<path> home;
    if (const char * home_var = std::getenv("HOME"))
      home = home_var;

    session.init_file  = home ? *home / ".ledgerrc"	: "./.ledgerrc";
    session.price_db   = home ? *home / ".pricedb"	: "./.pricedb";
    session.cache_file = home ? *home / ".ledger-cache" : "./.ledger-cache";

    // Process the environment settings

    TRACE_START(environment, 1, "Processed environment variables");

    process_environment(const_cast<const char **>(envp), "LEDGER_", report);

#if 1
    // These are here for backwards compatability, but are deprecated.

    if (const char * p = std::getenv("LEDGER"))
      process_option("file", report, p);
    if (const char * p = std::getenv("LEDGER_INIT"))
      process_option("init-file", report, p);
    if (const char * p = std::getenv("PRICE_HIST"))
      process_option("price-db", report, p);
    if (const char * p = std::getenv("PRICE_EXP"))
      process_option("price-exp", report, p);
#endif

    TRACE_FINISH(environment, 1);

    // Read the initialization file

    TRACE_START(init, 1, "Read initialization file");

    session.read_init();

    TRACE_FINISH(init, 1);

    // Handle the command-line arguments

    TRACE_START(arguments, 1, "Processed command-line arguments");

    strings_list args;
    process_arguments(argc - 1, argv + 1, report, args);

    if (args.empty()) {
      ledger::help(std::cout);
      return 1;
    }
    strings_list::iterator arg = args.begin();

    if (! session.cache_file)
      session.use_cache = false;
    else if (session.use_cache)
      session.use_cache = ! session.data_file.empty() && session.price_db;

    DEBUG("ledger.session.cache", "1. use_cache = " << session.use_cache);

    if (session.data_file == *session.cache_file)
      session.use_cache = false;

    DEBUG("ledger.session.cache", "2. use_cache = " << session.use_cache);

    INFO("Initialization file is " << session.init_file->string());
    INFO("Price database is " << session.price_db->string());
    INFO("Binary cache is " << session.cache_file->string());
    INFO("Journal file is " << session.data_file.string());

    if (! session.use_cache)
      INFO("Binary cache mechanism will not be used");

    TRACE_FINISH(arguments, 1);

    // Configure the output stream

#ifdef HAVE_UNIX_PIPES
    int status, pfd[2];		// Pipe file descriptors
#endif
    report.output_stream = &std::cout;

    if (report.output_file) {
      report.output_stream = new ofstream(*report.output_file);
    }
#ifdef HAVE_UNIX_PIPES
    else if (report.pager) {
      status = pipe(pfd);
      if (status == -1)
	throw_(std::logic_error, "Failed to create pipe");

      status = fork();
      if (status < 0) {
	throw_(std::logic_error, "Failed to fork child process");
      }
      else if (status == 0) {	// child
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
	execlp(report.pager->native_file_string().c_str(),
	       basename(*report.pager).c_str(), NULL);
	perror("execl");
	exit(1);
      }
      else {			// parent
	close(pfd[0]);
	report.output_stream = new boost::fdostream(pfd[1]);
      }
    }
#endif

    // Read the command word and see if it's any of the debugging commands
    // that Ledger supports.

    std::ostream& out(*report.output_stream);

    string verb = *arg++;

    if (verb == "parse") {
      out << "--- Input text ---" << std::endl;
      out << *arg << std::endl;

      out << std::endl << "--- Text as parsed ---" << std::endl;
      expr_t expr(*arg);
      expr.print(out);
      out << std::endl;

      out << std::endl << "--- Expression tree ---" << std::endl;
      expr.dump(out);

      out << std::endl << "--- Calculated value ---" << std::endl;
      expr.calc(report).print(out);
      out << std::endl;

      return 0;
    }
    else if (verb == "compile") {
      out << "--- Input text ---" << std::endl;
      out << *arg << std::endl;

      out << std::endl << "--- Text as parsed ---" << std::endl;
      expr_t expr(*arg);
      expr.print(out);
      out << std::endl;

      out << std::endl << "--- Expression tree ---" << std::endl;
      expr.dump(out);

      expr.compile(report);

      out << std::endl << "--- Compiled tree ---" << std::endl;
      expr.dump(out);

      out << std::endl << "--- Calculated value ---" << std::endl;
      expr.calc(report).print(out);
      out << std::endl;

      return 0;
    }
    else if (verb == "eval") {
      expr_t expr(*arg);
      out << expr.calc(report).strip_annotations() << std::endl;
      return 0;
    }
    else if (verb == "format") {
      format_t fmt(*arg);
      fmt.dump(out);
      return 0;
    }
    else if (verb == "period") {
      interval_t interval(*arg);

      if (! is_valid(interval.begin)) {
	out << "Time period has no beginning." << std::endl;
      } else {
	out << "begin: " << format_date(interval.begin) << std::endl;
	out << "  end: " << format_date(interval.end) << std::endl;
	out << std::endl;

	date_t date = interval.first();

	for (int i = 0; i < 20; i++) {
	  out << std::right;
	  out.width(2);

	  out << i << ": " << format_date(date) << std::endl;

	  date = interval.increment(date);
	  if (is_valid(interval.end) && date >= interval.end)
	    break;
	}
      }
      return 0;
    }

    // Parse the initialization file, which can only be textual; then
    // parse the journal data.

    INFO_START(journal, "Read journal file");

    journal_t& journal(*session.create_journal());

    std::size_t count = session.read_data(journal, report.account);
    if (count == 0)
      throw_(parse_error, "Failed to locate any journal entries; "
	     "did you specify a valid file with -f?");

    INFO_FINISH(journal);

    INFO("Found " << count << " entries");

    TRACE_FINISH(entry_text, 1);
    TRACE_FINISH(entry_date, 1);
    TRACE_FINISH(entry_details, 1);
    TRACE_FINISH(entry_xacts, 1);
    TRACE_FINISH(entries, 1);
    TRACE_FINISH(parsing_total, 1);

    // Lookup the command object corresponding to the command verb.

    function_t command;
    if (expr_t::ptr_op_t def = report.lookup(string("cmd_") + verb))
      command = def->as_function();

    if (! command)
      throw_(std::logic_error, string("Unrecognized command '") + verb + "'");

    // Patch up some of the reporting options based on what kind of
    // command it was.

    // jww (2008-08-14): This code really needs to be rationalized away
    // for 3.0.

    if (verb[0] == 'p' || verb == "entry" || verb == "dump") {
      report.show_related     = true;
      report.show_all_related = true;
    }
    else if (verb == "equity") {
      report.show_subtotal = true;
    }
    else if (report.show_related) {
      if (verb[0] == 'r') {
	report.show_inverted = true;
      } else {
	report.show_subtotal    = true;
	report.show_all_related = true;
      }
    }

    if (verb[0] != 'b' && verb[0] != 'r')
      amount_t::keep_base = true;

    // Setup the default value for the display predicate

    if (report.display_predicate.empty()) {
      if (verb[0] == 'b') {
	if (! report.show_empty)
	  report.display_predicate = "total";
	if (! report.show_subtotal) {
	  if (! report.display_predicate.empty())
	    report.display_predicate += "&";
	  report.display_predicate += "depth<=1";
	}
      }
      else if (verb == "equity") {
	report.display_predicate = "amount_expr"; // jww (2008-08-14): ???
      }
      else if (verb[0] == 'r' && ! report.show_empty) {
	report.display_predicate = "amount";
      }
    }

    // Now setup the various formatting strings

    // jww (2008-08-14): I hear a song, and it's sound is "HaAaaCcK"

#if 0
    if (! date_output_format.empty())
      date_t::output_format = date_output_format;
#endif

    amount_t::keep_price = report.keep_price;
    amount_t::keep_date  = report.keep_date;
    amount_t::keep_tag   = report.keep_tag;

    if (! report.report_period.empty() && ! report.sort_all)
      report.entry_sort = true;

    // Create an argument scope containing the report command's
    // arguments, and then invoke the command.

    call_scope_t command_args(report);

    for (strings_list::iterator i = arg; i != args.end(); i++)
      command_args.push_back(string_value(*i));

    INFO_START(command, "Did user command '" << verb << "'");

    command(command_args);

    INFO_FINISH(command);

#if 0
    // Write out the binary cache, if need be

    if (session.use_cache && session.cache_dirty && session.cache_file) {
      TRACE_START(binary_cache, 1, "Wrote binary journal file");

      ofstream stream(*session.cache_file);
      journal.write(stream);

      TRACE_FINISH(binary_cache, 1);
    }
#endif

    // If the user specified a pager, wait for it to exit now

#ifdef HAVE_UNIX_PIPES
    if (! report.output_file && report.pager) {
      checked_delete(report.output_stream);
      close(pfd[1]);

      // Wait for child to finish
      wait(&status);
      if (status & 0xffff != 0)
	throw_(std::logic_error, "Something went wrong in the pager");
    }
#endif
    else if (DO_VERIFY() && report.output_file) {
      checked_delete(report.output_stream);
    }

    return 0;
  }
}

int main(int argc, char * argv[], char * envp[])
{
  int status = 1;

  for (int i = 1; i < argc; i++)
    if (argv[i][0] == '-') {
      if (std::strcmp(argv[i], "--verify") == 0) {
#if defined(VERIFY_ON)
	ledger::verify_enabled = true;
#endif
      }
      else if (std::strcmp(argv[i], "--verbose") == 0 ||
	       std::strcmp(argv[i], "-v") == 0) {
#if defined(LOGGING_ON)
	ledger::_log_level    = ledger::LOG_INFO;
#endif
      }
      else if (i + 1 < argc && std::strcmp(argv[i], "--debug") == 0) {
#if defined(DEBUG_ON)
	ledger::_log_level    = ledger::LOG_DEBUG;
	ledger::_log_category = argv[i + 1];
	i++;
#endif
      }
      else if (i + 1 < argc && std::strcmp(argv[i], "--trace") == 0) {
#if defined(TRACING_ON)
	ledger::_log_level   = ledger::LOG_TRACE;
	try {
	  ledger::_trace_level = boost::lexical_cast<int>(argv[i + 1]);
	}
	catch (const boost::bad_lexical_cast& e) {
	  std::cerr << "Argument to --trace must be an integer."
		    << std::endl;
	  return 1;
	}
	i++;
#endif
      }
    }

  IF_VERIFY()
    ledger::initialize_memory_tracing();

  try {
    std::ios::sync_with_stdio(false);

    boost::filesystem::path::default_name_check
      (boost::filesystem::portable_posix_name);

    INFO("Ledger starting");

#if defined(HAVE_BOOST_PYTHON)
    std::auto_ptr<ledger::session_t> session(new ledger::python_interpreter_t);
#else
    std::auto_ptr<ledger::session_t> session(new ledger::session_t);
#endif

    ledger::set_session_context(session.get());

#if 0
    session->register_parser(new ledger::journal_t::binary_parser_t);
#endif
    session->register_parser(new ledger::xml_parser_t);
    session->register_parser(new ledger::gnucash_parser_t);
#ifdef HAVE_LIBOFX
    session->register_parser(new ledger::ofx_parser_t);
#endif
    session->register_parser(new ledger::qif_parser_t);
    session->register_parser(new ledger::textual_parser_t);

    session->current_report.reset(new ledger::report_t(*session.get()));

    status = read_and_report(*session->current_report.get(), argc, argv, envp);

    if (! DO_VERIFY())
      session.release();	// don't free anything! just let it leak
  }
  catch (const std::exception& err) {
    std::cout.flush();
    std::cerr << ledger::error_context()
	      << "Error: " << err.what() << std::endl;
  }
  catch (int _status) {
    status = _status;
  }

  IF_VERIFY() {
    INFO("Ledger ended (Boost/libstdc++ may still hold memory)");
    ledger::set_session_context();
    ledger::shutdown_memory_tracing();
  } else {
    INFO("Ledger ended");
  }

  return status;
}

// main.cc ends here.
