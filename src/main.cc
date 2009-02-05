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

#include <ledger.h>		// Read this file for a top-level overview

#include "work.h"		// This is where the meat of main() is, which
				// was moved there for the sake of clarity
using namespace ledger;

namespace {
  char * stripwhite (char * string)
  {
    if (! string)
      return NULL;

    register char *s, *t;

    for (s = string; isspace (*s); s++)
      ;

    if (*s == 0)
      return (s);

    t = s + strlen (s) - 1;
    while (t > s && isspace (*t))
      t--;
    *++t = '\0';

    return s;
  }

  void sigint_handler(int sig)
  {
    throw std::logic_error("Interrupted by user (use Control-D to quit)");
  }

  strings_list split_arguments(char * line)
  {
    strings_list args;

    // jww (2009-02-04): This is too naive
    for (char * p = std::strtok(line, " \t");
	 p;
	 p = std::strtok(NULL, " \t"))
      args.push_back(p);

    return args;
  }

  /**
   * @return \c true if a command was actually executed; otherwise, it probably
   *         just resulted in setting some options.
   */
  bool execute_command(session_t&	    session,
		       ledger::strings_list args,
		       char **              envp = NULL)
  {
    // Create the report object, which maintains state relating to each
    // command invocation.  Because we're running from main(), the distinction
    // between session and report doesn't really matter, but if a GUI were
    // calling into Ledger it would have one session object per open document,
    // with a separate report_t object for each report it generated.
    std::auto_ptr<report_t> manager(new report_t(session));
    report_t& report(*manager.get());

    session.global_scope = &report;

    // Read the user's options, in the following order:
    //
    //  1. environment variables (LEDGER_<option>)
    //  2. initialization file (~/.ledgerrc)
    //  3. command-line (--option or -o)
    //
    // Before processing command-line options, we must notify the session
    // object that such options are beginning, since options like -f cause a
    // complete override of files found anywhere else.
    if (envp) {
      session.now_at_command_line(false);
      read_environment_settings(report, envp);
      session.read_init();
    }
    session.now_at_command_line(true);
    args = read_command_arguments(report, args);

    // Look for a precommand first, which is defined as any defined function
    // whose name starts with "ledger_precmd_".  The difference between a
    // precommand and a regular command is that precommands ignore the journal
    // data file completely, nor is the user's init file read.
    //
    // Here are some examples of pre-commands:
    //
    //   parse STRING       ; show how a value expression is parsed
    //   eval STRING        ; simply evaluate a value expression
    //   format STRING      ; show how a format string is parsed
    //
    // If such a command is found, create the output stream for the result and
    // then invoke the command.

    if (args.empty()) {
      read_journal_files(session, report.account);
      return false;
    } else {
      string_iterator arg  = args.begin();
      string	      verb = *arg++;

      if (function_t command = look_for_precommand(report, verb)) {
	// Create the output stream (it might be a file, the console or a PAGER
	// subprocess) and invoke the report command.
	create_output_stream(report);
	invoke_command_verb(report, command, arg, args.end());
      }
      else if (function_t command = look_for_command(report, verb)) {
	// This is regular command verb, so parse the user's data if we
	// haven't already at the beginning of the REPL.
	if (! envp || read_journal_files(session, report.account)) {
	  normalize_report_options(report, verb); // jww (2009-02-02): a hack

	  // Create the output stream (it might be a file, the console or a
	  // PAGER subprocess) and invoke the report command.
	  create_output_stream(report);
	  invoke_command_verb(report, command, arg, args.end());
	}
      }
      else {
	throw_(std::logic_error, "Unrecognized command '" << verb << "'");
      }

      session.global_scope = NULL;

      return true; 
    }
  }

  int execute_command_wrapper(session_t&	   session,
			      ledger::strings_list args,
			      char **              envp = NULL)
  {
    int status = 1;

    try {
      if (! execute_command(session, args, envp))
	return -1;

      // If we've reached this point, everything succeeded fine.  Ledger uses
      // exceptions to notify of error conditions, so if you're using gdb,
      // just type "catch throw" to find the source point of any error.
      status = 0;
    }
    catch (const std::exception& err) {
      std::cout.flush();		// first display anything that was pending

      // Display any pending error context information
      string context = error_context();
      if (! context.empty())
	std::cerr << context << std::endl;
    
      std::cerr << "Error: " << err.what() << std::endl;
    }
    catch (int _status) {
      status = _status;		// used for a "quick" exit, and is used only
				// if help text (such as --help) was displayed
    }
    return status;
  }
}

int main(int argc, char * argv[], char * envp[])
{
  session_t * session = NULL;

  // The very first thing we do is handle some very special command-line
  // options, since they affect how the environment is setup:
  //
  //   --verify            ; turns on memory tracing
  //   --verbose           ; turns on logging
  //   --debug CATEGORY    ; turns on debug logging
  //   --trace LEVEL       ; turns on trace logging
  handle_debug_options(argc, argv);
  IF_VERIFY() initialize_memory_tracing();

  INFO("Ledger starting");

  // Initialize global Boost/C++ environment
  std::ios::sync_with_stdio(false);
  filesystem::path::default_name_check(filesystem::portable_posix_name);

  // Create the session object, which maintains nearly all state relating to
  // this invocation of Ledger; and register all known journal parsers.
  session = new LEDGER_SESSION_T;
  set_session_context(session);

  strings_list cmd_args;
  for (int i = 1; i < argc; i++)
    cmd_args.push_back(argv[i]);

  int status = execute_command_wrapper(*session, cmd_args, envp);
  if (status == -1) {		// no command was given; enter the REPL
    session->option_version(*session);
    
    std::signal(SIGINT, sigint_handler);

#ifdef HAVE_LIBEDIT

    rl_readline_name = const_cast<char *>("Ledger");
#if 0
    rl_attempted_completion_function = ledger_completion;
#endif

    while (char * line = stripwhite(readline("==> "))) {
      char * expansion = NULL;
      int    result;

      if (std::strcmp(line, "quit") == 0) {
	std::free(line);
	break;
      }

      result = history_expand(line, &expansion);

      if (result < 0 || result == 2) {
	std::free(line);
	throw_(std::logic_error,
	       "Failed to expand history reference '" << line << "'");
      }
      else if (expansion) {
	add_history(expansion);

	strings_list line_argv = split_arguments(line);
	execute_command_wrapper(*session, line_argv);

	std::free(expansion);
      }
      std::free(line);
    }

#else // HAVE_LIBEDIT

    while (! std::cin.eof()) {
      std::cout << "--> ";
      char line[1024];
      std::cin.getline(line, 1023);

      char * p = stripwhite(line);
      if (*p) {
	if (std::strcmp(p, "quit") == 0)
	  break;

	execute_command_wrapper(*session, split_arguments(line));
      }
    }

#endif // HAVE_LIBEDIT

    status = 0;			// report success
  }

  // If memory verification is being performed (which can be very slow), clean
  // up everything by closing the session and deleting the session object, and
  // then shutting down the memory tracing subsystem.  Otherwise, let it all
  // leak because we're about to exit anyway.
  IF_VERIFY() {
    set_session_context(NULL);
    if (session != NULL)
      checked_delete(session);

    INFO("Ledger ended (Boost/libstdc++ may still hold memory)");
    shutdown_memory_tracing();
  } else {
    // Don't free anything, just let it all leak.
    INFO("Ledger ended");
  }

  // Return the final status to the operating system, either 1 for error or 0
  // for a successful completion.
  return status;
}

// main.cc ends here.
