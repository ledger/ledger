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

  char * prompt_string(const ptr_list<report_t>& report_stack)
  {
    static char prompt[32];
    std::size_t i;
    for (i = 0; i < report_stack.size(); i++)
      prompt[i] = ']';
    prompt[i++] = ' ';
    prompt[i]   = '\0';
    return prompt;
  }

  void report_error(const std::exception& err)
  {
    std::cout.flush();		// first display anything that was pending

    if (caught_signal == NONE_CAUGHT) {
      // Display any pending error context information
      string context = error_context();
      if (! context.empty())
	std::cerr << context << std::endl;
    
      std::cerr << "Error: " << err.what() << std::endl;
    } else {
      caught_signal = NONE_CAUGHT;
    }
  }

  /**
   * @return \c true if a command was actually executed; otherwise, it probably
   *         just resulted in setting some options.
   */
  void execute_command(session_t&   session,
		       report_t&    report,
		       strings_list args,
		       bool         at_repl)
  {
    // Create a new report command object based on the current one, so that
    // the next command's option don't corrupt state.
    std::auto_ptr<report_t> manager(new report_t(report));

    // Process the command verb, arguments and options
    args = read_command_arguments(*manager.get(), args);
    if (args.empty())
      return;

    string_iterator arg  = args.begin();
    string	    verb = *arg++;

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

    function_t command;
    bool       is_precommand = false;

    if (bool(command = look_for_precommand(*manager.get(), verb)))
      is_precommand = true;
    else if (! bool(command = look_for_command(*manager.get(), verb)))
      throw_(std::logic_error, "Unrecognized command '" << verb << "'");

    // If it is not a pre-command, then parse the user's ledger data at this
    // time if not done alreday (i.e., if not at a REPL).  Then patch up the
    // report options based on the command verb.

    if (! is_precommand) {
      if (! at_repl)
	read_journal_files(session, manager->account);

      // jww (2009-02-02): This is a complete hack, and a leftover from long,
      // long ago.  The question is, how best to remove its necessity...
      normalize_report_options(*manager.get(), verb);
    }

    // Create the output stream (it might be a file, the console or a PAGER
    // subprocess) and invoke the report command.

    create_output_stream(*manager.get()); // closed by auto_ptr destructor
    invoke_command_verb(*manager.get(), command, arg, args.end());
  }

  int execute_command_wrapper(session_t&   session,
			      report_t&    report,
			      strings_list args,
			      bool         at_repl)
  {
    int status = 1;

    try {
      execute_command(session, report, args, at_repl);

      // If we've reached this point, everything succeeded fine.  Ledger uses
      // exceptions to notify of error conditions, so if you're using gdb,
      // just type "catch throw" to find the source point of any error.
      status = 0;
    }
    catch (const std::exception& err) {
      report_error(err);
    }
    return status;
  }
}

int main(int argc, char * argv[], char * envp[])
{
  int status;

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

  std::signal(SIGINT, sigint_handler);
  std::signal(SIGPIPE, sigpipe_handler);

  // Create the session object, which maintains nearly all state relating to
  // this invocation of Ledger; and register all known journal parsers.
  session_t * session = new LEDGER_SESSION_T;
  set_session_context(session);

  // Create the report object, which maintains state relating to each command
  // invocation.  Because we're running from main(), the distinction between
  // session and report doesn't really matter, but if a GUI were calling into
  // Ledger it would have one session object per open document, with a
  // separate report_t object for each report it generated.
  ptr_list<report_t> report_stack;
  report_stack.push_front(new report_t(*session));

  try {
    // Read the user's options, in the following order:
    //
    //  1. environment variables (LEDGER_<option>)
    //  2. initialization file (~/.ledgerrc)
    //  3. command-line (--option or -o)
    //
    // Before processing command-line options, we must notify the session object
    // that such options are beginning, since options like -f cause a complete
    // override of files found anywhere else.
    session->now_at_command_line(false);
    read_environment_settings(report_stack.front(), envp);
    session->read_init();
    session->now_at_command_line(true);

    // Construct an STL-style argument list from the process command arguments
    strings_list args;
    for (int i = 1; i < argc; i++)
      args.push_back(argv[i]);

    // Look for options and a command verb in the command-line arguments
    args = read_command_arguments(report_stack.front(), args);

    if (! args.empty()) {	// user has invoke a command-line verb
      status = execute_command_wrapper(*session, report_stack.front(), args,
				       false);
    } else {
      // Commence the REPL by displaying the current Ledger version
      session->option_version(*session);

      read_journal_files(*session, report_stack.front().account);

      bool exit_loop = false;

#ifdef HAVE_LIBEDIT

      rl_readline_name = const_cast<char *>("Ledger");
#if 0
      // jww (2009-02-05): NYI
      rl_attempted_completion_function = ledger_completion;
#endif

      while (char * p = readline(prompt_string(report_stack))) {
	char * expansion = NULL;
	int    result;

	result = history_expand(skip_ws(p), &expansion);

	if (result < 0 || result == 2) {
	  if (expansion)
	    std::free(expansion);
	  std::free(p);
	  throw_(std::logic_error,
		 "Failed to expand history reference '" << p << "'");
	}
	else if (expansion) {
	  add_history(expansion);
	}

#else // HAVE_LIBEDIT

      while (! std::cin.eof()) {
	std::cout << prompt_string(report_stack);
	char line[1024];
	std::cin.getline(line, 1023);

	char * p = skip_ws(line);

#endif // HAVE_LIBEDIT

	bool do_command = true;

	check_for_signal();

	if (! *p) {
	  do_command = false;
	}
	else if (std::strncmp(p, "quit", 4) == 0) {
	  exit_loop = true;
	  do_command = false;
	}
	else if (std::strncmp(p, "push", 4) == 0) {
	  report_stack.push_front(new report_t(report_stack.front()));
	}
	else if (std::strncmp(p, "pop", 3) == 0) {
	  report_stack.pop_front();
	  do_command = false;
	}

	if (do_command)
	  execute_command_wrapper(*session, report_stack.front(),
				  split_arguments(p), true);

#ifdef HAVE_LIBEDIT
	if (expansion)
	  std::free(expansion);
	std::free(p);
#endif // HAVE_LIBEDIT

	if (exit_loop)
	  break;
      }

      status = 0;			// report success
    }
  }
  catch (const std::exception& err) {
    report_error(err);
  }
  catch (int _status) {
    status = _status;		// used for a "quick" exit, and is used only
				// if help text (such as --help) was displayed
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
