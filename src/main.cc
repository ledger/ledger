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
  class global_scope_t : public noncopyable, public scope_t
  {
    scoped_ptr<session_t> session_ptr;
    ptr_list<report_t>	  report_stack;

  public:
    path script_file;

    global_scope_t(char ** envp)
    {
      TRACE_CTOR(global_scope_t, "");

      session_ptr.reset(new LEDGER_SESSION_T);

      set_session_context(session_ptr.get());

      // Create the report object, which maintains state relating to each
      // command invocation.  Because we're running from main(), the
      // distinction between session and report doesn't really matter, but if
      // a GUI were calling into Ledger it would have one session object per
      // open document, with a separate report_t object for each report it
      // generated.
      report_stack.push_front(new report_t(session()));

      // Read the user's options, in the following order:
      //
      //  1. environment variables (LEDGER_<option>)
      //  2. initialization file (~/.ledgerrc)
      //  3. command-line (--option or -o)
      //
      // Before processing command-line options, we must notify the session object
      // that such options are beginning, since options like -f cause a complete
      // override of files found anywhere else.
      session().now_at_command_line(false);
      read_environment_settings(report(), envp);
      session().read_init();
    }
    ~global_scope_t()
    {
      TRACE_DTOR(global_scope_t);

      // If memory verification is being performed (which can be very slow),
      // clean up everything by closing the session and deleting the session
      // object, and then shutting down the memory tracing subsystem.
      // Otherwise, let it all leak because we're about to exit anyway.
      IF_VERIFY() set_session_context(NULL);
    }

    void read_journal_files()
    {
      INFO_START(journal, "Read journal file");

      std::size_t count = session().read_data(*session().create_journal(),
					      report().account);
      if (count == 0)
	throw_(parse_error, "Failed to locate any journal entries; "
	       "did you specify a valid file with -f?");

      INFO_FINISH(journal);

      INFO("Found " << count << " entries");
    }

    char * prompt_string()
    {
      static char prompt[32];
      std::size_t i;
      for (i = 0; i < report_stack.size(); i++)
	prompt[i] = ']';
      prompt[i++] = ' ';
      prompt[i]   = '\0';
      return prompt;
    }

    session_t& session() {
      return *session_ptr.get();
    }
    report_t& report() {
      return report_stack.front();
    }

    void push_report() {
      report_stack.push_front(new report_t(report_stack.front()));
    }
    void pop_report() {
      if (! report_stack.empty())
	report_stack.pop_front();
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
    void execute_command(strings_list args, bool at_repl)
    {
      // Process the command verb, arguments and options
      args = read_command_arguments(report(), args);
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

      function_t   command;
      bool	   is_precommand = false;
      bind_scope_t bound_scope(*this, report());

      if (bool(command = look_for_precommand(bound_scope, verb)))
	is_precommand = true;
      else if (! bool(command = look_for_command(bound_scope, verb)))
	throw_(std::logic_error, "Unrecognized command '" << verb << "'");

      // If it is not a pre-command, then parse the user's ledger data at this
      // time if not done alreday (i.e., if not at a REPL).  Then patch up the
      // report options based on the command verb.

      if (! is_precommand) {
	if (! at_repl)
	  read_journal_files();

	// jww (2009-02-02): This is a complete hack, and a leftover from long,
	// long ago.  The question is, how best to remove its necessity...
	normalize_report_options(report(), verb);
      }

      // Create the output stream (it might be a file, the console or a PAGER
      // subprocess) and invoke the report command.  The output stream is closed
      // by the caller of this function.

      report().output_stream.initialize(report().output_file,
					session().pager_path);

      // Create an argument scope containing the report command's arguments, and
      // then invoke the command.  The bound scope causes lookups to happen
      // first in the global scope, and then in the report scope.

      call_scope_t command_args(bound_scope);
      for (string_iterator i = arg; i != args.end(); i++)
	command_args.push_back(string_value(*i));

      INFO_START(command, "Finished executing command");
      command(command_args);
      INFO_FINISH(command);
    }

    int execute_command_wrapper(strings_list args, bool at_repl)
    {
      int status = 1;

      try {
	push_report();
	execute_command(args, at_repl);
	pop_report();

	// If we've reached this point, everything succeeded fine.  Ledger uses
	// exceptions to notify of error conditions, so if you're using gdb,
	// just type "catch throw" to find the source point of any error.
	status = 0;
      }
      catch (const std::exception& err) {
	pop_report();
	report_error(err);
      }

      return status;
    }

    value_t push_report_cmd(call_scope_t&) {
      // Make a copy at position 2, because the topmost report object has an
      // open output stream at this point.  We want it to get popped off as
      // soon as this command terminate so that the stream is closed cleanly.
      report_stack.insert(++report_stack.begin(),
			  new report_t(report_stack.front()));
      return true;
    }
    value_t pop_report_cmd(call_scope_t&) {
      pop_report();
      return true;
    }

    value_t option_script_(call_scope_t& args) {
      script_file = args[0].as_string();
      return true;
    }

    value_t ignore(call_scope_t&) {
      return true;
    }

    virtual expr_t::ptr_op_t lookup(const string& name)
    {
      const char * p = name.c_str();
      switch (*p) {
      case 'l':
	if (std::strncmp(p, "ledger_precmd_", 14) == 0) {
	  p = p + 14;
	  switch (*p) {
	  case 'p':
	    if (std::strcmp(p, "push") == 0)
	      return MAKE_FUNCTOR(global_scope_t::push_report_cmd);
	    else if (std::strcmp(p, "pop") == 0)
	      return MAKE_FUNCTOR(global_scope_t::pop_report_cmd);
	    break;
	  }
	}
	break;

      case 'o':
	if (std::strncmp(p, "opt_", 4) == 0) {
	  p = p + 4;
	  switch (*p) {
	  case 'd':
	    if (std::strcmp(p, "debug_") == 0)
	      return MAKE_FUNCTOR(global_scope_t::ignore);
	    break;

	  case 's':
	    if (std::strcmp(p, "script_") == 0)
	      return MAKE_FUNCTOR(global_scope_t::option_script_);
	    break;

	  case 't':
	    if (std::strcmp(p, "trace_") == 0)
	      return MAKE_FUNCTOR(global_scope_t::ignore);
	    break;

	  case 'v':
	    if (! *(p + 1) || std::strcmp(p, "verbose") == 0)
	      return MAKE_FUNCTOR(global_scope_t::ignore);
	    else if (std::strcmp(p, "verify") == 0)
	      return MAKE_FUNCTOR(global_scope_t::ignore);
	    break;
	  }
	}
      }

      // If you're wondering how symbols from report() will be found, it's
      // because of the bind_scope_t object in execute_command() below.
      return expr_t::ptr_op_t();
    }
  };

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
  std::auto_ptr<global_scope_t> global_scope(new global_scope_t(envp));

  try {
    global_scope->session().now_at_command_line(true);

    // Construct an STL-style argument list from the process command arguments
    strings_list args;
    for (int i = 1; i < argc; i++)
      args.push_back(argv[i]);

    // Look for options and a command verb in the command-line arguments
    bind_scope_t bound_scope(*global_scope.get(), global_scope->report());

    args = read_command_arguments(bound_scope, args);

    if (! global_scope->script_file.empty() &&
	exists(global_scope->script_file)) {
      // Ledger is being invoked as a script command interpreter
      global_scope->read_journal_files();

      status = 0;

      ifstream in(global_scope->script_file);
      while (status == 0 && ! in.eof()) {
	char line[1024];
	in.getline(line, 1023);

	char * p = skip_ws(line);
	if (*p && *p != '#')
	  status = global_scope->execute_command_wrapper(split_arguments(p),
							 true);
      }
    }
    else if (! args.empty()) {
      // User has invoke a verb at the interactive command-line
      status = global_scope->execute_command_wrapper(args, false);
    }
    else {
      // Commence the REPL by displaying the current Ledger version
      global_scope->session().option_version(global_scope->session());

      global_scope->read_journal_files();

      bool exit_loop = false;

#ifdef HAVE_LIBEDIT

      rl_readline_name = const_cast<char *>("Ledger");
#if 0
      // jww (2009-02-05): NYI
      rl_attempted_completion_function = ledger_completion;
#endif

      while (char * p = readline(global_scope->prompt_string())) {
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
	std::cout << global_scope->prompt_string();
	char line[1024];
	std::cin.getline(line, 1023);

	char * p = skip_ws(line);

#endif // HAVE_LIBEDIT

	bool do_command = true;

	check_for_signal();

	if (*p && *p != '#') {
	  if (std::strncmp(p, "quit", 4) == 0)
	    exit_loop = true;
	  else
	    global_scope->execute_command_wrapper(split_arguments(p), true);
	}

#ifdef HAVE_LIBEDIT
	if (expansion)
	  std::free(expansion);
	std::free(p);
#endif

	if (exit_loop)
	  break;
      }

      status = 0;			// report success
    }
  }
  catch (const std::exception& err) {
    global_scope->report_error(err);
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
    global_scope.reset();

    INFO("Ledger ended (Boost/libstdc++ may still hold memory)");
    shutdown_memory_tracing();
  } else {
    // Don't free anything, just let it all leak.
    global_scope.release();

    INFO("Ledger ended");
  }

  // Return the final status to the operating system, either 1 for error or 0
  // for a successful completion.
  return status;
}

// main.cc ends here.
