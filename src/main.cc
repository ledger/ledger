/*
 * Copyright (c) 2003-2018, John Wiegley.  All rights reserved.
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

#include <system.hh>

#include "global.h"             // This is where the meat of main() is, which
                                // was moved there for the sake of clarity here
#include "session.h"

using namespace ledger;

#if HAVE_BOOST_PYTHON
namespace ledger {
  extern char * argv0;
}
#endif

int main(int argc, char * argv[], char * envp[])
{
  int status = 1;

#if HAVE_BOOST_PYTHON
  argv0 = argv[0];
#endif

  // The very first thing we do is handle some very special command-line
  // options, since they affect how the environment is setup:
  //
  //   --verify            ; turns on memory tracing
  //   --verbose           ; turns on logging
  //   --debug CATEGORY    ; turns on debug logging
  //   --trace LEVEL       ; turns on trace logging
  //   --memory            ; turns on memory usage tracing
  //   --init-file         ; directs ledger to use a different init file
  handle_debug_options(argc, argv);
#if VERIFY_ON
  IF_VERIFY() initialize_memory_tracing();
#endif

  INFO("Ledger starting");

  // Initialize global Boost/C++ environment
  std::ios::sync_with_stdio(false);
#if BOOST_VERSION < 104600
  filesystem::path::default_name_check(filesystem::portable_posix_name);
#endif

  std::signal(SIGINT, sigint_handler);
#if !defined(_WIN32) && !defined(__CYGWIN__)
  std::signal(SIGPIPE, sigpipe_handler);
#endif

#if HAVE_GETTEXT
  ::textdomain("ledger");
#endif

  global_scope_t * global_scope = NULL;

  try {
    // Create the session object, which maintains nearly all state relating to
    // this invocation of Ledger; and register all known journal parsers.
    global_scope = new global_scope_t(envp);
    global_scope->session().set_flush_on_next_data_file(true);

    // Construct an STL-style argument list from the process command arguments
    strings_list args;
    for (int i = 1; i < argc; i++)
      args.push_back(argv[i]);

    // Look for options and a command verb in the command-line arguments
    bind_scope_t bound_scope(*global_scope, global_scope->report());
    args = global_scope->read_command_arguments(bound_scope, args);

    if (global_scope->HANDLED(script_)) {
      // Ledger is being invoked as a script command interpreter
      global_scope->session().read_journal_files();

      status = 0;

      ifstream in(global_scope->HANDLER(script_).str());
      while (status == 0 && ! in.eof()) {
        char line[1024];
        in.getline(line, 1023);

        char * p = skip_ws(line);
        if (*p && *p != '#')
          status =
            global_scope->execute_command_wrapper(split_arguments(p), true);
      }
    }
    else if (! args.empty()) {
      // User has invoke a verb at the interactive command-line
      status = global_scope->execute_command_wrapper(args, false);
    }
    else {
      // Commence the REPL by displaying the current Ledger version
      global_scope->show_version_info(std::cout);

      global_scope->session().read_journal_files();

      bool exit_loop = false;

#if HAVE_EDIT

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
                 _f("Failed to expand history reference '%1%'") % p);
        }
        else if (expansion) {
          add_history(expansion);
        }

#else // HAVE_EDIT

      while (! std::cin.eof()) {
        std::cout << global_scope->prompt_string();
        char line[1024];
        std::cin.getline(line, 1023);

        char * p = skip_ws(line);

#endif // HAVE_EDIT

        check_for_signal();

        if (*p && *p != '#') {
          if (std::strncmp(p, "quit", 4) == 0)
            exit_loop = true;
          else
            global_scope->execute_command_wrapper(split_arguments(p), true);
        }

#if HAVE_EDIT
        if (expansion)
          std::free(expansion);
        std::free(p);
#endif

        if (exit_loop)
          break;
      }

      status = 0;                       // report success
    }
  }
  catch (const std::exception& err) {
    if (global_scope)
      global_scope->report_error(err);
    else
      std::cerr << "Exception during initialization: " << err.what()
                << std::endl;
  }
  catch (const error_count& errors) {
    // used for a "quick" exit, and is used only if help text (such as
    // --help) was displayed
    status = static_cast<int>(errors.count);
  }

  // If memory verification is being performed (which can be very slow), clean
  // up everything by closing the session and deleting the session object, and
  // then shutting down the memory tracing subsystem.  Otherwise, let it all
  // leak because we're about to exit anyway.
#if VERIFY_ON
  IF_VERIFY() {
    checked_delete(global_scope);

    INFO("Ledger ended (Boost/libstdc++ may still hold memory)");
#if VERIFY_ON
    shutdown_memory_tracing();
#endif
  } else
#endif
  {
    if (global_scope)
      global_scope->quick_close();
    INFO("Ledger ended");       // let global_scope leak!
  }

  // Return the final status to the operating system, either 1 for error or 0
  // for a successful completion.
  return status;
}

// main.cc ends here.
