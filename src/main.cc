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

#include <ledger.h>

#include "work.h"		// this is where the top-level code is

int main(int argc, char * argv[], char * envp[])
{
  using namespace ledger;

  // The very first thing we do is handle some very special command-line
  // options, since they affect how the whole environment is setup:
  //
  //   --verify            ; turns on memory tracing
  //   --verbose           ; turns on logging
  //   --debug CATEGORY    ; turns on debug logging
  //   --trace LEVEL       ; turns on trace logging
  handle_debug_options(argc, argv);

  IF_VERIFY()
    initialize_memory_tracing();

  // Initialize the global C++ environment
  std::ios::sync_with_stdio(false);
  filesystem::path::default_name_check(filesystem::portable_posix_name);

  // Initialization of Ledger can now begin.  The whole series of operations
  // is contained in a try block, so we can report errors in a nicer fashion.
  INFO("Ledger starting");

  session_t * session = NULL;
  int	      status  = 1;
  try {
    // Create the session object, which maintains nearly all state relating to
    // this invocation of Ledger.
    session = new LEDGER_SESSION_T;
    set_session_context(session);

    // Register all known journal parsers.  The order of these is important.
    register_journal_parsers(*session);

    // Create the report object, which maintains state relating to each
    // command invocation.  Because we're running this from main() the
    // distinction between session and report doesn't matter, but if a GUI
    // were calling into Ledger, it would have one session object, with a
    // separate report object for each report it generated.
    session->report.reset(new report_t(*session));
    report_t& report(*session->report.get());

    // Read user option settings, first in the environment, then from the
    // user's initialization file and then from the command-line.  The first
    // non-option argument thereafter is the "command verb".
    read_environment_settings(report, envp);
    session->read_init();

    // Notify the session object that all option handlers invoked beyond this
    // point came from the command-line
    session->now_at_command_line(true);

    strings_list    args = read_command_line_arguments(report, argc, argv);
    string_iterator arg  = args.begin();
    string	    verb = *arg++;

    // Look for a precommand first, which is defined as any defined function
    // whose name starts with "ledger_precmd_".  The difference between a
    // precommand and a regular command is that precommands ignore the journal
    // data file completely, nor is the user's init file read.
    //
    // Here are some examples:
    //
    //   parse STRING       ; show how a value expression is parsed
    //   eval STRING        ; simply evaluate a value expression
    //   format STRING      ; show how a format string is parsed
    //
    // If such a command is found, create the output stream for the result and
    // then invoke the command.
    if (function_t command = look_for_precommand(report, verb)) {
      create_output_stream(report);
      invoke_command_verb(report, command, arg, args.end());
    }
    else if (function_t command = look_for_command(report, verb)) {
      // Parse the user's journal files.
      if (journal_t * journal = read_journal_files(*session)) {
	normalize_report_options(report, verb); // this is a total hack

	// Create the output stream (it might be a file, the console or a
	// PAGER subprocess) and invoke the report command.
	create_output_stream(report);
	invoke_command_verb(report, command, arg, args.end());

	// Write out a binary cache of the journal data, if needed and
	// appropriate to do so
	write_binary_cache(*session, journal);
      }
    }
    else {
      throw_(std::logic_error, "Unrecognized command '" << verb << "'");
    }

    // If we got here, everything succeeded just fine.  Ledger uses exceptions
    // to notify of any error conditions, so if you're using gdb, type "catch
    // throw" to find the source of any errors.
    status = 0;
  }
  catch (const std::exception& err) {
    std::cout.flush();
    std::cerr << error_context() << std::endl
	      << "Error: " << err.what() << std::endl;
  }
  catch (int _status) {
    status = _status;
  }

  // Close the output stream, waiting on the pager process if need be
  session->report->output_stream.close();

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

  return status;
}

// main.cc ends here.
