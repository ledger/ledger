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

#include "global.h"

namespace ledger {

global_scope_t::global_scope_t(char ** envp)
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
  session().set_flush_on_next_data_file(true);
  read_environment_settings(envp);
  session().set_flush_on_next_data_file(true);
  read_init();
}

global_scope_t::~global_scope_t()
{
  TRACE_DTOR(global_scope_t);

  // If memory verification is being performed (which can be very slow),
  // clean up everything by closing the session and deleting the session
  // object, and then shutting down the memory tracing subsystem.
  // Otherwise, let it all leak because we're about to exit anyway.
  IF_VERIFY() set_session_context(NULL);
}

void global_scope_t::read_init()
{
  if (HANDLED(init_file_)) {
    path init_file(HANDLER(init_file_).str());
    if (exists(init_file)) {
      TRACE_START(init, 1, "Read initialization file");

      ifstream init(init_file);

      if (session().read_journal(init_file) > 0 ||
	  session().journal->auto_entries.size() > 0 ||
	  session().journal->period_entries.size() > 0) {
	throw_(parse_error,
	       "Entries found in initialization file '" << init_file << "'");
      }

      TRACE_FINISH(init, 1);
    }
  }
}

void global_scope_t::read_journal_files()
{
  INFO_START(journal, "Read journal file");

  string master_account;
  if (session().HANDLED(account_))
    master_account = session().HANDLER(account_).str();

  std::size_t count = session().read_data(master_account);
  if (count == 0)
    throw_(parse_error, "Failed to locate any journal entries; "
	   "did you specify a valid file with -f?");

  INFO_FINISH(journal);

  INFO("Found " << count << " entries");
}

char * global_scope_t::prompt_string()
{
  static char prompt[32];
  std::size_t i;
  for (i = 0; i < report_stack.size(); i++)
    prompt[i] = ']';
  prompt[i++] = ' ';
  prompt[i]   = '\0';
  return prompt;
}

void global_scope_t::report_error(const std::exception& err)
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

void global_scope_t::execute_command(strings_list args, bool at_repl)
{
  session().set_flush_on_next_data_file(true);

  // Process the command verb, arguments and options
  args = read_command_arguments(report(), args);
  if (args.empty())
    return;

  strings_list::iterator arg  = args.begin();
  string		 verb = *arg++;

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
  bool	       is_precommand = false;
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
    normalize_report_options(verb);
  }

  // Create the output stream (it might be a file, the console or a PAGER
  // subprocess) and invoke the report command.  The output stream is closed
  // by the caller of this function.

  report().output_stream
    .initialize(report().HANDLED(output_) ?
		optional<path>(path(report().HANDLER(output_).str())) :
		optional<path>(),
		report().HANDLED(pager_) ?
		optional<path>(path(report().HANDLER(pager_).str())) :
		optional<path>());

  // Create an argument scope containing the report command's arguments, and
  // then invoke the command.  The bound scope causes lookups to happen
  // first in the global scope, and then in the report scope.

  call_scope_t command_args(bound_scope);
  for (strings_list::iterator i = arg; i != args.end(); i++)
    command_args.push_back(string_value(*i));

  INFO_START(command, "Finished executing command");
  command(command_args);
  INFO_FINISH(command);
}

int global_scope_t::execute_command_wrapper(strings_list args, bool at_repl)
{
  int status = 1;

  try {
    if (at_repl) push_report();
    execute_command(args, at_repl);
    if (at_repl) pop_report();

    // If we've reached this point, everything succeeded fine.  Ledger uses
    // exceptions to notify of error conditions, so if you're using gdb,
    // just type "catch throw" to find the source point of any error.
    status = 0;
  }
  catch (const std::exception& err) {
    if (at_repl) pop_report();
    report_error(err);
  }

  return status;
}

option_t<global_scope_t> * global_scope_t::lookup_option(const char * p)
{
  switch (*p) {
  case 'd':
    OPT(debug_);
    break;
  case 'i':
    OPT(init_file_);
    break;
  case 's':
    OPT(script_);
    break;
  case 't':
    OPT(trace_);
    break;
  case 'v':
    OPT_(verbose);
    else OPT(verify);
    else OPT(version);
    break;
  }
  return NULL;
}

expr_t::ptr_op_t global_scope_t::lookup(const string& name)
{
  const char * p = name.c_str();
  switch (*p) {
  case 'o':
    if (WANT_OPT()) { p += OPT_PREFIX_LEN;
      if (option_t<global_scope_t> * handler = lookup_option(p))
	return MAKE_OPT_HANDLER(global_scope_t, handler);
    }
    break;

  case 'p':
    if (WANT_PRECMD()) { p += PRECMD_PREFIX_LEN;
      switch (*p) {
      case 'p':
	if (is_eq(p, "push"))
	  MAKE_FUNCTOR(global_scope_t::push_command);
	else if (is_eq(p, "pop"))
	  MAKE_FUNCTOR(global_scope_t::pop_command);
	break;
      }
    }
    break;
  }

  // If you're wondering how symbols from report() will be found, it's
  // because of the bind_scope_t object in execute_command() below.
  return NULL;
}

void global_scope_t::read_environment_settings(char * envp[])
{
  TRACE_START(environment, 1, "Processed environment variables");

  process_environment(const_cast<const char **>(envp), "LEDGER_", report());

#if 1
  // These are here for backwards compatability, but are deprecated.

  if (const char * p = std::getenv("LEDGER")) {
    if (! std::getenv("LEDGER_FILE"))
      process_option("file", report(), p, "LEDGER");
  }
  if (const char * p = std::getenv("LEDGER_INIT")) {
    if (! std::getenv("LEDGER_INIT_FILE"))
      process_option("init-file", report(), p, "LEDGER_INIT");
  }
  if (const char * p = std::getenv("PRICE_HIST")) {
    if (! std::getenv("LEDGER_PRICEDB"))
      process_option("price-db", report(), p, "PRICE_HIST");
  }
  if (const char * p = std::getenv("PRICE_EXP"))
    process_option("price-exp", report(), p, "PRICE_EXP");
#endif

  TRACE_FINISH(environment, 1);
}

strings_list
global_scope_t::read_command_arguments(scope_t& scope, strings_list args)
{
  TRACE_START(arguments, 1, "Processed command-line arguments");

  strings_list remaining = process_arguments(args, scope);

  TRACE_FINISH(arguments, 1);

  return remaining;
}

void global_scope_t::normalize_session_options()
{
  INFO("Initialization file is " << HANDLER(init_file_).str());
  INFO("Price database is " << session().HANDLER(price_db_).str());

  foreach (const path& pathname, session().HANDLER(file_).data_files)
    INFO("Journal file is " << pathname.string());
}

function_t global_scope_t::look_for_precommand(scope_t&	     scope,
					       const string& verb)
{
  if (expr_t::ptr_op_t def = scope.lookup(string("precmd_") + verb))
    return def->as_function();
  else
    return function_t();
}

function_t global_scope_t::look_for_command(scope_t&	  scope,
					    const string& verb)
{
  if (expr_t::ptr_op_t def = scope.lookup(string("cmd_") + verb))
    return def->as_function();
  else
    return function_t();
}

void global_scope_t::normalize_report_options(const string& verb)
{
  // Patch up some of the reporting options based on what kind of
  // command it was.

  report_t& rep(report());

  // jww (2008-08-14): This code really needs to be rationalized away
  // for 3.0.

  if (verb == "print" || verb == "entry" || verb == "dump") {
    rep.HANDLER(related).on();
    rep.HANDLER(related_all).on();
  }
  else if (verb == "equity") {
    rep.HANDLER(subtotal).on();
  }
  else if (rep.HANDLED(related)) {
    if (verb[0] == 'r') {
      rep.HANDLER(invert).on();
    } else {
      rep.HANDLER(subtotal).on();
      rep.HANDLER(related_all).on();
    }
  }

  if (verb[0] != 'b' && verb[0] != 'r')
    rep.HANDLER(base).on();

  // Setup the default value for the display predicate

  if (! rep.HANDLED(display_)) {
    if (verb[0] == 'b') {
      if (! rep.HANDLED(empty))
	rep.append_display_predicate("total");
    }
    else if (verb == "equity") {
      // jww (2008-08-14): ???
      rep.append_display_predicate("amount_expr");
    }
    else if (verb[0] == 'r' && ! rep.HANDLED(empty)) {
      rep.append_display_predicate("amount");
    }
  }

  if (rep.HANDLED(period_) && ! rep.HANDLED(sort_all_))
    rep.HANDLER(sort_entries_).on();
}

void handle_debug_options(int argc, char * argv[])
{
  for (int i = 1; i < argc; i++) {
    if (argv[i][0] == '-') {
      if (std::strcmp(argv[i], "--verify") == 0) {
#if defined(VERIFY_ON)
	verify_enabled = true; // global in utils.h
#endif
      }
      else if (std::strcmp(argv[i], "--verbose") == 0 ||
	       std::strcmp(argv[i], "-v") == 0) {
#if defined(LOGGING_ON)
	_log_level = LOG_INFO; // global in utils.h
#endif
      }
      else if (i + 1 < argc && std::strcmp(argv[i], "--debug") == 0) {
#if defined(DEBUG_ON)
	_log_level    = LOG_DEBUG; // global in utils.h
	_log_category = argv[i + 1]; // global in utils.h
	i++;
#endif
      }
      else if (i + 1 < argc && std::strcmp(argv[i], "--trace") == 0) {
#if defined(TRACING_ON)
	_log_level   = LOG_TRACE; // global in utils.h
	try {
	  // global in utils.h
	  _trace_level = boost::lexical_cast<int>(argv[i + 1]);
	}
	catch (const boost::bad_lexical_cast& e) {
	  throw std::logic_error("Argument to --trace must be an integer");
	}
	i++;
#endif
      }
    }
  }
}

} // namespace ledger
