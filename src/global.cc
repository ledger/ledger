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

#include <system.hh>

#include "global.h"
#if defined(HAVE_BOOST_PYTHON)
#include "pyinterp.h"
#else
#include "session.h"
#endif
#include "item.h"
#include "journal.h"
#include "pool.h"

namespace ledger {

static bool args_only = false;

global_scope_t::global_scope_t(char ** envp)
{
  TRACE_CTOR(global_scope_t, "");

#if defined(HAVE_BOOST_PYTHON)
  if (! python_session.get()) {
    python_session.reset(new ledger::python_interpreter_t);
    session_ptr = python_session;
  }
#else
  session_ptr.reset(new session_t);
#endif

  set_session_context(session_ptr.get());

  // Create the report object, which maintains state relating to each
  // command invocation.  Because we're running from main(), the
  // distinction between session and report doesn't really matter, but if
  // a GUI were calling into Ledger it would have one session object per
  // open document, with a separate report_t object for each report it
  // generated.
  report_stack.push_front(new report_t(session()));
  scope_t::default_scope = &report();

  // Read the user's options, in the following order:
  //
  //  1. environment variables (LEDGER_<option>)
  //  2. initialization file (~/.ledgerrc)
  //  3. command-line (--option or -o)
  //
  // Before processing command-line options, we must notify the session object
  // that such options are beginning, since options like -f cause a complete
  // override of files found anywhere else.
  if (! args_only) {
    session().set_flush_on_next_data_file(true);
    read_environment_settings(envp);
    session().set_flush_on_next_data_file(true);
    read_init();
  } else {
    session().HANDLER(price_db_).off();
  }
}

global_scope_t::~global_scope_t()
{
  TRACE_DTOR(global_scope_t);

  // If memory verification is being performed (which can be very slow),
  // clean up everything by closing the session and deleting the session
  // object, and then shutting down the memory tracing subsystem.
  // Otherwise, let it all leak because we're about to exit anyway.
  IF_VERIFY() set_session_context(NULL);

#if defined(HAVE_BOOST_PYTHON)
  python_session.reset();
#endif
}

void global_scope_t::read_init()
{
  if (HANDLED(init_file_)) {
    path init_file(HANDLER(init_file_).str());
    if (exists(init_file)) {
      TRACE_START(init, 1, "Read initialization file");

      ifstream init(init_file);

      if (session().journal->read(init_file, NULL, &report()) > 0 ||
	  session().journal->auto_xacts.size() > 0 ||
	  session().journal->period_xacts.size() > 0) {
	throw_(parse_error, _("Transactions found in initialization file '%1'")
	       << init_file);
      }

      TRACE_FINISH(init, 1);
    }
  }
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

    std::cerr << _("Error: ") << err.what() << std::endl;
  } else {
    caught_signal = NONE_CAUGHT;
  }
}

void global_scope_t::execute_command(strings_list args, bool at_repl)
{
  session().set_flush_on_next_data_file(true);

  // Process the command verb, arguments and options
  if (at_repl) {
    args = read_command_arguments(report(), args);
    if (args.empty())
      return;
  }

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

  expr_t::func_t command;
  bool		 is_precommand = false;
  bind_scope_t	 bound_scope(*this, report());

  if (bool(command = look_for_precommand(bound_scope, verb)))
    is_precommand = true;

  // If it is not a pre-command, then parse the user's ledger data at this
  // time if not done alreday (i.e., if not at a REPL).  Then patch up the
  // report options based on the command verb.

  if (! is_precommand) {
    if (! at_repl)
      session().read_journal_files();

    normalize_report_options(verb);

    if (! bool(command = look_for_command(bound_scope, verb)))
      throw_(std::logic_error, _("Unrecognized command '%1'") << verb);
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

  // Now that the output stream is initialized, report the options that will
  // participate in this report, if the user specified --options

  if (HANDLED(options))
    report_options(report(), report().output_stream);

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

void global_scope_t::report_options(report_t& report, std::ostream& out)
{
  out << "==============================================================================="
      << std::endl;
  out << "[Global scope options]" << std::endl;

  HANDLER(args_only).report(out);
  HANDLER(debug_).report(out);
  HANDLER(init_file_).report(out);
  HANDLER(script_).report(out);
  HANDLER(trace_).report(out);
  HANDLER(verbose).report(out);
  HANDLER(verify).report(out);

  out << std::endl << "[Session scope options]" << std::endl;
  report.session.report_options(out);

  out << std::endl << "[Report scope options]" << std::endl;
  report.report_options(out);
  out << "==============================================================================="
      << std::endl;
}

option_t<global_scope_t> * global_scope_t::lookup_option(const char * p)
{
  switch (*p) {
  case 'a':
    OPT(args_only);
    break;
  case 'd':
    OPT(debug_);
    break;
  case 'f':
    OPT(full_help);
    break;
  case 'h':
    OPT_(help);
    else OPT(help_calc);
    else OPT(help_comm);
    else OPT(help_disp);
    break;
  case 'i':
    OPT(init_file_);
    break;
  case 'o':
    OPT(options);
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

expr_t::ptr_op_t global_scope_t::lookup(const symbol_t::kind_t kind,
					const string& name)
{
  switch (kind) {
  case symbol_t::FUNCTION:
    if (option_t<global_scope_t> * handler = lookup_option(name.c_str()))
      return MAKE_OPT_FUNCTOR(global_scope_t, handler);
    break;

  case symbol_t::OPTION:
    if (option_t<global_scope_t> * handler = lookup_option(name.c_str()))
      return MAKE_OPT_HANDLER(global_scope_t, handler);
    break;

  case symbol_t::PRECOMMAND: {
    const char * p = name.c_str();
    switch (*p) {
    case 'p':
      if (is_eq(p, "push"))
	return MAKE_FUNCTOR(global_scope_t::push_command);
      else if (is_eq(p, "pop"))
	return MAKE_FUNCTOR(global_scope_t::pop_command);
      break;
    }
  }
  default:
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
      process_option("environ", "file", report(), p, "LEDGER");
  }
  if (const char * p = std::getenv("LEDGER_INIT")) {
    if (! std::getenv("LEDGER_INIT_FILE"))
      process_option("environ", "init-file", report(), p, "LEDGER_INIT");
  }
  if (const char * p = std::getenv("PRICE_HIST")) {
    if (! std::getenv("LEDGER_PRICEDB"))
      process_option("environ", "price-db", report(), p, "PRICE_HIST");
  }
  if (const char * p = std::getenv("PRICE_EXP"))
    process_option("environ", "price-exp", report(), p, "PRICE_EXP");
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

expr_t::func_t global_scope_t::look_for_precommand(scope_t&	 scope,
						   const string& verb)
{
  if (expr_t::ptr_op_t def = scope.lookup(symbol_t::PRECOMMAND, verb))
    return def->as_function();
  else
    return expr_t::func_t();
}

expr_t::func_t global_scope_t::look_for_command(scope_t&      scope,
						const string& verb)
{
  if (expr_t::ptr_op_t def = scope.lookup(symbol_t::COMMAND, verb))
    return def->as_function();
  else
    return expr_t::func_t();
}

void global_scope_t::normalize_report_options(const string& verb)
{
  // Patch up some of the reporting options based on what kind of
  // command it was.

  report_t& rep(report());

#ifdef HAVE_ISATTY
  if (! rep.HANDLED(force_color)) {
    if (! rep.HANDLED(no_color) && isatty(STDOUT_FILENO))
      rep.HANDLER(color).on_only(string("?normalize"));
    if (rep.HANDLED(color) && ! isatty(STDOUT_FILENO))
      rep.HANDLER(color).off();
  }
  if (! rep.HANDLED(force_pager)) {
    if (rep.HANDLED(pager_) && ! isatty(STDOUT_FILENO))
      rep.HANDLER(pager_).off();
  }
#endif

  item_t::use_effective_date = (rep.HANDLED(effective) &&
				! rep.HANDLED(actual_dates));

  rep.session.journal->commodity_pool->keep_base  = rep.HANDLED(base);
  rep.session.journal->commodity_pool->get_quotes = rep.session.HANDLED(download);

  if (rep.session.HANDLED(price_exp_))
    rep.session.journal->commodity_pool->quote_leeway =
      rep.session.HANDLER(price_exp_).value.as_long();

  if (rep.session.HANDLED(price_db_))
    rep.session.journal->commodity_pool->price_db =
      rep.session.HANDLER(price_db_).str();
  else
    rep.session.journal->commodity_pool->price_db = none;

  if (rep.HANDLED(date_format_))
    set_date_format(rep.HANDLER(date_format_).str().c_str());
  if (rep.HANDLED(datetime_format_))
    set_datetime_format(rep.HANDLER(datetime_format_).str().c_str());
  if (rep.HANDLED(start_of_week_)) {
    if (optional<date_time::weekdays> weekday =
	string_to_day_of_week(rep.HANDLER(start_of_week_).str()))
      start_of_week = *weekday;
  }

  if (verb == "print" || verb == "xact" || verb == "dump") {
    rep.HANDLER(related).on_only(string("?normalize"));
    rep.HANDLER(related_all).on_only(string("?normalize"));
  }
  else if (verb == "equity") {
    rep.HANDLER(equity).on_only(string("?normalize"));
  }

  if (verb == "print")
    rep.HANDLER(limit_).on(string("?normalize"), "actual");

  if (! rep.HANDLED(empty))
    rep.HANDLER(display_).on(string("?normalize"), "amount|(!post&total)");

  if (verb[0] != 'b' && verb[0] != 'r')
    rep.HANDLER(base).on_only(string("?normalize"));

  // If a time period was specified with -p, check whether it also gave a
  // begin and/or end to the report period (though these can be overridden
  // using -b or -e).  Then, if no _duration_ was specified (such as monthly),
  // then ignore the period since the begin/end are the only interesting
  // details.
  if (rep.HANDLED(period_)) {
    if (! rep.HANDLED(sort_all_))
      rep.HANDLER(sort_xacts_).on_only(string("?normalize"));

    date_interval_t interval(rep.HANDLER(period_).str());

    if (! rep.HANDLED(begin_) && interval.start) {
      string predicate =
	"date>=[" + to_iso_extended_string(*interval.start) + "]";
      rep.HANDLER(limit_).on(string("?normalize"), predicate);
    }
    if (! rep.HANDLED(end_) && interval.end) {
      string predicate =
	"date<[" + to_iso_extended_string(*interval.end) + "]";
      rep.HANDLER(limit_).on(string("?normalize"), predicate);
    }

    if (! interval.duration)
      rep.HANDLER(period_).off();
  }

  // If -j or -J were specified, set the appropriate format string now so as
  // to avoid option ordering issues were we to have done it during the
  // initial parsing of the options.
  if (rep.HANDLED(amount_data)) {
    rep.HANDLER(format_)
      .on_with(string("?normalize"), rep.HANDLER(plot_amount_format_).value);
  }
  else if (rep.HANDLED(total_data)) {
    rep.HANDLER(format_)
      .on_with(string("?normalize"), rep.HANDLER(plot_total_format_).value);
  }

  // If the --exchange (-X) option was used, parse out any final price
  // settings that may be there.
  if (rep.HANDLED(exchange_) &&
      rep.HANDLER(exchange_).str().find('=') != string::npos) {
    value_t(0L).exchange_commodities(rep.HANDLER(exchange_).str(), true,
				     rep.terminus);
  }

  long cols = 0;
  if (rep.HANDLED(columns_))
    cols = rep.HANDLER(columns_).value.to_long();
  else if (const char * columns = std::getenv("COLUMNS"))
    cols = lexical_cast<long>(columns);
  else
    cols = 80L;

  if (cols > 0) {
    DEBUG("auto.columns", "cols = " << cols);

    if (! rep.HANDLER(date_width_).specified)
      rep.HANDLER(date_width_)
	.on_with(none, static_cast<long>(format_date(CURRENT_DATE(),
						     FMT_PRINTED).length()));

    long date_width    = rep.HANDLER(date_width_).value.to_long();
    long payee_width   = (rep.HANDLER(payee_width_).specified ?
			  rep.HANDLER(payee_width_).value.to_long() :
			  int(double(cols) * 0.263157));
    long account_width = (rep.HANDLER(account_width_).specified ?
			  rep.HANDLER(account_width_).value.to_long() :
			  int(double(cols) * 0.302631));
    long amount_width  = (rep.HANDLER(amount_width_).specified ?
			  rep.HANDLER(amount_width_).value.to_long() :
			  int(double(cols) * 0.157894));
    long total_width   = (rep.HANDLER(total_width_).specified ?
			  rep.HANDLER(total_width_).value.to_long() :
			  amount_width);

    DEBUG("auto.columns", "date_width	 = " << date_width);
    DEBUG("auto.columns", "payee_width	 = " << payee_width);
    DEBUG("auto.columns", "account_width = " << account_width);
    DEBUG("auto.columns", "amount_width	 = " << amount_width);
    DEBUG("auto.columns", "total_width	 = " << total_width);

    if (! rep.HANDLER(date_width_).specified &&
	! rep.HANDLER(payee_width_).specified &&
	! rep.HANDLER(account_width_).specified &&
	! rep.HANDLER(amount_width_).specified &&
	! rep.HANDLER(total_width_).specified) {
      long total = (4 /* the spaces between */ + date_width + payee_width +
		    account_width + amount_width + total_width);
      if (total > cols) {
	DEBUG("auto.columns", "adjusting account down");
	account_width -= total - cols;
	DEBUG("auto.columns", "account_width now = " << account_width);
      }
    }

    if (! rep.HANDLER(date_width_).specified)
      rep.HANDLER(date_width_).on_with(string("?normalize"), date_width);
    if (! rep.HANDLER(payee_width_).specified)
      rep.HANDLER(payee_width_).on_with(string("?normalize"), payee_width);
    if (! rep.HANDLER(account_width_).specified)
      rep.HANDLER(account_width_).on_with(string("?normalize"), account_width);
    if (! rep.HANDLER(amount_width_).specified)
      rep.HANDLER(amount_width_).on_with(string("?normalize"), amount_width);
    if (! rep.HANDLER(total_width_).specified)
      rep.HANDLER(total_width_).on_with(string("?normalize"), total_width);
  }
}

void global_scope_t::visit_man_page() const
{
  int pid = fork();
  if (pid < 0) {
    throw std::logic_error(_("Failed to fork child process"));
  }
  else if (pid == 0) {	// child
    execlp("man", "man", "1", "ledger", NULL);

    // We should never, ever reach here
    perror("execlp: man");
    exit(1);
  }

  int status = -1;
  wait(&status);
  exit(0);			// parent
}

void handle_debug_options(int argc, char * argv[])
{
  for (int i = 1; i < argc; i++) {
    if (argv[i][0] == '-') {
      if (std::strcmp(argv[i], "--args-only") == 0) {
	args_only = true;
      }
      else if (std::strcmp(argv[i], "--verify") == 0) {
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
	  _trace_level = boost::lexical_cast<uint8_t>(argv[i + 1]);
	}
	catch (const boost::bad_lexical_cast& e) {
	  throw std::logic_error(_("Argument to --trace must be an integer"));
	}
	i++;
#endif
      }
    }
  }
}

} // namespace ledger
