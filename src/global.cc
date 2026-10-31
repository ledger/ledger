/*
 * Copyright (c) 2003-2025, John Wiegley.  All rights reserved.
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

/**
 * @file   global.cc
 * @author John Wiegley
 *
 * @ingroup report
 *
 * @brief Application lifecycle: initialization, option processing, and
 *        command execution.
 *
 * This file implements the global_scope_t methods that drive Ledger from
 * start to finish.  The lifecycle is:
 *
 *   1. handle_debug_options() -- early scan for --debug/--trace/--verify
 *   2. global_scope_t constructor -- create session, read env + init file
 *   3. read_command_arguments() -- parse remaining CLI options
 *   4. execute_command() -- resolve verb, load journal, run handler
 *   5. destructor -- tear down (or leak, unless --verify is active)
 */

#include <system.hh>
#include <utility>

#include "global.h"
#if HAVE_BOOST_PYTHON
#include "pyinterp.h"
#else
#include "session.h"
#endif
#include "item.h"
#include "journal.h"
#include "pool.h"

namespace ledger {

/// Set by handle_debug_options when --args-only is present.
/// When true, the constructor skips environment variables and the init file,
/// and keeps the session from reading the default price database.
static bool args_only = false;

/// Path to the init file, captured early by handle_debug_options.
std::string _init_file;

/*--- Construction and Destruction ---*/

global_scope_t::global_scope_t(char** envp) {
  epoch = CURRENT_TIME();

  // Create the session object.  When Boost.Python is available, the session
  // is actually a python_interpreter_t that extends session_t with a Python
  // runtime, enabling Python-based value expressions and scripting.
#if HAVE_BOOST_PYTHON
  if (!python_session.get()) {
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
  report_stack.push_front(new report_t(*session_ptr));
  scope_t::default_scope = &report();
  scope_t::empty_scope = &empty_scope;

  // Read the user's options, in the following order:
  //
  //  1. environment variables (LEDGER_<option>)
  //  2. initialization file (~/.ledgerrc)
  //  3. command-line (--option or -o)
  //
  // Before processing command-line options, we must notify the session object
  // that such options are beginning, since options like -f cause a complete
  // override of files found anywhere else.
  if (!args_only) {
    session().set_flush_on_next_data_file(true);
    read_environment_settings(envp);
    session().set_flush_on_next_data_file(true);
    read_init();
  } else {
    session().use_default_price_db = false;
  }

  TRACE_CTOR(global_scope_t, "");
}

global_scope_t::~global_scope_t() {
  TRACE_DTOR(global_scope_t);

  // If memory verification is being performed (which can be very slow),
  // clean up everything by closing the session and deleting the session
  // object, and then shutting down the memory tracing subsystem.
  // Otherwise, let it all leak because we're about to exit anyway.
  IF_VERIFY() set_session_context(nullptr);

#if HAVE_BOOST_PYTHON
  python_session.reset();
#endif
}

/*--- Init File Processing ---*/

void global_scope_t::parse_init(const path& init_file, bool skip_options) {
  TRACE_START(init, 1, "Read initialization file");

  // The init file is parsed as a journal, but it must not contain any
  // transactions -- only option directives and account/commodity
  // declarations are permitted.  Automated and periodic transactions are
  // also forbidden because they would silently affect report output in
  // ways that are hard to debug.  It is parsed in the scope of the base
  // report, the only one at startup, whatever command reads it again.
  parse_context_stack_t parsing_context;
  parsing_context.push(init_file);
  parsing_context.get_current().journal = session().journal.get();
  parsing_context.get_current().scope = &report_stack.back();
  parsing_context.get_current().skip_options = skip_options;

  if (session().journal->read(parsing_context, NO_HASHES) > 0 ||
      session().journal->auto_xacts.size() > 0 || session().journal->period_xacts.size() > 0) {
    throw_(parse_error, _f("Transactions found in initialization file '%1%'") % init_file);
  }

  TRACE_FINISH(init, 1);
}

void global_scope_t::read_init() {
  // If specified on the command line, init_file_ is filled in
  // handle_debug_options.  If it was specified on the command line,
  // fail if the file doesn't exist.  If no init file was specified
  // on the command line, try the default locations but don't fail
  // if none is found.
  path init_file;
  if (HANDLED(init_file_)) {
    init_file = HANDLER(init_file_).str();
    if (!exists(init_file)) {
      throw_(parse_error, _f("Could not find specified init file %1%") % init_file);
    }
  } else {
    // In order, try to read the init file from:
    // - $XDG_CONFIG_HOME/ledger/ledgerrc
    // - $HOME/.config/ledger/ledgerrc
    // - $HOME/.ledgerrc
    // - ./.ledgerrc
    if (const char* xdg_config_home = std::getenv("XDG_CONFIG_HOME")) {
      init_file = (path(xdg_config_home) / "ledger" / "ledgerrc");
    }
    if (!exists(init_file)) {
      if (const char* home_var = std::getenv("HOME")) {
        init_file = (path(home_var) / ".config" / "ledger" / "ledgerrc");
        if (!exists(init_file)) {
          init_file = (path(home_var) / ".ledgerrc");
        }
      }
      if (!exists(init_file)) {
        init_file = ("./.ledgerrc");
      }
    }
  }
  if (exists(init_file)) {
    parse_init(init_file);
    parsed_init_file = init_file;
  }
}

/*--- REPL Support ---*/

string global_scope_t::prompt_string() {
  std::size_t depth = std::min(report_stack.size(), std::size_t(30));
  return string(depth, ']') + " ";
}

/*--- Error Reporting ---*/

void global_scope_t::report_error(const std::exception& err) {
  std::cout.flush(); // first display anything that was pending

  if (caught_signal == NONE_CAUGHT) {
    // Display any pending error context information
    const string context = error_context();
    if (!context.empty())
      std::cerr << context << '\n';

    std::cerr << _("Error: ") << err.what() << '\n';
  } else {
    caught_signal = NONE_CAUGHT;
  }
}

/*--- Command Execution ---*/

namespace {
#if HAVE_BOOST_PYTHON
/// Point Python's `journal`, which a python directive sets to the journal
/// being read, at @p journal, unless that name holds something else.
void point_python_at(journal_t* journal) {
  if (!python_session || !python_session->is_initialized)
    return;
  boost::python::dict& globals(python_session->main_module->module_globals);
  if (!globals.has_key("journal"))
    return;
  boost::python::object current = globals["journal"];
  if (boost::python::extract<journal_t*>(current).check())
    python_session->main_module->define_global("journal",
                                               boost::python::object(boost::python::ptr(journal)));
}
#endif

/// Thrown, in place of the parser's error_count, when a session could not
/// read its journal again for a command (see read_journal_again).  The
/// parser has reported the errors, and the session has its journal back.
struct reread_failed {
  std::size_t count; ///< The number of errors the parser reported
};

/// Whether @p option, as set for a command in a session, selects its date
/// mode, which the command line decides before it reads the journal: so not
/// if an option line in a data file set it as the session first read the
/// journal (@p from_file), and nothing has set it since.
bool selects_date_mode(const option_t<report_t>& option, const optional<string>& from_file) {
  return option && !(from_file && option.whence() == from_file);
}

/// Read @p session's journal again under the date mode @p aux_date, from the
/// data files of its latest reading: a -f given to a command since then
/// takes effect at the next reload, as it did before, and not here, where
/// only the date mode changes.  If the reading fails, put back the journal
/// the session had, with its prices and the parser's state, before passing
/// the failure on.
void read_journal_again(session_t& session, bool aux_date) {
  session_t::reread_t& reread(*session.reread);
  commodity_pool_t& pool(*commodity_pool_t::current_pool);

  const boost::shared_ptr<journal_t> journal(session.journal);
  commodity_history_t::price_changes_t prices(reread.prices);
  const bool journal_aux_date = reread.aux_date;
  const optional<int> year = year_directive_year;
  const optional<datetime_t> now = epoch;
  commodity_t* const commodity = pool.default_commodity;

  // The files are read as they were at startup, after -f had named them,
  // so that a --file line in a data file adds to them rather than emptying
  // the list being read; the command gets back -f as it had it.
  const std::list<path> files(session.HANDLER(file_).data_files);
  const bool flush = session.flush_on_next_data_file;
  auto put_back_files = [&] {
    session.HANDLER(file_).data_files = files;
    session.set_flush_on_next_data_file(flush);
  };

  auto put_back = [&] {
    pool.undo_price_changes(reread.prices);
    reread.prices = std::move(prices);
    pool.redo_price_changes(reread.prices);
    session.journal = journal;
    reread.aux_date = journal_aux_date;
    item_t::use_aux_date = journal_aux_date;
    year_directive_year = year;
    epoch = now;
    pool.default_commodity = commodity;
    put_back_files();
#if HAVE_BOOST_PYTHON
    point_python_at(journal.get());
#endif
  };

  item_t::use_aux_date = aux_date;
  session.HANDLER(file_).data_files = reread.files;
  session.set_flush_on_next_data_file(false);
  try {
    session.close_journal_files();
    session.read_journal_files();
  } catch (const error_count& errors) {
    put_back();
    throw reread_failed{errors.count};
  } catch (...) {
    put_back();
    throw;
  }
  put_back_files();
}
} // namespace

void global_scope_t::load_journal(bool at_repl) {
  if (at_repl && session().reread) {
    // The command line reads the journal under the date mode that the
    // command at hand selects, so a command that selects the other one has
    // the journal read again.
    const bool aux_date = selects_date_mode(report().HANDLER(aux_date), file_aux_date) &&
                          !selects_date_mode(report().HANDLER(primary_date), file_primary_date);
    if (aux_date != session().reread->aux_date)
      read_journal_again(session(), aux_date);
    return;
  }

  if (at_repl) {
    // A session keeps its journal from command to command and may read it
    // again (session_t::reread), each time from where this first reading
    // starts: with the year, the time and the default commodity as they are
    // now, before any data file has set them, and with the init file's
    // declarations but not its options, which have been in effect since
    // startup.  A --now given to the command at hand stands, as it would on
    // the command line.
    session_t::reread_t& reread(session().reread.emplace());
    reread.scope = &report_stack.back();
    reread.prepare = [this, year = year_directive_year, now = epoch,
                      commodity = commodity_pool_t::current_pool->default_commodity] {
      year_directive_year = year;
      if (!report().HANDLED(now_))
        epoch = now;
      commodity_pool_t::current_pool->default_commodity = commodity;
      if (parsed_init_file && exists(*parsed_init_file))
        parse_init(*parsed_init_file, true);
#if HAVE_BOOST_PYTHON
      point_python_at(session().journal.get());
#endif
    };
  }

  const bool aux_date_set = report().HANDLED(aux_date);
  const bool primary_date_set = report().HANDLED(primary_date);

  item_t::use_aux_date = aux_date_set && !primary_date_set;
  session().read_journal_files();

  if (at_repl) {
    // Option lines in the data files set this report's options, as they do
    // on the command line -- where, though, they take effect only as the
    // journal is read, too late to change the date mode it is read in.
    // Note the date options they set, so as not to count them for that, and
    // have later readings apply such lines to a copy of the report: here
    // they are in effect already.
    if (!aux_date_set && report().HANDLED(aux_date))
      file_aux_date = report().HANDLER(aux_date).whence();
    if (!primary_date_set && report().HANDLED(primary_date))
      file_primary_date = report().HANDLER(primary_date).whence();
    session().reread->option_scope = [this] {
      return std::unique_ptr<scope_t>(new report_t(report_stack.back()));
    };
  }
}

void global_scope_t::execute_command(strings_list args, bool at_repl) {
  session().set_flush_on_next_data_file(true);

  // Phase 1: Process the command verb, arguments and options.
  // At the REPL, each line is re-parsed for options; in batch mode
  // this was already done by read_command_arguments in main().
  if (at_repl) {
    args = read_command_arguments(report(), args);
    if (args.empty())
      return;
  }

  strings_list::iterator arg = args.begin();
  const string verb = *arg++; // NOLINT(bugprone-unused-local-non-trivial-variable)

  // Phase 2: Look for a precommand first, which is defined as any defined
  // function whose name starts with "ledger_precmd_".  The difference
  // between a precommand and a regular command is that precommands ignore
  // the journal data file completely, nor is the user's init file read.
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
  bool is_precommand = false;
  bind_scope_t bound_scope(*this, report());

  if (bool(command = look_for_precommand(bound_scope, verb)))
    is_precommand = true;

  // Phase 3: If it is not a pre-command, then parse the user's ledger
  // data at this time if not done already (i.e., if not at a REPL).
  // Then patch up the report options based on the command verb.

  if (!is_precommand) {
    load_journal(at_repl);

    report().normalize_options(verb);

    // Pre-fetch all commodity quotes in a single batch invocation
    // of the getquote script, rather than one-at-a-time during
    // report generation (issue #588).
    if (commodity_pool_t::current_pool->get_quotes)
      commodity_pool_t::current_pool->prefetch_quotes();

    if (!bool(command = look_for_command(bound_scope, verb)))
      throw_(std::logic_error, _f("Unrecognized command '%1%'") % verb);
  }

  // Phase 4: Create the output stream (it might be a file, the console
  // or a PAGER subprocess) and invoke the report command.  The output
  // stream is closed by the caller of this function.

  report().output_stream.initialize(
      report().HANDLED(output_) ? optional<path>(path(report().HANDLER(output_).str()))
                                : optional<path>(),
      report().HANDLED(pager_) ? optional<path>(path(report().HANDLER(pager_).str()))
                               : optional<path>());

  // Now that the output stream is initialized, report the options that will
  // participate in this report, if the user specified --options

  if (HANDLED(options))
    report_options(report(), report().output_stream);

  // Phase 5: Create an argument scope containing the report command's
  // arguments, and then invoke the command.  The bound scope causes
  // lookups to happen first in the global scope, and then in the report
  // scope.

  call_scope_t command_args(bound_scope);
  for (strings_list::iterator i = arg; i != args.end(); i++)
    command_args.push_back(string_value(*i));

  INFO_START(command, "Finished executing command");
  command(command_args);
  INFO_FINISH(command);
}

int global_scope_t::execute_command_wrapper(strings_list args, bool at_repl) {
  int status = 1;

  try {
    if (at_repl)
      push_report();
    execute_command(std::move(args), at_repl);
    if (at_repl)
      pop_report();

    // If we've reached this point, everything succeeded fine.  Ledger uses
    // exceptions to notify of error conditions, so if you're using gdb,
    // just type "catch throw" to find the source point of any error.
    status = 0;
  } catch (const std::exception& err) {
    if (at_repl) {
      // Failed reports can leave cached totals and display flags in the journal.
      session().journal->clear_xdata();
      pop_report();
    }
    report_error(err);
  } catch (const reread_failed& failure) {
    // The command line would have failed to read the journal for this
    // command, and the parser has said why; the session goes on with the
    // journal it had.
    pop_report();
    status = boost::numeric_cast<int>(failure.count);
  }

  return status;
}

/*--- Option Reporting ---*/

void global_scope_t::report_options(report_t& report, std::ostream& out) {
  out << "===============================================================================" << '\n';
  out << "[Global scope options]" << '\n';

  HANDLER(args_only).report(out);
  HANDLER(debug_).report(out);
  HANDLER(init_file_).report(out);
  HANDLER(script_).report(out);
  HANDLER(trace_).report(out);
  HANDLER(verbose).report(out);
  HANDLER(verify).report(out);
  HANDLER(verify_memory).report(out);

  out << '\n' << "[Session scope options]" << '\n';
  report.session.report_options(out);

  out << '\n' << "[Report scope options]" << '\n';
  report.report_options(out);
  out << "===============================================================================" << '\n';
}

/*--- Option Lookup ---*/

option_t<global_scope_t>* global_scope_t::lookup_option(const char* p) {
  switch (*p) {
  case 'a': // NOLINT(bugprone-branch-clone)
    OPT(args_only);
    break;
  case 'd':
    OPT(debug_);
    break;
  case 'h':
    OPT_(help);
    break;
  case 'i':
    OPT_(init_file_);
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
    else OPT(verify_memory);
    else OPT(version);
    break;
  default:
    break;
  }
  return nullptr;
}

/*--- Symbol Lookup ---*/

expr_t::ptr_op_t global_scope_t::lookup(const symbol_t::kind_t kind, const string& name) {
  switch (kind) {
  case symbol_t::FUNCTION: // NOLINT(bugprone-branch-clone)
    if (option_t<global_scope_t>* handler = lookup_option(name.c_str()))
      return MAKE_OPT_FUNCTOR(global_scope_t, handler);
    break;

  case symbol_t::OPTION:
    if (option_t<global_scope_t>* handler = lookup_option(name.c_str()))
      return MAKE_OPT_HANDLER(global_scope_t, handler);
    break;

  case symbol_t::PRECOMMAND: {
    const char* p = name.c_str();
    switch (*p) {
    case 'p':
      if (is_eq(p, "push"))
        return MAKE_FUNCTOR(global_scope_t::push_command);
      else if (is_eq(p, "pop"))
        return MAKE_FUNCTOR(global_scope_t::pop_command);
      break;
    default:
      break;
    }
  }
  default:
    break;
  }

  // If you're wondering how symbols from report() will be found, it's
  // because of the bind_scope_t object in execute_command() below.
  return nullptr;
}

/*--- Environment Variable Processing ---*/

void global_scope_t::read_environment_settings(char* envp[]) {
  TRACE_START(environment, 1, "Processed environment variables");

  // Process LEDGER_* environment variables as options, applying them
  // first to the global scope and then to the report scope.  This
  // allows env vars like LEDGER_FILE and LEDGER_PAGER to work.
  process_environment(const_cast<const char**>(envp), "LEDGER_", *this);
  process_environment(const_cast<const char**>(envp), "LEDGER_", report());

#if 1
  // These are here for backwards compatibility, but are deprecated.

  if (const char* p = std::getenv("LEDGER")) {
    if (!std::getenv("LEDGER_FILE"))
      process_option("environ", "file", report(), p, "LEDGER");
  }
  if (const char* p = std::getenv("LEDGER_INIT")) {
    if (!std::getenv("LEDGER_INIT_FILE"))
      process_option("environ", "init-file", *this, p, "LEDGER_INIT");
  }
  if (const char* p = std::getenv("PRICE_HIST")) {
    if (!std::getenv("LEDGER_PRICE_DB"))
      process_option("environ", "price-db", report(), p, "PRICE_HIST");
  }
  if (const char* p = std::getenv("PRICE_EXP")) {
    if (!std::getenv("LEDGER_PRICE_EXP"))
      process_option("environ", "price-exp", report(), p, "PRICE_EXP");
  }
#endif

  TRACE_FINISH(environment, 1);
}

/*--- Command-Line Argument Processing ---*/

strings_list global_scope_t::read_command_arguments(scope_t& scope, strings_list args) {
  TRACE_START(arguments, 1, "Processed command-line arguments");

  // Pre-scan for --now so that date-relative options (--begin, --end) that
  // appear earlier on the command line resolve against the correct epoch.
  // The --now handler is idempotent, so processing it again during the
  // normal pass is harmless.
  for (auto i = args.begin(); i != args.end(); ++i) {
    if ((*i)[0] != '-' || (*i)[1] != '-')
      continue;
    const char* arg = (*i).c_str() + 2;
    const char* value = nullptr;
    if (std::strcmp(arg, "now") == 0) {
      auto next = std::next(i);
      if (next != args.end())
        value = (*next).c_str();
    } else if (std::strncmp(arg, "now=", 4) == 0) {
      value = arg + 4;
    }
    if (value) {
      date_interval_t interval(value);
      if (optional<date_t> begin = interval.begin())
        ledger::epoch = datetime_t(*begin);
      break;
    }
  }

  strings_list remaining = process_arguments(std::move(args), scope);

  TRACE_FINISH(arguments, 1);

  return remaining;
}

void global_scope_t::normalize_session_options() {
  INFO("Initialization file is " << HANDLER(init_file_).str());
  INFO("Price database is " << session().HANDLER(price_db_).str());

  for (const path& pathname : session().HANDLER(file_).data_files)
    INFO("Journal file is " << pathname.string());
}

/*--- Command Verb Resolution ---*/

expr_t::func_t global_scope_t::look_for_precommand(scope_t& scope, const string& verb) {
  if (expr_t::ptr_op_t def = scope.lookup(symbol_t::PRECOMMAND, verb))
    return def->as_function();
  else
    return expr_t::func_t();
}

expr_t::func_t global_scope_t::look_for_command(scope_t& scope, const string& verb) {
  if (expr_t::ptr_op_t def = scope.lookup(symbol_t::COMMAND, verb))
    return def->as_function();
  else
    return expr_t::func_t();
}

/*--- Help ---*/

void global_scope_t::visit_man_page() const {
#if !defined(_WIN32) && !defined(__CYGWIN__)
  int pid = fork();
  if (pid < 0) {
    throw std::logic_error(_("Failed to fork child process"));
  } else if (pid == 0) { // child
    execlp("man", "man", "1", "ledger", NULL);

    // We should never, ever reach here
    perror("execlp: man");
    exit(1);
  }

  int status = -1;
  wait(&status);
#endif
  exit(0); // parent
}

/*--- Early Debug Option Handling ---*/

/**
 * @brief Scan argv for debug/trace options before any other initialization.
 *
 * This function runs before global_scope_t is constructed.  It handles
 * options that must take effect immediately:
 *
 *   --args-only      : skip init file, environment and ~/.pricedb (testing aid)
 *   --verify-memory  : enable memory tracing with debug logging
 *   --verify         : enable runtime verification checks
 *   --verbose / -v   : set log level to LOG_INFO
 *   --init-file PATH : override the default init file location
 *   --debug CATEGORY : enable debug logging for a specific category
 *   --trace LEVEL    : enable trace logging at a numeric verbosity level
 *
 * These are handled separately because the normal option processing
 * infrastructure depends on the session and scope objects, which have
 * not yet been created at this point.
 */
void handle_debug_options(int argc, char* argv[]) {
  for (int i = 1; i < argc; i++) {
    if (argv[i][0] == '-') {
      if (std::strcmp(argv[i], "--args-only") == 0) {
        args_only = true;
      } else if (std::strcmp(argv[i], "--verify-memory") == 0) {
#if VERIFY_ON
        verify_enabled = true;

        _log_level = LOG_DEBUG;
        _log_category = "memory\\.counts";
#endif
      } else if (std::strcmp(argv[i], "--verify") == 0) {
#if VERIFY_ON
        verify_enabled = true;
#endif
      } else if (std::strcmp(argv[i], "--verbose") == 0 || std::strcmp(argv[i], "-v") == 0) {
        _log_level = LOG_INFO;
      } else if (i + 1 < argc && std::strcmp(argv[i], "--init-file") == 0) {
        _init_file = argv[i + 1];
        i++;
      } else if (i + 1 < argc && std::strcmp(argv[i], "--debug") == 0) {
#if DEBUG_ON
        _log_level = LOG_DEBUG;
        _log_category = argv[i + 1];
        i++;
#endif
      } else if (i + 1 < argc && std::strcmp(argv[i], "--trace") == 0) {
#if TRACING_ON
        _log_level = LOG_TRACE;
        try {
          _trace_level = boost::lexical_cast<uint16_t>(argv[i + 1]);
        } catch (const boost::bad_lexical_cast&) {
          throw std::logic_error(_("Argument to --trace must be an integer"));
        }
        i++;
#endif
      }
    }
  }
}

} // namespace ledger
