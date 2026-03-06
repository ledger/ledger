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
 * @file   global.h
 * @author John Wiegley
 *
 * @ingroup report
 *
 * @brief The top-level application scope that owns the session and report
 *        stack.
 *
 * global_scope_t is the outermost scope in Ledger's three-tier scope
 * hierarchy: global > session > report.  It orchestrates the entire
 * application lifecycle:
 *
 *   1. Parsing debug/trace options that must be known before any other
 *      initialization (handle_debug_options).
 *   2. Constructing the session (which holds journal data) and the
 *      initial report object.
 *   3. Reading options from the environment, the init file, and then
 *      the command line -- in that precedence order.
 *   4. Dispatching a command verb (pre-command or regular command) and
 *      directing its output to the appropriate stream.
 *   5. Supporting a REPL loop by pushing/popping report objects on a
 *      stack so that each interactive command starts from a clean
 *      report state.
 *
 * A GUI embedding Ledger would create one session_t per open document
 * and one report_t per generated report, but for the CLI all three
 * tiers live inside this single global_scope_t.
 */
#pragma once

#include <ledger.hh>
#include "option.h"
#include "report.h"

namespace ledger {

class session_t;
class report_t;

/// Path to the init file, captured early by handle_debug_options so that
/// it is available before the global_scope_t constructor runs.
extern std::string _init_file;

/**
 * @brief The outermost scope that owns the application's session and
 *        report stack.
 *
 * global_scope_t inherits from scope_t so that option lookups can
 * chain: global -> report -> session -> symbol_scope.  It is
 * noncopyable because it owns the session via shared_ptr and manages
 * the report stack's lifetime.
 *
 * @see session_t, report_t
 */
class global_scope_t : public noncopyable, public scope_t {
  std::shared_ptr<session_t> session_ptr; ///< The single session for this invocation
  ptr_list<report_t> report_stack;        ///< Stack of report objects; front() is current
  empty_scope_t empty_scope;              ///< Sentinel scope used for empty lookups

public:
  /// Construct the global scope, reading environment and init-file options.
  global_scope_t(char** envp);
  ~global_scope_t() override;

  /// Release resources held by the topmost report without full destruction.
  /// Called at exit when memory verification is disabled, allowing the OS
  /// to reclaim everything at once rather than walking destructors.
  void quick_close() {
    if (!report_stack.empty())
      report_stack.front().quick_close();
  }

  string description() override { return _("global scope"); }

  /// Parse an initialization file, which may contain option directives
  /// and account/commodity declarations but must not contain transactions.
  void parse_init(const path& init_file);

  /// Locate and read the user's init file.  Searches, in order:
  /// --init-file path, $XDG_CONFIG_HOME/ledger/ledgerrc,
  /// ~/.config/ledger/ledgerrc, ~/.ledgerrc, ./.ledgerrc.
  void read_init();

  /// Process LEDGER_* environment variables as options.
  void read_environment_settings(char* envp[]);

  /// Parse command-line arguments, separating options from the command
  /// verb and its arguments.
  /// @return The remaining non-option arguments (verb + operands).
  strings_list read_command_arguments(scope_t& scope, strings_list args);

  /// Log informational messages about which files and databases are in use.
  void normalize_session_options();

  /// Search the scope chain for a pre-command matching @p verb.
  /// Pre-commands (e.g., "parse", "eval") bypass journal loading entirely.
  expr_t::func_t look_for_precommand(scope_t& scope, const string& verb);

  /// Search the scope chain for a regular command matching @p verb.
  /// Regular commands (e.g., "balance", "register") require journal data.
  expr_t::func_t look_for_command(scope_t& scope, const string& verb);

  /// Build the REPL prompt string, whose depth indicates the report
  /// stack size (e.g., "] " for depth 1).
  string prompt_string();

  /// @return A reference to the current session.
  session_t& session() { return *session_ptr.get(); }

  /// @return A reference to the topmost (current) report.
  report_t& report() { return report_stack.front(); }

  /// Push a new report onto the stack, copying the current report's state.
  /// Used at the start of each REPL command so that option changes are
  /// isolated to a single invocation.
  void push_report() {
    report_stack.push_front(new report_t(report_stack.front()));
    scope_t::default_scope = &report();
  }

  /// Pop the topmost report off the stack, restoring the previous one.
  /// The stack must always retain at least the "default report" created
  /// during construction.
  void pop_report() {
    assert(!report_stack.empty());
    report_stack.pop_front();

    // There should always be the "default report" waiting on the stack.
    assert(!report_stack.empty());
    scope_t::default_scope = &report();
  }

  /// Display an error message to stderr, including any accumulated
  /// error context, unless a signal was caught (in which case the
  /// signal is silently cleared).
  void report_error(const std::exception& err);

  /// Execute a single command: resolve the verb, load journal data if
  /// needed, open the output stream, and invoke the command handler.
  void execute_command(strings_list args, bool at_repl);

  /**
   * @brief Wrapper around execute_command that handles report stack
   *        management and exception reporting.
   *
   * @return \c true if a command was actually executed; otherwise, it probably
   *         just resulted in setting some options.
   */
  int execute_command_wrapper(strings_list args, bool at_repl);

  /// REPL pre-command: push a copy of the current report onto the stack
  /// at position 2 (below the active report whose output stream is open).
  value_t push_command(call_scope_t&) {
    // Make a copy at position 2, because the topmost report object has an
    // open output stream at this point.  We want it to get popped off as
    // soon as this command terminate so that the stream is closed cleanly.
    report_stack.insert(++report_stack.begin(), new report_t(report_stack.front()));
    return true;
  }

  /// REPL pre-command: pop the topmost report off the stack.
  value_t pop_command(call_scope_t&) {
    pop_report();
    return true;
  }

  /// Print the Ledger version string, including build type and optional
  /// feature support (GPG, Python), to the given output stream.
  void show_version_info(std::ostream& out) {
    out << "Ledger " << Ledger_VERSION_MAJOR << '.' << Ledger_VERSION_MINOR << '.'
        << Ledger_VERSION_PATCH;
    if (Ledger_VERSION_PRERELEASE != nullptr)
      out << Ledger_VERSION_PRERELEASE;
    if (std::strlen(Ledger_VERSION_DATE) > 0)
      out << '-' << Ledger_VERSION_DATE;
    if (std::strlen(Ledger_BUILD_TYPE) > 0)
      out << " (" << Ledger_BUILD_TYPE << ')';
    out << _(", the command-line accounting tool");
    out << _("\nwith");
#if !HAVE_GPGME
    out << _("out");
#endif
    out << _(" support for gpg encrypted journals and with");
#if !HAVE_BOOST_PYTHON
    out << _("out");
#endif
    out << _(" Python support");
    out << _("\n\nCopyright (c) 2003-2025, John Wiegley.  All rights reserved.\n\n\
This program is made available under the terms of the BSD Public License.\n\
See LICENSE file included with the distribution for details and disclaimer.");
    out << '\n';
  }

  /// Dump all active option values (global, session, report) to the output
  /// stream, triggered by the --options flag.
  void report_options(report_t& report, std::ostream& out);

  /// Resolve an option name to its handler, using first-character dispatch.
  option_t<global_scope_t>* lookup_option(const char* p);

  /// Scope lookup override: resolves functions, options, and the special
  /// "push"/"pop" pre-commands defined at the global level.
  expr_t::ptr_op_t lookup(const symbol_t::kind_t kind, const string& name) override;

  OPTION(global_scope_t, args_only); ///< Skip init file and environment processing
  OPTION(global_scope_t, debug_);    ///< Enable debug logging for a category

  /// Open the ledger(1) man page via fork/exec.
  void visit_man_page() const;

  OPTION_(global_scope_t, help, DO() { parent->visit_man_page(); }); // -h

  OPTION_CTOR(
      global_scope_t, init_file_, // -i
      CTOR(global_scope_t, init_file_) {
        if (!_init_file.empty())
          // _init_file is filled during handle_debug_options
          on(none, _init_file);
      });

  OPTION(global_scope_t, options);       ///< Dump all options before command output
  OPTION(global_scope_t, script_);       ///< Run commands from a script file
  OPTION(global_scope_t, trace_);        ///< Set trace verbosity level
  OPTION(global_scope_t, verbose);       ///< Enable informational log messages
  OPTION(global_scope_t, verify);        ///< Enable runtime verification checks
  OPTION(global_scope_t, verify_memory); ///< Enable memory allocation tracing

  OPTION_(
      global_scope_t, version, DO() { // -v
        parent->show_version_info(std::cout);
        throw error_count(0, ""); // exit immediately
      });
};

/**
 * @brief Early command-line scan for debug and tracing options.
 *
 * This function must be called before global_scope_t is constructed because
 * the options it handles (--verify, --verbose, --debug, --trace, --init-file,
 * --args-only) control how the runtime environment is set up: logging levels,
 * memory tracing, and the choice of init file.  These cannot wait for normal
 * option processing because that infrastructure depends on the very things
 * being configured here.
 */
void handle_debug_options(int argc, char* argv[]);

} // namespace ledger
