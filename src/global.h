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

/**
 * @file   global.h
 * @author John Wiegley
 *
 * @brief Contains the top-level functions used by main.cc
 */
#ifndef _GLOBAL_H
#define _GLOBAL_H

#include "option.h"
#include "report.h"

namespace ledger {

class session_t;
class report_t;

extern std::string       _init_file;

class global_scope_t : public noncopyable, public scope_t
{
  shared_ptr<session_t> session_ptr;
  ptr_list<report_t>    report_stack;
  empty_scope_t         empty_scope;

public:
  global_scope_t(char ** envp);
  ~global_scope_t();

  void quick_close() {
    if (! report_stack.empty())
      report_stack.front().quick_close();
  }

  virtual string description() {
    return _("global scope");
  }

  void           parse_init(path init_file);
  void           read_init();
  void           read_environment_settings(char * envp[]);
  strings_list   read_command_arguments(scope_t& scope, strings_list args);
  void           normalize_session_options();
  expr_t::func_t look_for_precommand(scope_t& scope, const string& verb);
  expr_t::func_t look_for_command(scope_t& scope, const string& verb);

  char * prompt_string();

  session_t& session() {
    return *session_ptr.get();
  }
  report_t& report() {
    return report_stack.front();
  }

  void push_report() {
    report_stack.push_front(new report_t(report_stack.front()));
    scope_t::default_scope = &report();
  }
  void pop_report() {
    assert(! report_stack.empty());
    report_stack.pop_front();

    // There should always be the "default report" waiting on the stack.
    assert(! report_stack.empty());
    scope_t::default_scope = &report();
  }

  void report_error(const std::exception& err);

  void execute_command(strings_list args, bool at_repl);
  /**
   * @return \c true if a command was actually executed; otherwise, it probably
   *         just resulted in setting some options.
   */
  int  execute_command_wrapper(strings_list args, bool at_repl);

  value_t push_command(call_scope_t&) {
    // Make a copy at position 2, because the topmost report object has an
    // open output stream at this point.  We want it to get popped off as
    // soon as this command terminate so that the stream is closed cleanly.
    report_stack.insert(++report_stack.begin(),
                        new report_t(report_stack.front()));
    return true;
  }
  value_t pop_command(call_scope_t&) {
    pop_report();
    return true;
  }

  void show_version_info(std::ostream& out) {
    out <<
      "Ledger " << Ledger_VERSION_MAJOR << '.' << Ledger_VERSION_MINOR << '.'
                << Ledger_VERSION_PATCH;
    if (Ledger_VERSION_PRERELEASE != 0)
      out << Ledger_VERSION_PRERELEASE;
    if (Ledger_VERSION_DATE != 0)
      out << '-' << Ledger_VERSION_DATE;
    out << _(", the command-line accounting tool");
    out <<
      _("\n\nCopyright (c) 2003-2018, John Wiegley.  All rights reserved.\n\n\
This program is made available under the terms of the BSD Public License.\n\
See LICENSE file included with the distribution for details and disclaimer.");
    out << std::endl;
  }

  void report_options(report_t& report, std::ostream& out);

  option_t<global_scope_t> * lookup_option(const char * p);

  virtual expr_t::ptr_op_t lookup(const symbol_t::kind_t kind,
                                  const string& name);

  OPTION(global_scope_t, args_only);
  OPTION(global_scope_t, debug_);

  void visit_man_page() const;

  OPTION_(global_scope_t, help,      DO() { parent->visit_man_page(); }); // -h

  OPTION__
  (global_scope_t, init_file_, // -i
   CTOR(global_scope_t, init_file_) {
    if (!_init_file.empty())
      // _init_file is filled during handle_debug_options
      on(none, _init_file);
   });

  OPTION(global_scope_t, options);
  OPTION(global_scope_t, script_);
  OPTION(global_scope_t, trace_);
  OPTION(global_scope_t, verbose);
  OPTION(global_scope_t, verify);
  OPTION(global_scope_t, verify_memory);

  OPTION_(global_scope_t, version, DO() { // -v
      parent->show_version_info(std::cout);
      throw error_count(0, "");     // exit immediately
    });
};

void handle_debug_options(int argc, char * argv[]);

} // namespace ledger

#endif // _GLOBAL_H
