/*
 * Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
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

#ifndef _SESSION_H
#define _SESSION_H

#include "valexpr.h"
#include "journal.h"
#include "parser.h"

namespace ledger {

class session_t : public expr::symbol_scope_t
{
 public:
  static session_t * current;

  path		 data_file;
  optional<path> init_file;
  optional<path> cache_file;
  optional<path> price_db;

  string register_format;
  string wide_register_format;
  string print_format;
  string balance_format;
  string equity_format;
  string plot_amount_format;
  string plot_total_format;
  string write_hdr_format;
  string write_xact_format;
  string prices_format;
  string pricesdb_format;

  unsigned long pricing_leeway;

  bool download_quotes;
  bool use_cache;
  bool cache_dirty;

  datetime_t now;

#if 0
  elision_style_t elision_style;
#endif
  int abbrev_length;

  bool ansi_codes;
  bool ansi_invert;

  ptr_list<journal_t> journals;
  ptr_list<parser_t>  parsers;

  account_t *	       master;
  mutable accounts_map accounts_cache;

  session_t();

  virtual ~session_t() {
    TRACE_DTOR(session_t);

    assert(master);
    checked_delete(master);
  }

  journal_t * create_journal() {
    journal_t * journal = new journal_t(this);
    journals.push_back(journal);
    return journal;
  }
  void close_journal(journal_t * journal) {
    for (ptr_list<journal_t>::iterator i = journals.begin();
	 i != journals.end();
	 i++)
      if (&*i == journal) {
	journals.erase(i);
	return;
      }
    assert(false);
    checked_delete(journal);
  }

  std::size_t read_journal(journal_t&	 journal,
			   std::istream& in,
			   const path&	 pathname,
			   account_t *   master = NULL);
  std::size_t read_journal(journal_t&	 journal,
			   const path&	 pathname,
			   account_t *   master = NULL);

  void read_init();

  std::size_t read_data(journal_t&    journal,
			const string& master_account = "");

  void register_parser(parser_t * parser) {
    parsers.push_back(parser);
  }
  void unregister_parser(parser_t * parser) {
    for (ptr_list<parser_t>::iterator i = parsers.begin();
	 i != parsers.end();
	 i++)
      if (&*i == parser) {
	parsers.erase(i);
	return;
      }
    assert(false);
    checked_delete(parser);
  }

  //
  // Dealing with accounts
  //

  void add_account(account_t * acct) {
    master->add_account(acct);
  }
  bool remove_account(account_t * acct) {
    return master->remove_account(acct);
  }

  account_t * find_account(const string& name, bool auto_create = true) {
    accounts_map::iterator c = accounts_cache.find(name);
    if (c != accounts_cache.end())
      return (*c).second;

    account_t * account = master->find_account(name, auto_create);
    accounts_cache.insert(accounts_map::value_type(name, account));
    return account;
  }
  account_t * find_account_re(const string& regexp);

  //
  // Scope members
  //

  virtual expr::ptr_op_t lookup(const string& name);

  //
  // Help options
  //

  value_t option_version(expr::scope_t&) {
    std::cout << "Ledger " << ledger::version << ", the command-line accounting tool";
    std::cout << "\n\nCopyright (c) 2003-2008, John Wiegley.  All rights reserved.\n\n\
This program is made available under the terms of the BSD Public License.\n\
See LICENSE file included with the distribution for details and disclaimer.\n";
    std::cout << "\n(modules: gmp, pcre";
#if defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)
    std::cout << ", xml";
#endif
#ifdef HAVE_LIBOFX
    std::cout << ", ofx";
#endif
    std::cout << ")\n";
    return NULL_VALUE;
  }

  //
  // Debug options
  //

  value_t option_trace_(expr::scope_t& locals) {
    return NULL_VALUE;
  }
  value_t option_debug_(expr::scope_t& locals) {
    return NULL_VALUE;
  }

  value_t option_verify(expr::scope_t&) {
    return NULL_VALUE;
  }
  value_t option_verbose(expr::scope_t&) {
#if defined(LOGGING_ON)
    if (_log_level < LOG_INFO)
      _log_level = LOG_INFO;
#endif
    return NULL_VALUE;
  }

  //
  // Option handlers
  //

  value_t option_file_(expr::call_scope_t& args) {
    assert(args.size() == 1);
    data_file = args[0].as_string();
    return NULL_VALUE;
  }

#if 0
#if defined(USE_BOOST_PYTHON)
  value_t option_import_(expr::call_scope_t& args) {
    python_import(optarg);
    return NULL_VALUE;
  }
  value_t option_import_stdin(expr::call_scope_t& args) {
    python_eval(std::cin, PY_EVAL_MULTI);
    return NULL_VALUE;
  }
#endif
#endif
};

/**
 * This sets the current session context, transferring all static
 * globals to point at the data structures related to this session.
 * Although Ledger itself is not thread-safe, by locking, switching
 * session context, then unlocking after the operation is done,
 * multiple threads can sequentially make use of the library.  Thus, a
 * session_t maintains all of the information relating to a single
 * usage of the Ledger library.
 */
void set_session_context(session_t * session = NULL);

} // namespace ledger

#endif // _SESSION_H
