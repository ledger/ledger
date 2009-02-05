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

/**
 * @defgroup report Reporting
 */

/**
 * @file   session.h
 * @author John Wiegley
 *
 * @ingroup report
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _SESSION_H
#define _SESSION_H

#include "scope.h"
#include "journal.h"
#include "account.h"
#include "format.h"

namespace ledger {

/**
 * @brief Brief
 *
 * Long.
 */
class session_t : public noncopyable, public scope_t
{
  friend void set_session_context(session_t * session);

public:
  scope_t *                     global_scope;

  std::list<path>		data_files;
  bool                          next_data_file_from_command_line;
  bool                          saw_data_file_from_command_line;
  optional<path>		init_file;
  optional<path>		cache_file;
  optional<path>	        price_db;
  optional<path>                pager_path;
  bool                          next_price_db_from_command_line;
  bool                          saw_price_db_from_command_line;

  string			register_format;
  string			wide_register_format;
  string			print_format;
  string			balance_format;
  string			equity_format;
  string			plot_amount_format;
  string			plot_total_format;
  string			write_hdr_format;
  string			write_xact_format;
  string			prices_format;
  string			pricesdb_format;

  std::size_t			pricing_leeway;
  int				current_year;

  bool				download_quotes;
  bool				use_cache;
  bool				cache_dirty;

  format_t::elision_style_t	elision_style;
  int				abbrev_length;

  bool				ansi_codes;
  bool				ansi_invert;

  shared_ptr<commodity_pool_t>	commodity_pool;
  ptr_list<journal_t::parser_t> parsers;
  ptr_list<journal_t>		journals;
  scoped_ptr<account_t>		master;

  session_t();
  virtual ~session_t();

  void now_at_command_line(const bool truth) {
    next_data_file_from_command_line = truth;
    next_price_db_from_command_line  = truth;
  }

  journal_t * create_journal() {
    journal_t * journal = new journal_t(master.get());
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

  void register_parser(journal_t::parser_t * parser) {
    parsers.push_back(parser);
  }
  void unregister_parser(journal_t::parser_t * parser) {
    for (ptr_list<journal_t::parser_t>::iterator i = parsers.begin();
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

  void clean_accounts();

  void clean_xacts();
  void clean_xacts(entry_t& entry);

  void clean_all() {
    clean_xacts();
    clean_accounts();
  }

  //
  // Scope members
  //

  virtual expr_t::ptr_op_t lookup(const string& name);

  //
  // Help options
  //

  value_t option_version(scope_t&) {
    std::cout << "Ledger " << ledger::version << ", the command-line accounting tool";
    std::cout << "\n\nCopyright (c) 2003-2009, John Wiegley.  All rights reserved.\n\n\
This program is made available under the terms of the BSD Public License.\n\
See LICENSE file included with the distribution for details and disclaimer.";
    std::cout << std::endl;
    return NULL_VALUE;
  }

  //
  // Debug options
  //

  value_t option_trace_(scope_t&) {
    return NULL_VALUE;
  }
  value_t option_debug_(scope_t&) {
    return NULL_VALUE;
  }
  value_t option_verify(scope_t&) {
    return NULL_VALUE;
  }

  value_t option_verbose(scope_t&) {
#if defined(LOGGING_ON)
    if (_log_level < LOG_INFO)
      _log_level = LOG_INFO;
#endif
    return NULL_VALUE;
  }

  //
  // Option handlers
  //

  value_t option_file_(call_scope_t& args) { // f
    assert(args.size() == 1);
    if (next_data_file_from_command_line &&
	! saw_data_file_from_command_line) {
      data_files.clear();
      use_cache = false;
      saw_data_file_from_command_line = true;
    }
    data_files.push_back(args[0].as_string());
    return true;
  }

  value_t option_pager_(call_scope_t& args) { // :
    pager_path = args[0].as_string();
    return true;
  }
};

/**
 * Set the current session context, transferring all static globals to point
 * at the data structures related to this session.  Although Ledger itself is
 * not thread-safe, by locking, switching session context, then unlocking
 * after an operation is done, multiple threads can sequentially make use of
 * the library.  Thus, a session_t maintains all of the information relating
 * to a single usage of the Ledger library.
 */
void set_session_context(session_t * session);

} // namespace ledger

#endif // _SESSION_H
