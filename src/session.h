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
 * @defgroup report Reporting
 */

/**
 * @file   session.h
 * @author John Wiegley
 *
 * @ingroup report
 */
#ifndef _SESSION_H
#define _SESSION_H

#include "account.h"
#include "journal.h"
#include "context.h"
#include "option.h"
#include "commodity.h"

namespace ledger {

class xact_t;

struct ComparePaths
{
  bool operator()(const path& p1, const path& p2) const
  {
    return p1 < p2 && !boost::filesystem::equivalent(p1, p2);
  }
};

#define COMMA ,

class session_t : public symbol_scope_t
{
  friend void set_session_context(session_t * session);

public:
  bool flush_on_next_data_file;

  unique_ptr<journal_t> journal;
  parse_context_stack_t parsing_context;
  optional<expr_t>      value_expr;

  explicit session_t();
  virtual ~session_t() {
    TRACE_DTOR(session_t);
    parsing_context.pop();
  }

  virtual string description() {
    return _("current session");
  }

  void set_flush_on_next_data_file(const bool truth) {
    flush_on_next_data_file = truth;
  }

  journal_t * read_journal(const path& pathname);
  journal_t * read_journal_from_string(const string& data);
  std::size_t read_data(const string& master_account = "");

  journal_t * read_journal_files();
  void close_journal_files();

  journal_t * get_journal();

  value_t fn_account(call_scope_t& scope);
  value_t fn_min(call_scope_t& scope);
  value_t fn_max(call_scope_t& scope);
  value_t fn_int(call_scope_t& scope);
  value_t fn_str(call_scope_t& scope);
  value_t fn_lot_price(call_scope_t& scope);
  value_t fn_lot_date(call_scope_t& scope);
  value_t fn_lot_tag(call_scope_t& scope);

  void report_options(std::ostream& out)
  {
    HANDLER(check_payees).report(out);
    HANDLER(day_break).report(out);
    HANDLER(download).report(out);
    HANDLER(decimal_comma).report(out);
    HANDLER(time_colon).report(out);
    HANDLER(file_).report(out);
    HANDLER(input_date_format_).report(out);
    HANDLER(explicit).report(out);
    HANDLER(master_account_).report(out);
    HANDLER(pedantic).report(out);
    HANDLER(permissive).report(out);
    HANDLER(price_db_).report(out);
    HANDLER(price_exp_).report(out);
    HANDLER(recursive_aliases).report(out);
    HANDLER(no_aliases).report(out);
    HANDLER(strict).report(out);
    HANDLER(value_expr_).report(out);
  }

  option_t<session_t> * lookup_option(const char * p);

  virtual expr_t::ptr_op_t lookup(const symbol_t::kind_t kind,
                                  const string& name);

  /**
   * Option handlers
   */

  OPTION(session_t, check_payees);
  OPTION(session_t, day_break);
  OPTION(session_t, download); // -Q

  OPTION_(session_t, decimal_comma, DO() {
      commodity_t::decimal_comma_by_default = true;
    });

  OPTION_(session_t, time_colon, DO() {
      commodity_t::time_colon_by_default = true;
    });

  OPTION__
  (session_t, price_exp_, // -Z
   CTOR(session_t, price_exp_) { value = "24"; });

  OPTION__
  (session_t, file_, // -f
   std::set<path COMMA ComparePaths> data_files;
   CTOR(session_t, file_) {}
   DO_(str) {
     if (parent->flush_on_next_data_file) {
       data_files.clear();
       parent->flush_on_next_data_file = false;
     }
     data_files.insert(str);
   });

  OPTION_(session_t, input_date_format_, DO_(str) {
      // This changes static variables inside times.h, which affects the
      // basic date parser.
      set_input_date_format(str.c_str());
    });

  OPTION(session_t, explicit);
  OPTION(session_t, master_account_);
  OPTION(session_t, pedantic);
  OPTION(session_t, permissive);
  OPTION(session_t, price_db_);
  OPTION(session_t, strict);
  OPTION(session_t, value_expr_);
  OPTION(session_t, recursive_aliases);
  OPTION(session_t, no_aliases);
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
