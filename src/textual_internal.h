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

#pragma once

#include <system.hh>

#include "journal.h"
#include "context.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "option.h"
#include "query.h"
#include "pstream.h"
#include "pool.h"
#include "session.h"
#include <algorithm>
#if HAVE_BOOST_PYTHON
#include "pyinterp.h"
#endif

#define TIMELOG_SUPPORT 1
#if TIMELOG_SUPPORT
#include "timelog.h"
#endif

namespace ledger::detail {

typedef std::pair<commodity_t*, amount_t> fixed_rate_t;

struct application_t {
  string label;
  std::variant<optional<datetime_t>, account_t*, string, fixed_rate_t> value;
  optional<int> saved_year_directive;

  application_t(string _label, optional<datetime_t> epoch) : label(_label), value(epoch) {}
  application_t(string _label, account_t* acct) : label(_label), value(acct) {}
  application_t(string _label, string tag) : label(_label), value(tag) {}
  application_t(string _label, fixed_rate_t rate) : label(_label), value(rate) {}
};

class instance_t : public noncopyable, public scope_t {
public:
  parse_context_stack_t& context_stack;
  parse_context_t& context;
  std::istream& in;
  instance_t* parent;
  std::list<application_t> apply_stack;
  bool no_assertions;
  hash_type_t hash_type;
#if TIMELOG_SUPPORT
  time_log_t timelog;
#endif

  instance_t(parse_context_stack_t& _context_stack, parse_context_t& _context,
             instance_t* _parent = NULL, const bool _no_assertions = false,
             const hash_type_t _hash_type = NO_HASHES)
      : context_stack(_context_stack), context(_context), in(*context.stream.get()),
        parent(_parent), no_assertions(_no_assertions), hash_type(_hash_type), timelog(context) {}

  virtual string description() override { return _("textual parser"); }

  template <typename T>
  void get_applications(std::vector<T>& result) {
    for (application_t& state : apply_stack) {
      if (std::holds_alternative<T>(state.value))
        result.push_back(std::get<T>(state.value));
    }
    if (parent)
      parent->get_applications<T>(result);
  }

  template <typename T>
  optional<T> get_application() {
    for (application_t& state : apply_stack) {
      if (std::holds_alternative<T>(state.value))
        return std::get<T>(state.value);
    }
    return parent ? parent->get_application<T>() : none;
  }

  account_t* top_account() {
    if (optional<account_t*> acct = get_application<account_t*>())
      return *acct;
    else
      return NULL;
  }

  void parse();

  std::streamsize read_line(char*& line);

  bool peek_whitespace_line() {
    return (in.good() && !in.eof() && (in.peek() == ' ' || in.peek() == '\t'));
  }
#if HAVE_BOOST_PYTHON
  bool peek_blank_line() {
    return (in.good() && !in.eof() && (in.peek() == '\n' || in.peek() == '\r'));
  }
#endif

  xact_t* read_next_directive(bool& error_flag, xact_t* previous_xact);

#if TIMELOG_SUPPORT
  void clock_in_directive(char* line, bool capitalized);
  void clock_out_directive(char* line, bool capitalized);
#endif

  bool general_directive(char* line);

  void account_directive(char* line);
  void account_alias_directive(account_t* account, string alias);
  void account_payee_directive(account_t* account, string payee);
  void account_value_directive(account_t* account, string expr_str);
  void account_default_directive(account_t* account);

  void default_account_directive(char* args);
  void alias_directive(char* line);

  void payee_directive(char* line);
  void payee_alias_directive(const string& payee, string alias);
  void payee_uuid_directive(const string& payee, string uuid);

  void commodity_directive(char* line);
  void commodity_alias_directive(commodity_t& comm, string alias);
  void commodity_value_directive(commodity_t& comm, string expr_str);
  void commodity_format_directive(commodity_t& comm, string format);
  void commodity_nomarket_directive(commodity_t& comm);
  void commodity_default_directive(commodity_t& comm);

  void default_commodity_directive(char* line);

  void tag_directive(char* line);

  void apply_directive(char* line);
  void apply_account_directive(char* line);
  void apply_tag_directive(char* line);
  void apply_rate_directive(char* line);
  void apply_year_directive(char* line, bool use_apply_stack = false);
  void end_apply_directive(char* line);

  xact_t* xact_directive(char* line, std::streamsize len, xact_t* previous_xact);
  void period_xact_directive(char* line);
  void automated_xact_directive(char* line);
  void price_xact_directive(char* line);
  void price_conversion_directive(char* line);
  void nomarket_directive(char* line);

  void include_directive(char* line);
  void option_directive(char* line);
  void comment_directive(char* line);

  void eval_directive(char* line);
  void assert_directive(char* line);
  void check_directive(char* line);
  void value_directive(char* line);

  void import_directive(char* line);
  void python_directive(char* line);

  post_t* parse_post(char* line, std::streamsize len, account_t* account, xact_t* xact,
                     bool defer_expr = false);

  bool parse_posts(account_t* account, xact_base_t& xact, const bool defer_expr = false);

  xact_t* parse_xact(char* line, std::streamsize len, account_t* account, xact_t* previous_xact);

  virtual expr_t::ptr_op_t lookup(const symbol_t::kind_t kind, const string& name) override;
};

void parse_amount_expr(std::istream& in, scope_t& scope, post_t& post, amount_t& amount,
                       const parse_flags_t& flags = PARSE_DEFAULT, const bool defer_expr = false,
                       optional<expr_t>* amount_expr = NULL);

} // namespace ledger::detail
