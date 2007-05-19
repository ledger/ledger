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

#ifndef _REPORT_H
#define _REPORT_H

#include "session.h"
#include "transform.h"

namespace ledger {

typedef std::list<string> strings_list;

class report_t : public xml::xpath_t::symbol_scope_t
{
public:
  optional<path> output_file;
  string	 format_string;
  string	 amount_expr;
  string	 total_expr;
  string	 date_output_format;

  unsigned long  budget_flags;

  string	 account;
  optional<path> pager;

  bool           show_totals;
  bool           raw_mode;

  session_t&	 session;
  transform_t *  last_transform;

  std::list<tuple<shared_ptr<transform_t>, value_t> > transforms;

  explicit report_t(session_t& _session)
    : xml::xpath_t::symbol_scope_t(downcast<xml::xpath_t::scope_t>(_session)),
      show_totals(false),
      raw_mode(false),
      session(_session),
      last_transform(NULL)
  {
    TRACE_CTOR(report_t, "session_t&");
#if 0
    eval("t=total,TOT=0,T()=(TOT=TOT+t,TOT)");
#endif
  }

  virtual ~report_t();

  void apply_transforms(xml::xpath_t::scope_t& scope);

  //
  // Utility functions for value expressions
  //

  value_t ftime(xml::xpath_t::call_scope_t& args);
  value_t abbrev(xml::xpath_t::call_scope_t& args);

  //
  // Config options
  //

  void eval(const string& expr) {
#if 0
    xml::xpath_t(expr).compile((xml::document_t *)NULL, this);
#endif
  }
  value_t option_eval(xml::xpath_t::call_scope_t& args) {
    eval(args[0].as_string());
    return NULL_VALUE;
  }

  value_t option_amount(xml::xpath_t::call_scope_t& args) {
    eval(string("t=") + args[0].as_string());
    return NULL_VALUE;
  }
  value_t option_total(xml::xpath_t::call_scope_t& args) {
    eval(string("T()=") + args[0].as_string());
    return NULL_VALUE;
  }

  value_t option_format(xml::xpath_t::call_scope_t& args) {
    format_string = args[0].as_string();
    return NULL_VALUE;
  }

  value_t option_raw(xml::xpath_t::call_scope_t& args) {
    raw_mode = true;
    return NULL_VALUE;
  }

  value_t option_foo(xml::xpath_t::call_scope_t& args) {
    std::cout << "This is foo" << std::endl;
    return NULL_VALUE;
  }
  value_t option_bar(xml::xpath_t::call_scope_t& args) {
    std::cout << "This is bar: " << args[0] << std::endl;
    return NULL_VALUE;
  }

  //
  // Transform options
  //

#if 0
  value_t option_select(xml::xpath_t::call_scope_t& args) {
    transforms.push_back(new select_transform(args[0].as_string()));
    return NULL_VALUE;
  }
  value_t option_limit(xml::xpath_t::call_scope_t& args) {
    string expr = (string("//xact[") +
		   args[0].as_string() + "]");
    transforms.push_back(new select_transform(expr));
    return NULL_VALUE;
  }

  value_t option_remove(xml::xpath_t::call_scope_t& args) {
    transforms.push_back(new remove_transform(args[0].as_string()));
    return NULL_VALUE;
  }

  value_t option_accounts(xml::xpath_t::call_scope_t& args) {
    transforms.push_back(new accounts_transform);
    return NULL_VALUE;
  }
  value_t option_compact(xml::xpath_t::call_scope_t& args) {
    transforms.push_back(new compact_transform);
    return NULL_VALUE;
  }
  value_t option_clean(xml::xpath_t::call_scope_t& args) {
    transforms.push_back(new clean_transform);
    return NULL_VALUE;
  }
  value_t option_entries(xml::xpath_t::call_scope_t& args) {
    transforms.push_back(new entries_transform);
    return NULL_VALUE;
  }

  value_t option_split(xml::xpath_t::call_scope_t& args) {
    transforms.push_back(new split_transform);
    return NULL_VALUE;
  }
  value_t option_merge(xml::xpath_t::call_scope_t& args) {
    transforms.push_back(new merge_transform);
    return NULL_VALUE;
  }
#endif

  //
  // Scope members
  //

  virtual xml::xpath_t::ptr_op_t lookup(const string& name);
};

string abbrev(const string& str, unsigned int width,
		   const bool is_account);

} // namespace ledger

#endif // _REPORT_H
