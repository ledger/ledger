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
 * @addtogroup expr
 */

/**
 * @file   exprbase.h
 * @author John Wiegley
 *
 * @ingroup expr
 *
 * This class provides basic behavior for all the domain specific expression
 * languages used in Leger:
 *
 * | Typename    | Description                | result_type     | Derives     |
 * |-------------+----------------------------+-----------------+-------------|
 * | expr_t      | Value expressions          | value_t         |             |
 * | predicate_t | Special form of expr_t     | bool            | expr_t      |
 * | query_t     | Report queries             | bool            | predicate_t |
 * | period_t    | Time periods and durations | date_interval_t |             |
 * | draft_t     | Partially filled xacts     | xact_t *        |             |
 * | format_t    | Format strings             | string          |             |
 */
#ifndef _EXPRBASE_H
#define _EXPRBASE_H

#include "utils.h"
#include "amount.h"

namespace ledger {

DECLARE_EXCEPTION(parse_error, std::runtime_error);
DECLARE_EXCEPTION(compile_error, std::runtime_error);
DECLARE_EXCEPTION(calc_error, std::runtime_error);
DECLARE_EXCEPTION(usage_error, std::runtime_error);

class scope_t;
class call_scope_t;

template <typename ResultType>
class expr_base_t
{
public:
  typedef ResultType result_type;

  typedef function<result_type (call_scope_t&)> func_t;

protected:
  scope_t * context;
  string    str;
  bool      compiled;

  virtual result_type real_calc(scope_t& scope) = 0;

public:
  expr_base_t(const expr_base_t& other)
    : context(other.context), str(other.str), compiled(false) {
    TRACE_CTOR(expr_base_t, "copy");
  }
  expr_base_t(scope_t * _context = NULL)
    : context(_context), compiled(false)
  {
    TRACE_CTOR(expr_base_t, "scope_t *");
  }
  virtual ~expr_base_t() {
    TRACE_DTOR(expr_base_t);
  }

  expr_base_t& operator=(const expr_base_t& _expr) {
    if (this != &_expr) {
      str      = _expr.str;
      context  = _expr.context;
      compiled = _expr.compiled;
    }
    return *this;
  }
  expr_base_t& operator=(const string& _expr) {
    parse(_expr);
    return *this;
  }

  virtual operator bool() const throw() {
    return ! str.empty();
  }

  virtual string text() const throw() {
    return str;
  }
  void set_text(const string& txt) {
    str      = txt;
    compiled = false;
  }

  void parse(const string& expr_str,
             const parse_flags_t& flags = PARSE_DEFAULT) {
    std::istringstream stream(expr_str);
    return parse(stream, flags, expr_str);
  }
  virtual void parse(std::istream&,
                     const parse_flags_t& = PARSE_DEFAULT,
                     const optional<string>& original_string = none) {
    set_text(original_string ? *original_string : "<stream>");
  }

  virtual void mark_uncompiled() {
    compiled = false;
  }

  void recompile(scope_t& scope) {
    compiled = false;
    compile(scope);
  }

  virtual void compile(scope_t& scope) {
    if (! compiled) {
      // Derived classes need to do something here.
      context  = &scope;
      compiled = true;
    }
  }

  result_type operator()(scope_t& scope) {
    return calc(scope);
  }

  result_type calc(scope_t& scope)
  {
    if (! compiled) {
#if DEBUG_ON
      if (SHOW_DEBUG("expr.compile")) {
        DEBUG("expr.compile", "Before compilation:");
        dump(*_log_stream);
      }
#endif // DEBUG_ON

      DEBUG("expr.compile", "Compiling: " << str);
      compile(scope);

#if DEBUG_ON
      if (SHOW_DEBUG("expr.compile")) {
        DEBUG("expr.compile", "After compilation:");
        dump(*_log_stream);
      }
#endif // DEBUG_ON
    }

    DEBUG("expr.calc", "Calculating: " << str);
    return real_calc(scope);
  }

  result_type calc() {
    assert(context);
    return calc(*context);
  }

  scope_t * get_context() {
    return context;
  }
  void set_context(scope_t * scope) {
    context = scope;
  }

  virtual string context_to_str() const {
    return empty_string;
  }

  string print_to_str() const {
    std::ostringstream out;
    print(out);
    return out.str();
  }
  string dump_to_str() const {
    std::ostringstream out;
    dump(out);
    return out.str();
  }
  string preview_to_str(scope_t&) const {
    std::ostringstream out;
    preview(out);
    return out.str();
  }

  virtual void print(std::ostream&) const {}
  virtual void dump(std::ostream&) const {}

  result_type preview(std::ostream& out, scope_t& scope) const {
    out << _("--- Input expression ---") << std::endl;
    out << text() << std::endl;

    out << std::endl << _("--- Text as parsed ---") << std::endl;
    print(out);
    out << std::endl;

    out << std::endl << _("--- Expression tree ---") << std::endl;
    dump(out);

    out << std::endl << _("--- Compiled tree ---") << std::endl;
    compile(scope);
    dump(out);

    out << std::endl << _("--- Result value ---") << std::endl;
    return calc();
  }
};

template <typename ResultType>
std::ostream& operator<<(std::ostream& out,
                         const expr_base_t<ResultType>& expr) {
  expr.print(out);
  return out;
}

} // namespace ledger

#endif // _EXPRBASE_H
