/*
 * Copyright (c) 2003-2010, John Wiegley.  All rights reserved.
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
 * @file   series.h
 * @author John Wiegley
 *
 * @ingroup expr
 */
#ifndef _SERIES_H
#define _SERIES_H

#include "scope.h"

namespace ledger {

class expr_series_t
{
protected:
  scope_t * context;

public:
  optional<std::list<expr_t> > exprs;
  expr_t                       default_expr;
  std::string                  variable;

  expr_series_t(const std::string& _variable)
    : context(NULL), default_expr(_variable), variable(_variable) {
    TRACE_CTOR(expr_series_t, "std::string");
  }
  expr_series_t(const expr_t& expr, const std::string& _variable)
    : context(const_cast<expr_t&>(expr).get_context()),
      default_expr(expr), variable(_variable) {
    TRACE_CTOR(expr_series_t, "expr_t, std::string");
  }
  expr_series_t(const expr_series_t& other)
    : context(other.context), exprs(other.exprs),
      default_expr(other.default_expr), variable(other.variable) {
    TRACE_CTOR(expr_series_t, "copy");
  }
  virtual ~expr_series_t() {
    TRACE_DTOR(expr_series_t);
  }

  scope_t * get_context() {
    return context;
  }
  void set_context(scope_t * scope) {
    context = scope;
  }

  bool empty() const {
    return ! exprs || exprs->empty();
  }

  void push_back(const expr_t& expr) {
    if (! exprs)
      exprs = std::list<expr_t>();
    exprs->push_back(expr);
  }
  void pop_back() {
    assert(exprs);
    exprs->pop_back();
  }

  void mark_uncompiled() {
    if (exprs)
      foreach (expr_t& expr, *exprs)
        expr.mark_uncompiled();
    else
      default_expr.mark_uncompiled();
  }

  void compile(scope_t& scope) {
    if (exprs)
      foreach (expr_t& expr, *exprs)
        expr.compile(scope);
    else
      default_expr.compile(scope);
  }

  value_t calc(scope_t& scope) {
    if (exprs) {
      value_t        result;
      symbol_scope_t sym_scope(scope);
      std::size_t    len(exprs->size());

      foreach (expr_t& expr, *exprs) {
        result = expr.calc(sym_scope);
        if (--len > 0)
          sym_scope.define(symbol_t::FUNCTION, variable,
                           expr_t::op_t::wrap_value(result));
      }
      return result;
    } else {
      return default_expr.calc(scope);
    }
  }
};

} // namespace ledger

#endif // _SERIES_H
