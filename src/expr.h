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
 * @addtogroup expr
 */

/**
 * @file   expr.h
 * @author John Wiegley
 *
 * @ingroup expr
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _EXPR_H
#define _EXPR_H

#include "value.h"

namespace ledger {

DECLARE_EXCEPTION(parse_error, std::runtime_error);
DECLARE_EXCEPTION(compile_error, std::runtime_error);
DECLARE_EXCEPTION(calc_error, std::runtime_error);
DECLARE_EXCEPTION(usage_error, std::runtime_error);

class scope_t;
class call_scope_t;

typedef function<value_t (call_scope_t&)> function_t;

/**
 * @brief Brief
 *
 * Long.
 */
class expr_t
{
  struct token_t;
  class  parser_t;

  friend string expr_context(const expr_t& expr);

public:
  class op_t;
  typedef intrusive_ptr<op_t>	    ptr_op_t;
  typedef intrusive_ptr<const op_t> const_ptr_op_t;

  enum parse_flags_enum_t {
    PARSE_NORMAL     = 0x00,
    PARSE_PARTIAL    = 0x01,
    PARSE_SINGLE     = 0x02,
    PARSE_NO_MIGRATE = 0x04,
    PARSE_NO_REDUCE  = 0x08,
    PARSE_NO_ASSIGN  = 0x10,
    PARSE_NO_DATES   = 0x20,
    PARSE_OP_CONTEXT = 0x40
  };

private:
  ptr_op_t  ptr;
  scope_t * context;
  string    str;
  bool	    compiled;

public:
  expr_t();
  expr_t(const expr_t& other);
  expr_t(const ptr_op_t& _ptr, scope_t * context = NULL,
	 const string& _str = "");

  expr_t(const string& _str, const uint_least8_t flags = 0);
  expr_t(std::istream& in, const uint_least8_t flags = 0);

  virtual ~expr_t() throw();

  expr_t& operator=(const expr_t& _expr);
  expr_t& operator=(const string& _expr) {
    parse(_expr);
    return *this;
  }

  operator bool() const throw() {
    return ptr.get() != NULL;
  }

  ptr_op_t get_op() throw();

  string text() const throw() {
    return str;
  }

  // This has special use in the textual parser
  void set_text(const string& txt) {
    str = txt;
  }

  void     parse(const string& _str, const uint32_t flags = 0);
  void     parse(std::istream& in, const uint32_t flags = 0,
		 const string * original_string = NULL);
	   
  void     mark_uncompiled() {
    compiled = false;
  }
  void     recompile(scope_t& scope);
  void     compile(scope_t& scope);
  value_t  calc(scope_t& scope);
  value_t  calc(scope_t& scope) const;

  value_t  calc() {
    assert(context);
    return calc(*context);
  }
  value_t  calc() const {
    assert(context);
    return calc(*context);
  }

  scope_t * get_context() {
    return context;
  }
  void set_context(scope_t * scope) {
    context = scope;
  }

  bool	   is_constant() const;
  bool     is_function() const;

  value_t&       constant_value();
  const value_t& constant_value() const;

  function_t& get_function();

  void print(std::ostream& out) const;
  void dump(std::ostream& out) const;

  static value_t eval(const string& _expr, scope_t& scope);
};

std::ostream& operator<<(std::ostream& out, const expr_t& expr);

string expr_context(const expr_t& expr);

} // namespace ledger

#endif // _EXPR_H
