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
 * @file   expr.h
 * @author John Wiegley
 *
 * @ingroup expr
 */
#ifndef _EXPR_H
#define _EXPR_H

#include "exprbase.h"
#include "value.h"

namespace ledger {

class expr_t : public expr_base_t<value_t>
{
  class parser_t;
  typedef expr_base_t<value_t> base_type;

public:
  struct token_t;
  class op_t;
  typedef intrusive_ptr<op_t>       ptr_op_t;
  typedef intrusive_ptr<const op_t> const_ptr_op_t;

  enum check_expr_kind_t {
    EXPR_GENERAL,
    EXPR_ASSERTION,
    EXPR_CHECK
  };

  typedef std::pair<expr_t, check_expr_kind_t> check_expr_pair;
  typedef std::list<check_expr_pair>           check_expr_list;

protected:
  ptr_op_t ptr;

public:
  expr_t();
  expr_t(const expr_t& other);
  expr_t(ptr_op_t _ptr, scope_t * _context = NULL);

  expr_t(const string& _str, const parse_flags_t& flags = PARSE_DEFAULT);
  expr_t(std::istream& in, const parse_flags_t& flags = PARSE_DEFAULT);

  virtual ~expr_t();

  expr_t& operator=(const expr_t& _expr);

  virtual operator bool() const throw();

  ptr_op_t get_op() throw();

  void parse(const string& str, const parse_flags_t& flags = PARSE_DEFAULT) {
    std::istringstream stream(str);
    return parse(stream, flags, str);
  }

  virtual void    parse(std::istream&           in,
                        const parse_flags_t&    flags           = PARSE_DEFAULT,
                        const optional<string>& original_string = none);
  virtual void    compile(scope_t& scope);
  virtual value_t real_calc(scope_t& scope);

  bool            is_constant() const;
  value_t&        constant_value();
  const value_t&  constant_value() const;
  bool            is_function() const;
  func_t&         get_function();

  virtual string  context_to_str() const;
  virtual void    print(std::ostream& out) const;
  virtual void    dump(std::ostream& out) const;
};

/**
 * Dealing with expr pointers tucked into value objects.
 */
inline bool is_expr(const value_t& val) {
  return val.is_any() && val.as_any().type() == typeid(expr_t::ptr_op_t);
}

expr_t::ptr_op_t as_expr(const value_t& val);
void             set_expr(value_t& val, expr_t::ptr_op_t op);
value_t          expr_value(expr_t::ptr_op_t op);

// A merged expression allows one to set an expression term, "foo", and
// a base expression, "bar", and then merge in later expressions that
// utilize foo.  For example:
//
//    foo: bar
//  merge: foo * 10
//  merge: foo + 20
//
// When this expression is finally compiled, the base and merged
// elements are written into this:
//
//   __tmp=(foo=bar; foo=foo*10; foo=foo+20);__tmp
//
// This allows users to select flags like -O, -B or -I at any time, and
// also combine flags such as -V and -A.

class merged_expr_t : public expr_t
{
public:
  string term;
  string base_expr;
  string merge_operator;

  std::list<string> exprs;

  merged_expr_t(const string& _term, const string& expr,
                const string& merge_op = ";")
    : expr_t(), term(_term), base_expr(expr), merge_operator(merge_op) {
    TRACE_CTOR(merged_expr_t, "string, string, string");
  }
  virtual ~merged_expr_t() {
    TRACE_DTOR(merged_expr_t);
  }

  void set_term(const string& _term) {
    term = _term;
  }
  void set_base_expr(const string& expr) {
    base_expr = expr;
  }
  void set_merge_operator(const string& merge_op) {
    merge_operator = merge_op;
  }

  bool check_for_single_identifier(const string& expr);

  void prepend(const string& expr) {
    if (! check_for_single_identifier(expr))
      exprs.push_front(expr);
  }
  void append(const string& expr) {
    if (! check_for_single_identifier(expr))
      exprs.push_back(expr);
  }
  void remove(const string& expr) {
    exprs.remove(expr);
  }

  virtual void compile(scope_t& scope);
};

class call_scope_t;
value_t source_command(call_scope_t& scope);

} // namespace ledger

#endif // _EXPR_H
