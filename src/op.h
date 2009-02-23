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
 * @file   op.h
 * @author John Wiegley
 *
 * @ingroup expr
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _OP_H
#define _OP_H

#include "expr.h"

namespace ledger {

/**
 * @brief Brief
 *
 * Long.
 */
class expr_t::op_t : public noncopyable
{
  friend class expr_t;
  friend class expr_t::parser_t;

public:
  typedef expr_t::ptr_op_t ptr_op_t;

private:
  mutable short refc;
  ptr_op_t	left_;

  variant<ptr_op_t,		// used by all binary operators
	  value_t,		// used by constant VALUE
	  string,		// used by constant IDENT
	  function_t		// used by terminal FUNCTION
	  > data;

public:
  enum kind_t {
    // Constants
    VALUE,
    IDENT,

    CONSTANTS,

    FUNCTION,

    TERMINALS,

    // Binary operators
    O_NOT,
    O_NEG,

    UNARY_OPERATORS,

    O_EQ,
    O_LT,
    O_LTE,
    O_GT,
    O_GTE,

    O_AND,
    O_OR,

    O_ADD,
    O_SUB,
    O_MUL,
    O_DIV,

    O_QUERY,
    O_COLON,

    O_CONS,

    O_DEFINE,
    O_LOOKUP,
    O_CALL,
    O_MATCH,

    BINARY_OPERATORS,

    OPERATORS,

    UNKNOWN,

    LAST
  };

  kind_t kind;

  explicit op_t() : refc(0), kind(UNKNOWN) {
    TRACE_CTOR(op_t, "");
  }
  explicit op_t(const kind_t _kind) : refc(0), kind(_kind) {
    TRACE_CTOR(op_t, "const kind_t");
  }
  ~op_t() {
    TRACE_DTOR(op_t);
    assert(refc == 0);
  }

  bool is_value() const {
    if (kind == VALUE) {
      assert(data.type() == typeid(value_t));
      return true;
    }
    return false;
  }
  value_t& as_value_lval() {
    assert(is_value());
    value_t& val(boost::get<value_t>(data));
    assert(val.valid());
    return val;
  }
  const value_t& as_value() const {
    return const_cast<op_t *>(this)->as_value_lval();
  }
  void set_value(const value_t& val) {
    assert(val.valid());
    data = val;
  }

  bool is_ident() const {
    if (kind == IDENT) {
      assert(data.type() == typeid(string));
      return true;
    }
    return false;
  }
  string& as_ident_lval() {
    assert(is_ident());
    return boost::get<string>(data);
  }
  const string& as_ident() const {
    return const_cast<op_t *>(this)->as_ident_lval();
  }
  void set_ident(const string& val) {
    data = val;
  }

  bool is_function() const {
    return kind == FUNCTION;
  }
  function_t& as_function_lval() {
    assert(kind == FUNCTION);
    return boost::get<function_t>(data);
  }
  const function_t& as_function() const {
    return const_cast<op_t *>(this)->as_function_lval();
  }
  void set_function(const function_t& val) {
    data = val;
  }

  ptr_op_t& left() {
    return left_;
  }
  const ptr_op_t& left() const {
    assert(kind > TERMINALS || kind == IDENT);
    return left_;
  }
  void set_left(const ptr_op_t& expr) {
    assert(kind > TERMINALS || kind == IDENT);
    left_ = expr;
  }

  ptr_op_t& as_op_lval() {
    assert(kind > TERMINALS || kind == IDENT);
    return boost::get<ptr_op_t>(data);
  }
  const ptr_op_t& as_op() const {
    return const_cast<op_t *>(this)->as_op_lval();
  }

  ptr_op_t& right() {
    assert(kind > TERMINALS);
    return as_op_lval();
  }
  const ptr_op_t& right() const {
    assert(kind > TERMINALS);
    return as_op();
  }
  void set_right(const ptr_op_t& expr) {
    assert(kind > TERMINALS);
    data = expr;
  }
  bool has_right() const {
    if (kind < TERMINALS)
      return false;
    return as_op();
  }

private:
  void acquire() const {
    DEBUG("ledger.xpath.memory",
	  "Acquiring " << this << ", refc now " << refc + 1);
    assert(refc >= 0);
    refc++;
  }
  void release() const {
    DEBUG("ledger.xpath.memory",
	  "Releasing " << this << ", refc now " << refc - 1);
    assert(refc > 0);
    if (--refc == 0)
      checked_delete(this);
  }

  friend inline void intrusive_ptr_add_ref(op_t * op) {
    op->acquire();
  }
  friend inline void intrusive_ptr_release(op_t * op) {
    op->release();
  }

  static ptr_op_t new_node(kind_t _kind, ptr_op_t _left = NULL,
			   ptr_op_t _right = NULL);

  ptr_op_t copy(ptr_op_t _left = NULL, ptr_op_t _right = NULL) const {
    ptr_op_t node(new_node(kind, _left, _right));
    if (kind < TERMINALS)
      node->data = data;
    return node;
  }

public:
  ptr_op_t compile(scope_t& scope);
  value_t  calc(scope_t& scope, ptr_op_t * locus = NULL);

  struct context_t
  {
    ptr_op_t	       expr_op;
    ptr_op_t	       op_to_find;
    ostream_pos_type * start_pos;
    ostream_pos_type * end_pos;
    bool	       relaxed;

    context_t(const ptr_op_t&          _expr_op    = NULL,
	      const ptr_op_t&	       _op_to_find = NULL,
	      ostream_pos_type * const _start_pos  = NULL,
	      ostream_pos_type * const _end_pos    = NULL,
	      const bool	       _relaxed	   = true)
      : expr_op(_expr_op), op_to_find(_op_to_find),
	start_pos(_start_pos), end_pos(_end_pos),
	relaxed(_relaxed) {}
  };

  bool print(std::ostream& out, const context_t& context = context_t()) const;
  void dump(std::ostream& out, const int depth) const;

  static ptr_op_t wrap_value(const value_t& val);
  static ptr_op_t wrap_functor(const function_t& fobj);
};

inline expr_t::ptr_op_t
expr_t::op_t::new_node(kind_t _kind, ptr_op_t _left, ptr_op_t _right)
{
  ptr_op_t node(new op_t(_kind));
  if (_left)  node->set_left(_left);
  if (_right) node->set_right(_right);
  return node;
}

inline expr_t::ptr_op_t expr_t::op_t::wrap_value(const value_t& val) {
  ptr_op_t temp(new op_t(op_t::VALUE));
  temp->set_value(val);
  return temp;
}

inline expr_t::ptr_op_t expr_t::op_t::wrap_functor(const function_t& fobj) {
  ptr_op_t temp(new op_t(op_t::FUNCTION));
  temp->set_function(fobj);
  return temp;
}

#define MAKE_FUNCTOR(x) expr_t::op_t::wrap_functor(bind(&x, this, _1))
#define WRAP_FUNCTOR(x) expr_t::op_t::wrap_functor(x)

string op_context(const expr_t::ptr_op_t op,
		  const expr_t::ptr_op_t locus = NULL);

} // namespace ledger

#endif // _OP_H
