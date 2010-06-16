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
  struct token_t;
  class  parser_t;

  typedef expr_base_t<value_t> base_type;

public:
  class op_t;
  typedef intrusive_ptr<op_t>       ptr_op_t;
  typedef intrusive_ptr<const op_t> const_ptr_op_t;
protected:
  ptr_op_t ptr;

public:
  expr_t() : base_type() {
    TRACE_CTOR(expr_t, "");
  }
  expr_t(const expr_t& other)
    : base_type(other), ptr(other.ptr) {
    TRACE_CTOR(expr_t, "copy");
  }
  expr_t(ptr_op_t _ptr, scope_t * _context = NULL)
    : base_type(_context), ptr(_ptr) {
    TRACE_CTOR(expr_t, "const ptr_op_t&, scope_t *");
  }

  expr_t(const string& _str, const parse_flags_t& flags = PARSE_DEFAULT)
    : base_type() {
    TRACE_CTOR(expr_t, "string, parse_flags_t");
    if (! _str.empty())
      parse(_str, flags);
  }
  expr_t(std::istream& in, const parse_flags_t& flags = PARSE_DEFAULT)
    : base_type() {
    TRACE_CTOR(expr_t, "std::istream&, parse_flags_t");
    parse(in, flags);
  }

  virtual ~expr_t() {
    TRACE_DTOR(expr_t);
  }

  expr_t& operator=(const expr_t& _expr) {
    if (this != &_expr) {
      base_type::operator=(_expr);
      ptr = _expr.ptr;
    }
    return *this;
  }

  virtual operator bool() const throw() {
    return ptr.get() != NULL;
  }

  ptr_op_t get_op() throw() {
    return ptr;
  }

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

#if defined(HAVE_BOOST_SERIALIZATION)
private:
  /** Serialization. */

  friend class boost::serialization::access;

  template<class Archive>
  void serialize(Archive& ar, const unsigned int /* version */) {
    ar & boost::serialization::base_object<base_type>(*this);
    ar & ptr;
  }
#endif // HAVE_BOOST_SERIALIZATION
};

/**
 * Dealing with expr pointers tucked into value objects.
 */
inline bool is_expr(const value_t& val) {
  return val.is_any() && val.as_any().type() == typeid(expr_t::ptr_op_t);
}
inline expr_t::ptr_op_t as_expr(const value_t& val) {
  VERIFY(val.is_any());
  return val.as_any<expr_t::ptr_op_t>();
}
inline void set_expr(value_t& val, expr_t::ptr_op_t op) {
  val.set_any(op);
}
inline value_t expr_value(expr_t::ptr_op_t op) {
  value_t temp;
  temp.set_any(op);
  return temp;
}

} // namespace ledger

#endif // _EXPR_H
