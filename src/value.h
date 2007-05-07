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

#ifndef _VALUE_H
#define _VALUE_H

#include "amount.h"
#include "balance.h"

namespace ledger {

namespace xml {
  class node_t;
}

// The following type is a polymorphous value type used solely for
// performance reasons.  The alternative is to compute value
// expressions (valexpr.cc) in terms of the largest data type,
// balance_t. This was found to be prohibitively expensive, especially
// when large logic chains were involved, since many temporary
// allocations would occur for every operator.  With value_t, and the
// fact that logic chains only need boolean values to continue, no
// memory allocations need to take place at all.

class value_t
  : public ordered_field_operators<value_t,
           ordered_field_operators<value_t, balance_pair_t,
           ordered_field_operators<value_t, balance_t,
           ordered_field_operators<value_t, amount_t,
           ordered_field_operators<value_t, double,
           ordered_field_operators<value_t, unsigned long,
           ordered_field_operators<value_t, long> > > > > > >
{
  char data[sizeof(balance_pair_t)];

 public:
  typedef std::vector<value_t> sequence_t;

  enum type_t {
    VOID,
    BOOLEAN,
    DATETIME,
    INTEGER,
    AMOUNT,
    BALANCE,
    BALANCE_PAIR,
    STRING,
    SEQUENCE,
    XML_NODE,
    POINTER
  } type;

  value_t() : type(VOID) {
    TRACE_CTOR(value_t, "");
  }

  value_t(const value_t& val) : type(VOID) {
    TRACE_CTOR(value_t, "copy");
    *this = val;
  }
  value_t(const bool val) {
    TRACE_CTOR(value_t, "const bool");
    type = BOOLEAN;
    as_boolean() = val;
  }
  value_t(const long val) {
    TRACE_CTOR(value_t, "const long");
    type = INTEGER;
    as_long() = val;
  }
  value_t(const moment_t val) {
    TRACE_CTOR(value_t, "const moment_t");
    type = DATETIME;
    new((moment_t *) data) moment_t(val);
  }
  value_t(const double val) {
    TRACE_CTOR(value_t, "const double");
    type = AMOUNT;
    new((amount_t *) data) amount_t(val);
  }
  value_t(const unsigned long val) {
    TRACE_CTOR(value_t, "const unsigned long");
    type = AMOUNT;
    new((amount_t *) data) amount_t(val);
  }
  value_t(const string& val, bool literal = false) {
    TRACE_CTOR(value_t, "const string&, bool");
    if (literal) {
      type = STRING;
      new((string *) data) string(val);
    } else {
      type = AMOUNT;
      new((amount_t *) data) amount_t(val);
    }
  }
  value_t(const char * val, bool literal = false) {
    TRACE_CTOR(value_t, "const char *");
    if (literal) {
      type = STRING;
      new((string *) data) string(val);
    } else {
      type = AMOUNT;
      new((amount_t *) data) amount_t(val);
    }
  }
  value_t(const amount_t& val) {
    TRACE_CTOR(value_t, "const amount_t&");
    type = AMOUNT;
    new((amount_t *)data) amount_t(val);
  }
  value_t(const balance_t& val) : type(VOID) {
    TRACE_CTOR(value_t, "const balance_t&");
    type = BALANCE;
    new((balance_t *)data) balance_t(val);
  }
  value_t(const balance_pair_t& val) : type(VOID) {
    TRACE_CTOR(value_t, "const balance_pair_t&");
    type = BALANCE_PAIR;
    new((balance_pair_t *)data) balance_pair_t(val);
  }
  value_t(const sequence_t& val) {
    TRACE_CTOR(value_t, "const sequence_t&");
    type = SEQUENCE;
    new((sequence_t *)data) sequence_t(val);
  }
  value_t(xml::node_t * xml_node) {
    TRACE_CTOR(value_t, "xml::node_t *");
    type = XML_NODE;
    as_xml_node() = xml_node;
  }
  value_t(void * item) {
    TRACE_CTOR(value_t, "void *");
    type = POINTER;
    as_pointer() = item;
  }

  ~value_t() {
    TRACE_DTOR(value_t);
    destroy();
  }

  void destroy();
  value_t simplify() const {
    value_t temp = *this;
    temp.in_place_simplify();
    return temp;
  }
  void in_place_simplify();

  value_t& operator=(const value_t& val);

  value_t& set_string(const string& str = "") {
    if (type != STRING) {
      destroy();
      type = STRING;
    }
    as_string() = str;
    return *this;
  }

  bool& as_boolean() {
    assert(type == BOOLEAN);
    return *(bool *) data;
  }
  const bool& as_boolean() const {
    assert(type == BOOLEAN);
    return *(bool *) data;
  }
  long& as_long() {
    assert(type == INTEGER);
    return *(long *) data;
  }
  const long& as_long() const {
    assert(type == INTEGER);
    return *(long *) data;
  }
  moment_t& as_datetime() {
    assert(type == DATETIME);
    return *(moment_t *) data;
  }
  const moment_t& as_datetime() const {
    assert(type == DATETIME);
    return *(moment_t *) data;
  }
  amount_t& as_amount() {
    assert(type == AMOUNT);
    return *(amount_t *) data;
  }
  const amount_t& as_amount() const {
    assert(type == AMOUNT);
    return *(amount_t *) data;
  }
  balance_t& as_balance() {
    assert(type == BALANCE);
    return *(balance_t *) data;
  }
  const balance_t& as_balance() const {
    assert(type == BALANCE);
    return *(balance_t *) data;
  }
  balance_pair_t& as_balance_pair() {
    assert(type == BALANCE_PAIR);
    return *(balance_pair_t *) data;
  }
  const balance_pair_t& as_balance_pair() const {
    assert(type == BALANCE_PAIR);
    return *(balance_pair_t *) data;
  }
  string& as_string() {
    assert(type == STRING);
    return *(string *) data;
  }
  const string& as_string() const {
    assert(type == STRING);
    return *(string *) data;
  }
  sequence_t& as_sequence() {
    assert(type == SEQUENCE);
    return *(sequence_t *) data;
  }
  const sequence_t& as_sequence() const {
    assert(type == SEQUENCE);
    return *(sequence_t *) data;
  }

  xml::node_t *& as_xml_node() {
    assert(type == XML_NODE);
    return *(xml::node_t **) data;
  }
  xml::node_t * as_xml_node() const {
    assert(type == XML_NODE);
    return *(xml::node_t **) data;
  }
  void *& as_pointer() {
    assert(type == POINTER);
    return *(void **) data;
  }
  void * as_pointer() const {
    assert(type == POINTER);
    return *(void **) data;
  }

  bool		 to_boolean() const;
  long		 to_long() const;
  moment_t       to_datetime() const;
  amount_t	 to_amount() const;
  balance_t	 to_balance() const;
  balance_pair_t to_balance_pair() const;
  string	 to_string() const;
  sequence_t     to_sequence() const;

  value_t& operator[](const int index) {
    return as_sequence()[index];
  }

  void push_back(const value_t& val) {
    return as_sequence().push_back(val);
  }

  std::size_t size() const {
    return as_sequence().size();
  }

  value_t& operator+=(const value_t& val);
  value_t& operator-=(const value_t& val);
  value_t& operator*=(const value_t& val);
  value_t& operator/=(const value_t& val);

  bool operator==(const value_t& val) const;
  bool operator<(const value_t& val) const;
  //bool operator>(const value_t& val) const;

  string label(optional<type_t> the_type = optional<type_t>()) const {
    switch (the_type ? *the_type : type) {
    case VOID:
      return "an uninitialized value";
    case BOOLEAN:
      return "a boolean";
    case INTEGER:
      return "an integer";
    case DATETIME:
      return "a date/time";
    case AMOUNT:
      return "an amount";
    case BALANCE:
      return "a balance";
    case BALANCE_PAIR:
      return "a balance pair";
    case STRING:
      return "a string";
    case SEQUENCE:
      return "a sequence";
    case XML_NODE:
      return "an xml node";
    case POINTER:
      return "a pointer";
    default:
      assert(false);
      break;
    }
  }
  
  operator bool() const;

  value_t operator-() const {
    return negate();
  }
  value_t negate() const {
    value_t temp = *this;
    temp.in_place_negate();
    return temp;
  }
  void in_place_negate();

  bool    is_realzero() const;
  value_t abs() const;
  void    in_place_cast(type_t cast_type);
  value_t cost() const;
  value_t annotated_price() const;
  value_t annotated_date() const;
  value_t annotated_tag() const;

  value_t cast(type_t cast_type) const {
    value_t temp(*this);
    temp.in_place_cast(cast_type);
    return temp;
  }

  value_t strip_annotations(const bool keep_price = amount_t::keep_price,
			    const bool keep_date  = amount_t::keep_date,
			    const bool keep_tag   = amount_t::keep_tag) const;

  value_t& add(const amount_t& amount,
	       const optional<amount_t>& cost = optional<amount_t>());
  value_t  value(const optional<moment_t>& moment =
		 optional<moment_t>()) const;

  void    in_place_reduce();
  value_t reduce() const {
    value_t temp(*this);
    temp.in_place_reduce();
    return temp;
  }

  value_t round() const;
  value_t unround() const;

  void print(std::ostream& out, const int first_width,
	     const int latter_width = -1) const;

  friend std::ostream& operator<<(std::ostream& out, const value_t& val);
};

std::ostream& operator<<(std::ostream& out, const value_t& val);

#if 0
class value_context : public error_context
{
  value_t * bal;
 public:
  value_context(const value_t& _bal,
		const string& desc = "") throw();
  virtual ~value_context() throw();

  virtual void describe(std::ostream& out) const throw();
};
#endif

DECLARE_EXCEPTION(value_error);

} // namespace ledger

#endif // _VALUE_H
