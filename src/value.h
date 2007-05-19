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

#include "balpair.h"		// pulls in balance.h and amount.h

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
public:
  typedef std::vector<value_t> sequence_t;

  typedef sequence_t::iterator	      iterator;
  typedef sequence_t::const_iterator  const_iterator;
  typedef sequence_t::difference_type difference_type;

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
  };

private:
  class storage_t
  {
    char   data[sizeof(balance_pair_t)];
    type_t type;

    BOOST_STATIC_ASSERT(sizeof(balance_pair_t) > sizeof(bool));
    BOOST_STATIC_ASSERT(sizeof(balance_pair_t) > sizeof(moment_t));
    BOOST_STATIC_ASSERT(sizeof(balance_pair_t) > sizeof(long));
    BOOST_STATIC_ASSERT(sizeof(balance_pair_t) > sizeof(amount_t));
    BOOST_STATIC_ASSERT(sizeof(balance_pair_t) > sizeof(balance_t));
    BOOST_STATIC_ASSERT(sizeof(balance_pair_t) > sizeof(string));
    BOOST_STATIC_ASSERT(sizeof(balance_pair_t) > sizeof(sequence_t));
    BOOST_STATIC_ASSERT(sizeof(balance_pair_t) > sizeof(xml::node_t *));

    explicit storage_t() : type(VOID), refc(0) {
      TRACE_CTOR(value_t::storage_t, "");
    }
    explicit storage_t(const storage_t& rhs)
      : type(rhs.type), refc(0) {
      TRACE_CTOR(value_t::storage_t, "");
      std::memcpy(data, rhs.data, sizeof(balance_pair_t));
    }

  public:			// so `checked_delete' can access it
    ~storage_t() {
      TRACE_DTOR(value_t::storage_t);
      DEBUG("value.storage.refcount", "Destroying " << this);
      assert(refc == 0);
      destroy();
    }

  private:
    storage_t& operator=(const storage_t& rhs) {
      type = rhs.type;
      std::memcpy(data, rhs.data, sizeof(balance_pair_t));
      return *this;
    }

    mutable int refc;

    void acquire() const {
      DEBUG("value.storage.refcount",
	     "Acquiring " << this << ", refc now " << refc + 1);
      assert(refc >= 0);
      refc++;
    }
    void release() const {
      DEBUG("value.storage.refcount",
	     "Releasing " << this << ", refc now " << refc - 1);
      assert(refc > 0);
      if (--refc == 0)
	checked_delete(this);
    }

    void destroy();

    friend class value_t;

    friend inline void intrusive_ptr_add_ref(value_t::storage_t * storage) {
      storage->acquire();
    }
    friend inline void intrusive_ptr_release(value_t::storage_t * storage) {
      storage->release();
    }
  };

  intrusive_ptr<storage_t> storage;

  static intrusive_ptr<storage_t> true_value;
  static intrusive_ptr<storage_t> false_value;

  // jww (2007-05-03): Make this private, and then make
  // ledger::initialize into a member function of session_t.
public:
  static void initialize();
  static void shutdown();

public:
  value_t() {
    TRACE_CTOR(value_t, "");
  }

  value_t(const value_t& val) {
    TRACE_CTOR(value_t, "copy");
    *this = val;
  }
  value_t(const bool val) {
    TRACE_CTOR(value_t, "const bool");
    set_boolean(val);
  }
  value_t(const long val) {
    TRACE_CTOR(value_t, "const long");
    set_long(val);
  }
  value_t(const moment_t val) {
    TRACE_CTOR(value_t, "const moment_t");
    set_datetime(val);
  }
  value_t(const double val) {
    TRACE_CTOR(value_t, "const double");
    set_amount(val);
  }
  value_t(const unsigned long val) {
    TRACE_CTOR(value_t, "const unsigned long");
    set_amount(val);
  }
  explicit value_t(const string& val, bool literal = false) {
    TRACE_CTOR(value_t, "const string&, bool");
    if (literal)
      set_string(val);
    else
      set_amount(amount_t(val));
  }
  explicit value_t(const char * val, bool literal = false) {
    TRACE_CTOR(value_t, "const char *");
    if (literal)
      set_string(val);
    else
      set_amount(amount_t(val));
  }
  value_t(const amount_t& val) {
    TRACE_CTOR(value_t, "const amount_t&");
    set_amount(val);
  }
  value_t(const balance_t& val) {
    TRACE_CTOR(value_t, "const balance_t&");
    set_balance(val);
  }
  value_t(const balance_pair_t& val) {
    TRACE_CTOR(value_t, "const balance_pair_t&");
    set_balance_pair(val);
  }
  value_t(const sequence_t& val) {
    TRACE_CTOR(value_t, "const sequence_t&");
    set_sequence(val);
  }
  value_t(xml::node_t * xml_node) {
    TRACE_CTOR(value_t, "xml::node_t *");
    set_xml_node(xml_node);
  }
  template <typename T>
  value_t(T * item) {
    TRACE_CTOR(value_t, "T *");
    set_pointer(item);
  }
  ~value_t() {
    TRACE_DTOR(value_t);
  }

  value_t& operator=(const value_t& val) {
    if (this == &val || storage == val.storage)
      return *this;
    storage = val.storage;
    return *this;
  }

  /**
   * _dup() makes a private copy of the current value so that it can
   * subsequently be modified.
   *
   * _clear() removes our pointer to the current value and initializes
   * a new value for things to be stored in.
   */
  void _dup() {
    assert(storage);
    if (storage->refc > 1)
      storage = new storage_t(*storage.get());
  }
  void _clear() {
    if (! storage || storage->refc > 1)
      storage = new storage_t;
    else
      storage->destroy();
  }
  void _reset() {
    if (storage) {
      storage->destroy();
      storage = intrusive_ptr<storage_t>();
    }
  }

  operator bool() const;

  bool is_null() const {
    if (! storage) {
      return true;
    } else {
      assert(! is_type(VOID));
      return false;
    }
  }
  type_t type() const {
    type_t result = storage ? storage->type : VOID;
    assert(result >= VOID && result <= POINTER);
    return result;
  }

private:
  bool is_type(type_t _type) const {
    return type() == _type;
  }
  void set_type(type_t new_type) {
    assert(new_type >= VOID && new_type <= POINTER);
    if (new_type == VOID) {
      _reset();
      assert(is_null());
    } else {
      _clear();
      storage->type = new_type;
      assert(is_type(new_type));
    }
  }

public:
  bool is_boolean() const {
    return is_type(BOOLEAN);
  }
  bool& as_boolean_lval() {
    assert(is_boolean());
    _dup();
    return *(bool *) storage->data;
  }
  const bool& as_boolean() const {
    assert(is_boolean());
    return *(bool *) storage->data;
  }
  void set_boolean(const bool val) {
    set_type(BOOLEAN);
    storage = val ? true_value : false_value;
  }

  bool is_long() const {
    return is_type(INTEGER);
  }
  long& as_long_lval() {
    assert(is_long());
    _dup();
    return *(long *) storage->data;
  }
  const long& as_long() const {
    assert(is_long());
    return *(long *) storage->data;
  }
  void set_long(const long val) {
    set_type(INTEGER);
    *(long *) storage->data = val;
  }

  bool is_datetime() const {
    return is_type(DATETIME);
  }
  moment_t& as_datetime_lval() {
    assert(is_datetime());
    _dup();
    return *(moment_t *) storage->data;
  }
  const moment_t& as_datetime() const {
    assert(is_datetime());
    return *(moment_t *) storage->data;
  }
  void set_datetime(const moment_t& val) {
    set_type(DATETIME);
    new((moment_t *) storage->data) moment_t(val);
  }

  bool is_amount() const {
    return is_type(AMOUNT);
  }
  amount_t& as_amount_lval() {
    assert(is_amount());
    _dup();
    return *(amount_t *) storage->data;
  }
  const amount_t& as_amount() const {
    assert(is_amount());
    return *(amount_t *) storage->data;
  }
  void set_amount(const amount_t& val) {
    set_type(AMOUNT);
    new((amount_t *) storage->data) amount_t(val);
  }

  bool is_balance() const {
    return is_type(BALANCE);
  }
  balance_t& as_balance_lval() {
    assert(is_balance());
    _dup();
    return *(balance_t *) storage->data;
  }
  const balance_t& as_balance() const {
    assert(is_balance());
    return *(balance_t *) storage->data;
  }
  void set_balance(const balance_t& val) {
    set_type(BALANCE);
    new((balance_t *) storage->data) balance_t(val);
  }

  bool is_balance_pair() const {
    return is_type(BALANCE_PAIR);
  }
  balance_pair_t& as_balance_pair_lval() {
    assert(is_balance_pair());
    _dup();
    return *(balance_pair_t *) storage->data;
  }
  const balance_pair_t& as_balance_pair() const {
    assert(is_balance_pair());
    return *(balance_pair_t *) storage->data;
  }
  void set_balance_pair(const balance_pair_t& val) {
    set_type(BALANCE_PAIR);
    new((balance_pair_t *) storage->data) balance_pair_t(val);
  }

  bool is_string() const {
    return is_type(STRING);
  }
  string& as_string_lval() {
    assert(is_string());
    _dup();
    return *(string *) storage->data;
  }
  const string& as_string() const {
    assert(is_string());
    return *(string *) storage->data;
  }
  void set_string(const string& val = "") {
    set_type(STRING);
    new((string *) storage->data) string(val);
  }

  bool is_sequence() const {
    return is_type(SEQUENCE);
  }
  sequence_t& as_sequence_lval() {
    assert(is_sequence());
    _dup();
    return *(sequence_t *) storage->data;
  }
  const sequence_t& as_sequence() const {
    assert(is_sequence());
    return *(sequence_t *) storage->data;
  }
  void set_sequence(const sequence_t& val) {
    set_type(SEQUENCE);
    new((sequence_t *) storage->data) sequence_t(val);
  }

  bool is_xml_node() const {
    return is_type(XML_NODE);
  }
  xml::node_t *& as_xml_node_lval() {
    assert(is_xml_node());
    _dup();
    return *(xml::node_t **) storage->data;
  }
  xml::node_t * as_xml_node() const {
    assert(is_xml_node());
    return *(xml::node_t **) storage->data;
  }
  void set_xml_node(xml::node_t * val) {
    set_type(XML_NODE);
    *(xml::node_t **) storage->data = val;
  }

  bool is_pointer() const {
    return is_type(POINTER);
  }
  boost::any& as_any_pointer_lval() {
    assert(is_pointer());
    _dup();
    return *(boost::any *) storage->data;
  }
  template <typename T>
  T *& as_pointer_lval() {
    assert(is_pointer());
    _dup();
    return any_cast<T *>(*(boost::any *) storage->data);
  }
  template <typename T>
  T& as_ref_lval() {
    assert(is_pointer());
    _dup();
    return *any_cast<T *>(*(boost::any *) storage->data);
  }
  boost::any as_any_pointer() const {
    assert(is_pointer());
    return *(boost::any *) storage->data;
  }
  template <typename T>
  T * as_pointer() const {
    assert(is_pointer());
    return any_cast<T *>(*(boost::any *) storage->data);
  }
  template <typename T>
  T& as_ref() const {
    assert(is_pointer());
    return *any_cast<T *>(*(boost::any *) storage->data);
  }
  void set_any_pointer(const boost::any& val) {
    set_type(POINTER);
    new((boost::any *) storage->data) boost::any(val);
  }
  template <typename T>
  void set_pointer(T * val) {
    set_type(POINTER);
    new((boost::any *) storage->data) boost::any(val);
  }

  bool		 to_boolean() const;
  long		 to_long() const;
  moment_t       to_datetime() const;
  amount_t	 to_amount() const;
  balance_t	 to_balance() const;
  balance_pair_t to_balance_pair() const;
  string	 to_string() const;
  sequence_t     to_sequence() const;

  value_t simplify() const {
    value_t temp = *this;
    temp.in_place_simplify();
    return temp;
  }
  void in_place_simplify();

  value_t& operator[](const int index) {
    assert(! is_null());
    if (is_sequence())
      return as_sequence_lval()[index];
    else if (index == 0)
      return *this;

    assert(false);
    static value_t null;
    return null;
  }
  const value_t& operator[](const int index) const {
    assert(! is_null());
    if (is_sequence())
      return as_sequence()[index];
    else if (index == 0)
      return *this;

    assert(false);
    static value_t null;
    return null;
  }

  void push_back(const value_t& val) {
    if (is_null()) {
      *this = val;
    } else {
      if (! is_sequence())
	in_place_cast(SEQUENCE);

      if (! val.is_sequence())
	as_sequence_lval().push_back(val);
      else
	std::copy(val.as_sequence().begin(), val.as_sequence().end(),
		  as_sequence_lval().end());
    }
  }

  void pop_back() {
    assert(! is_null());

    if (! is_sequence()) {
      _reset();
    } else {
      as_sequence_lval().pop_back();

      std::size_t new_size = as_sequence().size();
      if (new_size == 0)
	_reset();
      else if (new_size == 1)
	*this = as_sequence().front();
    }
  }

  const std::size_t size() const {
    if (is_null())
      return 0;
    else if (is_sequence())
      return as_sequence().size();
    else
      return 1;
  }

  value_t& operator+=(const value_t& val);
  value_t& operator-=(const value_t& val);
  value_t& operator*=(const value_t& val);
  value_t& operator/=(const value_t& val);

  bool operator==(const value_t& val) const;
  bool operator<(const value_t& val) const;
#if 0
  bool operator>(const value_t& val) const;
#endif

  string label(optional<type_t> the_type = none) const {
    switch (the_type ? *the_type : type()) {
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
    assert(false);
    return "<invalid>";
  }
  
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
	       const optional<amount_t>& cost = none);
  value_t  value(const optional<moment_t>& moment = none) const;

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

#define NULL_VALUE (value_t())

std::ostream& operator<<(std::ostream& out, const value_t& val);

DECLARE_EXCEPTION(value_error);

} // namespace ledger

#endif // _VALUE_H
