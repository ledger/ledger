#ifndef _VALUE_H
#define _VALUE_H

#include "amount.h"
#include "balance.h"
#include "error.h"

#include <deque>
#include <exception>

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
{
 public:
  typedef std::deque<value_t> sequence_t;

  char data[sizeof(balance_pair_t)];

  enum type_t {
    BOOLEAN,
    INTEGER,
    DATETIME,
    AMOUNT,
    BALANCE,
    BALANCE_PAIR,
    STRING,
    XML_NODE,
    POINTER,
    SEQUENCE
  } type;

  value_t() {
    TRACE_CTOR("value_t()");
    *((long *) data) = 0;
    type = INTEGER;
  }

  value_t(const value_t& val) : type(INTEGER) {
    TRACE_CTOR("value_t(copy)");
    *this = val;
  }
  value_t(const bool val) {
    TRACE_CTOR("value_t(const bool)");
    *((bool *) data) = val;
    type = BOOLEAN;
  }
  value_t(const long val) {
    TRACE_CTOR("value_t(const long)");
    *((long *) data) = val;
    type = INTEGER;
  }
  value_t(const ptime val) {
    TRACE_CTOR("value_t(const ptime)");
    *((ptime *) data) = val;
    type = DATETIME;
  }
  value_t(const unsigned long val) {
    TRACE_CTOR("value_t(const unsigned long)");
    new((amount_t *) data) amount_t(val);
    type = AMOUNT;
  }
  value_t(const double val) {
    TRACE_CTOR("value_t(const double)");
    new((amount_t *) data) amount_t(val);
    type = AMOUNT;
  }
  value_t(const std::string& val, bool literal = false) {
    TRACE_CTOR("value_t(const std::string&, bool)");
    if (literal) {
      type = INTEGER;
      set_string(val);
    } else {
      new((amount_t *) data) amount_t(val);
      type = AMOUNT;
    }
  }
  value_t(const char * val) {
    TRACE_CTOR("value_t(const char *)");
    new((amount_t *) data) amount_t(val);
    type = AMOUNT;
  }
  value_t(const amount_t& val) {
    TRACE_CTOR("value_t(const amount_t&)");
    new((amount_t *)data) amount_t(val);
    type = AMOUNT;
  }
  value_t(const balance_t& val) : type(INTEGER) {
    TRACE_CTOR("value_t(const balance_t&)");
    *this = val;
  }
  value_t(const balance_pair_t& val) : type(INTEGER) {
    TRACE_CTOR("value_t(const balance_pair_t&)");
    *this = val;
  }
  value_t(xml::node_t * xml_node) : type(INTEGER) { // gets set in =
    TRACE_CTOR("value_t(xml::node_t *)");
    *this = xml_node;
  }
  value_t(void * item) : type(INTEGER) { // gets set in =
    TRACE_CTOR("value_t(void *)");
    *this = item;
  }
  value_t(sequence_t * seq) : type(INTEGER) { // gets set in =
    TRACE_CTOR("value_t(sequence_t *)");
    *this = seq;
  }

  ~value_t() {
    TRACE_DTOR("value_t");
    destroy();
  }

  void destroy();
  void simplify();

  value_t& operator=(const value_t& val);
  value_t& operator=(const bool val) {
    if ((bool *) data != &val) {
      destroy();
      *((bool *) data) = val;
      type = BOOLEAN;
    }
    return *this;
  }
  value_t& operator=(const long val) {
    if ((long *) data != &val) {
      destroy();
      *((long *) data) = val;
      type = INTEGER;
    }
    return *this;
  }
  value_t& operator=(const ptime val) {
    if ((ptime *) data != &val) {
      destroy();
      *((ptime *) data) = val;
      type = DATETIME;
    }
    return *this;
  }
  value_t& operator=(const unsigned long val) {
    return *this = amount_t(val);
  }
  value_t& operator=(const double val) {
    return *this = amount_t(val);
  }
  value_t& operator=(const std::string& val) {
    return *this = amount_t(val);
  }
  value_t& operator=(const char * val) {
    return *this = amount_t(val);
  }
  value_t& operator=(const amount_t& val) {
    if (type == AMOUNT &&
	(amount_t *) data == &val)
      return *this;

    if (val.realzero()) {
      return *this = 0L;
    } else {
      destroy();
      new((amount_t *)data) amount_t(val);
      type = AMOUNT;
    }
    return *this;
  }
  value_t& operator=(const balance_t& val) {
    if (type == BALANCE &&
	(balance_t *) data == &val)
      return *this;

    if (val.realzero()) {
      return *this = 0L;
    }
    else if (val.amounts.size() == 1) {
      return *this = (*val.amounts.begin()).second;
    }
    else {
      destroy();
      new((balance_t *)data) balance_t(val);
      type = BALANCE;
      return *this;
    }
  }
  value_t& operator=(const balance_pair_t& val) {
    if (type == BALANCE_PAIR &&
	(balance_pair_t *) data == &val)
      return *this;

    if (val.realzero()) {
      return *this = 0L;
    }
    else if (! val.cost) {
      return *this = val.quantity;
    }
    else {
      destroy();
      new((balance_pair_t *)data) balance_pair_t(val);
      type = BALANCE_PAIR;
      return *this;
    }
  }
  value_t& operator=(xml::node_t * xml_node) {
    assert(xml_node);
    if (type == XML_NODE && *(xml::node_t **) data == xml_node)
      return *this;

    if (! xml_node) {
      type = XML_NODE;
      return *this = 0L;
    }
    else {
      destroy();
      *(xml::node_t **)data = xml_node;
      type = XML_NODE;
      return *this;
    }
  }
  value_t& operator=(void * item) {
    assert(item);
    if (type == POINTER && *(void **) data == item)
      return *this;

    if (! item) {
      type = POINTER;
      return *this = 0L;
    }
    else {
      destroy();
      *(void **)data = item;
      type = POINTER;
      return *this;
    }
  }
  value_t& operator=(sequence_t * seq) {
    assert(seq);
    if (type == SEQUENCE && *(sequence_t **) data == seq)
      return *this;

    if (! seq) {
      type = SEQUENCE;
      return *this = 0L;
    }
    else {
      destroy();
      *(sequence_t **)data = seq;
      type = SEQUENCE;
      return *this;
    }
  }

  value_t& set_string(const std::string& str = "") {
    if (type != STRING) {
      destroy();
      *(std::string **) data = new std::string(str);
      type = STRING;
    } else {
      **(std::string **) data = str;
    }
    return *this;
  }

  bool		 to_boolean() const;
  long		 to_integer() const;
  ptime          to_datetime() const;
  amount_t	 to_amount() const;
  balance_t	 to_balance() const;
  balance_pair_t to_balance_pair() const;
  std::string	 to_string() const;
  xml::node_t *	 to_xml_node() const;
  void *	 to_pointer() const;
  sequence_t *	 to_sequence() const;

  value_t& operator[](const int index) {
    sequence_t * seq = to_sequence();
    assert(seq);
    return (*seq)[index];
  }

  void push_back(const value_t& val) {
    sequence_t * seq = to_sequence();
    assert(seq);
    return seq->push_back(val);
  }

  std::size_t size() const {
    sequence_t * seq = to_sequence();
    assert(seq);
    return seq->size();
  }

  value_t& operator+=(const value_t& val);
  value_t& operator-=(const value_t& val);
  value_t& operator*=(const value_t& val);
  value_t& operator/=(const value_t& val);

  template <typename T>
  value_t& operator+=(const T& val) {
    return *this += value_t(val);
  }
  template <typename T>
  value_t& operator-=(const T& val) {
    return *this -= value_t(val);
  }
  template <typename T>
  value_t& operator*=(const T& val) {
    return *this *= value_t(val);
  }
  template <typename T>
  value_t& operator/=(const T& val) {
    return *this /= value_t(val);
  }

  value_t operator+(const value_t& val) {
    value_t temp(*this);
    temp += val;
    return temp;
  }
  value_t operator-(const value_t& val) {
    value_t temp(*this);
    temp -= val;
    return temp;
  }
  value_t operator*(const value_t& val) {
    value_t temp(*this);
    temp *= val;
    return temp;
  }
  value_t operator/(const value_t& val) {
    value_t temp(*this);
    temp /= val;
    return temp;
  }

  template <typename T>
  value_t operator+(const T& val) {
    return *this + value_t(val);
  }
  template <typename T>
  value_t operator-(const T& val) {
    return *this - value_t(val);
  }
  template <typename T>
  value_t operator*(const T& val) {
    return *this * value_t(val);
  }
  template <typename T>
  value_t operator/(const T& val) {
    return *this / value_t(val);
  }

  bool operator<(const value_t& val);
  bool operator<=(const value_t& val);
  bool operator>(const value_t& val);
  bool operator>=(const value_t& val);
  bool operator==(const value_t& val);
  bool operator!=(const value_t& val) {
    return ! (*this == val);
  }

  template <typename T>
  bool operator<(const T& val) {
    return *this < value_t(val);
  }
  template <typename T>
  bool operator<=(const T& val) {
    return *this <= value_t(val);
  }
  template <typename T>
  bool operator>(const T& val) {
    return *this > value_t(val);
  }
  template <typename T>
  bool operator>=(const T& val) {
    return *this >= value_t(val);
  }
  template <typename T>
  bool operator==(const T& val) {
    return *this == value_t(val);
  }
  template <typename T>
  bool operator!=(const T& val) {
    return ! (*this == val);
  }

  template <typename T>
  operator T() const;

  void in_place_negate();
  value_t negate() const {
    value_t temp = *this;
    temp.in_place_negate();
    return temp;
  }
  value_t operator-() const {
    return negate();
  }

  bool realzero() const {
    switch (type) {
    case BOOLEAN:
      return ! *((bool *) data);
    case INTEGER:
      return *((long *) data) == 0;
    case DATETIME:
      return ((ptime *) data)->is_not_a_date_time();
    case AMOUNT:
      return ((amount_t *) data)->realzero();
    case BALANCE:
      return ((balance_t *) data)->realzero();
    case BALANCE_PAIR:
      return ((balance_pair_t *) data)->realzero();
    case STRING:
      return ((std::string *) data)->empty();
    case XML_NODE:
    case POINTER:
    case SEQUENCE:
      return *(void **) data == NULL;

    default:
      assert(0);
      break;
    }
    assert(0);
    return 0;
  }

  void    in_place_abs();
  value_t abs() const;
  void    in_place_cast(type_t cast_type);
  value_t cost() const;
  value_t price() const;
  value_t date() const;

  value_t cast(type_t cast_type) const {
    value_t temp(*this);
    temp.in_place_cast(cast_type);
    return temp;
  }

  value_t strip_annotations(const bool keep_price = amount_t::keep_price,
			    const bool keep_date  = amount_t::keep_date,
			    const bool keep_tag   = amount_t::keep_tag) const;

  value_t& add(const amount_t&  amount, const amount_t * cost = NULL);
  value_t  value(const ptime& moment) const;
  void     in_place_reduce();

  value_t reduce() const {
    value_t temp(*this);
    temp.in_place_reduce();
    return temp;
  }

  value_t round() const;
  value_t unround() const;

  void write(std::ostream& out, const int first_width,
	     const int latter_width = -1) const;
};

#define DEFINE_VALUE_OPERATORS(T, OP)				\
inline value_t operator OP(const T& val, const value_t& obj) {	\
  return value_t(val) OP obj;					\
}

DEFINE_VALUE_OPERATORS(bool, ==)
DEFINE_VALUE_OPERATORS(bool, !=)

DEFINE_VALUE_OPERATORS(long, +)
DEFINE_VALUE_OPERATORS(long, -)
DEFINE_VALUE_OPERATORS(long, *)
DEFINE_VALUE_OPERATORS(long, /)
DEFINE_VALUE_OPERATORS(long, <)
DEFINE_VALUE_OPERATORS(long, <=)
DEFINE_VALUE_OPERATORS(long, >)
DEFINE_VALUE_OPERATORS(long, >=)
DEFINE_VALUE_OPERATORS(long, ==)
DEFINE_VALUE_OPERATORS(long, !=)

DEFINE_VALUE_OPERATORS(amount_t, +)
DEFINE_VALUE_OPERATORS(amount_t, -)
DEFINE_VALUE_OPERATORS(amount_t, *)
DEFINE_VALUE_OPERATORS(amount_t, /)
DEFINE_VALUE_OPERATORS(amount_t, <)
DEFINE_VALUE_OPERATORS(amount_t, <=)
DEFINE_VALUE_OPERATORS(amount_t, >)
DEFINE_VALUE_OPERATORS(amount_t, >=)
DEFINE_VALUE_OPERATORS(amount_t, ==)
DEFINE_VALUE_OPERATORS(amount_t, !=)

DEFINE_VALUE_OPERATORS(balance_t, +)
DEFINE_VALUE_OPERATORS(balance_t, -)
DEFINE_VALUE_OPERATORS(balance_t, *)
DEFINE_VALUE_OPERATORS(balance_t, /)
DEFINE_VALUE_OPERATORS(balance_t, <)
DEFINE_VALUE_OPERATORS(balance_t, <=)
DEFINE_VALUE_OPERATORS(balance_t, >)
DEFINE_VALUE_OPERATORS(balance_t, >=)
DEFINE_VALUE_OPERATORS(balance_t, ==)
DEFINE_VALUE_OPERATORS(balance_t, !=)

DEFINE_VALUE_OPERATORS(balance_pair_t, +)
DEFINE_VALUE_OPERATORS(balance_pair_t, -)
DEFINE_VALUE_OPERATORS(balance_pair_t, *)
DEFINE_VALUE_OPERATORS(balance_pair_t, /)
DEFINE_VALUE_OPERATORS(balance_pair_t, <)
DEFINE_VALUE_OPERATORS(balance_pair_t, <=)
DEFINE_VALUE_OPERATORS(balance_pair_t, >)
DEFINE_VALUE_OPERATORS(balance_pair_t, >=)
DEFINE_VALUE_OPERATORS(balance_pair_t, ==)
DEFINE_VALUE_OPERATORS(balance_pair_t, !=)

template <typename T>
value_t::operator T() const
{
  switch (type) {
  case BOOLEAN:
    return *(bool *) data;
  case INTEGER:
    return *(long *) data;
  case DATETIME:
    return *(ptime *) data;
  case AMOUNT:
    return *(amount_t *) data;
  case BALANCE:
    return *(balance_t *) data;
  case STRING:
    return **(std::string **) data;
  case XML_NODE:
    return *(xml::node_t **) data;
  case POINTER:
    return *(void **) data;
  case SEQUENCE:
    return *(sequence_t **) data;

  default:
    assert(0);
    break;
  }
  assert(0);
  return 0;
}

template <> value_t::operator bool() const;
template <> value_t::operator long() const;
template <> value_t::operator ptime() const;
template <> value_t::operator double() const;
template <> value_t::operator std::string() const;

std::ostream& operator<<(std::ostream& out, const value_t& val);

class value_context : public error_context
{
  value_t * bal;
 public:
  value_context(const value_t& _bal,
		const std::string& desc = "") throw();
  virtual ~value_context() throw();

  virtual void describe(std::ostream& out) const throw();
};

class value_error : public error {
 public:
  value_error(const std::string& _reason,
	      error_context * _ctxt = NULL) throw()
    : error(_reason, _ctxt) {}
  virtual ~value_error() throw() {}
};

} // namespace ledger

#endif // _VALUE_H
