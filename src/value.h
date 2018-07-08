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
 * @addtogroup math
 */

/**
 * @file   value.h
 * @author John Wiegley
 *
 * @ingroup math
 *
 * @brief  Abstract dynamic type representing various numeric types
 *
 * A value_t object can be one of many types, and changes its type
 * dynamically based on how it is used.  For example, if you assign
 * the number 10 to a value object, it's internal type will be
 * INTEGER.
 */
#ifndef _VALUE_H
#define _VALUE_H

#include "balance.h"            // includes amount.h
#include "mask.h"

namespace ledger {

DECLARE_EXCEPTION(value_error, std::runtime_error);

class scope_t;

/**
 * @class value_t
 *
 * @brief Dynamic type representing various numeric types.
 *
 * The following type is a polymorphous value type used solely for
 * performance reasons.  The alternative is to compute value
 * expressions (valexpr.cc) in terms of the largest data type,
 * balance_t. This was found to be prohibitively expensive, especially
 * when large logic chains were involved, since many temporary
 * allocations would occur for every operator.  With value_t, and the
 * fact that logic chains only need boolean values to continue, no
 * memory allocations need to take place at all.
 */
class value_t
  : public ordered_field_operators<value_t,
           equality_comparable<value_t, balance_t,
           additive<value_t, balance_t,
           multiplicative<value_t, balance_t,
           ordered_field_operators<value_t, amount_t,
           ordered_field_operators<value_t, double,
           ordered_field_operators<value_t, unsigned long,
           ordered_field_operators<value_t, long> > > > > > > >
{
public:
  /**
   * The sequence_t member type abstracts the type used to represent a
   * resizable "array" of value_t objects.
   */
  typedef ptr_deque<value_t>          sequence_t;
  typedef sequence_t::iterator        iterator;
  typedef sequence_t::const_iterator  const_iterator;
  typedef sequence_t::difference_type difference_type;

  /**
   * type_t gives the type of the data contained or referenced by a
   * value_t object.  Use the type() method to get a value of type
   * type_t.
   */
  enum type_t {
    VOID,                       // a null value (i.e., uninitialized)
    BOOLEAN,                    // a boolean
    DATETIME,                   // a date and time (Boost posix_time)
    DATE,                       // a date (Boost gregorian::date)
    INTEGER,                    // a signed integer value
    AMOUNT,                     // a ledger::amount_t
    BALANCE,                    // a ledger::balance_t
    STRING,                     // a string object
    MASK,                       // a regular expression mask
    SEQUENCE,                   // a vector of value_t objects
    SCOPE,                      // a pointer to a scope
    ANY                         // a pointer to an arbitrary object
  };

  class storage_t
  {
    friend class value_t;

    /**
     * The `data' member holds the actual bytes relating to whatever
     * has been stuffed into this storage object.  There is a set of
     * asserts in value.cc to guarantee that the sizeof expression
     * used here is indeed at least as big as the largest object that
     * will ever be copied into `data'.
     *
     * The `type' member holds the value_t::type_t value representing
     * the type of the object stored.
     */
    variant<bool,               // BOOLEAN
            datetime_t,         // DATETIME
            date_t,             // DATE
            long,               // INTEGER
            amount_t,           // AMOUNT
            balance_t *,        // BALANCE
            string,             // STRING
            mask_t,             // MASK
            sequence_t *,       // SEQUENCE
            scope_t *,          // SCOPE
            boost::any          // ANY
            > data;

    type_t type;

    /**
     * `refc' holds the current reference count for each storage_t
     * object.
     */
    mutable int refc;

    /**
     * Constructor.  Since all storage object are assigned to after
     * construction, the only constructors allowed are explicit, and
     * copy (see below).  The default starting type is VOID, which
     * should rarely ever be seen in practice, since the first thing
     * that value_t typically does is to assign a valid value.
     */
    explicit storage_t() : type(VOID), refc(0) {
      TRACE_CTOR(value_t::storage_t, "");
    }

  public:                       // so `checked_delete' can access it
    /**
     * Destructor.  Must only be called when the reference count has
     * reached zero.  The `destroy' method is used to do the actual
     * cleanup of the data, since it's quite possible for `destroy' to
     * be called while the object is still active -- to clear the
     * stored data for subsequent reuse of the storage_t object.
     */
    ~storage_t() {
      TRACE_DTOR(value_t::storage_t);
      VERIFY(refc == 0);
      destroy();
    }

  private:
    /**
     * Assignment and copy operators.  These are called when making a
     * new copy of a storage object in order to modify the copy.
     */
    explicit storage_t(const storage_t& rhs)
      : type(rhs.type), refc(0) {
      *this = rhs;
      TRACE_CTOR(value_t::storage_t, "copy");
    }
    storage_t& operator=(const storage_t& rhs);

    /**
     * Reference counting methods.  The intrusive_ptr_* methods are
     * used by boost::intrusive_ptr to manage the calls to acquire and
     * release.
     */
    void acquire() const {
      DEBUG("value.storage.refcount",
             "Acquiring " << this << ", refc now " << refc + 1);
      VERIFY(refc >= 0);
      refc++;
    }
    void release() const {
      DEBUG("value.storage.refcount",
             "Releasing " << this << ", refc now " << refc - 1);
      VERIFY(refc > 0);
      if (--refc == 0)
        checked_delete(this);
    }

    friend inline void intrusive_ptr_add_ref(value_t::storage_t * storage_ptr) {
      storage_ptr->acquire();
    }
    friend inline void intrusive_ptr_release(value_t::storage_t * storage_ptr) {
      storage_ptr->release();
    }

    void destroy() {
      DEBUG("value.storage.refcount", "Destroying " << this);
      switch (type) {
      case VOID:
        return;
      case BALANCE:
        checked_delete(boost::get<balance_t *>(data));
        break;
      case SEQUENCE:
        checked_delete(boost::get<sequence_t *>(data));
        break;
      default:
        break;
      }
      data = false;
      type = VOID;
    }
  };

private:
  /**
   * The actual data for each value_t is kept in reference counted storage.
   * Data is modified using a copy-on-write policy.
   */
  intrusive_ptr<storage_t> storage;

  /**
   * Make a private copy of the current value (if necessary) so it can
   * subsequently be modified.
   */
  void _dup() {
    if (storage && storage->refc > 1)
      storage = new storage_t(*storage.get());
  }

  /**
   * Because boolean "true" and "false" are so common, a pair of static
   * references are kept to prevent the creation of throwaway storage_t
   * objects just to represent these two common values.
   */
  static intrusive_ptr<storage_t> true_value;
  static intrusive_ptr<storage_t> false_value;

public:
  static void initialize();
  static void shutdown();

public:
  /**
   * Constructors.  value_t objects may be constructed from almost any
   * value type that they can contain, including variations on those
   * types (such as long, unsigned long, etc).  The ordering of the
   * methods here reflects the ordering of the constants in type_t
   * above.
   *
   * One constructor of special note is that taking a string or
   * character pointer as an argument.  Because value_t("$100") is
   * interpreted as a commoditized amount, the form value_t("$100",
   * true) is required to represent the literal string "$100", and not
   * the amount "one hundred dollars".
   */
  value_t() {
    TRACE_CTOR(value_t, "");
  }

  value_t(const bool val) {
    set_boolean(val);
    TRACE_CTOR(value_t, "const bool");
  }

  value_t(const datetime_t& val) {
    set_datetime(val);
    TRACE_CTOR(value_t, "const datetime_t&");
  }
  value_t(const date_t& val) {
    set_date(val);
    TRACE_CTOR(value_t, "const date_t&");
  }

  value_t(const long val) {
    set_long(val);
    TRACE_CTOR(value_t, "const long");
  }
  value_t(const unsigned long val) {
    set_amount(val);
    TRACE_CTOR(value_t, "const unsigned long");
  }
  value_t(const double val) {
    set_amount(val);
    TRACE_CTOR(value_t, "const double");
  }
  value_t(const amount_t& val) {
    set_amount(val);
    TRACE_CTOR(value_t, "const amount_t&");
  }
  value_t(const balance_t& val) {
    set_balance(val);
    TRACE_CTOR(value_t, "const balance_t&");
  }
  value_t(const mask_t& val) {
    set_mask(val);
    TRACE_CTOR(value_t, "const mask_t&");
  }

  explicit value_t(const string& val, bool literal = false) {
    if (literal)
      set_string(val);
    else
      set_amount(amount_t(val));

    TRACE_CTOR(value_t, "const string&, bool");
  }
  explicit value_t(const char * val, bool literal = false) {
    if (literal)
      set_string(val);
    else
      set_amount(amount_t(val));

    TRACE_CTOR(value_t, "const char *");
  }

  value_t(const sequence_t& val) {
    set_sequence(val);
    TRACE_CTOR(value_t, "const sequence_t&");
  }

  explicit value_t(scope_t * item) {
    set_scope(item);
    TRACE_CTOR(value_t, "scope_t *");
  }
#if 0
  template <typename T>
  explicit value_t(T& item) {
    set_any(item);
    TRACE_CTOR(value_t, "T&");
  }
#endif

  /**
   * Destructor.  This does not do anything, because the intrusive_ptr
   * that refers to our storage object will decrease its reference
   * count itself upon destruction.
   */
  ~value_t() {
    TRACE_DTOR(value_t);
  }

  /**
   * Assignment and copy operators.  Values are cheaply copied by
   * simply creating another reference to the other value's storage
   * object.  A true copy is only ever made prior to modification.
   */
  value_t(const value_t& val) {
    *this = val;
    TRACE_CTOR(value_t, "copy");
  }
  value_t& operator=(const value_t& val) {
    if (! (this == &val || storage == val.storage))
      storage = val.storage;
    return *this;
  }

  /**
   * Comparison operators.  Values can be compared to other values
   */
  bool is_equal_to(const value_t& val) const;
  bool is_less_than(const value_t& val) const;
  bool is_greater_than(const value_t& val) const;

  template <typename T>
  bool operator==(const T& amt) const {
    return is_equal_to(amt);
  }
  template <typename T>
  bool operator<(const T& amt) const {
    return is_less_than(amt);
  }
  template <typename T>
  bool operator>(const T& amt) const {
    return is_greater_than(amt);
  }

  /**
   * Binary arithmetic operators.
   *
   * add(amount_t, optional<amount_t>) allows for the possibility of
   * adding both an amount and its cost in a single operation.
   * Otherwise, there is no way to separately represent the "cost"
   * part of an amount addition statement.
   */
  value_t& operator+=(const value_t& val);
  value_t& operator-=(const value_t& val);
  value_t& operator*=(const value_t& val);
  value_t& operator/=(const value_t& val);

  /**
   * Unary arithmetic operators.
   */
  value_t negated() const {
    value_t temp = *this;
    temp.in_place_negate();
    return temp;
  }
  void    in_place_negate();    // exists for efficiency's sake
  void    in_place_not();       // exists for efficiency's sake

  value_t operator-() const {
    return negated();
  }

  value_t abs() const;

  value_t rounded() const {
    value_t temp(*this);
    temp.in_place_round();
    return temp;
  }
  void in_place_round();

  value_t roundto(int places) const {
    value_t temp(*this);
    temp.in_place_roundto(places);
    return temp;
  }
  void in_place_roundto(int places);

  value_t truncated() const {
    value_t temp(*this);
    temp.in_place_truncate();
    return temp;
  }
  void in_place_truncate();

  value_t floored() const {
    value_t temp(*this);
    temp.in_place_floor();
    return temp;
  }
  void in_place_floor();

  value_t ceilinged() const {
    value_t temp(*this);
    temp.in_place_ceiling();
    return temp;
  }
  void in_place_ceiling();

  value_t unrounded() const {
    value_t temp(*this);
    temp.in_place_unround();
    return temp;
  }
  void in_place_unround();

  value_t reduced() const {
    value_t temp(*this);
    temp.in_place_reduce();
    return temp;
  }
  void    in_place_reduce();    // exists for efficiency's sake

  value_t unreduced() const {
    value_t temp(*this);
    temp.in_place_unreduce();
    return temp;
  }
  void    in_place_unreduce();  // exists for efficiency's sake

  // Return the "market value" of a given value at a specific time.
  value_t value(const datetime_t&   moment      = datetime_t(),
                const commodity_t * in_terms_of = NULL) const;

  value_t exchange_commodities(const std::string& commodities,
                               const bool         add_prices = false,
                               const datetime_t&  moment     = datetime_t());

  /**
   * Truth tests.
   */
  operator bool() const;

  bool is_nonzero() const {
    return ! is_zero();
  }

  bool is_realzero() const;
  bool is_zero() const;
  bool is_null() const {
    if (! storage) {
      VERIFY(is_type(VOID));
      return true;
    } else {
      VERIFY(! is_type(VOID));
      return false;
    }
  }

  type_t type() const {
    return storage ? storage->type : VOID;
  }
  bool is_type(type_t _type) const {
    return type() == _type;
  }

private:
  void set_type(type_t new_type);

public:
  /**
   * Data manipulation methods.  A value object may be truth tested for the
   * existence of every type it can contain:
   *
   * is_boolean()
   * is_long()
   * is_datetime()
   * is_date()
   * is_amount()
   * is_balance()
   * is_string()
   * is_mask()
   * is_sequence()
   * is_any()
   *
   * There are corresponding as_*() methods that represent a value as a
   * reference to its underlying type.  For example, as_long() returns a
   * reference to a "const long".
   *
   * There are also as_*_lval() methods, which represent the underlying data
   * as a reference to a non-const type.  The difference here is that an
   * _lval() call causes the underlying data to be fully copied before the
   * resulting reference is returned.
   *
   * Lastly, there are corresponding set_*(data) methods for directly
   * assigning data of a particular type, rather than using the regular
   * assignment operator (whose implementation simply calls the various set_
   * methods).
   */
  bool is_boolean() const {
    return is_type(BOOLEAN);
  }
  bool& as_boolean_lval() {
    VERIFY(is_boolean());
    _dup();
    return boost::get<bool>(storage->data);
  }
  const bool& as_boolean() const {
    VERIFY(is_boolean());
    return boost::get<bool>(storage->data);
  }
  void set_boolean(const bool val) {
    set_type(BOOLEAN);
    storage = val ? true_value : false_value;
  }

  bool is_datetime() const {
    return is_type(DATETIME);
  }
  datetime_t& as_datetime_lval() {
    VERIFY(is_datetime());
    _dup();
    return boost::get<datetime_t>(storage->data);
  }
  const datetime_t& as_datetime() const {
    VERIFY(is_datetime());
    return boost::get<datetime_t>(storage->data);
  }
  void set_datetime(const datetime_t& val) {
    set_type(DATETIME);
    storage->data = val;
  }

  bool is_date() const {
    return is_type(DATE);
  }
  date_t& as_date_lval() {
    VERIFY(is_date());
    _dup();
    return boost::get<date_t>(storage->data);
  }
  const date_t& as_date() const {
    VERIFY(is_date());
    return boost::get<date_t>(storage->data);
  }
  void set_date(const date_t& val) {
    set_type(DATE);
    storage->data = val;
  }

  bool is_long() const {
    return is_type(INTEGER);
  }
  long& as_long_lval() {
    VERIFY(is_long());
    _dup();
    return boost::get<long>(storage->data);
  }
  const long& as_long() const {
    VERIFY(is_long());
    return boost::get<long>(storage->data);
  }
  void set_long(const long val) {
    set_type(INTEGER);
    storage->data = val;
  }

  bool is_amount() const {
    return is_type(AMOUNT);
  }
  amount_t& as_amount_lval() {
    VERIFY(is_amount());
    _dup();
    return boost::get<amount_t>(storage->data);
  }
  const amount_t& as_amount() const {
    VERIFY(is_amount());
    return boost::get<amount_t>(storage->data);
  }
  void set_amount(const amount_t& val) {
    VERIFY(val.valid());
    set_type(AMOUNT);
    storage->data = val;
  }

  bool is_balance() const {
    return is_type(BALANCE);
  }
  balance_t& as_balance_lval() {
    VERIFY(is_balance());
    _dup();
    return *boost::get<balance_t *>(storage->data);
  }
  const balance_t& as_balance() const {
    VERIFY(is_balance());
    return *boost::get<balance_t *>(storage->data);
  }
  void set_balance(const balance_t& val) {
    VERIFY(val.valid());
    set_type(BALANCE);
    storage->data = new balance_t(val);
  }

  bool is_string() const {
    return is_type(STRING);
  }
  string& as_string_lval() {
    VERIFY(is_string());
    _dup();
    return boost::get<string>(storage->data);
  }
  const string& as_string() const {
    VERIFY(is_string());
    return boost::get<string>(storage->data);
  }
  void set_string(const string& val = "") {
    set_type(STRING);
    storage->data = val;
    VERIFY(boost::get<string>(storage->data) == val);
  }
  void set_string(const char * val = "") {
    set_type(STRING);
    storage->data = string(val);
    VERIFY(boost::get<string>(storage->data) == val);
  }

  bool is_mask() const {
    return is_type(MASK);
  }
  mask_t& as_mask_lval() {
    VERIFY(is_mask());
    _dup();
    VERIFY(boost::get<mask_t>(storage->data).valid());
    return boost::get<mask_t>(storage->data);
  }
  const mask_t& as_mask() const {
    VERIFY(is_mask());
    VERIFY(boost::get<mask_t>(storage->data).valid());
    return boost::get<mask_t>(storage->data);
  }
  void set_mask(const string& val) {
    set_type(MASK);
    storage->data = mask_t(val);
  }
  void set_mask(const mask_t& val) {
    set_type(MASK);
    storage->data = val;
  }

  bool is_sequence() const {
    return is_type(SEQUENCE);
  }
  sequence_t& as_sequence_lval() {
    VERIFY(is_sequence());
    _dup();
    return *boost::get<sequence_t *>(storage->data);
  }
  const sequence_t& as_sequence() const {
    VERIFY(is_sequence());
    return *boost::get<sequence_t *>(storage->data);
  }
  void set_sequence(const sequence_t& val) {
    set_type(SEQUENCE);
    storage->data = new sequence_t(val);
  }

  /**
   * Dealing with scope pointers.
   */
  bool is_scope() const {
    return is_type(SCOPE);
  }
  scope_t * as_scope() const {
    VERIFY(is_scope());
    return boost::get<scope_t *>(storage->data);
  }
  void set_scope(scope_t * val) {
    set_type(SCOPE);
    storage->data = val;
  }

  /**
   * Dealing with any type at all is bit involved because we actually
   * deal with typed object.  For example, if you call as_any it returns
   * a boost::any object, but if you use as_any<type_t>, then it returns
   * a type_t by value.
   */
  bool is_any() const {
    return is_type(ANY);
  }
  template <typename T>
  bool is_any() const {
    return (is_type(ANY) &&
            boost::get<boost::any>(storage->data).type() == typeid(T));
  }
  boost::any& as_any_lval() {
    VERIFY(is_any());
    _dup();
    return boost::get<boost::any>(storage->data);
  }
  template <typename T>
  T& as_any_lval() {
    return any_cast<T&>(as_any_lval());
  }
  const boost::any& as_any() const {
    VERIFY(is_any());
    return boost::get<boost::any>(storage->data);
  }
  template <typename T>
  const T& as_any() const {
    return any_cast<const T&>(as_any());
  }
  void set_any(const boost::any& val) {
    set_type(ANY);
    storage->data = val;
  }
  template <typename T>
  void set_any(T& val) {
    set_type(ANY);
    storage->data = boost::any(val);
  }

  /**
   * Data conversion methods.  These methods convert a value object to
   * its underlying type, where possible.  If not possible, an
   * exception is thrown.
   */
  bool        to_boolean() const;
  int         to_int() const;
  long        to_long() const;
  std::size_t to_size_t() const { return static_cast<std::size_t>(to_long()); }
  datetime_t  to_datetime() const;
  date_t      to_date() const;
  amount_t    to_amount() const;
  balance_t   to_balance() const;
  string      to_string() const;
  mask_t      to_mask() const;
  sequence_t  to_sequence() const;

  /**
   * Dynamic typing conversion methods.
   *
   * `cast(type_t)' returns a new value whose type has been cast to
   * the given type, but whose value is based on the original value.
   * For example, the uncommoditized AMOUNT "100.00" could be cast to
   * an INTEGER value.  If a cast would lose information or is not
   * meaningful, an exception is thrown.
   *
   * `simplify()' is an automatic cast to the simplest type that can
   * still represent the original value.
   *
   * There are also "in-place" versions of these two methods:
   *   in_place_cast
   *   in_place_simplify
   */
  value_t casted(type_t cast_type) const {
    value_t temp(*this);
    temp.in_place_cast(cast_type);
    return temp;
  }
  void    in_place_cast(type_t cast_type);

  value_t simplified() const {
    value_t temp = *this;
    temp.in_place_simplify();
    return temp;
  }
  void    in_place_simplify();

  value_t number() const;

  /**
   * Annotated commodity methods.
   */
  void          annotate(const annotation_t& details);
  bool          has_annotation() const;

  annotation_t& annotation();
  const annotation_t& annotation() const {
    return const_cast<value_t&>(*this).annotation();
  }

  value_t strip_annotations(const keep_details_t& what_to_keep) const;

  /**
   * Collection-style access methods for SEQUENCE values.
   */
  value_t& operator[](const std::size_t index) {
    VERIFY(! is_null());
    if (is_sequence())
      return as_sequence_lval()[index];
    else if (index == 0)
      return *this;

    assert(false);
    static value_t null;
    return null;
  }
  const value_t& operator[](const std::size_t index) const {
    VERIFY(! is_null());
    if (is_sequence())
      return as_sequence()[index];
    else if (index == 0)
      return *this;

    assert(false);
    static value_t null;
    return null;
  }

  void push_front(const value_t& val) {
    if (is_null())
      *this = sequence_t();
    if (! is_sequence())
      in_place_cast(SEQUENCE);
    as_sequence_lval().push_front(new value_t(val));
  }

  void push_back(const value_t& val) {
    if (is_null())
      *this = sequence_t();
    if (! is_sequence())
      in_place_cast(SEQUENCE);
    as_sequence_lval().push_back(new value_t(val));
  }

  void pop_back() {
    VERIFY(! is_null());

    if (! is_sequence()) {
#if BOOST_VERSION >= 103700
      storage.reset();
#else
      storage = intrusive_ptr<storage_t>();
#endif
    } else {
      as_sequence_lval().pop_back();

      const sequence_t& seq(as_sequence());
      std::size_t new_size = seq.size();
      if (new_size == 0) {
#if BOOST_VERSION >= 103700
        storage.reset();
#else
        storage = intrusive_ptr<storage_t>();
#endif
      }
      else if (new_size == 1) {
        *this = seq.front();
      }
    }
  }

  sequence_t::iterator begin() {
    VERIFY(is_sequence());
    return as_sequence_lval().begin();
  }
  sequence_t::iterator end() {
    VERIFY(is_sequence());
    return as_sequence_lval().end();
  }

  sequence_t::const_iterator begin() const {
    VERIFY(is_sequence());
    return as_sequence().begin();
  }
  sequence_t::const_iterator end() const {
    VERIFY(is_sequence());
    return as_sequence().end();
  }

  std::size_t size() const {
    if (is_null())
      return 0;
    else if (is_sequence())
      return as_sequence().size();
    else
      return 1;
  }

  bool empty() const {
    return size() == 0;
  }

  /**
   * Informational methods.
   */
  string label(optional<type_t> the_type = none) const;

  /**
   * Printing methods.
   */
  void print(std::ostream&       out,
             const int           first_width  = -1,
             const int           latter_width = -1,
             const uint_least8_t flags        = AMOUNT_PRINT_NO_FLAGS) const;

  void dump(std::ostream& out, const bool relaxed = true) const;

  /**
   * Debugging methods.
   */
  bool valid() const;
};

#define NULL_VALUE (value_t())

inline value_t string_value(const string& str = "") {
  return value_t(str, true);
}

#define VALUE_OR_ZERO(val) ((val).is_null() ? value_t(0L) : (val))
#define SIMPLIFIED_VALUE_OR_ZERO(val) \
  ((val).is_null() ? value_t(0L) : (val).simplified())

inline value_t mask_value(const string& str) {
  return value_t(mask_t(str));
}

inline std::ostream& operator<<(std::ostream& out, const value_t& val) {
  val.print(out);
  return out;
}

inline string value_context(const value_t& val) {
  std::ostringstream buf;
  val.print(buf, 20, 20, true);
  return buf.str();
}

inline value_t scope_value(scope_t * val) {
  return value_t(val);
}

template <typename T>
inline value_t& add_or_set_value(value_t& lhs, const T& rhs) {
  if (lhs.is_null())
    lhs = rhs;
  else
    lhs += rhs;
  return lhs;
}

struct sort_value_t
{
  bool    inverted;
  value_t value;

  sort_value_t() : inverted(false) {}
};

bool sort_value_is_less_than(const std::list<sort_value_t>& left_values,
                             const std::list<sort_value_t>& right_values);

void put_value(property_tree::ptree& pt, const value_t& value);

} // namespace ledger

#endif // _VALUE_H
