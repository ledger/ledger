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

#include "balpair.h"		// pulls in balance.h and amount.h

namespace ledger {

DECLARE_EXCEPTION(value_error, std::runtime_error);

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
	   equality_comparable<value_t, balance_pair_t,
	   equality_comparable<value_t, balance_t,
	   additive<value_t, balance_pair_t,
	   additive<value_t, balance_t,
	   multiplicative<value_t, balance_pair_t,
	   multiplicative<value_t, balance_t,
	   ordered_field_operators<value_t, amount_t,
	   ordered_field_operators<value_t, double,
	   ordered_field_operators<value_t, unsigned long,
	   ordered_field_operators<value_t, long> > > > > > > > > > >
{
public:
  /**
   * The sequence_t member type abstracts the type used to represent a
   * resizable "array" of value_t objects.
   */
  typedef std::vector<value_t> sequence_t;

  typedef sequence_t::iterator	      iterator;
  typedef sequence_t::const_iterator  const_iterator;
  typedef sequence_t::difference_type difference_type;

  /**
   * type_t gives the type of the data contained or referenced by a
   * value_t object.  Use the type() method to get a value of type
   * type_t.
   */
  enum type_t {
    VOID,			// a null value (i.e., uninitialized)
    BOOLEAN,			// a boolean
    DATETIME,			// a date and time (Boost posix_time)
    DATE,			// a date (Boost gregorian::date)
    INTEGER,			// a signed integer value
    AMOUNT,			// a ledger::amount_t
    BALANCE,			// a ledger::balance_t
    BALANCE_PAIR,		// a ledger::balance_pair_t
    STRING,			// a string object
    SEQUENCE,			// a vector of value_t objects
    POINTER			// an opaque pointer of any type
  };

private:
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
    char   data[sizeof(amount_t)];
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

  public:			// so `checked_delete' can access it
    /**
     * Destructor.  Must only be called when the reference count has
     * reached zero.  The `destroy' method is used to do the actual
     * cleanup of the data, since it's quite possible for `destroy' to
     * be called while the object is still active -- to clear the
     * stored data for subsequent reuse of the storage_t object.
     */
    ~storage_t() {
      TRACE_DTOR(value_t::storage_t);
      DEBUG("value.storage.refcount", "Destroying " << this);
      assert(refc == 0);
      destroy();
    }

    void destroy();

  private:
    /**
     * Assignment and copy operators.  These are called when making a
     * new copy of a storage object in order to modify the copy.
     */
    explicit storage_t(const storage_t& rhs)
      : type(rhs.type), refc(0) {
      TRACE_CTOR(value_t::storage_t, "copy");
      *this = rhs;
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

    friend inline void intrusive_ptr_add_ref(value_t::storage_t * storage) {
      storage->acquire();
    }
    friend inline void intrusive_ptr_release(value_t::storage_t * storage) {
      storage->release();
    }
  };

  /**
   * The actual data for each value_t is kept in the `storage' member.
   * Data is modified using a copy-on-write policy.
   */
  intrusive_ptr<storage_t> storage;

  /**
   * _dup() makes a private copy of the current value (if necessary)
   * so it can subsequently be modified.
   *
   * _clear() removes our pointer to the current value and initializes
   * a new storage bin for things to be stored in.
   *
   * _reset() makes the current object appear as if it were
   * uninitialized.
   */
  void _dup();
  void _clear() {
    if (! storage || storage->refc > 1)
      storage = new storage_t;
    else
      storage->destroy();
  }
  void _reset() {
    if (storage)
      storage = intrusive_ptr<storage_t>();
  }

  /**
   * Because boolean "true" and "false" are so common, a pair of
   * static references are kept to prevent the creation of throwaway
   * storage_t objects just to represent these two common values.
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
    TRACE_CTOR(value_t, "const bool");
    set_boolean(val);
  }

  value_t(const datetime_t& val) {
    TRACE_CTOR(value_t, "const datetime_t&");
    set_datetime(val);
  }
  value_t(const date_t& val) {
    TRACE_CTOR(value_t, "const date_t&");
    set_date(val);
  }

  value_t(const long val) {
    TRACE_CTOR(value_t, "const long");
    set_long(val);
  }
  value_t(const unsigned long val) {
    TRACE_CTOR(value_t, "const unsigned long");
    set_amount(val);
  }
  value_t(const double val) {
    TRACE_CTOR(value_t, "const double");
    set_amount(val);
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

  value_t(const sequence_t& val) {
    TRACE_CTOR(value_t, "const sequence_t&");
    set_sequence(val);
  }

  template <typename T>
  explicit value_t(T * item) {
    TRACE_CTOR(value_t, "T *");
    set_pointer(item);
  }

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
    TRACE_CTOR(value_t, "copy");
    *this = val;
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

  // This special form of add is use to produce a balance pair by
  // simultaneously adding both an amount and its cost.
  value_t& add(const amount_t& amount,
	       const optional<amount_t>& cost = none);

  /**
   * Unary arithmetic operators.
   */
  value_t negate() const {
    value_t temp = *this;
    temp.in_place_negate();
    return temp;
  }
  void    in_place_negate();	// exists for efficiency's sake
  void    in_place_not();	// exists for efficiency's sake

  value_t operator-() const {
    return negate();
  }

  value_t abs() const;

  value_t rounded() const;
  value_t unrounded() const;

  value_t reduce() const {
    value_t temp(*this);
    temp.in_place_reduce();
    return temp;
  }
  void    in_place_reduce();	// exists for efficiency's sake

  // Return the "market value" of a given value at a specific time.
  value_t value(const optional<datetime_t>&   moment	  = none,
		const optional<commodity_t&>& in_terms_of = none) const;
  value_t cost() const;


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
      assert(is_type(VOID));
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
  bool is_type(type_t _type) const {
    return type() == _type;
  }

private:
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
   * is_balance_pair()
   * is_string()
   * is_sequence()
   * is_pointer()
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
    assert(is_boolean());
    _dup();
    return *reinterpret_cast<bool *>(storage->data);
  }
  const bool& as_boolean() const {
    assert(is_boolean());
    return *reinterpret_cast<bool *>(storage->data);
  }
  void set_boolean(const bool val) {
    set_type(BOOLEAN);
    storage = val ? true_value : false_value;
  }

  bool is_datetime() const {
    return is_type(DATETIME);
  }
  datetime_t& as_datetime_lval() {
    assert(is_datetime());
    _dup();
    return *reinterpret_cast<datetime_t *>(storage->data);
  }
  const datetime_t& as_datetime() const {
    assert(is_datetime());
    return *reinterpret_cast<datetime_t *>(storage->data);
  }
  void set_datetime(const datetime_t& val) {
    set_type(DATETIME);
    new(reinterpret_cast<datetime_t *>(storage->data)) datetime_t(val);
  }

  bool is_date() const {
    return is_type(DATE);
  }
  date_t& as_date_lval() {
    assert(is_date());
    _dup();
    return *reinterpret_cast<date_t *>(storage->data);
  }
  const date_t& as_date() const {
    assert(is_date());
    return *reinterpret_cast<date_t *>(storage->data);
  }
  void set_date(const date_t& val) {
    set_type(DATE);
    new(reinterpret_cast<date_t *>(storage->data)) date_t(val);
  }

  bool is_long() const {
    return is_type(INTEGER);
  }
  long& as_long_lval() {
    assert(is_long());
    _dup();
    return *reinterpret_cast<long *>(storage->data);
  }
  const long& as_long() const {
    assert(is_long());
    return *reinterpret_cast<long *>(storage->data);
  }
  void set_long(const long val) {
    set_type(INTEGER);
    *reinterpret_cast<long *>(storage->data) = val;
  }

  bool is_amount() const {
    return is_type(AMOUNT);
  }
  amount_t& as_amount_lval() {
    assert(is_amount());
    _dup();
    amount_t& amt(*reinterpret_cast<amount_t *>(storage->data));
    assert(amt.valid());
    return amt;
  }
  const amount_t& as_amount() const {
    assert(is_amount());
    amount_t& amt(*reinterpret_cast<amount_t *>(storage->data));
    assert(amt.valid());
    return amt;
  }
  void set_amount(const amount_t& val) {
    assert(val.valid());
    set_type(AMOUNT);
    new(reinterpret_cast<amount_t *>(storage->data)) amount_t(val);
  }

  bool is_balance() const {
    return is_type(BALANCE);
  }
  balance_t& as_balance_lval() {
    assert(is_balance());
    _dup();
    balance_t& bal(**reinterpret_cast<balance_t **>(storage->data));
    assert(bal.valid());
    return bal;
  }
  const balance_t& as_balance() const {
    assert(is_balance());
    balance_t& bal(**reinterpret_cast<balance_t **>(storage->data));
    assert(bal.valid());
    return bal;
  }
  void set_balance(const balance_t& val) {
    assert(val.valid());
    set_type(BALANCE);
    *reinterpret_cast<balance_t **>(storage->data) = new balance_t(val);
  }

  bool is_balance_pair() const {
    return is_type(BALANCE_PAIR);
  }
  balance_pair_t& as_balance_pair_lval() {
    assert(is_balance_pair());
    _dup();
    balance_pair_t& bal_pair(**reinterpret_cast<balance_pair_t **>(storage->data));
    assert(bal_pair.valid());
    return bal_pair;
  }
  const balance_pair_t& as_balance_pair() const {
    assert(is_balance_pair());
    balance_pair_t& bal_pair(**reinterpret_cast<balance_pair_t **>(storage->data));
    assert(bal_pair.valid());
    return bal_pair;
  }
  void set_balance_pair(const balance_pair_t& val) {
    assert(val.valid());
    set_type(BALANCE_PAIR);
    *reinterpret_cast<balance_pair_t **>(storage->data) = new balance_pair_t(val);
  }

  bool is_string() const {
    return is_type(STRING);
  }
  string& as_string_lval() {
    assert(is_string());
    _dup();
    return *reinterpret_cast<string *>(storage->data);
  }
  const string& as_string() const {
    assert(is_string());
    return *reinterpret_cast<string *>(storage->data);
  }
  void set_string(const string& val = "") {
    set_type(STRING);
    new(reinterpret_cast<string *>(storage->data)) string(val);
  }
  void set_string(const char * val = "") {
    set_type(STRING);
    new(reinterpret_cast<string *>(storage->data)) string(val);
  }

  bool is_sequence() const {
    return is_type(SEQUENCE);
  }
  sequence_t& as_sequence_lval() {
    assert(is_sequence());
    _dup();
    return **reinterpret_cast<sequence_t **>(storage->data);
  }
  const sequence_t& as_sequence() const {
    assert(is_sequence());
    return **reinterpret_cast<sequence_t **>(storage->data);
  }
  void set_sequence(const sequence_t& val) {
    set_type(SEQUENCE);
    *reinterpret_cast<sequence_t **>(storage->data) = new sequence_t(val);
  }

  /**
   * Dealing with pointers is bit involved because we actually deal
   * with typed pointers.  For example, if you call as_pointer it
   * returns a boost::any object, but if you use as_pointer<void>,
   * then it returns a void *.  The latter form only succeeds if the
   * stored pointers was assigned to the value as a void*, otherwise
   * it throws an exception.
   */
  bool is_pointer() const {
    return is_type(POINTER);
  }
  boost::any& as_any_pointer_lval() {
    assert(is_pointer());
    _dup();
    return *reinterpret_cast<boost::any *>(storage->data);
  }
  template <typename T>
  T * as_pointer_lval() {
    assert(is_pointer());
    _dup();
    return any_cast<T *>(*reinterpret_cast<boost::any *>(storage->data));
  }
  template <typename T>
  T& as_ref_lval() {
    assert(is_pointer());
    _dup();
    return *any_cast<T *>(*reinterpret_cast<boost::any *>(storage->data));
  }
  const boost::any& as_any_pointer() const {
    assert(is_pointer());
    return *reinterpret_cast<boost::any *>(storage->data);
  }
  template <typename T>
  T * as_pointer() const {
    assert(is_pointer());
    return any_cast<T *>(*reinterpret_cast<boost::any *>(storage->data));
  }
  template <typename T>
  T& as_ref() const {
    assert(is_pointer());
    return *any_cast<T *>(*reinterpret_cast<boost::any *>(storage->data));
  }
  void set_any_pointer(const boost::any& val) {
    set_type(POINTER);
    new(reinterpret_cast<boost::any *>(storage->data)) boost::any(val);
  }
  template <typename T>
  void set_pointer(T * val) {
    set_type(POINTER);
    new(reinterpret_cast<boost::any *>(storage->data)) boost::any(val);
  }

  /**
   * Data conversion methods.  These methods convert a value object to
   * its underlying type, where possible.  If not possible, an
   * exception is thrown.
   */
  bool		 to_boolean() const;
  long		 to_long() const;
  datetime_t     to_datetime() const;
  date_t         to_date() const;
  amount_t	 to_amount() const;
  balance_t	 to_balance() const;
  balance_pair_t to_balance_pair() const;
  string	 to_string() const;
  sequence_t     to_sequence() const;

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
  value_t cast(type_t cast_type) const {
    value_t temp(*this);
    temp.in_place_cast(cast_type);
    return temp;
  }
  void    in_place_cast(type_t cast_type);

  value_t simplify() const {
    value_t temp = *this;
    temp.in_place_simplify();
    return temp;
  }
  void    in_place_simplify();

  /**
   * Annotated commodity methods.
   */
#if 0
  // These helper methods only apply to AMOUNT values.
  value_t annotated_price() const;
  value_t annotated_date() const;
  value_t annotated_tag() const;
#endif

  value_t strip_annotations(const bool keep_price = amount_t::keep_price,
			    const bool keep_date  = amount_t::keep_date,
			    const bool keep_tag   = amount_t::keep_tag) const;

  /**
   * Collection-style access methods for SEQUENCE values.
   */
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
    if (! val.is_null()) {
      if (is_null())
	*this = sequence_t();
      if (! is_sequence())
	in_place_cast(SEQUENCE);

      if (! val.is_sequence()) {
	as_sequence_lval().push_back(val);
      } else {
	const sequence_t& val_seq(val.as_sequence());
	std::copy(val_seq.begin(), val_seq.end(),
		  back_inserter(as_sequence_lval()));
      }
    }
  }

  void pop_back() {
    assert(! is_null());

    if (! is_sequence()) {
      _reset();
    } else {
      as_sequence_lval().pop_back();

      const sequence_t& seq(as_sequence());
      std::size_t new_size = seq.size();
      if (new_size == 0)
	_reset();
      else if (new_size == 1)
	*this = seq.front();
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

  /**
   * Informational methods.
   */
  string label(optional<type_t> the_type = none) const {
    switch (the_type ? *the_type : type()) {
    case VOID:
      return "an uninitialized value";
    case BOOLEAN:
      return "a boolean";
    case DATETIME:
      return "a date/time";
    case DATE:
      return "a date";
    case INTEGER:
      return "an integer";
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
    case POINTER:
      return "a pointer";
    default:
      assert(false);
      break;
    }
    assert(false);
    return "<invalid>";
  }

  /**
   * Printing methods.
   */
  void dump(std::ostream& out, const int first_width,
	    const int latter_width = -1) const;
  void print(std::ostream& out, const bool relaxed = true) const;

  /**
   * Serialization methods.  A value may be deserialized from an input
   * stream or a character pointer, and it may be serialized to an
   * output stream.  The methods used are:
   */
  void read(std::istream& in);
  void read(const char *& data);
  void write(std::ostream& out) const;

  /** @name XML Serialization
   */
  /*@{*/

  void read_xml(std::istream& in);
  void write_xml(std::ostream& out, const int depth = 0) const;

  /*@}*/

  /**
   * Debugging methods.
   */
  bool valid() const;
};

#define NULL_VALUE (value_t())

inline value_t string_value(const string& str) {
  return value_t(str, true);
}

inline std::ostream& operator<<(std::ostream& out, const value_t& val) {
  val.print(out, 12);
  return out;
}

inline string value_context(const value_t& val) {
  std::ostringstream buf;
  buf << std::right;
  buf.width(20);
  val.print(buf);
  buf << std::endl;
  return buf.str();
}

template <typename T>
inline value_t& add_or_set_value(value_t& lhs, const T& rhs) {
  if (lhs.is_null())
    lhs = rhs;
  else
    lhs += rhs;
  return lhs;
}

} // namespace ledger

#endif // _VALUE_H
