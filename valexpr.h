/*
 * Copyright (c) 2003-2008, John Wiegley.  All rights reserved.
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

#ifndef _VALEXPR_H
#define _VALEXPR_H

#include "value.h"
#include "utils.h"
#include "mask.h"

namespace ledger {

class entry_t;
class transaction_t;
class account_t;

namespace expr {

DECLARE_EXCEPTION(error, compile_error);
DECLARE_EXCEPTION(error, calc_error);

#if 0
struct context_t
{
  const entry_t * entry() {
    return NULL;
  }
  const transaction_t * xact() {
    return NULL;
  }
  const account_t * account() {
    return NULL;
  }
};

struct entry_context_t : public context_t
{
  const entry_t * entry_;

  const entry_t * entry() {
    return entry_;
  }
};

struct xact_context_t : public context_t
{
  const transaction_t * xact_;

  const entry_t * entry() {
    return xact_->entry;
  }
  const transaction_t * xact() {
    return xact_;
  }
  const account_t * account() {
    return xact_->account;
  }
};

struct account_context_t : public context_t
{
  const account_t * account_;

  const account_t * account() {
    return account_;
  }
};
#endif

struct details_t
{
  const entry_t *       entry;
  const transaction_t * xact;
  const account_t *     account;

  details_t() : entry(NULL), xact(NULL), account(NULL) {
    TRACE_CTOR(details_t, "");
  }
  details_t(const details_t& other)
    : entry(other.entry),
      xact(other.xact),
      account(other.account) {
    TRACE_CTOR(details_t, "copy");
  }
  details_t(const entry_t& _entry)
    : entry(&_entry), xact(NULL), account(NULL) {
    TRACE_CTOR(details_t, "const entry_t&");
  }
  details_t(const transaction_t& _xact);
  details_t(const account_t& _account)
    : entry(NULL), xact(NULL), account(&_account) {
    TRACE_CTOR(details_t, "const account_t&");
  }
  ~details_t() throw() {
    TRACE_DTOR(details_t);
  }
};

struct op_t;
typedef intrusive_ptr<op_t> ptr_op_t;

class call_scope_t;

typedef function<value_t (call_scope_t&)> function_t;

#define MAKE_FUNCTOR(x) expr::op_t::wrap_functor(bind(&x, this, _1))
#define WRAP_FUNCTOR(x) expr::op_t::wrap_functor(x)

class scope_t : public noncopyable
{
  scope_t();

public:
  enum type_t {
    CHILD_SCOPE,
    SYMBOL_SCOPE,
    CALL_SCOPE,
    CONTEXT_SCOPE
  } type_;

  explicit scope_t(type_t _type) : type_(_type) {
    TRACE_CTOR(scope_t, "type_t");
  }
  virtual ~scope_t() {
    TRACE_DTOR(scope_t);
  }

  const type_t type() const {
    return type_;
  }

  virtual void     define(const string& name, ptr_op_t def) = 0;
          void     define(const string& name, const value_t& val);
  virtual ptr_op_t lookup(const string& name) = 0;
          value_t  resolve(const string& name);

  virtual optional<scope_t&> find_scope(const type_t _type,
					bool skip_this = false) = 0;
  virtual optional<scope_t&> find_first_scope(const type_t _type1,
					      const type_t _type2,
					      bool skip_this = false) = 0;

  template <typename T>
  T& find_scope(bool skip_this = false) {
    assert(false);
  }
  template <typename T>
  optional<T&> maybe_find_scope(bool skip_this = false) {
    assert(false);
  }
};

class child_scope_t : public scope_t
{
  scope_t * parent;

  child_scope_t();

public:
  explicit child_scope_t(type_t _type = CHILD_SCOPE)
    : scope_t(_type), parent(NULL) {
    TRACE_CTOR(child_scope_t, "type_t");
  }
  explicit child_scope_t(scope_t& _parent, type_t _type = CHILD_SCOPE)
    : scope_t(_type), parent(&_parent) {
    TRACE_CTOR(child_scope_t, "scope_t&, type_t");
  }
  virtual ~child_scope_t() {
    TRACE_DTOR(child_scope_t);
  }
public:
  virtual void     define(const string& name, ptr_op_t def) {
    if (parent)
      parent->define(name, def);
  }
  virtual ptr_op_t lookup(const string& name) {
    if (parent)
      return parent->lookup(name);
    return ptr_op_t();
  }

  virtual optional<scope_t&> find_scope(type_t _type,
					bool skip_this = false) {
    for (scope_t * ptr = (skip_this ? parent : this); ptr; ) {
      if (ptr->type() == _type)
	return *ptr;

      ptr = polymorphic_downcast<child_scope_t *>(ptr)->parent;
    }
    return none;
  }

  virtual optional<scope_t&> find_first_scope(const type_t _type1,
					      const type_t _type2,
					      bool skip_this = false) {
    for (scope_t * ptr = (skip_this ? parent : this); ptr; ) {
      if (ptr->type() == _type1 || ptr->type() == _type2)
	return *ptr;

      ptr = polymorphic_downcast<child_scope_t *>(ptr)->parent;
    }
    return none;
  }
};

class symbol_scope_t : public child_scope_t
{
  typedef std::map<const string, ptr_op_t> symbol_map;
  symbol_map symbols;

public:
  explicit symbol_scope_t()
    : child_scope_t(SYMBOL_SCOPE) {
    TRACE_CTOR(symbol_scope_t, "");
  }
  explicit symbol_scope_t(scope_t& _parent)
    : child_scope_t(_parent, SYMBOL_SCOPE) {
    TRACE_CTOR(symbol_scope_t, "scope_t&");
  }
  virtual ~symbol_scope_t() {
    TRACE_DTOR(symbol_scope_t);
  }

  virtual void     define(const string& name, ptr_op_t def);
  void     define(const string& name, const value_t& val) {
    scope_t::define(name, val);
  }
  virtual ptr_op_t lookup(const string& name);
};

class call_scope_t : public child_scope_t
{
  value_t args;

  call_scope_t();

public:
  explicit call_scope_t(scope_t& _parent)
    : child_scope_t(_parent, CALL_SCOPE) {
    TRACE_CTOR(call_scope_t, "scope_t&");
  }
  virtual ~call_scope_t() {
    TRACE_DTOR(call_scope_t);
  }

  void set_args(const value_t& _args) {
    args = _args;
  }

  value_t& value() {
    return args;
  }

  value_t& operator[](const unsigned int index) {
    // jww (2008-07-21): exception here if it's out of bounds
    return args[index];
  }
  const value_t& operator[](const unsigned int index) const {
    // jww (2008-07-21): exception here if it's out of bounds
    return args[index];
  }

  void push_back(const value_t& val) {
    args.push_back(val);
  }
  void pop_back() {
    args.pop_back();
  }

  const std::size_t size() const {
    return args.size();
  }
};

template <typename T>
class var_t : public noncopyable
{
  T * value;

  var_t();

public:
  // jww (2008-07-21): Give a good exception here if we can't find "name"
  var_t(scope_t& scope, const string& name)
    : value(scope.resolve(name).template as_pointer<T>()) {
    TRACE_CTOR(var_t, "scope_t&, const string&");
  }
  var_t(call_scope_t& scope, const unsigned int idx)
    : value(scope[idx].template as_pointer<T>()) {
    TRACE_CTOR(var_t, "call_scope_t&, const unsigned int");
  }
  ~var_t() throw() {
    TRACE_DTOR(var_t);
  }

  T& operator *() { return *value; }
  T * operator->() { return value; }
};

#if 0
class context_scope_t : public child_scope_t
{
public:
  value_t     current_element;
  std::size_t element_index;
  std::size_t sequence_size;

  explicit context_scope_t(scope_t&	        _parent,
			   const value_t&     _element        = NULL_VALUE,
			   const std::size_t  _element_index  = 0,
			   const std::size_t  _sequence_size  = 0)
    : child_scope_t(_parent, CONTEXT_SCOPE), current_element(_element),
      element_index(_element_index), sequence_size(_sequence_size)
  {
    TRACE_CTOR(expr::context_scope_t, "scope_t&, const value_t&, ...");
  }
  virtual ~context_scope_t() {
    TRACE_DTOR(expr::context_scope_t);
  }

  const std::size_t index() const {
    return element_index;
  }
  const std::size_t size() const {
    return sequence_size;
  }

  value_t& value() {
    return current_element;
  }
};
#endif

class op_t : public noncopyable
{
  op_t();

public:
  enum kind_t {
    // Constants
    VALUE,
    MASK,
    ARG_INDEX,

    CONSTANTS,

    // Item details
    AMOUNT,
    COST,
    PRICE,
    DATE,
    ACT_DATE,
    EFF_DATE,
    CLEARED,
    PENDING,
    REAL,
    ACTUAL,
    INDEX,
    DEPTH,

    // Item totals
    COUNT,
    TOTAL,
    COST_TOTAL,
    PRICE_TOTAL,

    // Relating to format_t
    VALUE_EXPR,
    TOTAL_EXPR,

    // Functions
    FUNCTION,

    F_NOW,
    F_ARITH_MEAN,
    F_QUANTITY,
    F_COMMODITY,
    F_SET_COMMODITY,
    F_VALUE,
    F_ABS,
    F_ROUND,
    F_PRICE,
    F_DATE,
    F_DATECMP,
    F_YEAR,
    F_MONTH,
    F_DAY,

    BEGIN_MASKS,
    F_CODE_MASK,
    F_PAYEE_MASK,
    F_NOTE_MASK,
    F_ACCOUNT_MASK,
    F_SHORT_ACCOUNT_MASK,
    F_COMMODITY_MASK,
    END_MASKS,

    TERMINALS,

    F_PARENT,

    // Binary operators
    O_NEG,
    O_ADD,
    O_SUB,
    O_MUL,
    O_DIV,
    O_PERC,
    O_NEQ,
    O_EQ,
    O_LT,
    O_LTE,
    O_GT,
    O_GTE,
    O_NOT,
    O_AND,
    O_OR,
    O_QUES,
    O_COL,
    O_COMMA,
    O_DEF,
    O_REF,
    O_ARG,

    LAST
  };

  kind_t	kind;
  mutable short refc;
  ptr_op_t	left_;

  variant<unsigned int,		// used by ARG_INDEX and O_ARG
	  value_t,		// used by constant VALUE
	  mask_t,		// used by constant MASK
	  function_t,		// used by terminal FUNCTION
#if 0
	  node_t::nameid_t,	// used by NODE_ID and ATTR_ID
#endif
	  ptr_op_t>		// used by all binary operators
  data;

  explicit op_t(const kind_t _kind) : kind(_kind), refc(0){
    TRACE_CTOR(op_t, "const kind_t");
  }
  ~op_t() {
    TRACE_DTOR(op_t);
    assert(refc == 0);
  }

  bool is_long() const {
    return data.type() == typeid(unsigned int);
  }
  unsigned int& as_long_lval() {
    assert(kind == ARG_INDEX || kind == O_ARG);
    return boost::get<unsigned int>(data);
  }
  const unsigned int& as_long() const {
    return const_cast<op_t *>(this)->as_long_lval();
  }
  void set_long(unsigned int val) {
    data = val;
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

  bool is_string() const {
    if (kind == VALUE) {
      assert(data.type() == typeid(value_t));
      return boost::get<value_t>(data).is_string();
    }
    return false;
  }
  string& as_string_lval() {
    assert(is_string());
    return boost::get<value_t>(data).as_string_lval();
  }
  const string& as_string() const {
    return const_cast<op_t *>(this)->as_string_lval();
  }
  void set_string(const string& val) {
    data = value_t(val);
  }

  bool is_mask() const {
    if (kind > BEGIN_MASKS && kind < END_MASKS) {
      assert(data.type() == typeid(mask_t));
      return true;
    }
    return false;
  }
  mask_t& as_mask_lval() {
    assert(is_mask());
    return boost::get<mask_t>(data);
  }
  const mask_t& as_mask() const {
    return const_cast<op_t *>(this)->as_mask_lval();
  }
  void set_mask(const mask_t& val) {
    data = val;
  }
  void set_mask(const string& expr) {
    data = mask_t(expr);
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

#if 0
  bool is_name() const {
    return data.type() == typeid(node_t::nameid_t);
  }
  node_t::nameid_t& as_name_lval() {
    assert(kind == NODE_ID || kind == ATTR_ID);
    return boost::get<node_t::nameid_t>(data);
  }
  const node_t::nameid_t& as_name() const {
    return const_cast<op_t *>(this)->as_name_lval();
  }
  void set_name(const node_t::nameid_t& val) {
    data = val;
  }
#endif

  ptr_op_t& as_op_lval() {
    assert(kind > TERMINALS);
    return boost::get<ptr_op_t>(data);
  }
  const ptr_op_t& as_op() const {
    return const_cast<op_t *>(this)->as_op_lval();
  }

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

  ptr_op_t& left() {
    return left_;
  }
  const ptr_op_t& left() const {
    assert(kind > TERMINALS);
    return left_;
  }
  void set_left(const ptr_op_t& expr) {
    assert(kind > TERMINALS);
    left_ = expr;
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

  static ptr_op_t new_node(kind_t _kind, ptr_op_t _left = NULL,
			   ptr_op_t _right = NULL);
  ptr_op_t copy(ptr_op_t _left = NULL, ptr_op_t _right = NULL) const {
    return new_node(kind, _left, _right);
  }

  static ptr_op_t wrap_value(const value_t& val);
  static ptr_op_t wrap_functor(const function_t& fobj);

  ptr_op_t compile(scope_t& scope);
  value_t  current_value(scope_t& scope);
#if 0
  node_t&  current_xml_node(scope_t& scope);
#endif
  value_t  calc(scope_t& scope);

  void compute(value_t& result,
	       const details_t& details = details_t(),
	       ptr_op_t context = NULL) const;

  value_t compute(const details_t& details = details_t(),
		  ptr_op_t context = NULL) const {
    value_t temp;
    compute(temp, details, context);
    return temp;
  }

  struct print_context_t
  {
    scope_t&        scope;
    const bool      relaxed;
    const ptr_op_t& op_to_find;
    unsigned long * start_pos;
    unsigned long * end_pos;

    print_context_t(scope_t& _scope,
		    const bool      _relaxed	  = false,
		    const ptr_op_t& _op_to_find = ptr_op_t(),
		    unsigned long * _start_pos  = NULL,
		    unsigned long * _end_pos	  = NULL)
      : scope(_scope), relaxed(_relaxed), op_to_find(_op_to_find),
	start_pos(_start_pos), end_pos(_end_pos) {}
  };

  bool print(std::ostream& out, print_context_t& context) const;
  void dump(std::ostream& out, const int depth) const;

  friend inline void intrusive_ptr_add_ref(op_t * op) {
    op->acquire();
  }
  friend inline void intrusive_ptr_release(op_t * op) {
    op->release();
  }
};

class op_predicate : public noncopyable
{
  ptr_op_t op;

  op_predicate();

public:
  explicit op_predicate(ptr_op_t _op) : op(_op) {
    TRACE_CTOR(op_predicate, "ptr_op_t");
  }
  ~op_predicate() throw() {
    TRACE_DTOR(op_predicate);
  }
  bool operator()(scope_t& scope) {
    return op->calc(scope).to_boolean();
  }
};

class valexpr_context : public error_context
{
public:
  ptr_op_t expr;
  ptr_op_t error_node;

  valexpr_context(const ptr_op_t& _expr,
		  const string&   desc = "") throw()
    : error_context(desc), expr(_expr), error_node(_expr) {}
  virtual ~valexpr_context() throw() {}

  virtual void describe(std::ostream& out) const throw();
};

class compute_error : public error
{
public:
  compute_error(const string& reason, error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~compute_error() throw() {}
};

class value_expr_error : public error
{
public:
  value_expr_error(const string& reason,
		   error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~value_expr_error() throw() {}
};

extern std::auto_ptr<symbol_scope_t> global_scope;

extern datetime_t terminus;

bool compute_amount(const ptr_op_t expr, amount_t& amt,
		    const transaction_t * xact,
		    const ptr_op_t context = NULL);

//////////////////////////////////////////////////////////////////////

inline void guarded_compute(const ptr_op_t   expr,
			    value_t&	     result,
			    const details_t& details = details_t(),
			    const ptr_op_t   context = NULL) {
  try {
    expr->compute(result, details);
  }
  catch (error * err) {
    if (err->context.empty() ||
	! dynamic_cast<valexpr_context *>(err->context.back()))
      err->context.push_back(new valexpr_context(expr));
    error_context * last = err->context.back();
    if (valexpr_context * ctxt = dynamic_cast<valexpr_context *>(last)) {
      ctxt->expr = expr;
      ctxt->desc = "While computing value expression:";
    }
    throw err;
  }
}

inline value_t guarded_compute(const ptr_op_t expr,
			       const details_t&	    details = details_t(),
			       ptr_op_t       context = NULL) {
  value_t temp;
  guarded_compute(expr, temp, details, context);
  return temp;
}

template<>
inline symbol_scope_t&
scope_t::find_scope<symbol_scope_t>(bool skip_this) {
  optional<scope_t&> scope = find_scope(SYMBOL_SCOPE, skip_this);
  assert(scope);
  return downcast<symbol_scope_t>(*scope);
}

template<>
inline call_scope_t&
scope_t::find_scope<call_scope_t>(bool skip_this) {
  optional<scope_t&> scope = find_scope(CALL_SCOPE, skip_this);
  assert(scope);
  return downcast<call_scope_t>(*scope);
}

#if 0
template<>
inline context_scope_t&
scope_t::find_scope<context_scope_t>(bool skip_this) {
  optional<scope_t&> scope = find_scope(CONTEXT_SCOPE, skip_this);
  assert(scope);
  return downcast<context_scope_t>(*scope);
}
#endif

#define FIND_SCOPE(scope_type, scope_ref) \
  downcast<scope_t>(scope_ref).find_scope<scope_type>()

#define CALL_SCOPE(scope_ref) \
  FIND_SCOPE(call_scope_t, scope_ref)
#define SYMBOL_SCOPE(scope_ref) \
  FIND_SCOPE(symbol_scope_t, scope_ref)
#if 0
#define CONTEXT_SCOPE(scope_ref) \
  FIND_SCOPE(context_scope_t, scope_ref)
#endif

inline ptr_op_t op_t::new_node(kind_t _kind, ptr_op_t _left, ptr_op_t _right) {
  ptr_op_t node(new op_t(_kind));
  node->set_left(_left);
  node->set_right(_right);
  return node;
}

inline ptr_op_t op_t::wrap_value(const value_t& val) {
  ptr_op_t temp(new op_t(op_t::VALUE));
  temp->set_value(val);
  return temp;
}

inline ptr_op_t op_t::wrap_functor(const function_t& fobj) {
  ptr_op_t temp(new op_t(op_t::FUNCTION));
  temp->set_function(fobj);
  return temp;
}

class parser_t;

} // namespace expr

//////////////////////////////////////////////////////////////////////

class value_expr
{
  expr::ptr_op_t ptr;

public:
  string expr_str;

  typedef expr::details_t details_t;

  value_expr() {
    TRACE_CTOR(value_expr, "");
  }

  value_expr(const string& _expr_str);
  value_expr(const expr::ptr_op_t _ptr, const string& _expr_str = "")
    : ptr(_ptr), expr_str(_expr_str) {
    TRACE_CTOR(value_expr, "const expr::ptr_op_t");
  }
  value_expr(const value_expr& other)
    : ptr(other.ptr), expr_str(other.expr_str) {
    TRACE_CTOR(value_expr, "copy");
  }
  virtual ~value_expr() throw() {
    TRACE_DTOR(value_expr);
  }

  value_expr& operator=(const value_expr& _expr) {
    expr_str = _expr.expr_str;
    reset(_expr.get());
    return *this;
  }
  value_expr& operator=(const string& _expr) {
    return *this = value_expr(_expr);
  }

  operator bool() const throw() {
    return ptr.get() != NULL;
  }
  operator string() const throw() {
    return expr_str;
  }
  operator const expr::ptr_op_t() const throw() {
    return ptr;
  }

  const expr::ptr_op_t operator->() const throw() {
    return ptr;
  }

  const expr::ptr_op_t get() const throw() { return ptr; }
  const expr::ptr_op_t release() throw() {
    const expr::ptr_op_t tmp = ptr;
    ptr = expr::ptr_op_t();
    return tmp;
  }
  void reset(const expr::ptr_op_t p = expr::ptr_op_t()) throw() {
    ptr = p;
  }

  virtual void compute(value_t& result,
		       const details_t& details = details_t(),
		       expr::ptr_op_t   context = NULL) {
    guarded_compute(ptr, result, details, context);
  }
  virtual value_t compute(const details_t& details = details_t(),
			  expr::ptr_op_t   context = NULL) {
    value_t temp;
    guarded_compute(ptr, temp, details, context);
    return temp;
  }

  friend bool print_value_expr(std::ostream&	    out,
			       const expr::ptr_op_t node,
			       const expr::ptr_op_t node_to_find,
			       unsigned long *	    start_pos,
			       unsigned long *	    end_pos);

  static std::auto_ptr<value_expr>     amount_expr;
  static std::auto_ptr<value_expr>     total_expr;
  static std::auto_ptr<expr::parser_t> parser;

  static void initialize();
  static void shutdown();
};

typedef value_expr::details_t details_t; // jww (2008-07-20): remove

inline void compute_amount(value_t& result,
			   const details_t& details = details_t()) {
  if (value_expr::amount_expr.get())
    value_expr::amount_expr->compute(result, details);
}

inline value_t compute_amount(const details_t& details = details_t()) {
  if (value_expr::amount_expr.get())
    return value_expr::amount_expr->compute(details);
}

inline void compute_total(value_t& result,
			  const details_t& details = details_t()) {
  if (value_expr::total_expr.get())
    value_expr::total_expr->compute(result, details);
}

inline value_t compute_total(const details_t& details = details_t()) {
  if (value_expr::total_expr.get())
    return value_expr::total_expr->compute(details);
}

//////////////////////////////////////////////////////////////////////

template <typename T>
class item_predicate
{
public:
  value_expr predicate;

  item_predicate() {
    TRACE_CTOR(item_predicate, "");
  }
  item_predicate(const item_predicate& other) : predicate(other.predicate) {
    TRACE_CTOR(item_predicate, "copy");
  }
  item_predicate(const value_expr& _predicate) : predicate(_predicate) {
    TRACE_CTOR(item_predicate, "const value_expr&");
  }
  item_predicate(const string& _predicate) : predicate(_predicate) {
    TRACE_CTOR(item_predicate, "const string&");
  }
  ~item_predicate() throw() {
    TRACE_DTOR(item_predicate);
  }

  bool operator()(const T& item) const {
    return (! predicate ||
	    predicate->compute(value_expr::details_t(item)).strip_annotations());
  }
};

} // namespace ledger

#endif // _VALEXPR_H
