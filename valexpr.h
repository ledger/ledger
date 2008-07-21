#ifndef _VALEXPR_H
#define _VALEXPR_H

#include "value.h"
#include "utils.h"
#include "mask.h"

#include <memory>

namespace ledger {

class entry_t;
class transaction_t;
class account_t;

namespace expr {

DECLARE_EXCEPTION(error, parse_error);
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

  details_t() : entry(NULL), xact(NULL), account(NULL) {}
  details_t(const entry_t& _entry)
    : entry(&_entry), xact(NULL), account(NULL) {
    DEBUG("ledger.memory.ctors", "ctor details_t");
  }
  details_t(const transaction_t& _xact);
  details_t(const account_t& _account)
    : entry(NULL), xact(NULL), account(&_account) {
    DEBUG("ledger.memory.ctors", "ctor details_t");
  }
#ifdef DEBUG_ENABLED
  ~details_t() {
    DEBUG("ledger.memory.dtors", "dtor details_t");
  }
#endif
};

struct op_t;
typedef intrusive_ptr<op_t> ptr_op_t;

class call_scope_t;

typedef function<value_t (call_scope_t&)> function_t;

#define MAKE_FUNCTOR(x) expr::op_t::wrap_functor(bind(&x, this, _1))
#define WRAP_FUNCTOR(x) expr::op_t::wrap_functor(x)

class scope_t : public noncopyable
{
public:
  enum type_t {
    CHILD_SCOPE,
    SYMBOL_SCOPE,
    CALL_SCOPE,
    CONTEXT_SCOPE
  } type_;

  explicit scope_t(type_t _type) : type_(_type) {
    TRACE_CTOR(expr::scope_t, "type_t");
  }
  virtual ~scope_t() {
    TRACE_DTOR(expr::scope_t);
  }

  const type_t type() const {
    return type_;
  }

  virtual void     define(const string& name, ptr_op_t def) = 0;
          void     define(const string& name, const value_t& val);
  virtual ptr_op_t lookup(const string& name) = 0;
          value_t  resolve(const string& name) {
#if 0
    return lookup(name)->calc(*this);
#else
    return value_t();
#endif
  }

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

public:
  explicit child_scope_t(type_t _type = CHILD_SCOPE)
    : scope_t(_type), parent(NULL) {
    TRACE_CTOR(expr::child_scope_t, "type_t");
  }
  explicit child_scope_t(scope_t& _parent, type_t _type = CHILD_SCOPE)
    : scope_t(_type), parent(&_parent) {
    TRACE_CTOR(expr::child_scope_t, "scope_t&, type_t");
  }
  virtual ~child_scope_t() {
    TRACE_DTOR(expr::child_scope_t);
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
    TRACE_CTOR(expr::symbol_scope_t, "");
  }
  explicit symbol_scope_t(scope_t& _parent)
    : child_scope_t(_parent, SYMBOL_SCOPE) {
    TRACE_CTOR(expr::symbol_scope_t, "scope_t&");
  }
  virtual ~symbol_scope_t() {
    TRACE_DTOR(expr::symbol_scope_t);
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

public:
  explicit call_scope_t(scope_t& _parent)
    : child_scope_t(_parent, CALL_SCOPE) {
    TRACE_CTOR(expr::call_scope_t, "scope_t&");
  }
  virtual ~call_scope_t() {
    TRACE_DTOR(expr::call_scope_t);
  }

  void set_args(const value_t& _args) {
    args = _args;
  }

  value_t& value() {
    return args;
  }

  value_t& operator[](const int index) {
    return args[index];
  }
  const value_t& operator[](const int index) const {
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

struct op_t : public noncopyable
{
  enum kind_t {
    // Constants
    VALUE,
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
    F_CODE_MASK,
    F_PAYEE_MASK,
    F_NOTE_MASK,
    F_ACCOUNT_MASK,
    F_SHORT_ACCOUNT_MASK,
    F_COMMODITY_MASK,

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
    O_COM,
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
    TRACE_CTOR(expr::op_t, "const kind_t");
  }
  ~op_t() {
    TRACE_DTOR(expr::op_t);

    DEBUG("ledger.xpath.memory", "Destroying " << this);
    assert(refc == 0);
  }

  bool is_long() const {
    return data.type() == typeid(unsigned int);
  }
  unsigned int& as_long() {
    assert(kind == ARG_INDEX || kind == O_ARG);
    return boost::get<unsigned int>(data);
  }
  const unsigned int& as_long() const {
    return const_cast<op_t *>(this)->as_long();
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
  value_t& as_value() {
    assert(is_value());
    return boost::get<value_t>(data);
  }
  const value_t& as_value() const {
    return const_cast<op_t *>(this)->as_value();
  }
  void set_value(const value_t& val) {
    data = val;
  }

  bool is_string() const {
    if (kind == VALUE) {
      assert(data.type() == typeid(value_t));
      return boost::get<value_t>(data).is_string();
    }
    return false;
  }
  string& as_string() {
    assert(is_string());
    return boost::get<value_t>(data).as_string_lval();
  }
  const string& as_string() const {
    return const_cast<op_t *>(this)->as_string();
  }
  void set_string(const string& val) {
    data = value_t(val);
  }

  bool is_function() const {
    return kind == FUNCTION;
  }
  function_t& as_function() {
    assert(kind == FUNCTION);
    return boost::get<function_t>(data);
  }
  const function_t& as_function() const {
    return const_cast<op_t *>(this)->as_function();
  }
  void set_function(const function_t& val) {
    data = val;
  }

#if 0
  bool is_name() const {
    return data.type() == typeid(node_t::nameid_t);
  }
  node_t::nameid_t& as_name() {
    assert(kind == NODE_ID || kind == ATTR_ID);
    return boost::get<node_t::nameid_t>(data);
  }
  const node_t::nameid_t& as_name() const {
    return const_cast<op_t *>(this)->as_name();
  }
  void set_name(const node_t::nameid_t& val) {
    data = val;
  }
#endif

  ptr_op_t& as_op() {
    assert(kind > TERMINALS);
    return boost::get<ptr_op_t>(data);
  }
  const ptr_op_t& as_op() const {
    return const_cast<op_t *>(this)->as_op();
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
    return as_op();
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

#if 0
class op_predicate {
  ptr_op_t op;

public:
  explicit op_predicate(ptr_op_t _op) : op(_op) {}
  bool operator()(scope_t& scope) {
    return op->calc(scope).to_boolean();
  }
};
#endif

class valexpr_context : public error_context {
 public:
  ptr_op_t expr;
  ptr_op_t error_node;

  valexpr_context(const ptr_op_t& _expr,
		  const string&   desc = "") throw()
    : error_context(desc), expr(_expr), error_node(_expr) {}
  virtual ~valexpr_context() throw() {}

  virtual void describe(std::ostream& out) const throw();
};

class compute_error : public error {
 public:
  compute_error(const string& reason, error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~compute_error() throw() {}
};

class value_expr_error : public error {
 public:
  value_expr_error(const string& reason,
		   error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~value_expr_error() throw() {}
};

extern std::auto_ptr<symbol_scope_t> global_scope;

extern datetime_t terminus;
extern bool	  initialized;

void init_value_expr();

bool compute_amount(const ptr_op_t expr, amount_t& amt,
		    const transaction_t * xact,
		    const ptr_op_t context = NULL);

#define PARSE_VALEXPR_NORMAL	 0x00
#define PARSE_VALEXPR_PARTIAL	 0x01
#define PARSE_VALEXPR_RELAXED	 0x02
#define PARSE_VALEXPR_NO_MIGRATE 0x04
#define PARSE_VALEXPR_NO_REDUCE  0x08

ptr_op_t parse_boolean_expr(std::istream& in, scope_t * scope,
			    const short flags);

ptr_op_t parse_value_expr(std::istream& in,
			  scope_t * scope = NULL,
			  const short flags = PARSE_VALEXPR_RELAXED);

inline ptr_op_t
parse_value_expr(const string& str,
		 scope_t *     scope	  = NULL,
		 const short   flags = PARSE_VALEXPR_RELAXED) {
  std::istringstream stream(str);
  try {
    return parse_value_expr(stream, scope, flags);
  }
  catch (error * err) {
    err->context.push_back
      (new line_context(str, (long)stream.tellg() - 1,
			"While parsing value expression:"));
    throw err;
  }
}

inline ptr_op_t
parse_value_expr(const char * p,
		 scope_t *    scope = NULL,
		 const short  flags  = PARSE_VALEXPR_RELAXED) {
  return parse_value_expr(string(p), scope, flags);
}

void dump_value_expr(std::ostream& out, const ptr_op_t node,
		     const int depth = 0);

bool print_value_expr(std::ostream&	   out,
		      const ptr_op_t node,
		      const bool           relaxed      = true,
		      const ptr_op_t node_to_find = NULL,
		      unsigned long *	   start_pos    = NULL,
		      unsigned long *	   end_pos      = NULL);

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

template<>
inline context_scope_t&
scope_t::find_scope<context_scope_t>(bool skip_this) {
  optional<scope_t&> scope = find_scope(CONTEXT_SCOPE, skip_this);
  assert(scope);
  return downcast<context_scope_t>(*scope);
}

#define FIND_SCOPE(scope_type, scope_ref) \
  downcast<scope_t>(scope_ref).find_scope<scope_type>()

#define CALL_SCOPE(scope_ref) \
  FIND_SCOPE(call_scope_t, scope_ref)
#define SYMBOL_SCOPE(scope_ref) \
  FIND_SCOPE(symbol_scope_t, scope_ref)
#define CONTEXT_SCOPE(scope_ref) \
  FIND_SCOPE(context_scope_t, scope_ref)

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

} // namespace expr

//////////////////////////////////////////////////////////////////////

class value_expr
{
  expr::ptr_op_t ptr;

public:
  string expr;

  typedef expr::details_t details_t;

  value_expr() : ptr(NULL) {}

  value_expr(const string& _expr) : expr(_expr) {
    DEBUG("ledger.memory.ctors", "ctor value_expr");
    if (! _expr.empty())
      ptr = expr::parse_value_expr(expr);
    else
      ptr = expr::ptr_op_t();
  }
  value_expr(const expr::ptr_op_t _ptr) : ptr(_ptr) {
    DEBUG("ledger.memory.ctors", "ctor value_expr");
  }
  value_expr(const value_expr& other) : ptr(other.ptr), expr(other.expr) {
    DEBUG("ledger.memory.ctors", "ctor value_expr");
  }
  virtual ~value_expr() {
    DEBUG("ledger.memory.dtors", "dtor value_expr");
    if (ptr)
      ptr->release();
  }

  value_expr& operator=(const string& _expr) {
    expr = _expr;
    reset(expr::parse_value_expr(expr));
    return *this;
  }
  value_expr& operator=(expr::ptr_op_t _expr) {
    expr = "";
    reset(_expr);
    return *this;
  }
  value_expr& operator=(const value_expr& _expr) {
    expr = _expr.expr;
    reset(_expr.get());
    return *this;
  }

  operator bool() const throw() {
    return ptr != NULL;
  }
  operator string() const throw() {
    return expr;
  }
  operator const expr::ptr_op_t() const throw() {
    return ptr;
  }

  const expr::op_t& operator*() const throw() {
    return *ptr;
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
};

typedef value_expr::details_t details_t; // jww (2008-07-20): remove

extern value_expr amount_expr;
extern value_expr total_expr;

inline void compute_amount(value_t& result,
			   const details_t& details = details_t()) {
  if (amount_expr)
    amount_expr->compute(result, details);
}

inline value_t compute_amount(const details_t& details = details_t()) {
  if (amount_expr)
    return amount_expr->compute(details);
}

inline void compute_total(value_t& result,
			  const details_t& details = details_t()) {
  if (total_expr)
    total_expr->compute(result, details);
}

inline value_t compute_total(const details_t& details = details_t()) {
  if (total_expr)
    return total_expr->compute(details);
}

inline void parse_value_definition(const string& str,
				   expr::scope_t * scope = NULL) {
  std::istringstream def(str);
  value_expr expr
    (expr::parse_boolean_expr(def, scope ? scope : expr::global_scope.get(),
			      PARSE_VALEXPR_RELAXED));
}

//////////////////////////////////////////////////////////////////////

template <typename T>
class item_predicate
{
public:
  value_expr predicate;

  item_predicate() {
    TRACE_CTOR(item_predicate, "ctor item_predicate<T>()");
  }
  item_predicate(const value_expr& _predicate) : predicate(_predicate) {
    TRACE_CTOR(item_predicate, "ctor item_predicate<T>(const value_expr&)");
  }
  item_predicate(const string& _predicate) : predicate(_predicate) {
    TRACE_CTOR(item_predicate, "ctor item_predicate<T>(const string&)");
  }

  ~item_predicate() {
    TRACE_DTOR(item_predicate);
  }

  bool operator()(const T& item) const {
    return (! predicate ||
	    predicate->compute(value_expr::details_t(item)).strip_annotations());
  }
};

} // namespace ledger

#endif // _VALEXPR_H
