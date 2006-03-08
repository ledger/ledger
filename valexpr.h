#ifndef _VALEXPR_H
#define _VALEXPR_H

#include "journal.h"
#include "value.h"
#include "error.h"
#include "mask.h"

#include <memory>

namespace ledger {

struct details_t
{
  const entry_t *	entry;
  const transaction_t * xact;
  const account_t *     account;

  details_t() : entry(NULL), xact(NULL), account(NULL) {}
  details_t(const entry_t& _entry)
    : entry(&_entry), xact(NULL), account(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor details_t");
  }
  details_t(const transaction_t& _xact);
  details_t(const account_t& _account)
    : entry(NULL), xact(NULL), account(&_account) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor details_t");
  }
#ifdef DEBUG_ENABLED
  ~details_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor details_t");
  }
#endif
};

struct value_expr_t
{
  enum kind_t {
    // Constants
    CONSTANT_I,
    CONSTANT_T,
    CONSTANT_A,
    CONSTANT_V,

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

  kind_t	 kind;
  mutable short  refc;
  value_expr_t * left;

  union {
    datetime_t *   constant_t;
    long	   constant_i;
    amount_t *	   constant_a;
    value_t *	   constant_v;
    mask_t *	   mask;
    value_expr_t * right;
  };

  value_expr_t(const kind_t _kind)
    : kind(_kind), refc(0), left(NULL), right(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor value_expr_t " << this);
  }
  value_expr_t(const value_expr_t&) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor value_expr_t (copy) " << this);
  }
  ~value_expr_t();

  void release() const {
    DEBUG_PRINT("ledger.valexpr.memory",
		"Releasing " << this << ", refc now " << refc - 1);
    assert(refc > 0);
    if (--refc == 0)
      delete this;
  }
  value_expr_t * acquire() {
    DEBUG_PRINT("ledger.valexpr.memory",
		"Acquiring " << this << ", refc now " << refc + 1);
    assert(refc >= 0);
    refc++;
    return this;
  }
  const value_expr_t * acquire() const {
    DEBUG_PRINT("ledger.valexpr.memory",
		"Acquiring " << this << ", refc now " << refc + 1);
    refc++;
    return this;
  }

  void set_left(value_expr_t * expr) {
    assert(kind > TERMINALS);
    if (left)
      left->release();
    left = expr ? expr->acquire() : NULL;
  }

  void set_right(value_expr_t * expr) {
    assert(kind > TERMINALS);
    if (right)
      right->release();
    right = expr ? expr->acquire() : NULL;
  }

  void compute(value_t& result,
	       const details_t& details = details_t(),
	       value_expr_t *   context = NULL) const;
  value_t compute(const details_t& details = details_t(),
		  value_expr_t *   context = NULL) const {
    value_t temp;
    compute(temp, details, context);
    return temp;
  }
};

class valexpr_context : public error_context {
 public:
  const ledger::value_expr_t * expr;
  const ledger::value_expr_t * error_node;

  valexpr_context(const ledger::value_expr_t * _expr,
		  const std::string& desc = "") throw();
  virtual ~valexpr_context() throw();

  virtual void describe(std::ostream& out) const throw();
};

class compute_error : public error {
 public:
  compute_error(const std::string& reason, error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~compute_error() throw() {}
};

class value_expr_error : public error {
 public:
  value_expr_error(const std::string& reason,
		   error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~value_expr_error() throw() {}
};

struct scope_t
{
  scope_t * parent;

  typedef std::map<const std::string, value_expr_t *>  symbol_map;
  typedef std::pair<const std::string, value_expr_t *> symbol_pair;
  
  symbol_map symbols;

  scope_t(scope_t * _parent = NULL) : parent(_parent) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor scope_t");
  }
  ~scope_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor scope_t");
    for (symbol_map::iterator i = symbols.begin();
	 i != symbols.end();
	 i++)
      (*i).second->release();
  }

  void define(const std::string& name, value_expr_t * def) {
    DEBUG_PRINT("ledger.valexpr.syms",
		"Defining '" << name << "' = " << def);
    std::pair<symbol_map::iterator, bool> result
      = symbols.insert(symbol_pair(name, def));
    if (! result.second) {
      symbols.erase(name);
      std::pair<symbol_map::iterator, bool> result
	= symbols.insert(symbol_pair(name, def));
      if (! result.second) {
	def->release();
	throw new compute_error(std::string("Redefinition of '") +
				name + "' in same scope");
      }
    }
    def->acquire();
  }
  value_expr_t * lookup(const std::string& name) {
    symbol_map::const_iterator i = symbols.find(name);
    if (i != symbols.end())
      return (*i).second;
    else if (parent)
      return parent->lookup(name);
    return NULL;
  }
};

extern std::auto_ptr<scope_t> global_scope;

extern std::time_t terminus;
extern bool	   initialized;

void init_value_expr();

bool compute_amount(value_expr_t * expr, amount_t& amt,
		    const transaction_t * xact,
		    value_expr_t * context = NULL);

struct scope_t;
value_expr_t * parse_boolean_expr(std::istream& in, scope_t * scope);

inline value_expr_t * parse_boolean_expr(const std::string& str,
					 scope_t * scope = NULL) {
  std::istringstream stream(str);
  try {
    return parse_boolean_expr(stream, scope);
  }
  catch (error * err) {
    err->context.push_back
      (new error_context("While parsing value expression: " + str));
    throw err;
  }
}

inline value_expr_t * parse_boolean_expr(const char * p,
					 scope_t * scope = NULL) {
  return parse_boolean_expr(std::string(p), scope);
}

value_expr_t * parse_value_expr(std::istream& in,
				scope_t * scope = NULL,
				const bool partial = false);

inline value_expr_t * parse_value_expr(const std::string& str,
				       scope_t * scope = NULL,
				       const bool partial = false) {
  std::istringstream stream(str);
  try {
    return parse_value_expr(stream, scope, partial);
  }
  catch (error * err) {
    err->context.push_back
      (new line_context(str, (long)stream.tellg() - 1,
			"While parsing value expression:"));
    throw err;
  }
}

inline value_expr_t * parse_value_expr(const char * p,
				       scope_t * scope = NULL,
				       const bool partial = false) {
  return parse_value_expr(std::string(p), scope, partial);
}

void dump_value_expr(std::ostream& out, const value_expr_t * node,
		     const int depth = 0);

unsigned long write_value_expr(std::ostream&	    out,
			       const value_expr_t * node,
			       const value_expr_t * node_to_find = NULL,
			       unsigned long	    start_pos    = 0UL);

//////////////////////////////////////////////////////////////////////

inline void guarded_compute(const value_expr_t * expr,
			    value_t&		 result,
			    const details_t&	 details = details_t(),
			    value_expr_t *       context = NULL) {
  try {
    expr->compute(result, details);
  }
  catch (error * err) {
    if (err->context.empty() ||
	! dynamic_cast<valexpr_context *>(err->context.back()))
      err->context.push_back(new valexpr_context(expr));
    error_context * last = err->context.back();
    if (valexpr_context * ctxt = dynamic_cast<valexpr_context *>(last)) {
      ctxt->expr = expr->acquire();
      ctxt->desc = "While computing value expression:";
    }
    throw err;
  }
}

inline value_t guarded_compute(const value_expr_t * expr,
			       const details_t&	    details = details_t(),
			       value_expr_t *       context = NULL) {
  value_t temp;
  guarded_compute(expr, temp, details, context);
  return temp;
}

//////////////////////////////////////////////////////////////////////
//
// This class is used so that during the "in between" stages of value
// expression parsing -- while no one yet holds a reference to the
// value_expr_t object -- we can be assured of deletion should an
// exception happen to whip by.

struct value_auto_ptr {
  value_expr_t * ptr;
  value_auto_ptr() : ptr(NULL) {}
  explicit value_auto_ptr(value_expr_t * _ptr)
    : ptr(_ptr ? _ptr->acquire() : NULL) {}
  ~value_auto_ptr() {
    if (ptr)
      ptr->release();
  }
  value_expr_t& operator*() const throw() {
    return *ptr;
  }
  value_expr_t * operator->() const throw() {
    return ptr;
  }
  value_expr_t * get() const throw() { return ptr; }
  value_expr_t * release() throw() {
    value_expr_t * tmp = ptr;
    ptr = 0;
    return tmp;
  }
  void reset(value_expr_t * p = 0) throw() {
    if (p != ptr) {
      if (ptr)
	ptr->release();
      ptr = p->acquire();
    }
  }
};

//////////////////////////////////////////////////////////////////////

class value_expr
{
  value_expr_t * parsed;
public:
  std::string    expr;

  value_expr(const std::string& _expr) : expr(_expr) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor value_expr");
    parsed = parse_value_expr(expr)->acquire();
  }
  value_expr(value_expr_t * _parsed) : parsed(_parsed->acquire()) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor value_expr");
  }
  virtual ~value_expr() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor value_expr");
    if (parsed)
      parsed->release();
  }

  virtual void compute(value_t& result,
		       const details_t& details = details_t(),
		       value_expr_t *   context = NULL) {
    guarded_compute(parsed, result, details, context);
  }
  virtual value_t compute(const details_t& details = details_t(),
			  value_expr_t *   context = NULL) {
    value_t temp;
    guarded_compute(parsed, temp, details, context);
    return temp;
  }
};

extern std::auto_ptr<value_expr> amount_expr;
extern std::auto_ptr<value_expr> total_expr;

inline void compute_amount(value_t& result,
			   const details_t& details = details_t()) {
  if (amount_expr.get())
    amount_expr->compute(result, details);
}

inline value_t compute_amount(const details_t& details = details_t()) {
  if (amount_expr.get())
    return amount_expr->compute(details);
}

inline void compute_total(value_t& result,
			  const details_t& details = details_t()) {
  if (total_expr.get())
    total_expr->compute(result, details);
}

inline value_t compute_total(const details_t& details = details_t()) {
  if (total_expr.get())
    return total_expr->compute(details);
}

//////////////////////////////////////////////////////////////////////

template <typename T>
class item_predicate
{
 public:
  const value_expr_t * predicate;

  item_predicate(const std::string& _predicate) : predicate(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor item_predicate<T>");
    if (! _predicate.empty())
      predicate = parse_value_expr(_predicate)->acquire();
  }
  item_predicate(const value_expr_t * _predicate = NULL)
    : predicate(_predicate->acquire()) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor item_predicate<T>");
  }

  ~item_predicate() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor item_predicate<T>");
    if (predicate)
      predicate->release();
  }

  bool operator()(const T& item) const {
    return ! predicate || predicate->compute(details_t(item));
  }
};

} // namespace ledger

#endif // _VALEXPR_H
