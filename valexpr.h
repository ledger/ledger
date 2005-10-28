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

  details_t(const entry_t& _entry)
    : entry(&_entry), xact(NULL), account(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor details_t");
  }
  details_t(const transaction_t& _xact)
    : entry(_xact.entry), xact(&_xact), account(_xact.account) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor details_t");
  }
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

    // Item details
    AMOUNT,
    COST,
    DATE,
    CLEARED,
    REAL,
    ACTUAL,
    INDEX,
    DEPTH,

    // Item totals
    COUNT,
    TOTAL,
    COST_TOTAL,

    // Relating to format_t
    VALUE_EXPR,
    TOTAL_EXPR,

    // Functions
    F_PARENT,
    F_ARITH_MEAN,
    F_VALUE,
    F_NEG,
    F_ABS,
    F_STRIP,
    F_CODE_MASK,
    F_PAYEE_MASK,
    F_NOTE_MASK,
    F_ACCOUNT_MASK,
    F_SHORT_ACCOUNT_MASK,
    F_COMMODITY_MASK,
    F_INTERP_FUNC,

    // Binary operators
    O_ADD,
    O_SUB,
    O_MUL,
    O_DIV,
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
    O_ARG,

    LAST
  };

  kind_t	 kind;
  value_expr_t * left;
  value_expr_t * right;

  union {
    std::time_t	 constant_t;
    long         constant_i;
  };
  std::string    constant_s;
  amount_t       constant_a;
  mask_t *	 mask;

  value_expr_t(const kind_t _kind)
    : kind(_kind), left(NULL), right(NULL), mask(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor value_expr_t");
  }

  ~value_expr_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor value_expr_t");
    if (mask)  delete mask;
    if (left)  delete left;
    if (right) delete right;
  }

  void compute(value_t& result, const details_t& details) const;
};

extern std::auto_ptr<value_expr_t> amount_expr;
extern std::auto_ptr<value_expr_t> total_expr;

extern std::time_t terminus;

inline void compute_amount(value_t& result, const details_t& details) {
  if (amount_expr.get())
    amount_expr->compute(result, details);
}

inline void compute_total(value_t& result, const details_t& details) {
  if (total_expr.get())
    total_expr->compute(result, details);
}

value_expr_t * parse_value_expr(std::istream& in,
				const bool partial = false);

inline value_expr_t * parse_value_expr(const char * p,
				       const bool partial = false) {
  std::istringstream stream(p);
  return parse_value_expr(stream, partial);
}

inline value_expr_t * parse_value_expr(const std::string& str,
				       const bool partial = false) {
  return parse_value_expr(str.c_str());
}

#ifdef DEBUG_ENABLED
void dump_value_expr(std::ostream& out, const value_expr_t * node);
#endif

//////////////////////////////////////////////////////////////////////

template <typename T>
class item_predicate
{
  const value_expr_t * predicate;
  bool                 allocated;

 public:
  item_predicate(const std::string& _predicate)
    : predicate(NULL), allocated(false) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor item_predicate<T>");
    if (! _predicate.empty()) {
      try {
	predicate = parse_value_expr(_predicate);
	allocated = true;
      }
      catch (value_expr_error& err) {
	throw value_expr_error(std::string("In predicate '") +
			       _predicate + "': " + err.what());
      }
    }
  }
  item_predicate(const value_expr_t * _predicate = NULL)
    : predicate(_predicate), allocated(false) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor item_predicate<T>");
  }

  ~item_predicate() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor item_predicate<T>");
    if (predicate && allocated)
      delete predicate;
  }

  bool operator()(const T& item) const {
    if (predicate) {
      value_t result;
      predicate->compute(result, details_t(item));
      return result;
    } else {
      return true;
    }
  }
};

} // namespace ledger

#endif // _VALEXPR_H
