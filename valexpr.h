#ifndef _EXPR_H
#define _EXPR_H

#include "ledger.h"
#include "error.h"

namespace ledger {

class mask_t
{
 public:
  bool        exclude;
  std::string pattern;
  void *      regexp;

  explicit mask_t(const std::string& pattern);
  mask_t(const mask_t&);

  ~mask_t();

  bool match(const std::string& str) const;
};


struct details_t
{
  const entry_t *	entry;
  const transaction_t * xact;
  const account_t *     account;

  details_t(const entry_t * _entry)
    : entry(_entry), xact(NULL), account(NULL) {}

  details_t(const transaction_t * _xact)
    : entry(_xact->entry), xact(_xact), account(_xact->account) {}

  details_t(const account_t * _account)
    : entry(NULL), xact(NULL), account(_account) {}
};

struct value_expr_t
{
  enum kind_t {
    // Constants
    CONSTANT_A,
    CONSTANT_T,

    // Item details
    AMOUNT,
    COST,
    DATE,
    TODAY,
    CLEARED,
    REAL,
    INDEX,			// for accounts, this is the DEPTH

    // Item totals
    BALANCE,
    COST_BALANCE,
    TOTAL,
    COST_TOTAL,

    // Functions
    F_ARITH_MEAN,
    F_VALUE,
    F_NEG,
    F_ABS,
    F_STRIP,
    F_PAYEE_MASK,
    F_ACCOUNT_MASK,

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

    LAST
  };

  kind_t      type;
  value_expr_t * left;
  value_expr_t * right;

  amount_t    constant_a;
  std::time_t constant_t;
  mask_t *    mask;

  value_expr_t(const kind_t _type)
    : type(_type), left(NULL), right(NULL), mask(NULL) {}

  ~value_expr_t() {
    if (mask)  delete mask;
    if (left)  delete left;
    if (right) delete right;
  }

  void compute(balance_t& result, const details_t& details) const;
};

value_expr_t * parse_value_expr(std::istream& in);

inline value_expr_t * parse_value_expr(const char * p) {
  std::istringstream stream(p);
  return parse_value_expr(stream);
}

inline value_expr_t * parse_value_expr(const std::string& str) {
  return parse_value_expr(str.c_str());
}

#ifdef DEBUG_ENABLED
void dump_value_expr(std::ostream& out, const value_expr_t * node);
#endif

template <typename T>
class item_predicate
{
  const value_expr_t * predicate;

 public:
  item_predicate(const std::string& _predicate) {
    predicate = NULL;
    if (! _predicate.empty()) {
      try {
	DEBUG_CLASS("valexpr.predicate.parse");

	DEBUG_PRINT_("parsing: '" << _predicate << "'");
	predicate = parse_value_expr(_predicate);

#ifdef DEBUG_ENABLED
	if (DEBUG_() && ledger::debug_stream) {
	  *ledger::debug_stream << "dump: ";
	  dump_value_expr(*ledger::debug_stream, predicate);
	  *ledger::debug_stream << std::endl;
	}
#endif
      }
      catch (const value_expr_error& err) {
	std::cerr << "Error in predicate '" << _predicate << "': "
		  << err.what() << std::endl;
	std::exit(1);
      }
    }
  }
  item_predicate(const value_expr_t * _predicate)
    : predicate(_predicate) {}

  ~item_predicate() {
    if (predicate)
      delete predicate;
  }

  bool operator()(const T * item) const {
    if (predicate) {
      balance_t result;
      predicate->compute(result, details_t(item));
      return result;
    } else {
      return true;
    }
  }
};

std::string regexps_to_predicate(std::list<std::string>::const_iterator begin,
				 std::list<std::string>::const_iterator end,
				 const bool account_regexp = true);

} // namespace report

#endif // _REPORT_H
