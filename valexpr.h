#ifndef _EXPR_H
#define _EXPR_H

#include "ledger.h"

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

#if 1
typedef std::list<mask_t> masks_list;

bool matches(const masks_list& regexps, const std::string& str,
	     bool * by_exclusion = NULL);
#endif


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

struct node_t
{
  enum kind_t {
    // Constants
    CONSTANT_A,
    CONSTANT_T,

    // Item details
    AMOUNT,
    COST,
    DATE,
    CLEARED,
    REAL,
    INDEX,

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

  kind_t   type;
  node_t * left;
  node_t * right;

  amount_t    constant_a;
  std::time_t constant_t;
  mask_t *    mask;

  node_t(const kind_t _type)
    : type(_type), left(NULL), right(NULL), mask(NULL) {}

  ~node_t() {
    if (mask)  delete mask;
    if (left)  delete left;
    if (right) delete right;
  }

  void compute(balance_t& result, const details_t& details) const;
};

node_t * parse_expr(std::istream& in);

inline node_t * parse_expr(const char * p) {
  std::istringstream stream(p);
  return parse_expr(stream);
}

inline node_t * parse_expr(const std::string& str) {
  return parse_expr(str.c_str());
}

inline node_t * find_node(node_t * node, node_t::kind_t type) {
  node_t * result = NULL;
  if (node->type == type)
    result = node;
  if (! result && node->left)
    result = find_node(node->left, type);
  if (! result && node->right)
    result = find_node(node->right, type);
  return result;
}

template <typename T>
class item_predicate
{
  const node_t * predicate;

 public:
  item_predicate(const node_t * _predicate) : predicate(_predicate) {}

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

} // namespace report

#endif // _REPORT_H
