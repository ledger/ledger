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
  const entry_t *	 entry;
  const transaction_t *  xact;
  const account_t *      account;
  const balance_pair_t * balance;
  const unsigned int *   index;
  const unsigned int     depth;

  details_t(const entry_t *	   _entry,
	    const balance_pair_t * _balance = NULL,
	    const unsigned int *   _index   = NULL)
    : entry(_entry), xact(NULL), account(NULL),
      balance(_balance), index(_index), depth(0) {}

  details_t(const transaction_t *  _xact,
	    const balance_pair_t * _balance = NULL,
	    const unsigned int *   _index   = NULL)
    : entry(_xact->entry), xact(_xact), account(_xact->account),
      balance(_balance), index(_index), depth(0) {}

  details_t(const account_t *  _account,
	    const unsigned int _depth = 0)
    : entry(NULL), xact(NULL), account(_account),
      balance(NULL), index(NULL), depth(_depth) {}
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

} // namespace report

#endif // _REPORT_H
