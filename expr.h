#ifndef _REPORT_H
#define _REPORT_H

#include "ledger.h"
#include "constraint.h"
#include "balance.h"

namespace ledger {

enum kind_t {
  // Constants
  CONSTANT_A,
  CONSTANT_T,

  // Item details
  AMOUNT,
  COST,
  DATE,
  INDEX,

  // Item totals
  BALANCE,
  COST_BALANCE,
  TOTAL,
  COST_TOTAL,

  // Constraint details
  BEGIN_DATE,
  END_DATE,

  // Functions
  F_ARITH_MEAN,
  F_VALUE,
  F_NEG,
  F_ABS,
  F_REGEXP,

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

struct node_t
{
  kind_t   type;
  node_t * left;
  node_t * right;

  amount_t    constant_a;
  std::time_t constant_t;
  mask_t *    mask;

  node_t(const kind_t _type)
    : type(_type), left(NULL), right(NULL) {}

  ~node_t() {
    if (mask)  delete mask;
    if (left)  delete left;
    if (right) delete right;
  }

  balance_t compute(const item_t * item,
		    const std::time_t begin = -1,
		    const std::time_t end   = -1) const;

  balance_t compute(const item_t * item,
		    const constraint_t& constraints) const {
    return compute(item, constraints.begin(), constraints.end());
  }
};

node_t * parse_expr(std::istream& in, ledger_t * ledger);

inline node_t * parse_expr(const char * p, ledger_t * ledger) {
  std::istringstream stream(p);
  return parse_expr(stream, ledger);
}

inline node_t * parse_expr(const std::string& str, ledger_t * ledger) {
  return parse_expr(str.c_str(), ledger);
}

inline node_t * find_node(node_t * node, kind_t type) {
  node_t * result = NULL;
  if (node->type == type)
    result = node;
  if (! result && node->left)
    result = find_node(node->left, type);
  if (! result && node->right)
    result = find_node(node->right, type);
  return result;
}

void dump_tree(std::ostream& out, node_t * node);

} // namespace report

#endif // _REPORT_H
