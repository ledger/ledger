#ifndef _EXPR_H
#define _EXPR_H

#include "ledger.h"
#include "balance.h"
#include "constraint.h"

namespace ledger {

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

  balance_t compute(const item_t * item,
		    const std::time_t begin = -1,
		    const std::time_t end   = -1) const;
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

void dump_tree(std::ostream& out, node_t * node);

} // namespace report

#endif // _REPORT_H
