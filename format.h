#ifndef _FORMAT_H
#define _FORMAT_H

#include "ledger.h"
#include "balance.h"
#include "constraint.h"
#include "expr.h"

namespace ledger {

std::string truncated(const std::string& str, unsigned int width);
std::string maximal_account_name(const item_t * item, const item_t * parent);

struct element_t
{
  enum kind_t {
    STRING,
    VALUE_EXPR,
    DATE_STRING,
    PAYEE,
    ACCOUNT_NAME,
    ACCOUNT_FULLNAME,
    VALUE,
    TOTAL,
    SPACER
  };

  bool         align_left;
  unsigned int min_width;
  unsigned int max_width;

  kind_t       type;
  std::string  chars;
  node_t *     val_expr;

  struct element_t * next;

  element_t() : align_left(false), min_width(0), max_width(0),
		type(STRING), val_expr(NULL), next(NULL) {}

  ~element_t() {
    if (val_expr) delete val_expr;
    if (next) delete next;	// recursive, but not too deep
  }
};

struct format_t
{
  element_t * elements;

  static node_t * value_expr;
  static node_t * total_expr;

  format_t(const std::string& _format) {
    elements = parse_elements(_format);
  }
  ~format_t() {
    if (elements)   delete elements;
  }

  static element_t * parse_elements(const std::string& fmt);

  void format_elements(std::ostream& out, const item_t * item,
		       const item_t * displayed_parent = NULL) const;

#if 1
  static balance_t compute_value(const item_t * item) {
    if (value_expr)
      return value_expr->compute(item);
    else
      return balance_t();
  }

  static balance_t compute_total(const item_t * item) {
    if (total_expr)
      return total_expr->compute(item);
    else
      return balance_t();
  }
#else
  static balance_t compute_value(const item_t * item,
				 const constraints_t& constraints) {
    if (value_expr)
      return value_expr->compute(item, constraints.begin(), constraints.end());
    else
      return balance_t();
  }

  static balance_t compute_total(const item_t * item,
				 const constraints_t& constraints) {
    if (total_expr)
      return total_expr->compute(item, constraints.begin(), constraints.end());
    else
      return balance_t();
  }
#endif
};

} // namespace ledger

#endif // _REPORT_H
