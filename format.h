#ifndef _FORMAT_H
#define _FORMAT_H

#include "ledger.h"
#include "valexpr.h"

namespace ledger {

std::string truncated(const std::string& str, unsigned int width);

std::string partial_account_name(const account_t *  account,
				 const unsigned int start_depth);

struct element_t
{
  enum kind_t {
    STRING,
    VALUE_EXPR,
    DATE_STRING,
    CLEARED,
    CODE,
    PAYEE,
    ACCOUNT_NAME,
    ACCOUNT_FULLNAME,
    OPT_AMOUNT,
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
  std::auto_ptr<element_t> elements;

  static std::auto_ptr<node_t>	  value_expr;
  static std::auto_ptr<node_t>	  total_expr;

  format_t(const std::string& _format) {
    elements.reset(parse_elements(_format));
  }

  static element_t * parse_elements(const std::string& fmt);

  void format_elements(std::ostream& out, const details_t& details) const;

  static void compute_value(balance_t& result, const details_t& details) {
    if (value_expr.get())
      value_expr->compute(result, details);
  }

  static void compute_total(balance_t& result, const details_t& details) {
    if (total_expr.get())
      total_expr->compute(result, details);
  }
};

} // namespace ledger

#endif // _REPORT_H
