#ifndef _REPORT_H
#define _REPORT_H

#include "ledger.h"
#include "constraint.h"
#include "balance.h"

namespace ledger {

std::string truncated(const std::string& str, unsigned int width);
std::string maximal_account_name(const item_t * item, const item_t * parent);

struct format_t
{
  constraints_t constraints;

  std::string	format_string;
  node_t *	value_style;
  node_t *	total_style;

  format_t() {
    value_style = NULL;
    total_style = NULL;
  }

  ~format_t() {
    if (value_style) delete value_style;
    if (total_style) delete total_style;
  }

  balance_t compute_value(const item_t * item) const {
    if (value_style)
      return value_style->compute(begin(), end(), item);
    else
      return balance_t();
  }

  balance_t compute_total(const item_t * item) const {
    if (total_style)
      return total_style->compute(begin(), end(), item);
    else
      return balance_t();
  }

  std::string report_line(const item_t * item,
			  const item_t * displayed_parent = NULL);
};

} // namespace ledger

#endif // _REPORT_H
