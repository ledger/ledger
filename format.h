#ifndef _FORMAT_H
#define _FORMAT_H

#include "ledger.h"
#include "balance.h"
#include "constraint.h"
#include "expr.h"

namespace ledger {

std::string truncated(const std::string& str, unsigned int width);
std::string maximal_account_name(const item_t * item, const item_t * parent);

struct format_t
{
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

#if 1
  balance_t compute_value(const item_t * item) const {
    if (value_style)
      return value_style->compute(item);
    else
      return balance_t();
  }

  balance_t compute_total(const item_t * item) const {
    if (total_style)
      return total_style->compute(item);
    else
      return balance_t();
  }
#else
  balance_t compute_value(const item_t * item,
			  const constraints_t& constraints) const {
    if (value_style)
      return value_style->compute(item, constraints.begin(), constraints.end());
    else
      return balance_t();
  }

  balance_t compute_total(const item_t * item,
			  const constraints_t& constraints) const {
    if (total_style)
      return total_style->compute(item, constraints.begin(), constraints.end());
    else
      return balance_t();
  }
#endif

  std::string report_line(const item_t * item,
			  const item_t * displayed_parent = NULL) const;
};

} // namespace ledger

#endif // _REPORT_H
