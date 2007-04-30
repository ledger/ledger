#ifndef _MASK_H
#define _MASK_H

#include "utils.h"

namespace ledger {

class mask_t
{
 public:
  bool	       exclude;
  boost::regex expr;

  explicit mask_t(const string& pattern);
  mask_t(const mask_t& m) : exclude(m.exclude), expr(m.expr) {}

  bool match(const string& str) const {
    return boost::regex_match(str, expr) && ! exclude;
  }
};

} // namespace ledger

#endif // _MASK_H
