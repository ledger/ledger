#ifndef _MASK_H
#define _MASK_H

#include "utils.h"

#include <boost/regex.hpp>

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

class mask_error : public error {
 public:
  mask_error(const string& _reason) throw() : error(_reason) {}
  virtual ~mask_error() throw() {}
};

} // namespace ledger

#endif // _MASK_H
