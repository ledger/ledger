#ifndef _DERIVE_H
#define _DERIVE_H

#include "xpath.h"

namespace ledger {

class derive_command : public xml::xpath_t::functor_t
{
 public:
  derive_command() : xml::xpath_t::functor_t("entry", true) {}

  virtual void operator()(value_t& result, xml::xpath_t::scope_t * locals);
};

} // namespace ledger

#endif // _DERIVE_H
