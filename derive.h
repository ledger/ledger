#ifndef _DERIVE_H
#define _DERIVE_H

#include "journal.h"

namespace ledger {

class derive_command : public valexpr_t::functor_t
{
 public:
  derive_command() : valexpr_t::functor_t("entry", true) {}

  virtual void operator()(value_t& result, valexpr_t::scope_t * locals);
};

} // namespace ledger

#endif // _DERIVE_H
