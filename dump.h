#ifndef _DUMP_H
#define _DUMP_H

#include "valexpr.h"

namespace ledger {

class dump_command : public valexpr_t::functor_t
{
 public:
  dump_command() : valexpr_t::functor_t("dump") {}

  virtual void operator()(value_t& result, valexpr_t::scope_t * locals);
};

} // namespace ledger

#endif _DUMP_H
