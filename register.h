#ifndef _REGISTER_H
#define _REGISTER_H

#include "valexpr.h"

namespace ledger {

class register_command : public valexpr_t::functor_t
{
 public:
  register_command() : valexpr_t::functor_t("register") {}

  virtual void operator()(value_t& result, valexpr_t::scope_t * locals);
};

} // namespace ledger

#endif _REGISTER_H
