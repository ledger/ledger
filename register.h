#ifndef _REGISTER_H
#define _REGISTER_H

#include "valexpr.h"

namespace ledger {

class report_t;

class register_command : public valexpr_t::functor_t
{
  report_t * report;

 public:
  register_command(report_t * _report)
    : valexpr_t::functor_t("register"), report(_report) {}

  virtual void operator()(value_t& result, valexpr_t::scope_t * locals);
};

} // namespace ledger

#endif _REGISTER_H
