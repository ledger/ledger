#ifndef _RECONCILE_H
#define _RECONCILE_H

#include "value.h"
#include "walk.h"

namespace ledger {

class reconcile_xacts : public item_handler<xact_t>
{
  value_t    balance;
  date_t     cutoff;
  xacts_list xacts;

  reconcile_xacts();

public:
  reconcile_xacts(xact_handler_ptr handler,
		  const value_t&   _balance,
		  const date_t&	   _cutoff)
    : item_handler<xact_t>(handler),
      balance(_balance), cutoff(_cutoff) {
    TRACE_CTOR(reconcile_xacts,
	       "xact_handler_ptr, const value_t&, const date_t&");
  }
  virtual ~reconcile_xacts() throw() {
    TRACE_DTOR(reconcile_xacts);
  }

  void push_to_handler(xact_t * first);

  virtual void flush();
  virtual void operator()(xact_t& xact) {
    xacts.push_back(&xact);
  }
};

} // namespace ledger

#endif // _RECONCILE_H
