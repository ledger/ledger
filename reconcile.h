#ifndef _RECONCILE_H
#define _RECONCILE_H

#include "value.h"
#include "walk.h"

namespace ledger {

class reconcile_transactions : public item_handler<transaction_t>
{
  value_t balance;
  time_t  cutoff;
  bool	  all_pending;

  transactions_list xacts;

 public:
  reconcile_transactions(item_handler<transaction_t> * handler,
			 const value_t& _balance, const time_t _cutoff,
			 const bool _all_pending)
    : item_handler<transaction_t>(handler),
      balance(_balance), cutoff(_cutoff), all_pending(_all_pending) {}

  void push_to_handler(transaction_t * first);

  virtual void flush();
  virtual void operator()(transaction_t& xact) {
    xacts.push_back(&xact);
  }
};

} // namespace ledger

#endif // _RECONCILE_H
