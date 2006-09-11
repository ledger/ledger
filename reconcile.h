#ifndef _RECONCILE_H
#define _RECONCILE_H

#include "value.h"
#include "walk.h"

namespace ledger {

#if 0
class reconcile_transactions : public item_handler<transaction_t>
{
  value_t    balance;
  datetime_t cutoff;

  transactions_list xacts;

 public:
  reconcile_transactions(item_handler<transaction_t> * handler,
			 const value_t&    _balance,
			 const datetime_t& _cutoff)
    : item_handler<transaction_t>(handler),
      balance(_balance), cutoff(_cutoff) {}

  void push_to_handler(transaction_t * first);

  virtual void flush();
  virtual void operator()(transaction_t& xact) {
    xacts.push_back(&xact);
  }
};
#endif

} // namespace ledger

#endif // _RECONCILE_H
