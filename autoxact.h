#ifndef _AUTOXACT_H
#define _AUTOXACT_H

#include "ledger.h"
#include "valexpr.h"

#include <deque>

namespace ledger {

class automated_transaction_t
{
public:
  masks_list        masks;
  transactions_list transactions;

  automated_transaction_t(masks_list& _masks,
			  transactions_list& _transactions) {
    masks.insert(masks.begin(), _masks.begin(), _masks.end());
    transactions.insert(transactions.begin(),
			_transactions.begin(), _transactions.end());
    // Take over ownership of the pointers
    _transactions.clear();
  }

  ~automated_transaction_t() {
    for (transactions_list::iterator i = transactions.begin();
	 i != transactions.end();
	 i++)
      delete *i;
  }

  void extend_entry(entry_t * entry);
};


typedef std::deque<automated_transaction_t *> automated_transactions_deque;

class automated_transactions_t
{
public:
  automated_transactions_deque automated_transactions;

  ~automated_transactions_t() {
    for (automated_transactions_deque::iterator i
	   = automated_transactions.begin();
	 i != automated_transactions.end();
	 i++)
      delete *i;
  }

  void extend_entry(entry_t * entry) {
    for (automated_transactions_deque::iterator i
	   = automated_transactions.begin();
	 i != automated_transactions.end();
	 i++)
      (*i)->extend_entry(entry);
  }

  void add_automated_transaction(automated_transaction_t * auto_xact) {
    automated_transactions.push_back(auto_xact);
  }
  bool remove_automated_transaction(automated_transaction_t * auto_xact) {
    for (automated_transactions_deque::iterator i
	   = automated_transactions.begin();
	 i != automated_transactions.end();
	 i++) {
      if (*i == auto_xact) {
	automated_transactions.erase(i);
	return true;
      }
    }
    return false;
  }
};

} // namespace ledger

#endif // _AUTOXACT_H
