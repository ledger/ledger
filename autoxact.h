#ifndef _AUTOXACT_H
#define _AUTOXACT_H

#include "ledger.h"
#include "valexpr.h"

#include <deque>

namespace ledger {

typedef std::deque<transaction_t *> transactions_deque;

class automated_transaction_t
{
public:
  item_predicate<transaction_t> predicate;
  transactions_deque		transactions;

  automated_transaction_t(const std::string&  _predicate,
			  transactions_deque& _transactions)
    : predicate(_predicate) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor automated_transaction_t");
    transactions.insert(transactions.begin(),
			_transactions.begin(), _transactions.end());
    // Take over ownership of the pointers
    _transactions.clear();
  }

  ~automated_transaction_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor automated_transaction_t");
    for (transactions_deque::iterator i = transactions.begin();
	 i != transactions.end();
	 i++)
      delete *i;
  }

  void extend_entry(entry_t * entry);
};


typedef std::deque<automated_transaction_t *> automated_transactions_deque;

class automated_transactions_t
{
#ifdef DEBUG_ENABLED
  automated_transactions_t(const automated_transactions_t&);
#endif

public:
  automated_transactions_deque automated_transactions;

#ifdef DEBUG_ENABLED
  automated_transactions_t() {
    DEBUG_PRINT("ledger.memory.ctors", "ctor automated_transactions_t");
  }
#endif
  ~automated_transactions_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor automated_transactions_t");
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
