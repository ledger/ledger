#ifndef _CONSTRAINT_H
#define _CONSTRAINT_H

#include "ledger.h"
#include "expr.h"
#include "item.h"

template <typename ForwardIterator, typename ValueType, typename Constraint>
class constrained_iterator
{
  ForwardIterator iter, end;
  const Constraint& constraint;

  constrained_iterator& operator=(const constrained_iterator&);

 public:
  constrained_iterator(ForwardIterator begin, ForwardIterator _end,
		       const Constraint& _constraint)
    : iter(begin), end(_end), constraint(_constraint) {
    skip_nonmatching();
  }

  constrained_iterator(const constrained_iterator& other)
    : iter(other.iter), end(other.end), constraint(other.constraint) {}

  constrained_iterator operator++() const {
    ForwardIterator temp = iter;
    temp++;
    return constrained_iterator(temp, end);
  }

  constrained_iterator& operator++(int) {
    iter++;
    skip_nonmatching();
    return *this;
  }

  bool operator==(ForwardIterator other) const {
    return iter == other;
  }
  bool operator!=(ForwardIterator other) const {
    return ! (*this == other);
  }

  ValueType operator*() const {
    return *iter;
  }

  void skip_nonmatching() {
    bool failed;
    do {
      if (iter == end) return;
      failed = false;
      if (! constraint(*iter)) {
	failed = true;
	iter++;
      }
    } while (failed);
  }
};

namespace ledger {

class constraints_t
{
 public:
  bool show_expanded;
  bool show_related;
  bool show_inverted;
  bool show_subtotals;
  bool show_empty;

  node_t * predicate;

  explicit constraints_t() {
    show_expanded  = false;
    show_related   = false;
    show_inverted  = false;
    show_subtotals = true;
    show_empty     = false;

    predicate      = NULL;
  }

  ~constraints_t() {
    if (predicate)  delete predicate;
  }

  bool operator ()(const transaction_t * xact) const {
    if (! predicate) {
      return true;
    } else {
      item_t temp;
      temp.date    = xact->entry->date;
      temp.payee   = xact->entry->payee;
      temp.account = xact->account;
      return predicate->compute(&temp);
    }
  }

  bool operator ()(const entry_t * entry) const {
    if (! predicate) {
      return true;
    } else {
      item_t temp;
      temp.date  = entry->date;
      temp.payee = entry->payee;

      // Although there may be conflicting account masks for the whole
      // set of transactions -- for example, /rent/&!/expenses/, which
      // might match one by not another transactions -- we let the
      // entry through if at least one of the transactions meets the
      // criterion

      for (transactions_list::const_iterator i = entry->transactions.begin();
	   i != entry->transactions.end();
	   i++) {
	temp.account = (*i)->account;
	if (predicate->compute(&temp))
	  return true;
      }
      return false;
    }
  }

  bool operator ()(const item_t * item) const {
    return ! predicate || predicate->compute(item);
  }
};

typedef constrained_iterator<transactions_list::const_iterator, transaction_t *,
  constraints_t>
  constrained_transactions_list_const_iterator;

typedef constrained_iterator<transactions_list::iterator, transaction_t *,
  constraints_t>
  constrained_transactions_list_iterator;

typedef constrained_iterator<entries_list::const_iterator, entry_t *,
  constraints_t>
  constrained_entries_list_const_iterator;

typedef constrained_iterator<entries_list::iterator, entry_t *,
  constraints_t>
  constrained_entries_list_iterator;

typedef constrained_iterator<items_deque::const_iterator, item_t *,
  constraints_t>
  constrained_items_deque_const_iterator;

typedef constrained_iterator<items_deque::iterator, item_t *,
  constraints_t>
  constrained_items_deque_iterator;

} // namespace ledger

#endif // _CONSTRAINT_H
