#ifndef _CONSTRAINT_H
#define _CONSTRAINT_H

#include "ledger.h"
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

class mask_t
{
 public:
  bool        exclude;
  std::string pattern;
  void *      regexp;

  explicit mask_t(const std::string& pattern);
  mask_t(const mask_t&);

  ~mask_t();

  bool match(const std::string& str) const;
};

typedef std::list<mask_t> masks_list;

bool matches(const masks_list& regexps, const std::string& str,
	     bool * by_exclusion = NULL);


struct node_t;

enum periodicity_t {
  PERIOD_NONE,
  PERIOD_MONTHLY,
  PERIOD_WEEKLY_SUN,
  PERIOD_WEEKLY_MON
};

class constraints_t
{
 public:
  bool real_only;
  bool cleared_only;
  bool uncleared_only;

  bool show_expanded;
  bool show_related;
  bool show_inverted;
  bool show_subtotals;
  bool show_empty;

  std::time_t	 begin_date;
  bool		 have_beginning;
  std::time_t	 end_date;
  bool		 have_ending;
  struct std::tm date_mask;
  bool		 have_date_mask;

  masks_list	 payee_masks;
  masks_list	 account_masks;

  periodicity_t  period;
  node_t *	 predicate;
  node_t *	 sort_order;

  explicit constraints_t() {
    real_only      = false;
    cleared_only   = false;
    uncleared_only = false;

    show_expanded  = false;
    show_related   = false;
    show_inverted  = false;
    show_subtotals = true;
    show_empty     = false;

    have_beginning = false;
    have_ending    = false;
    have_date_mask = false;

    period         = PERIOD_NONE;
    predicate      = NULL;
    sort_order     = NULL;
  }

  ~constraints_t();

  std::time_t begin() const {
    return have_beginning ? begin_date : 0;
  }

  std::time_t end() const {
    return have_ending ? end_date : std::time(NULL);
  }

  bool matches_date_range(const std::time_t date) const;

  bool operator ()(const transaction_t * xact) const;
  bool operator ()(const entry_t * entry) const;
  bool operator ()(const item_t * item) const;
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
