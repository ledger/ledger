#include "item.h"
#include "constraint.h"
#include "expr.h"

namespace ledger {

// jww (2004-07-21): If format.show_empty is set, then include all
// subaccounts, empty balanced or no

item_t * walk_accounts(const account_t *    account,
		       const constraints_t& constraints,
		       const bool           compute_subtotals)
{
  item_t * item = new item_t;
  item->account = account;
  item->date    = constraints.end();

  for (constrained_transactions_list_const_iterator
	 i(account->transactions.begin(),
	   account->transactions.end(), constraints);
       i != account->transactions.end();
       i++) {
    item->value += *(*i);
    if (compute_subtotals)
      item->total += *(*i);
  }

  for (accounts_map::const_iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++) {
    item_t * subitem = walk_accounts((*i).second, constraints,
				     compute_subtotals);
    subitem->parent = item;

    if (compute_subtotals)
      item->total += subitem->total;

    if (compute_subtotals ? subitem->total : subitem->value)
      item->subitems.push_back(subitem);
  }

  return item;
}

static inline void sum_items(const item_t * top,
			     item_t *       item,
			     const bool     compute_subtotals)
{
  if (top->account == item->account) {
    item->value += top->value;
    if (compute_subtotals)
      item->total += top->value;
  }

  for (items_deque::const_iterator i = top->subitems.begin();
       i != top->subitems.end();
       i++)
    sum_items(*i, item, compute_subtotals);
}

item_t * walk_items(const item_t *       top,
		    const account_t *    account,
		    const constraints_t& constraints,
		    const bool           compute_subtotals)
{
  item_t * item = new item_t;
  item->account = account;

  sum_items(top, item, compute_subtotals);

  for (accounts_map::const_iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++) {
    item_t * subitem = walk_items(top, (*i).second, constraints,
				  compute_subtotals);
    subitem->parent = item;

    if (compute_subtotals)
      item->total += subitem->total;

    if (compute_subtotals ? subitem->total : subitem->value)
      item->subitems.push_back(subitem);
  }

  return item;
}

item_t * walk_entries(entries_list::const_iterator begin,
		      entries_list::const_iterator end,
		      const constraints_t&	   constraints)
{
#if 0
  int          last_mon = -1;
#endif
  unsigned int count  = 0;
  item_t *     result = NULL;

  for (constrained_entries_list_const_iterator i(begin, end, constraints);
       i != end;
       i++) {
    item_t * item  = NULL;

    for (constrained_transactions_list_const_iterator
	   j((*i)->transactions.begin(), (*i)->transactions.end(),
	     constraints);
	 j != (*i)->transactions.end();
	 j++) {
      assert(*i == (*j)->entry);

      if (! item) {
	item = new item_t;
	item->index = count++;
	item->date  = (*i)->date;
	item->payee = (*i)->payee;
      }

      if (! constraints.show_inverted) {
	item_t * subitem = new item_t;
	subitem->parent  = item;
	subitem->date    = item->date;
	subitem->account = (*j)->account;
	subitem->value   = *(*j);
	item->subitems.push_back(subitem);
      }

      if (constraints.show_related)
	for (transactions_list::iterator k = (*i)->transactions.begin();
	     k != (*i)->transactions.end();
	     k++)
	  if (*k != *j && ! ((*k)->flags & TRANSACTION_VIRTUAL)) {
	    item_t * subitem = new item_t;
	    subitem->parent  = item;
	    subitem->date    = item->date;
	    subitem->account = (*k)->account;
	    subitem->value   = *(*k);
	    if (constraints.show_inverted)
	      subitem->value.negate();
	    item->subitems.push_back(subitem);
	  }

#if 0
      // If we are collecting monthly totals, then add them if the
      // month of this entry is different from the month of previous
      // entries.

      if (format.period == PERIOD_MONTHLY) {
	int entry_mon = std::gmtime(&(*i)->date)->tm_mon;

	if (last_mon != -1 && entry_mon != last_mon &&
	    line_balances.size() > 0) {
	  if (last_date == 0)
	    last_date = (*i)->date;

	  if (reg) {
	    char buf[32];
	    std::strftime(buf, 31, "%B", std::gmtime(&last_date));

	    reg->lines.push_back(register_line_t(last_date, buf));
	    reg->lines.back().compute_items(line_balances, total, count);
	  } else {
	    count++;
	  }

	  line_balances.clear();
	}

	last_mon  = entry_mon;
	last_date = (*i)->date;
      }
#endif
    }

    if (item) {
      if (! result)
	result = new item_t;
      item->parent = result;
      result->subitems.push_back(item);
    }
  }

  return result;

#if 0
  // Wrap up any left over balance list information.

  if (line_balances.size() > 0) {
    assert(format.period == PERIOD_MONTHLY);
    assert(last_date != 0);

    if (reg) {
      char buf[32];
      std::strftime(buf, 31, "%B", std::gmtime(&last_date));

      reg->lines.push_back(register_line_t(last_date, buf));
      reg->lines.back().compute_items(line_balances, total, count);
    } else {
      count++;
    }

    //line_balances.clear();
  }

  return count;
#endif
}

struct cmp_items {
  const node_t * sort_order;

  cmp_items(const node_t * _sort_order) : sort_order(_sort_order) {}

  bool operator()(const item_t * left, const item_t * right) const {
    assert(left);
    assert(right);
    assert(sort_order);
    return sort_order->compute(left) < sort_order->compute(right);
  }
};

void item_t::sort(const node_t * sort_order)
{
  std::sort(subitems.begin(), subitems.end(), cmp_items(sort_order));
}

} // namespace ledger
