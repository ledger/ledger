#include "item.h"
#include "expr.h"

namespace ledger {

static inline void sum_items(const item_t * top,
			     const bool     show_subtotals,
			     item_t *	    item)
{
  if (top->account == item->account) {
    item->value += top->value;
    if (show_subtotals)
      item->total += top->value;
  }

  for (items_deque::const_iterator i = top->subitems.begin();
       i != top->subitems.end();
       i++)
    sum_items(*i, show_subtotals, item);
}

item_t * walk_accounts(const item_t * top,
		       account_t *    account,
		       const node_t * predicate,
		       const bool     show_subtotals,
		       const bool     show_flattened)
{
  item_t * item = new item_t;
  item->account = account;

  if (top) {
    sum_items(top, show_subtotals, item);
  } else {
    std::time_t latest = 0;
    for (transactions_list::iterator i
	   = std::find_if(account->transactions.begin(),
			  account->transactions.end(),
			  value_predicate(predicate));
	 i != account->transactions.end();
	 i = std::find_if(++i, account->transactions.end(),
			  value_predicate(predicate))) {
      if (std::difftime(latest, (*i)->entry->date) < 0)
	latest = (*i)->entry->date;

      item->value += *(*i);
      if (show_subtotals)
	item->total += *(*i);
    }
    item->date = latest;
  }

  for (accounts_map::iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++) {
    std::auto_ptr<item_t>
      subitem(walk_accounts(top, (*i).second, predicate, show_subtotals,
			    show_flattened));
    subitem->parent = item;

    if (std::difftime(item->date, subitem->date) < 0)
      item->date = subitem->date;

    if (show_flattened) {
      item_t *	     ptr = item;
      balance_pair_t total;

      for (items_deque::const_iterator i = subitem->subitems.begin();
	   i != subitem->subitems.end();
	   i++)
	if (show_subtotals ? (*i)->total : (*i)->value) {
	  if (! account->parent) {
	    if (! total) {
	      item_t * temp = new item_t;
	      temp->date  = top ? top->date : item->date;
	      temp->payee = "Opening balance";
	      item->subitems.push_back(temp);
	      ptr = temp;
	    }
	    total += show_subtotals ? (*i)->total : (*i)->value;
	  }

	  ptr->subitems.push_back(new item_t(*i));
	  ptr->subitems.back()->date  = ptr->date;
	  ptr->subitems.back()->payee = ptr->payee;
	}

      if (total) {
	item_t * temp = new item_t;
	temp->date    = ptr->date;
	temp->payee   = ptr->payee;
	temp->account = account->find_account("Equity:Opening Balances");
	temp->value   = total;
	temp->value.negate();
	ptr->subitems.push_back(temp);
      }
    }

    if (show_subtotals)
      item->total += subitem->total;

    if ((! show_flattened || account->parent) &&
	show_subtotals ? subitem->total : subitem->value)
      item->subitems.push_back(subitem.release());
  }

  return item;
}

item_t * walk_entries(entries_list::const_iterator begin,
		      entries_list::const_iterator end,
		      const node_t * predicate,
		      const bool     show_related,
		      const bool     show_inverted)
{
  unsigned int	  count  = 0;
  item_t *	  result = NULL;
  value_predicate pred_obj(predicate);

  for (entries_list::const_iterator i = std::find_if(begin, end, pred_obj);
       i != end;
       i = std::find_if(++i, end, pred_obj)) {
    transactions_list reckoned;
    item_t * item = NULL;

    for (transactions_list::const_iterator j
	   = std::find_if((*i)->transactions.begin(),
			  (*i)->transactions.end(), pred_obj);
	 j != (*i)->transactions.end();
	 j = std::find_if(++j,
		transactions_list::const_iterator((*i)->transactions.end()),
		pred_obj)) {
      assert(*i == (*j)->entry);

      if (! item) {
	item = new item_t(*i);
	item->index = count++;
      }

      // If show_inverted is true, it implies show_related.
      if (! show_inverted &&
	  std::find(reckoned.begin(),
		    reckoned.end(), *j) == reckoned.end()) {
	item->add_item(new item_t(*j));
	reckoned.push_back(*j);
      }

      if (show_related)
	for (transactions_list::iterator k = (*i)->transactions.begin();
	     k != (*i)->transactions.end();
	     k++) {
	  if (*k == *j || ((*k)->flags & TRANSACTION_AUTO) ||
	      std::find(reckoned.begin(),
			reckoned.end(), *k) != reckoned.end())
	    continue;

	  item->add_item(new item_t(*k));
	  if (show_inverted)
	    item->subitems.back()->value.negate();
	  reckoned.push_back(*k);
	}
    }

    if (item) {
      if (! result)
	result = new item_t;
      item->parent = result;
      result->subitems.push_back(item);

      if (std::difftime(result->date, item->date) < 0)
	result->date = item->date;
    }
  }

  return result;
}

struct cmp_items {
  const node_t * sort_order;

  cmp_items(const node_t * _sort_order) : sort_order(_sort_order) {
    assert(sort_order);
  }

  bool operator()(const item_t * left, const item_t * right) const {
    assert(left);
    assert(right);
    return sort_order->compute(left) < sort_order->compute(right);
  }
};

void item_t::sort(const node_t * sort_order)
{
  std::stable_sort(subitems.begin(), subitems.end(), cmp_items(sort_order));
}

} // namespace ledger
