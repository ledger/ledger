#ifndef _ITEM_H
#define _ITEM_H

#include "ledger.h"
#include "balance.h"

#include <deque>

namespace ledger {

struct node_t;
struct item_t;
typedef std::deque<item_t *> items_deque;

struct item_t
{
  struct item_t *   parent;
  items_deque       subitems;

  unsigned int      index;
  std::time_t       date;
  std::string       payee;
  const account_t * account;
  balance_pair_t    value;
  balance_pair_t    total;

  item_t() : parent(NULL), index(0), date(-1), account(NULL) {}

  ~item_t() {
    for (items_deque::iterator i = subitems.begin();
	 i != subitems.end();
	 i++)
      delete *i;
  }

  void sort(const node_t * sort_order);
};

class constraints_t;

item_t * walk_accounts(const account_t * account,
		       const constraints_t& constraints);

item_t * walk_items(const item_t * top, const account_t * account,
		    const constraints_t& constraints);

item_t * walk_entries(entries_list::const_iterator begin,
		      entries_list::const_iterator end,
		      const constraints_t& constraints);

} // namespace report

#endif // _REPORT_H
