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
  struct item_t * parent;
  items_deque     subitems;

  unsigned int		 index;
  std::time_t		 date;
  entry_t::entry_state_t state;
  std::string		 code;
  std::string		 payee;
  unsigned int           flags;
  const account_t *	 account;
  balance_pair_t	 value;
  balance_pair_t	 total;
  std::string		 note;

  item_t() : parent(NULL), index(0), date(-1),
	     state(entry_t::UNCLEARED), flags(0), account(NULL) {}

  item_t(const item_t * item)
    : parent(NULL), index(0), date(item->date), state(item->state),
      code(item->code), payee(item->payee), flags(item->flags),
      account(item->account), value(item->value), total(item->total),
      note(item->note) {}

  item_t(const entry_t * entry)
    : parent(NULL), index(0), date(entry->date), state(entry->state),
      code(entry->code), payee(entry->payee) {}

  item_t(const transaction_t * xact)
    : parent(NULL), index(0), date(xact->entry->date),
      state(xact->entry->state), code(xact->entry->code),
      payee(xact->entry->payee), flags(xact->flags),
      account(xact->account), value(*xact), note(xact->note) {}

  ~item_t() {
    for (items_deque::iterator i = subitems.begin();
	 i != subitems.end();
	 i++)
      delete *i;
  }

  void add_item(item_t * item) {
    item->parent = this;
    value += item->value;
    subitems.push_back(item);
  }

  void sort(const node_t * sort_order);
};

struct node_t;

item_t * walk_accounts(const item_t * top,
		       account_t *    account,
		       const node_t * predicate      = NULL,
		       const bool     show_subtotals = true,
		       const bool     show_flattened = false);

item_t * walk_entries(entries_list::const_iterator begin,
		      entries_list::const_iterator end,
		      const node_t * predicate     = NULL,
		      const bool     show_related  = false,
		      const bool     show_inverted = false);

} // namespace report

#endif // _REPORT_H
