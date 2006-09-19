#ifdef USE_PCH
#include "pch.h"
#else
#include "transform.h"
#include "repitem.h"
#endif

namespace ledger {

void split_transform::execute(repitem_t * items)
{
  for (repitem_t * i = items; i; i = i->next) {
    if (i->contents && i->contents->next) {
      repitem_t * j = new repitem_t(i->kind);

      j->parent = i->parent;
      j->prev = i;
      j->next = i->next;
      i->next = j;

      switch (i->kind) {
      case repitem_t::TRANSACTION:
	assert(0);
	break;
      case repitem_t::ENTRY:
	j->entry = i->entry;
	break;
      case repitem_t::ACCOUNT:
	j->account_ptr = i->account_ptr;
	break;
      }
      assert(i->reported_account == NULL);

      j->contents = i->contents->next;
      j->contents->prev = NULL;
      j->contents->parent = j;
      i->contents->next = NULL;
    }

    if (i->children)
      execute(i->children);
  }
}

#define REPITEM_FLAGGED 0x1

void mark_selected(repitem_t * item) {
  while (item->parent) {
    item->flags |= REPITEM_FLAGGED;
    item = item->parent;
  }
}

void delete_unmarked(repitem_t * item) {
  if (item->parent && ! (item->flags & REPITEM_FLAGGED))
    delete item;
}

void clear_flag(repitem_t * item) {
  item->flags = 0;
}

void select_transform::execute(repitem_t * items)
{
  if (! path) {
    items->clear();
    return;
  }
  items->select(path, mark_selected);

  items->select_all(delete_unmarked);
  items->select_all(clear_flag);
}

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#ifndef USE_PCH
#include <boost/python.hpp>
#endif

using namespace boost::python;
using namespace ledger;

void export_transform()
{
  class_< repitem_t > ("Transform")
    ;
}

#endif // USE_BOOST_PYTHON
