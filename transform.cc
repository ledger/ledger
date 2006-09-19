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
      repitem_t * j;

      switch (i->kind) {
      case repitem_t::TRANSACTION:
	assert(0);
	j = new xact_repitem_t(static_cast<xact_repitem_t *>(i)->xact);
	break;
      case repitem_t::ENTRY:
	j = new entry_repitem_t(static_cast<entry_repitem_t *>(i)->entry);
	break;
      case repitem_t::ACCOUNT:
	j = new repitem_t(i->kind);
	j->account_ptr = i->account_ptr;
	break;
      default:
	j = new repitem_t(i->kind);
	break;
      }

      j->parent = i->parent;
      j->prev	= i;
      j->next	= i->next;
      i->next	= j;

      j->contents = i->contents->next;
      j->contents->prev = NULL;
      j->contents->parent = j;
      i->contents->next = NULL;
    }

    if (i->children)
      execute(i->children);
  }
}

namespace {
#define REPITEM_FLAGGED 0x1

  void mark_selected(repitem_t * item) {
    item->flags |= REPITEM_FLAGGED;
  }

  void mark_selected_and_ancestors(repitem_t * item) {
    while (item->parent) {
      item->flags |= REPITEM_FLAGGED;
      item = item->parent;
    }
  }

  void delete_unmarked(repitem_t * item) {
    if (item->parent && ! (item->flags & REPITEM_FLAGGED))
      delete item;
  }

  void delete_marked(repitem_t * item) {
    if (item->flags & REPITEM_FLAGGED)
      delete item;
  }

  void clear_flags(repitem_t * item) {
    item->flags = 0;
  }
}

void select_transform::execute(repitem_t * items)
{
  if (! path) {
    items->clear();
    return;
  }
  items->select(path, mark_selected_and_ancestors);

  items->select_all(delete_unmarked);
  items->select_all(clear_flags);
}

void remove_transform::execute(repitem_t * items)
{
  if (! path)
    return;
  items->select(path, mark_selected);

  items->select_all(delete_marked);
  items->select_all(clear_flags);
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
