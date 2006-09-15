#include "transform.h"
#include "repitem.h"

namespace ledger {

void split_transform::walk_items(repitem_t * items)
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
      walk_items(i->children);
  }
}

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>

using namespace boost::python;
using namespace ledger;

void export_transform()
{
  class_< repitem_t > ("Transform")
    ;
}

#endif // USE_BOOST_PYTHON
