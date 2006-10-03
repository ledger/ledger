#ifdef USE_PCH
#include "pch.h"
#else
#include "transform.h"
#endif

namespace ledger {

#if 0
void populate_account(account_t& acct, xml::document_t * document)
{
  if (! acct.parent)
    return;

  account_repitem_t * acct_item;
  if (acct.data == NULL) {
    acct.data = acct_item =
      static_cast<account_repitem_t *>(repitem_t::wrap(&acct));
    if (acct.parent) {
      if (acct.parent->data == NULL)
	populate_account(*acct.parent, acct_item);
      else
	static_cast<account_repitem_t *>(acct.parent->data)->
	  add_child(acct_item);
    }
  } else {
    acct_item = static_cast<account_repitem_t *>(acct.data);
  }

  if (item->kind == repitem_t::ACCOUNT)
    acct_item->add_child(item);
  else
    acct_item->add_content(item);
}

class populate_accounts : public repitem_t::select_callback_t {
  virtual void operator()(xml::document_t * document) {
    if (item->kind == repitem_t::TRANSACTION) {
      item->extract();
      populate_account(*static_cast<xact_repitem_t *>(item)->account(), item);
    }
  }
};

class clear_account_data : public repitem_t::select_callback_t {
  virtual void operator()(xml::document_t * document) {
    if (item->kind == repitem_t::ACCOUNT)
      static_cast<account_repitem_t *>(item)->account->data = NULL;
  }
};

void accounts_transform::execute(xml::document_t * document)
{
  populate_accounts cb1;
  items->select_all(cb1);

  for (repitem_t * j = items->children; j; j = j->next) {
    assert(j->kind == repitem_t::JOURNAL);

    j->clear();

    for (accounts_map::iterator i = j->journal->master->accounts.begin();
	 i != j->journal->master->accounts.end();
	 i++) {
      assert((*i).second->data);
      j->add_child(static_cast<account_repitem_t *>((*i).second->data));
      (*i).second->data = NULL;
    }
  }

  clear_account_data cb2;
  items->select_all(cb2);
}

void compact_transform::execute(xml::document_t * document)
{
  for (repitem_t * i = items; i; i = i->next) {
    if (i->kind == repitem_t::ACCOUNT) {
      while (! i->contents &&
	     i->children && ! i->children->next) {
	account_repitem_t * p = static_cast<account_repitem_t *>(i);
	i = p->children;
	p->children = NULL;
	p->last_child = NULL;

	i->set_parent(p->parent);
	p->set_parent(NULL);
	i->prev = p->prev;
	if (p->prev)
	  p->prev->next = i;
	p->prev = NULL;
	i->next = p->next;
	if (p->next)
	  p->next->prev = i;
	p->next = NULL;

	if (i->parent->children == p)
	  i->parent->children = i;
	if (i->parent->last_child == p)
	  i->parent->last_child = i;

	account_repitem_t * acct = static_cast<account_repitem_t *>(i);
	acct->parents_elided = p->parents_elided + 1;

	delete p;
      }
    }

    if (i->children)
      execute(i->children);
  }
}

void clean_transform::execute(xml::document_t * document)
{
  repitem_t * i = items;
  while (i) {
    if (i->kind == repitem_t::ACCOUNT) {
      value_t temp;
      i->add_total(temp);
      if (! temp) {
	repitem_t * next = i->next;
	delete i;
	i = next;
	continue;
      }
    }
#if 0
    else if (i->kind == repitem_t::ENTRY && ! i->contents) {
      assert(! i->children);
      repitem_t * next = i->next;
      delete i;
      i = next;
      continue;
    }
#endif

    if (i->children)
      execute(i->children);

    i = i->next;
  }
}

void entries_transform::execute(xml::document_t * document)
{
}

void optimize_transform::execute(xml::document_t * document)
{
  for (repitem_t * i = items; i; i = i->next) {
    if (i->kind == repitem_t::ENTRY) {
      if (i->contents &&
	  i->contents->next &&
	  ! i->contents->next->next) { // exactly two transactions
	xact_repitem_t * first =
	  static_cast<xact_repitem_t *>(i->contents);
	xact_repitem_t * second =
	  static_cast<xact_repitem_t *>(i->contents->next);
	if (first->xact->amount == - second->xact->amount)
	  ;
      }
    }

    if (i->children)
      execute(i->children);
  }
}

void split_transform::execute(xml::document_t * document)
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
	j = new account_repitem_t(static_cast<account_repitem_t *>(i)->account);
	break;
      default:
	j = new repitem_t(i->kind);
	break;
      }

      j->set_parent(i->parent);
      j->prev	= i;
      j->next	= i->next;
      i->next	= j;

      j->contents = i->contents->next;
      j->contents->prev = NULL;
      j->contents->set_parent(j);
      i->contents->next = NULL;

      j->last_content = i->last_content;
      if (j->contents == i->last_content)
	i->last_content = i->contents;
    }

    if (i->children)
      execute(i->children);
  }
}

void merge_transform::execute(xml::document_t * document)
{
  for (repitem_t * i = items; i; i = i->next) {
    if (i->next) {
      assert(i->kind == i->next->kind);
      bool merge = false;
      switch (i->kind) {
      case repitem_t::TRANSACTION:
	assert(0);
	break;
      case repitem_t::ENTRY:
	if (static_cast<entry_repitem_t *>(i)->entry ==
	    static_cast<entry_repitem_t *>(i->next)->entry)
	  merge = true;
	break;
      case repitem_t::ACCOUNT:
#if 0
	if (static_cast<account_repitem_t *>(i)->account ==
	    static_cast<account_repitem_t *>(i->next)->account)
	  merge = true;
#endif
	break;
      default:
	break;
      }

      if (merge) {
	repitem_t * j = i->next;

	i->next = i->next->next;
	if (i->next)
	  i->next->prev = i;

	for (repitem_t * k = j->contents; k; k = k->next)
	  k->set_parent(i);

	i->last_content->next = j->contents;
	i->last_content = j->last_content;

	j->contents = NULL;
	assert(! j->children);
	delete j;
      }
    }

    if (i->children)
      execute(i->children);
  }
}

namespace {
#define REPITEM_FLAGGED 0x1

  class mark_selected : public repitem_t::select_callback_t {
    virtual void operator()(xml::document_t * document) {
      item->flags |= REPITEM_FLAGGED;
    }
  };

  class mark_selected_and_ancestors : public repitem_t::select_callback_t {
    virtual void operator()(xml::document_t * document) {
      while (item->parent) {
	item->flags |= REPITEM_FLAGGED;
	item = item->parent;
      }
    }
  };

  class delete_unmarked : public repitem_t::select_callback_t {
    virtual void operator()(xml::document_t * document) {
      if (item->parent && ! (item->flags & REPITEM_FLAGGED))
	delete item;
    }
  };

  class delete_marked : public repitem_t::select_callback_t {
    virtual void operator()(xml::document_t * document) {
      if (item->flags & REPITEM_FLAGGED)
	delete item;
    }
  };

  class clear_flags : public repitem_t::select_callback_t {
    virtual void operator()(xml::document_t * document) {
      item->flags = 0;
    }
  };
}

void select_transform::execute(xml::document_t * document)
{
  if (! path) {
    items->clear();
    return;
  }
  mark_selected_and_ancestors cb1;
  items->select(path, cb1);

  delete_unmarked cb2;
  items->select_all(cb2);
  clear_flags cb3;
  items->select_all(cb3);
}

void remove_transform::execute(xml::document_t * document)
{
  if (! path)
    return;
  mark_selected cb1;
  items->select(path, cb1);

  delete_marked cb2;
  items->select_all(cb2);
  clear_flags cb3;
  items->select_all(cb3);
}
#endif

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
