#ifdef USE_PCH
#include "pch.h"
#else
#include "repitem.h"
#endif

namespace ledger {

repitem_t::~repitem_t()
{
  TRACE_DTOR("repitem_t");

  if (prev) {
    prev->next = next;
  }
  else if (parent) {
    if (parent->contents == this)
      parent->contents = next;
    else if (parent->children == this)
      parent->children = next;
  }

  if (next)
    next->prev = prev;

  if (istemp) {
    switch (kind) {
    case TRANSACTION:
      delete xact;
      break;
    case ENTRY:
      delete entry;
      break;
    case ACCOUNT:
      delete account_ptr;
      break;
    case JOURNAL:
      assert(0);
      break;
    case SESSION:
      assert(0);
      break;
    }
  }
  if (contents) delete contents;
  if (children) delete children;
  if (next) delete next;
}

void repitem_t::add_total(value_t& val)
{
  add_value(val);

  for (repitem_t * ptr = children; ptr; ptr = ptr->next)
    ptr->add_total(val);
}

void add_transaction_to(const transaction_t& xact, value_t& value)
{
#if 0
  if (transaction_has_xdata(xact) &&
      transaction_xdata_(xact).dflags & TRANSACTION_COMPOUND) {
    value += transaction_xdata_(xact).value;
  }
  else if (xact.cost || ! value.realzero()) {
    value.add(xact.amount, xact.cost);
  }
  else {
    value = xact.amount;
  }
#endif
}

void repitem_t::add_value(value_t& val)
{
  switch (kind) {
  case TRANSACTION:
    add_transaction_to(*xact, val);
    break;

  case ENTRY:
  case ACCOUNT:
    for (repitem_t * ptr = contents; ptr; ptr = ptr->next)
      ptr->add_total(val);
    break;
  }
}

void repitem_t::add_sort_value(value_t& val)
{
  assert(0);
}

datetime_t repitem_t::date() const
{
  if (reported_date)
    return reported_date;

  switch (kind) {
  case TRANSACTION: return xact->date();
  case ENTRY: return entry->date();

  case ACCOUNT:
    assert(0);
    return datetime_t();
  }
}

datetime_t repitem_t::effective_date() const
{
  if (reported_date)
    return reported_date;

  switch (kind) {
  case TRANSACTION:  return xact->effective_date();
  case ENTRY: return entry->effective_date();

  case ACCOUNT:
    assert(0);
    return datetime_t();
  }
}

datetime_t repitem_t::actual_date() const
{
  if (reported_date)
    return reported_date;

  switch (kind) {
  case TRANSACTION: return xact->actual_date();
  case ENTRY: return entry->actual_date();

  case ACCOUNT:
    assert(0);
    return datetime_t();
  }
}

account_t * repitem_t::account() const
{
  if (reported_account != NULL)
    return reported_account;

  switch (kind) {
  case TRANSACTION:
    return xact->account;
  case ENTRY:
    return NULL;
  case ACCOUNT:
    return account_ptr;
  }
}

bool repitem_t::valid() const
{
  assert(0);
  return false;
}

repitem_t * repitem_t::wrap(transaction_t * txact, repitem_t * owner)
{
  if (txact->data != NULL) {
    repitem_t * temp = static_cast<repitem_t *>(txact->data);
    txact->data = NULL;
    return temp;
  }

  repitem_t * temp = new repitem_t(TRANSACTION, owner);
  temp->xact = txact;
  return temp;
}

repitem_t * repitem_t::wrap(entry_t * tentry, repitem_t * owner, bool deep)
{
  if (tentry->data != NULL) {
    repitem_t * temp = static_cast<repitem_t *>(tentry->data);
    tentry->data = NULL;
    return temp;
  }

  repitem_t * temp = new repitem_t(ENTRY, owner);
  temp->entry = tentry;

  if (deep) {
    for (transactions_list::iterator i = tentry->transactions.begin();
	 i != tentry->transactions.end();
	 i++)
      temp->add_content(wrap(*i, temp));
  }

  return temp;
}

repitem_t * repitem_t::wrap(account_t * taccount, repitem_t * owner, bool deep)
{
  repitem_t * temp = new repitem_t(ACCOUNT, owner);
  temp->account_ptr = taccount;
  assert(! deep);
  return temp;
}

repitem_t * repitem_t::wrap(journal_t * tjournal, repitem_t * owner, bool deep)
{
  if (tjournal->data != NULL) {
    repitem_t * temp = static_cast<repitem_t *>(tjournal->data);
    tjournal->data = NULL;
    return temp;
  }

  repitem_t * temp = new repitem_t(JOURNAL, owner);
  temp->journal = tjournal;

  if (deep) {
    for (entries_list::iterator i = tjournal->entries.begin();
	 i != tjournal->entries.end();
	 i++)
      temp->add_child(wrap(*i, temp, true));
  }

  return temp;
}

repitem_t * repitem_t::wrap(session_t * tsession, bool deep)
{
  repitem_t * temp = new repitem_t(SESSION);
  temp->session = tsession;

  if (deep) {
    for (std::list<journal_t *>::iterator i = tsession->journals.begin();
	 i != tsession->journals.end();
	 i++)
      temp->add_child(wrap(*i, temp, true));
  }

  return temp;
}

repitem_t * repitem_t::add_content(repitem_t * item)
{
  repitem_t * start = item;

  if (contents == NULL) {
    assert(last_content == NULL);
    contents = item;
    item->prev = NULL;
  } else {
    assert(last_content != NULL);
    last_content->next = item;
    item->prev = last_content;
  }

  item->parent = this;
  while (item->next) {
    repitem_t * next_item = item->next;
    next_item->prev   = item;
    next_item->parent = this;
    item = next_item;
  }

  last_content = item;

  return start;
}

repitem_t * repitem_t::add_child(repitem_t * item)
{
  repitem_t * start = item;

  if (children == NULL) {
    assert(last_child == NULL);
    children = item;
    item->prev = NULL;
  } else {
    assert(last_child != NULL);
    last_child->next = item;
    item->prev = last_child;
  }

  item->parent = this;
  while (item->next) {
    repitem_t * next_item = item->next;
    next_item->prev   = item;
    next_item->parent = this;
    item = next_item;
  }

  last_child = item;

  return start;
}

repitem_t * repitem_t::fake_transaction(account_t * taccount)
{
  repitem_t * temp = new repitem_t(TRANSACTION);
  temp->xact = new transaction_t(taccount);
  temp->istemp = true;
  return temp;
}

repitem_t * repitem_t::fake_entry(const datetime_t& edate,
				  const datetime_t& rdate,
				  const std::string& payee)
{
  repitem_t * temp = new repitem_t(ENTRY);
  temp->entry = new entry_t;
  temp->entry->_date_eff = edate;
  temp->entry->_date = rdate;
  temp->entry->payee = payee;
  temp->istemp = true;
  return temp;
}

void repitem_t::populate_account(account_t& acct, repitem_t * item)
{
  repitem_t * acct_item;
  if (acct.parent == NULL)
    acct_item = this;
  else if (acct.data == NULL)
    acct.data = acct_item = repitem_t::wrap(&acct);
  else
    acct_item = (repitem_t *) acct.data;

  if (item->kind == ACCOUNT)
    acct_item->add_child(item);
  else
    acct_item->add_content(item);

  if (acct.parent && acct.parent->data == NULL)
    populate_account(*acct.parent, acct_item);
}

void repitem_t::populate_accounts(entries_list& entries,
				  const valexpr_t& filter)
{
  for (entries_list::iterator i = entries.begin();
       i != entries.end();
       i++)
    for (transactions_list::iterator j = (*i)->transactions.begin();
	 j != (*i)->transactions.end();
	 j++)
      // jww (2006-09-10): Make a scope based on **j
      if (filter.calc().get_boolean())
	populate_account(*(*j)->account, repitem_t::wrap(*j));
}

void repitem_t::populate_accounts(entries_list& entries)
{
  for (entries_list::iterator i = entries.begin();
       i != entries.end();
       i++)
    for (transactions_list::iterator j = (*i)->transactions.begin();
	 j != (*i)->transactions.end();
	 j++)
      populate_account(*(*j)->account, repitem_t::wrap(*j));
}

void repitem_t::print_tree(std::ostream& out, int depth)
{
  for (int i = 0; i < depth; i++)
    out << "  ";

  switch (kind) {
  case TRANSACTION:
    out << "TRANSACTION " << xact
	<< " - line " << xact->beg_line; break;
  case ENTRY:
    out << "ENTRY " << entry; break;
  case ACCOUNT:
    out << "ACCOUNT " << account_ptr; break;
  case JOURNAL:
    out << "JOURNAL " << journal; break;
  case SESSION:
    out << "SESSION " << session; break;
  }
  out << std::endl;

  if (contents) {
    for (int i = 0; i < depth; i++)
      out << "  ";
    out << "Contents:" << std::endl;

    for (repitem_t * ptr = contents; ptr; ptr = ptr->next)
      ptr->print_tree(out, depth + 1);
  }

  if (children) {
    for (int i = 0; i < depth; i++)
      out << "  ";
    out << "Children:" << std::endl;

    for (repitem_t * ptr = children; ptr; ptr = ptr->next)
      ptr->print_tree(out, depth + 1);
  }
}

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#ifndef USE_PCH
#include <boost/python.hpp>
#endif

using namespace boost::python;
using namespace ledger;

value_t py_repitem_total(repitem_t * item) {
  value_t temp;
  item->add_total(temp);
  return temp;
}

value_t py_repitem_value(repitem_t * item) {
  value_t temp;
  item->add_value(temp);
  return temp;
}

value_t py_repitem_sort_value(repitem_t * item) {
  value_t temp;
  item->add_sort_value(temp);
  return temp;
}

void export_repitem()
{
  class_< repitem_t > ("ReportItem")
    .def(self == self)
    .def(self != self)

    .add_property("total", &py_repitem_total)
    .add_property("value", &py_repitem_value)
    .add_property("sort_value", &py_repitem_sort_value)

    .add_property("date", &repitem_t::date)
    .add_property("effective_date", &repitem_t::effective_date)
    .add_property("actual_date", &repitem_t::actual_date)

    .add_property("account",
		  make_function(&repitem_t::account,
				return_value_policy<reference_existing_object>()))

    .def("add_content", &repitem_t::add_content,
	 return_internal_reference<1, with_custodian_and_ward<1, 2> >())

    .def("add_child", &repitem_t::add_child,
	 return_internal_reference<1, with_custodian_and_ward<1, 2> >())

    .def("valid", &repitem_t::valid)
    ;
}

#endif // USE_BOOST_PYTHON
