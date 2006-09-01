#include "repitem.h"

namespace ledger {

repitem_t::~repitem_t()
{
  if (istemp) {
    switch (kind) {
    case XACT:
      delete xact;
      break;
    case ENTRY:
      delete entry;
      break;
    case ACCOUNT:
      delete account_ptr;
      break;
    case REPORT:
      break;
    }
  }
  if (contents) delete contents;
  if (children) delete children;
  if (next) delete next;
}

void repitem_t::add_total(value_t& val) const
{
  add_value(val);

  for (repitem_t * ptr = children; ptr; ptr = ptr->next)
    ptr->add_total(val);
}

void add_transaction_to(const transaction_t& xact, value_t& value);

void repitem_t::add_value(value_t& val) const
{
  switch (kind) {
  case XACT:
    add_transaction_to(*xact, val);
    break;

  case ENTRY:
  case ACCOUNT:
    for (repitem_t * ptr = contents; ptr; ptr = ptr->next)
      ptr->add_total(val);
    break;
  }
}

void repitem_t::add_sort_value(value_t& val) const
{
  assert(0);
}

datetime_t repitem_t::date() const
{
  if (reported_date)
    return reported_date;

  switch (kind) {
  case XACT: return xact->date();
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
  case XACT:  return xact->effective_date();
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
  case XACT: return xact->actual_date();
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
  case XACT:
    return xact->account;
  case ENTRY:
    return NULL;
  case ACCOUNT:
    return account_ptr;
  }
}

report_t * repitem_t::report() const
{
  if (kind == REPORT) {
    assert(! parent);
    return report_ptr;
  } else {
    assert(parent);
    return parent->report();
  }
}

bool repitem_t::valid() const
{
  assert(0);
  return false;
}

repitem_t * repitem_t::wrap_item(transaction_t * txact)
{
  repitem_t * temp = new repitem_t;
  temp->xact = txact;
  temp->kind = XACT;
  return temp;
}

repitem_t * repitem_t::wrap_item(entry_t * tentry)
{
  repitem_t * temp = new repitem_t;
  temp->entry = tentry;
  temp->kind = ENTRY;
  return temp;
}

repitem_t * repitem_t::wrap_item(account_t * taccount)
{
  repitem_t * temp = new repitem_t;
  temp->account_ptr = taccount;
  temp->kind = ACCOUNT;
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
  repitem_t * temp = new repitem_t;
  temp->xact = new transaction_t(taccount);
  temp->kind = XACT;
  temp->istemp = true;
  return temp;
}

repitem_t * repitem_t::fake_entry(const datetime_t& date, const std::string& payee)
{
  repitem_t * temp = new repitem_t;
  temp->entry = new entry_t;
  temp->entry->_date = date;
  temp->entry->payee = payee;
  temp->kind = ENTRY;
  temp->istemp = true;
  return temp;
}

void repitem_t::populate_entries(entries_list& entries,
				 const value_expr_t * filter)
{
  item_predicate<transaction_t> predicate(filter);

  for (entries_list::iterator i = entries.begin();
       i != entries.end();
       i++) {
    repitem_t * entry = NULL;
    for (transactions_list::iterator j = (*i)->transactions.begin();
	 j != (*i)->transactions.end();
	 j++) {
      if (predicate(**j)) {
	if (entry == NULL)
	  entry = repitem_t::wrap_item(*i);
	entry->add_content(repitem_t::wrap_item(*j));
      }
    }
    if (entry != NULL)
      add_content(entry);
  }
}

void repitem_t::populate_entries(entries_list& entries)
{
  for (entries_list::iterator i = entries.begin();
       i != entries.end();
       i++) {
    repitem_t * entry = repitem_t::wrap_item(*i);
    for (transactions_list::iterator j = (*i)->transactions.begin();
	 j != (*i)->transactions.end();
	 j++)
      entry->add_content(repitem_t::wrap_item(*j));
    add_content(entry);
  }
}

void repitem_t::populate_account(account_t& acct, repitem_t * item)
{
  repitem_t * acct_item;
  if (acct.parent == NULL)
    acct_item = this;
  else if (acct.data == NULL)
    acct.data = acct_item = repitem_t::wrap_item(&acct);
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
				  const value_expr_t * filter)
{
  item_predicate<transaction_t> predicate(filter);
  for (entries_list::iterator i = entries.begin();
       i != entries.end();
       i++)
    for (transactions_list::iterator j = (*i)->transactions.begin();
	 j != (*i)->transactions.end();
	 j++)
      if (predicate(**j))
	populate_account(*(*j)->account, repitem_t::wrap_item(*j));
}

void repitem_t::populate_accounts(entries_list& entries)
{
  for (entries_list::iterator i = entries.begin();
       i != entries.end();
       i++)
    for (transactions_list::iterator j = (*i)->transactions.begin();
	 j != (*i)->transactions.end();
	 j++)
      populate_account(*(*j)->account, repitem_t::wrap_item(*j));
}

void repitem_t::print_tree(std::ostream& out, int depth)
{
  for (int i = 0; i < depth; i++)
    out << "  ";

  switch (kind) {
  case XACT: out << "XACT " << xact; break;
  case ENTRY: out << "ENTRY " << entry; break;
  case ACCOUNT: out << "ACCOUNT " << account_ptr; break;
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

#include <boost/python.hpp>
#if 0
#include <boost/python/suite/indexing/list_indexing_suite.hpp>
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
#if 0
    .def(self == self)
    .def(self != self)
#endif

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

#if 0
  class_< transform_queue_list > ("TransformQueueList")
    .def(list_indexing_suite<transform_queue_list>())
    ;
#endif
}

#endif // USE_BOOST_PYTHON
