#ifdef USE_PCH
#include "pch.h"
#else
#include "repitem.h"
#endif

namespace ledger {

void repitem_t::add_total(value_t& val)
{
  add_value(val);
  for (repitem_t * ptr = children; ptr; ptr = ptr->next)
    ptr->add_total(val);
}

void repitem_t::add_value(value_t& val)
{
  if (c_value) {
    val += *c_value;
  } else {
    for (repitem_t * ptr = contents; ptr; ptr = ptr->next)
      ptr->add_value(val);
  }
}

void repitem_t::add_sort_value(value_t& val)
{
  assert(0);
}

bool repitem_t::valid() const
{
  assert(0);
  return false;
}

repitem_t * repitem_t::wrap(transaction_t * txact, repitem_t * owner)
{
  if (txact->data != NULL) {
    xact_repitem_t * temp = static_cast<xact_repitem_t *>(txact->data);
    txact->data = NULL;
    return temp;
  }
  return new xact_repitem_t(txact, owner);
}

repitem_t * repitem_t::wrap(entry_t * tentry, repitem_t * owner, bool deep)
{
  if (tentry->data != NULL) {
    entry_repitem_t * temp = static_cast<entry_repitem_t *>(tentry->data);
    tentry->data = NULL;
    return temp;
  }

  entry_repitem_t * temp = new entry_repitem_t(tentry, owner);

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
  if (taccount->data != NULL) {
    account_repitem_t * temp = static_cast<account_repitem_t *>(taccount->data);
    taccount->data = NULL;
    return temp;
  }

  assert(! deep);
  return new account_repitem_t(taccount, owner);
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

repitem_t * repitem_t::wrap(session_t * tsession, valexpr_t::scope_t * parent,
			    bool deep)
{
  repitem_t * temp = new repitem_t(SESSION);
  temp->session = tsession;

  temp->valexpr_t::scope_t::parent = parent;

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

  item->set_parent(this);

  while (item->next) {
    repitem_t * next_item = item->next;
    next_item->prev   = item;
    next_item->set_parent(this);
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

  item->set_parent(this);
  while (item->next) {
    repitem_t * next_item = item->next;
    next_item->prev   = item;
    next_item->set_parent(this);
    item = next_item;
  }

  last_child = item;

  return start;
}

repitem_t * repitem_t::fake_transaction(account_t * taccount)
{
  xact_repitem_t * temp = new xact_repitem_t(new transaction_t(taccount));
  temp->istemp = true;
  return temp;
}

repitem_t * repitem_t::fake_entry(const datetime_t& edate,
				  const datetime_t& rdate,
				  const std::string& payee)
{
  entry_repitem_t * temp = new entry_repitem_t(new entry_t);
  temp->entry->_date_eff = edate;
  temp->entry->_date = rdate;
  temp->entry->payee = payee;
  temp->istemp = true;
  return temp;
}

void repitem_t::print_tree(std::ostream& out, int depth)
{
  for (int i = 0; i < depth; i++)
    out << "  ";

  switch (kind) {
  case TRANSACTION: {
    out << "XACT: "
	<< static_cast<xact_repitem_t *>(this)->account()->fullname()
	<< " " << static_cast<xact_repitem_t *>(this)->xact->amount;
    break;
  }
  case ENTRY: {
    out << "ENTRY: "
	<< static_cast<entry_repitem_t *>(this)->entry->payee;
    break;
  }
  case ACCOUNT: {
    out << "ACCOUNT: "
	<< static_cast<account_repitem_t *>(this)->account->fullname();
    break;
  }
  case JOURNAL:
    out << "JOURNAL: " << journal->sources.front();
    break;
  case SESSION:
    out << "SESSION"; break;
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

bool repitem_t::resolve(const std::string& name, value_t& result,
			scope_t * locals)
{
  const char * p = name.c_str();
  switch (*p) {
#if 0
  case 'l':
    if (name == "last") {
      last(result);
      return true;
    }
    break;

  case 'p':
    if (name == "position") {
      position(result);
      return true;
    }
    break;
#endif

  case 't':
    if (name == "total") {
      add_total(result);
      return true;
    }
    break;

  case 'v':
    if (name == "value") {
      add_value(result);
      return true;
    }
    break;
  }

  return valexpr_t::scope_t::resolve(name, result, locals);
}

bool xact_repitem_t::resolve(const std::string& name, value_t& result,
			     scope_t * locals)
{
  const char * p = name.c_str();
  switch (*p) {
  case 'a':
    switch (*++p) {
    case 'c':
      if (name == "account") {
	account(result);
	return true;
      }
      break;
    case 'm':
      if (name == "amount") {
	add_value(result);
	return true;
      }
      break;
    }
    break;

  case 'd':
    if (name == "date") {
      result = xact->date();
      return true;
    }
    break;
  }

  return repitem_t::resolve(name, result, locals);
}

bool entry_repitem_t::resolve(const std::string& name, value_t& result,
			     scope_t * locals)
{
  const char * p = name.c_str();
  switch (*p) {
  case 'd':
    if (name == "date") {
      result = entry->date();
      return true;
    }
    break;

  case 'p':
    if (name == "payee") {
      result.set_string(entry->payee);
      return true;
    }
    break;
  }

  return repitem_t::resolve(name, result, locals);
}

bool account_repitem_t::resolve(const std::string& name, value_t& result,
				scope_t * locals)
{
  const char * p = name.c_str();
  switch (*p) {
  case 'd':
    if (name == "depth") {
      long count = 0;
      for (account_t * acct = account->parent; acct; acct = acct->parent)
	count++;
      result = count;
      return true;
    }
    break;

  case 'f':
    if (name == "fullname") {
      result.set_string(account->fullname());
      return true;
    }
    break;

  case 'n':
    if (name == "name") {
      result.set_string(account->name);
      return true;
    }
    else if (name == "note") {
      result.set_string(account->note);
      return true;
    }
    break;

  case 'r':
    if (name == "rdepth") {
      long count = 0;
      for (repitem_t * acct = parent; acct; acct = acct->parent) {
	if (acct->kind != ACCOUNT)
	  break;
	count++;
      }
      result = count;
      return true;
    }
    else if (name == "rname") {
      if (parents_elided == 0) {
	result.set_string(account->name);
      } else {
	std::string name = account->name;
	account_t * parent = account->parent;
	for (int i = 0; i < parents_elided; i++)
	  name = parent->name + ":" + name;
	result.set_string(name);
      }
      return true;
    }
    break;
  }

  return repitem_t::resolve(name, result, locals);
}

#if 0
struct formatter_callback_t : public repitem_t::select_callback_t
{
  std::ostream&	  out;
  const format_t& format;
  int		  column;

  const format_t::element_formatter_t& formatter;

  formatter_callback_t(std::ostream& _out, const format_t& _format, int _column,
		       const format_t::element_formatter_t& _formatter)
    : out(_out), format(_format), column(_column), formatter(_formatter) {}

  virtual void operator()(repitem_t * item) {
    column = format.format(out, item, column, formatter);
  }
};
#endif

int repitem_t::formatter_t::operator()(std::ostream& out, element_t * element,
				       valexpr_t::scope_t * scope,
				       int column) const
{
  if (element->kind == element_t::GROUP) {
    element_t * prefix = NULL;
    if (element->format->elements.size() > 0)
      prefix = element->format->elements.front();

    assert(dynamic_cast<repitem_t *>(scope));
    repitem_t * item = static_cast<repitem_t *>(scope);

    if (prefix && prefix->kind == element_t::TEXT) {
      const char * p = prefix->chars->c_str();
      int xpath_len = 0;
      std::string xpath;
      if (*p == '(') {
	++p;
	const char * q = p;
	int depth = 1;
	while (*p) {
	  if (*p == '\\') {
	    p++; if (! *p) break;
	  }
	  if (*p == ')' && --depth == 0)
	    break;
	  else if (*p == '(')
	    ++depth;
	  p++;
	}
	if (*p != ')')
	  throw new error("Missing ')'");

	xpath_len = (p - q) + 2;
	xpath = std::string(q, p - q);
      }

      std::string * prev_str = prefix->chars;
      std::string   copy_str(*prev_str, xpath_len);

      try {
	prefix->chars = &copy_str;
#if 0
	formatter_callback_t callback(out, *element->format, column, *this);
	item->select(xpath, callback);
	column = callback.column;
#endif
      }
      catch (...) {
	prefix->chars = prev_str;
	throw;
      }
      prefix->chars = prev_str;

      return column;
    }
  }

 base_handler:
  return element_formatter_t::operator()(out, element, scope, column);
}

void dump_command::operator()(value_t& result, valexpr_t::scope_t * locals)
{
  std::ostream * out   = get_ptr<std::ostream>(locals, 0);
  repitem_t *	 items = get_ptr<repitem_t>(locals, 1);
  items->print_tree(*out);
}

void format_command::operator()(value_t& result, valexpr_t::scope_t * locals)
{
  std::ostream * out   = get_ptr<std::ostream>(locals, 0);
  repitem_t *	 items = get_ptr<repitem_t>(locals, 1);

  formatter.format(*out, items, 0, repitem_t::formatter_t());
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
