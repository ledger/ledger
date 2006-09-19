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
    if (parent->contents == this) {
      parent->contents = next;
      if (parent->last_content == this)
	parent->last_content = NULL;
    }
    else if (parent->children == this) {
      parent->children = next;
      if (parent->last_child == this)
	parent->last_child = NULL;
    }
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

  clear();
}

void repitem_t::clear()
{
  repitem_t * temp = contents;
  contents = NULL;
  for (repitem_t * content = temp; content; content = content->next) {
    content->parent = NULL;
    delete content;
  }

  temp = children;
  children = NULL;
  for (repitem_t * child = temp; child; child = child->next) {
    child->parent = NULL;
    delete child;
  }
}

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
    return;
  }

  switch (kind) {
  case TRANSACTION:
    if (xact->cost || ! val.realzero())
      val.add(xact->amount, xact->cost);
    else
      val = xact->amount;
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

  item->parent = this;
  item->valexpr_t::scope_t::parent = this;

  while (item->next) {
    repitem_t * next_item = item->next;
    next_item->prev   = item;
    next_item->parent = this;
    next_item->valexpr_t::scope_t::parent = this;
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
  item->valexpr_t::scope_t::parent = this;
  while (item->next) {
    repitem_t * next_item = item->next;
    next_item->prev   = item;
    next_item->parent = this;
    next_item->valexpr_t::scope_t::parent = this;
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
      if (filter.calc().to_boolean())
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

const repitem_t::path_t *
repitem_t::parse_selector(const std::string& expr)
{
  path_t * path = new path_t;

  const char * p = expr.c_str();
  do {
    const path_element_t * subpath = parse_subselector(p);
    if (subpath)
      path->paths.push_back(subpath);
    if (*p == '|')
      p++;
    else if (*p)
      throw error("Error in report item path selector");
  } while (*p);

  return path;
}

const repitem_t::path_element_t *
repitem_t::parse_subselector(const char *& b)
{
  path_element_t * first   = NULL;
  path_element_t * current = first;

  bool have_nodename = false;

  const char * p;
  for (p = b; *p; p++) {
    switch (*p) {
    case ' ':
      break;

    case '/':
      if (first && ! have_nodename)
	throw error("Error in report item path selector");

      if (! current) {
	current = new path_element_t;
      } else {
	current->next = new path_element_t;
	current = current->next;
      }

      if (! first) {
	first = current;
	first->root = true;
      }

      if (*(p + 1) == '/') {
	p++;
	current->recurse = true;
	current->next = new path_element_t;
	current = current->next;
      }

      have_nodename = false;
      break;

    case '[': {
      if (! have_nodename)
	throw error("Error in report item path selector");

      ++p;
      const char * q = p;
      int depth = 1;
      while (*p) {
	if (*p == ']' && --depth == 0)
	  break;
	else if (*p == '[')
	  ++depth;
	p++;
      }
      if (*p != ']')
	throw new error("Missing ']'");

      if (! current)
	first = current = new path_element_t;
      current->valexpr = std::string(q, p);
      break;
    }

    case '.':
      if (have_nodename)
	throw error("Error in report item path selector");

      if (! current)
	first = current = new path_element_t;
      if (*(p + 1) == '.') {
	p++;
	current->parent = true;
      }

      have_nodename = true;
      break;

    case '|':
      b = p;
      return first;

    default: {			// should be a node name here
      if (have_nodename || ! std::isalpha(*p))
	throw error("Error in report item path selector");

      const char * q = p;
      while (std::isalpha(*p++))
	;
      p--;

      if (! current)
	first = current = new path_element_t;

      switch (*q) {
      case 's':
	if (std::strncmp(q, "session", p - q) == 0)
	  current->kind = SESSION;
	else
	  throw error("Error in report item path selector");
	break;
      case 'j':
	if (std::strncmp(q, "journal", p - q) == 0)
	  current->kind = JOURNAL;
	else
	  throw error("Error in report item path selector");
	break;
      case 'a':
	if (std::strncmp(q, "account", p - q) == 0)
	  current->kind = ACCOUNT;
	else
	  throw error("Error in report item path selector");
	break;
      case 'e':
	if (std::strncmp(q, "entry", p - q) == 0)
	  current->kind = ENTRY;
	else
	  throw error("Error in report item path selector");
	break;
      case 'x':
	if (std::strncmp(q, "xact", p - q) == 0)
	  current->kind = TRANSACTION;
	else
	  throw error("Error in report item path selector");
	break;
      default:
	throw error("Error in report item path selector");
      }

      have_nodename = true;
      p--;
      break;
    }
    }
  }

  b = p;
  return first;
}

#ifdef DEBUG_ENABLED
void repitem_t::dump_path(std::ostream& out, const path_t * path)
{
  for (std::list<const path_element_t *>::const_iterator
	 i = path->paths.begin();
       i != path->paths.end();
       i++) {
    for (const path_element_t * p = *i; p; p = p->next) {
      switch (p->kind) {
      case UNKNOWN:
	out << "UNKNOWN ";
	break;
      case SESSION:
	out << "SESSION ";
	break;
      case JOURNAL:
	out << "JOURNAL ";
	break;
      case ACCOUNT:
	out << "ACCOUNT ";
	break;
      case ENTRY:
	out << "ENTRY ";
	break;
      case TRANSACTION:
	out << "TRANSACTION ";
	break;
      }

      if (p->valexpr) {
	out << '[';
	p->valexpr.write(out);
	out << "] ";
      }

      if (p->root)
	out << "/ ";
      if (p->parent)
	out << ".. ";
      if (p->recurse)
	out << "// ";

      out << std::endl;
    }

    out << std::endl;
  }
}
#endif

void repitem_t::select(const path_t * path, select_callback_t callback)
{
  for (std::list<const path_element_t *>::const_iterator
	 i = path->paths.begin();
       i != path->paths.end();
       i++) {
    if ((*i)->root && parent) {
      repitem_t * top = this;
      while (top->parent)
	top = top->parent;
      top->traverse_selection(*i, callback);
    } else {
      traverse_selection(*i, callback);
    }
  }
}

void repitem_t::select_all(select_callback_t callback)
{
  callback(this);

  for (repitem_t * content = contents; content; content = content->next)
    content->select_all(callback);

  for (repitem_t * child = children; child; child = child->next)
    child->select_all(callback);
}

void repitem_t::traverse_selection(const path_element_t * path,
				   select_callback_t callback)
{
  if (! path)
    return;

  if (path->parent) {
    if (parent)
      parent->traverse_selection(path->next, callback);
    return;
  }

  if (path->kind != UNKNOWN && kind != path->kind)
    return;

  if (path->valexpr && ! path->valexpr.calc(this))
    return;

  if (kind == TRANSACTION) {
    callback(this);
    return;
  }

  for (repitem_t * child =
	 (! path->next || path->next->kind == TRANSACTION) ?
	 contents : children; child; child = child->next)
    if (path->recurse && path->next->kind != child->kind)
      child->traverse_selection(path, callback);
    else
      child->traverse_selection(path->next, callback);
}

bool repitem_t::resolve(const std::string& name, value_t& result,
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
      date(result);
      return true;
    }
    break;

  case 'p':
    if (name == "payee") {
      payee(result);
      return true;
    }
    break;

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

int repitem_t::formatter_t::write_elements(std::ostream& out, format_t& format,
					   repitem_t * item, bool recursive,
					   bool children, int column) const
{
  for (repitem_t * ptr = children ? item->children : item->contents;
       ptr;
       ptr = ptr->next) {
    column = format.format(out, ptr, column, *this);
    if (recursive)
      column = write_elements(out, format, item, recursive, children, column);
  }
  return column;
}

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
      bool children  = false;
      bool recursive = false;
      int  length = prefix->chars->length();
      char initial;

      if (length > 0) {
	initial = (*prefix->chars)[0];
	switch (initial) {
	case '/':
	  children = true;
	  break;
	case ':':
	  break;
	default:
	  goto base_handler;
	}
      }

      if (length > 1 && initial == (*prefix->chars)[1])
	recursive = true;

      std::string * prev_str = prefix->chars;
      std::string   copy_str;

      if (! recursive)
	copy_str = std::string(*prev_str, 1);
      else
	copy_str = std::string(*prev_str, 2);

      try {
	prefix->chars = &copy_str;
	column = write_elements(out, *element->format, item, recursive,
				children, column);
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
