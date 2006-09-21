#ifdef USE_PCH
#include "pch.h"
#else
#include "xpath.h"
#endif

namespace ledger {

int xpath_t::traversable::position()
{
  if (! parent()) {
    assert(! next());
    return 1;
  }

  int index = 1;
  bool found = false;
  for (traversable * p = parent()->children(); p; p = p->next()) {
    if (p == this) {
      found = true;
      break;
    }
    index++;
  }

  assert(found);
  return index;
}

int xpath_t::traversable::last()
{
  if (! parent()) {
    assert(! next());
    return 1;
  }

  int count = 0;
  for (traversable * p = parent()->children(); p; p = p->next())
    count++;

  assert(count > 0);
  return count;
}

bool xpath_t::traversable::resolve(const std::string& name,
				   value_t& result, scope_t * locals)
{
  const char * p = name.c_str();
  switch (*p) {
  case 'c':
    if (name == "current") {
      result = current();
      return true;
    }
    break;
  case 'l':
    if (name == "last") {
      result = (long)last();
      return true;
    }
    break;
  case 'p':
    if (name == "parent") {
      result = parent();
      return true;
    }
    else if (name == "position") {
      result = (long)position();
      return true;
    }
    break;
  }
  return valexpr_t::scope_t::resolve(name, result, locals);
}

void xpath_t::clear()
{
  for (std::list<const element_t *>::const_iterator
	 i = paths.begin();
       i != paths.end();
       i++)
    delete *i;
}

void xpath_t::parse(const std::string& expr)
{
  if (paths.size() > 0)
    clear();

  const char * p = expr.c_str();
  do {
    if (const element_t * subpath = parse_subselector(p))
      paths.push_back(subpath);

    if (*p == '|')
      p++;
    else if (*p)
      throw error("Error in XPath selector");
  }
  while (*p);
}

const xpath_t::element_t * xpath_t::parse_subselector(const char *& b)
{
  element_t * first   = NULL;
  element_t * current = first;

  bool have_nodename = false;

  const char * p;
  for (p = b; *p; p++) {
    switch (*p) {
    case ' ':
      break;

    case '/':
      if (first && ! have_nodename)
	throw error("Error in XPath selector");

      if (! current) {
	current = new element_t;
      } else {
	current->next = new element_t;
	current = current->next;
      }

      if (! first) {
	first = current;
	current->target = element_t::ROOT;

	current->next = new element_t;
	current = current->next;
      }

      if (*(p + 1) == '/') {
	p++;
	current->recurse = true;
      }

      have_nodename = false;
      break;

    case '{':
    case '[': {
      if (! have_nodename)
	throw error("Error in XPath selector");

      const char open  = *p;
      const char close = open == '[' ? ']' : '}';

      ++p;
      const char * q = p;
      int depth = 1;
      while (*p) {
	if (*p == '\\') {
	  p++; if (! *p) break;
	}
	else if (*p == close && --depth == 0)
	  break;
	else if (*p == open)
	  ++depth;
	p++;
      }
      if (*p != close)
	throw new error(std::string("Missing '") + close + "'");

      if (! current)
	first = current = new element_t;

      valexpr_t valexpr(std::string(q, p));
      current->predicates.push_back(predicate_t());

      current->predicates.back().sort    = open == '{';;
      current->predicates.back().valexpr = valexpr;
      break;
    }

    case '*':
      if (have_nodename)
	throw error("Error in XPath selector");

      if (! current)
	first = current = new element_t;
      current->target = element_t::ALL;

      have_nodename = true;
      break;

    case '.':
      if (have_nodename)
	throw error("Error in XPath selector");

      if (! current)
	first = current = new element_t;

      if (*(p + 1) == '.') {
	p++;
	current->target = element_t::PARENT;
      } else {
	current->target = element_t::CURRENT;
      }

      have_nodename = true;
      break;

    case '|':
      b = p;
      return first;

    case '@':
      if (have_nodename || ! std::isalpha(*++p))
	throw error("Error in XPath selector");

      const char * q = p;
      while (std::isalnum(*p) || *p == '_')
	p++;

      current->name   = std::string(q, p - q);
      current->target = element_t::ATTRIBUTE;
      break;

    default: {			// should be a node name here
      if (have_nodename || ! std::isalpha(*p))
	throw error("Error in XPath selector");

      const char * q = p;
      while (std::isalpha(*p++))
	;
      p--;

      if (! current)
	first = current = new element_t;

      current->name   = std::string(q, p - q);
      current->target = element_t::NODE;

      have_nodename = true;
      p--;
      break;
    }
    }
  }

  if (current->target == element_t::NONE)
	throw error("Error in XPath selector");

  b = p;
  return first;
}

void xpath_t::write(std::ostream& out)
{
  bool first = true;

  for (std::list<const element_t *>::const_iterator i = paths.begin();
       i != paths.end();
       i++) {
    if (first)
      first = false;
    else
      out << " | ";

    for (const element_t * p = *i; p; p = p->next) {
      switch (p->target) {
      case element_t::NONE:
	assert(0);
	break;
      case element_t::ALL:
	out << "*";
	break;
      case element_t::NODE:
	assert(! p->name.empty());
	out << p->name;
	break;
      case element_t::ATTRIBUTE:
	assert(! p->name.empty());
	out << '@' << p->name;
	break;
      case element_t::ROOT:
	out << "/";
	break;
      case element_t::PARENT:
	out << "..";
	break;
      case element_t::CURRENT:
	out << ".";
	break;
      }

      for (std::list<predicate_t>::const_iterator
	     i = p->predicates.begin();
	   i != p->predicates.end();
	   i++) {
	if ((*i).sort)
	  out << '{';
	else
	  out << '[';

	(*i).valexpr.write(out);

	if ((*i).sort)
	  out << '}';
	else
	  out << ']';
      }

      if (p->next) {
	if (p->target != element_t::ROOT)
	  out << '/';
	if (p->next->recurse)
	  out << '/';
      }
    }
  }
}

#if 0
void repitem_t::select(const path_t * path, select_callback_t& callback)
{
  for (std::list<const element_t *>::const_iterator
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

void repitem_t::traverse_selection(const element_t * path,
				   select_callback_t& callback)
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

  for (std::list<valexpr_t>::const_iterator
	 i = path->predicates.begin();
       i != path->predicates.end();
       i++) {
    value_t result;
    (*i).calc(result, this);
    if (result.type == value_t::INTEGER ||
	result.type == value_t::AMOUNT) {
      value_t pos;
      position(pos);
      if (result != pos)
	return;
    }
    else if (! result) {
      return;
    }
  }

  repitem_t * content = contents;
  while (content) {
    repitem_t * next = content->next;

    if (path->recurse && (! path->next || path->next->kind != content->kind))
      content->traverse_selection(path, callback);
    else if (path->next)
      content->traverse_selection(path->next, callback);

    content = next;
  }

  repitem_t * child = children;
  while (child) {
    repitem_t * next = child->next;

    if (path->recurse && (! path->next || path->next->kind != child->kind))
      child->traverse_selection(path, callback);
    else if (path->next)
      child->traverse_selection(path->next, callback);

    child = next;
  }

  if (! path->next)
    callback(this);
}
#endif

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#ifndef USE_PCH
#include <boost/python.hpp>
#endif

using namespace boost::python;
using namespace ledger;

void export_xpath()
{
}

#endif // USE_BOOST_PYTHON
