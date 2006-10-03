#ifdef USE_PCH
#include "pch.h"
#else
#include "format.h"
#include "error.h"
#include "util.h"
#ifdef USE_BOOST_PYTHON
#include "py_eval.h"
#endif

#include <cstdlib>
#endif

namespace ledger {

void format_t::parse(const std::string& fmt)
{
  element_t * current = NULL;

  char   buf[1024];
  char * q = buf;

  if (elements.size() > 0)
    clear_elements();
  format_string = fmt;

  for (const char * p = fmt.c_str(); *p; p++) {
    if (*p != '%' && *p != '\\') {
      *q++ = *p;
      continue;
    }
    else if (*p == '\\') {
      p++;
      switch (*p) {
      case 'b': *q++ = '\b'; break;
      case 'f': *q++ = '\f'; break;
      case 'n': *q++ = '\n'; break;
      case 'r': *q++ = '\r'; break;
      case 't': *q++ = '\t'; break;
      case 'v': *q++ = '\v'; break;
      default:
	*q++ = *p;
	break;
      }
      continue;
    }
    else {
      assert(*p == '%');
      if (*(p + 1) == '%') {
	p++;			// %% is the same as \%
	*q++ = *p;
	continue;
      }
    }

    current = new element_t;
    elements.push_back(current);

    if (q != buf) {
      current->kind  = element_t::TEXT;
      current->chars = new std::string(buf, q);
      q = buf;

      current = new element_t;
      elements.push_back(current);
    }

    ++p;
    if (*p == '-') {
      current->align_left = true;
      ++p;
    }

    if (*p && std::isdigit(*p)) {
      int num = *p++ - '0';
      while (*p && std::isdigit(*p)) {
	num *= 10;
	num += *p++ - '0';
      }
      current->min_width = num;
    }

    if (*p == '.') {
      ++p;
      int num = 0;
      while (*p && std::isdigit(*p)) {
	num *= 10;
	num += *p++ - '0';
      }

      current->max_width = num;
      if (current->min_width == -1)
	current->min_width = current->max_width;
    }

    if (current->max_width != -1 && current->min_width != -1 &&
	current->max_width < current->min_width)
      throw new format_error("Maximum width is less than the minimum width");

    switch (*p) {
    case '|':
      current->kind = element_t::COLUMN;
      break;

    case '{':
    case '(': {
      char open  = *p;
      char close = *p == '{' ? '}' : ')';
      ++p;
      const char * b = p;
      int depth = 1;
      while (*p) {
	if (*p == close && --depth == 0)
	  break;
	else if (*p == open)
	  ++depth;
	p++;
      }
      if (*p != close)
	throw new format_error(std::string("Missing '") + close + "'");

      if (open == '{') {
	assert(! current->xpath);
	current->kind   = element_t::XPATH;
	current->xpath  = new xml::xpath_t(std::string(b, p));
      } else {
	assert(! current->format);
	current->kind   = element_t::GROUP;
	current->format = new format_t(std::string(b, p));
      }
      break;
    }

    default:
      assert(! current->xpath);
      current->kind  = element_t::XPATH;
      current->xpath = new xml::xpath_t(std::string(p, p + 1));
      break;
    }
  }

 END:
  if (q != buf) {
    current = new element_t;
    elements.push_back(current);

    current->kind  = element_t::TEXT;
    current->chars = new std::string(buf, q);
  }
}

void format_t::compile(xml::node_t * context)
{
  for (std::list<element_t *>::iterator i = elements.begin();
       i != elements.end();
       i++)
  switch ((*i)->kind) {
  case element_t::XPATH:
    assert((*i)->xpath);
    (*i)->xpath->compile(context);
    break;
  case element_t::GROUP:
    assert((*i)->format);
    (*i)->format->compile(context);
    break;
  }
}

int format_t::element_formatter_t::operator()
  (std::ostream& out_str, element_t * elem, xml::node_t * context,
   int column) const
{
  if (elem->kind == element_t::COLUMN) {
    if (elem->max_width != -1 && elem->max_width < column) {
      out_str << '\n';
      column = 0;
    }

    if (elem->min_width != -1 && elem->min_width > column) {
      out_str << std::string(elem->min_width - column, ' ');
      column = elem->min_width;
    }
    return column;
  }

  std::ostringstream out;

  if (elem->align_left)
    out << std::left;
  else
    out << std::right;

  if (elem->min_width > 0)
    out.width(elem->min_width);

  int start_column = column;

  if (elem->kind == element_t::XPATH)
    elem->xpath->calc(context).strip_annotations()
      .write(out, elem->min_width, elem->max_width);
  else if (elem->kind == element_t::GROUP)
    column = elem->format->format(out, context, column);
  else if (elem->kind == element_t::TEXT)
    out << *elem->chars;
  else
    assert(0);

  std::string temp = out.str();
  for (std::string::const_iterator i = temp.begin();
       i != temp.end();
       i++)
    if (*i == '\n' || *i == '\r')
      column = 0;
    else
      column++;

  int virtual_width = column - start_column;

  if (elem->min_width != -1 && virtual_width < elem->min_width) {
    out_str << temp << std::string(' ', elem->min_width - virtual_width);
  }
  else if (elem->max_width != -1 && virtual_width > elem->max_width) {
    temp.erase(temp.length() - (virtual_width - elem->max_width));
    out_str << temp;
  }
  else {
    out_str << temp;
  }

  return column;
}

int format_t::format(std::ostream& out, xml::node_t * context,
		     int column, const element_formatter_t& formatter) const
{
  for (std::list<element_t *>::const_iterator i = elements.begin();
       i != elements.end();
       i++)
    column = formatter(out, *i, context, column);

  return column;
}

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#ifndef USE_PCH
#include <boost/python.hpp>
#endif

using namespace boost::python;
using namespace ledger;

void export_format()
{
  class_< format_t > ("Format")
    .def(init<std::string>())
    .def("parse", &format_t::parse)
    .def("format", &format_t::format)
    ;
}

#endif // USE_BOOST_PYTHON
