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

  for (const char * p = fmt.c_str(); *p; p++) {
    if (*p != '%' && *p != '\\') {
      *q++ = *p;
      continue;
    }

    elements.push_back(element_t());
    current = &elements.back();

    if (q != buf) {
      current->chars = std::string(buf, q);
      q = buf;

      elements.push_back(element_t());
      current = &elements.back();
    }

    if (*p == '\\') {
      p++;
      switch (*p) {
      case 'b': current->chars = "\b"; break;
      case 'f': current->chars = "\f"; break;
      case 'n': current->chars = "\n"; break;
      case 'r': current->chars = "\r"; break;
      case 't': current->chars = "\t"; break;
      case 'v': current->chars = "\v"; break;
      }
      continue;
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
	current->max_width > current->min_width)
      throw new format_error("Maximum width is greater than minimum width");

    switch (*p) {
    case '%':
      current->chars = "%";
      break;

    case '|':
      current->column = true;
      break;

    case '{': {
      ++p;
      const char * b = p;
      int depth = 1;
      while (*p) {
	if (*p == '}' && --depth == 0)
	  break;
	else if (*p == '{')
	  ++depth;
	p++;
      }
      if (*p != '}')
	throw new format_error("Missing '}'");

      assert(! current->valexpr);
      current->valexpr = std::string(b, p);
      break;
    }

    default:
      assert(! current->valexpr);
      current->valexpr = std::string(p, p + 1);
      break;
    }
  }

 END:
  if (q != buf) {
    elements.push_back(element_t());
    current = &elements.back();
    current->chars = std::string(buf, q);
  }
}

void format_t::format(std::ostream& out_str, valexpr_t::scope_t * scope,
		      int column) const
{
  for (std::list<element_t>::const_iterator i = elements.begin();
       i != elements.end();
       i++) {
    const element_t * elem = &(*i);

    if (elem->column) {
      if (elem->max_width != -1 && elem->max_width < column) {
	out_str << '\n';
	column = 0;
      }

      if (elem->min_width != -1 && elem->min_width > column) {
	out_str << std::string(elem->min_width - column, ' ');
	column = elem->min_width;
      }
      continue;
    }

    std::ostringstream out;

    if (elem->align_left)
      out << std::left;
    else
      out << std::right;

    if (elem->min_width > 0)
      out.width(elem->min_width);

    if (elem->valexpr)
      out << elem->valexpr.calc(scope).get_string();
    else
      out << elem->chars;

    std::string temp = out.str();
    if (elem->max_width > 0 && elem->max_width < temp.length())
      temp.erase(elem->max_width);

    out_str << temp;
    column += temp.length();
  }
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
