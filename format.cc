#include "format.h"
#include "error.h"
#include "util.h"
#ifdef USE_BOOST_PYTHON
#include "py_eval.h"
#endif

#include <cstdlib>

namespace ledger {

#if 0

format_t::elision_style_t format_t::elision_style = ABBREVIATE;
int format_t::abbrev_length = 2;

bool format_t::ansi_codes  = false;
bool format_t::ansi_invert = false;

std::string format_t::abbrev(const std::string& str, unsigned int width,
			     const bool is_account)
{
  const int len = str.length();
  if (len <= width)
    return str;

  assert(width < 4095);

  char buf[4096];

  switch (elision_style) {
  case TRUNCATE_LEADING:
    // This method truncates at the beginning.
    std::strncpy(buf, str.c_str() + (len - width), width);
    buf[0] = '.';
    buf[1] = '.';
    break;

  case TRUNCATE_MIDDLE:
    // This method truncates in the middle.
    std::strncpy(buf, str.c_str(), width / 2);
    std::strncpy(buf + width / 2,
		 str.c_str() + (len - (width / 2 + width % 2)),
		 width / 2 + width % 2);
    buf[width / 2 - 1] = '.';
    buf[width / 2] = '.';
    break;

  case ABBREVIATE:
    if (is_account) {
      std::list<std::string> parts;
      std::string::size_type beg = 0;
      for (std::string::size_type pos = str.find(':');
	   pos != std::string::npos;
	   beg = pos + 1, pos = str.find(':', beg))
	parts.push_back(std::string(str, beg, pos - beg));
      parts.push_back(std::string(str, beg));

      std::string result;
      int newlen = len;
      for (std::list<std::string>::iterator i = parts.begin();
	   i != parts.end();
	   i++) {
	// Don't contract the last element
	std::list<std::string>::iterator x = i;
	if (++x == parts.end()) {
	  result += *i;
	  break;
	}

	if (newlen > width) {
	  result += std::string(*i, 0, abbrev_length);
	  result += ":";
	  newlen -= (*i).length() - abbrev_length;
	} else {
	  result += *i;
	  result += ":";
	}
      }

      if (newlen > width) {
	// Even abbreviated its too big to show the last account, so
	// abbreviate all but the last and truncate at the beginning.
	std::strncpy(buf, result.c_str() + (result.length() - width), width);
	buf[0] = '.';
	buf[1] = '.';
      } else {
	std::strcpy(buf, result.c_str());
      }
      break;
    }
    // fall through...

  case TRUNCATE_TRAILING:
    // This method truncates at the end (the default).
    std::strncpy(buf, str.c_str(), width - 2);
    buf[width - 2] = '.';
    buf[width - 1] = '.';
    break;
  }
  buf[width] = '\0';

  return buf;
}

std::string partial_account_name(const account_t& account)
{
  std::string name;

  for (const account_t * acct = &account;
       acct && acct->parent;
       acct = acct->parent) {
    if (account_has_xdata(*acct) &&
	account_xdata_(*acct).dflags & ACCOUNT_DISPLAYED)
      break;

    if (name.empty())
      name = acct->name;
    else
      name = acct->name + ":" + name;
  }

  return name;
}

#endif

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

#include <boost/python.hpp>

using namespace boost::python;
using namespace ledger;

void export_format()
{
  class_< format_t > ("Format")
    .def(init<std::string>())
    .def("parse", &format_t::parse)
    .def("format", &format_t::format)
    ;

#if 0
  def("truncated", truncated);
  def("partial_account_name", partial_account_name);
  def("display_account", display_account);
#endif
}

#endif // USE_BOOST_PYTHON
