#include "format.h"

namespace ledger {

std::string truncated(const std::string& str, unsigned int width)
{
  char buf[256];
  std::memset(buf, '\0', 255);
  assert(width < 256);
  std::strncpy(buf, str.c_str(), str.length());
  if (buf[width])
    std::strcpy(&buf[width - 2], "..");
  return buf;
}

std::string maximal_account_name(const item_t * item,
				 const item_t * parent)
{
  std::string name = item->account->name;
  for (const item_t * i = item->parent;
       i && i->account && i != parent;
       i = i->parent)
    name = i->account->name + ":" + name;
  return name;
}

std::string format_string(const item_t * item, const format_t& format,
			  const item_t * displayed_parent)
{
  std::string result;

  for (const char * p = format.format_string.c_str(); *p; p++) {
    if (*p == '%') {
      bool leftalign	= false;
      bool ignore	= false;
      int  width	= 0;
      int  strict_width = 0;

      ++p;
      if (*p == '?') {
	ignore = false; //subsequent_line;
	++p;
      }

      if (*p == '-') {
	leftalign = true;
	++p;
      }

      std::string num;
      while (*p && std::isdigit(*p))
	num += *p++;
      if (! num.empty())
	width = std::atol(num.c_str());

      if (*p == '.') {
	++p;
	num = "";
	while (*p && std::isdigit(*p))
	  num += *p++;
	if (! num.empty()) {
	  strict_width = std::atol(num.c_str());
	  if (width == 0)
	    width = strict_width;
	}
      }

      std::ostringstream out;

      if (leftalign)
	out << std::left;
      else
	out << std::right;

      if (width > 0)
	out.width(width);

      if (ignore) {
	out << " ";
	result += out.str();
	continue;
      }

      switch (*p) {
      case '%':
	out << "%";
	break;

      case '(': {
	++p;
	num = "";
	while (*p && *p != ')')
	  num += *p++;
	assert(*p == ')');

	node_t *  style = parse_expr(num, NULL);
	balance_t value = style->compute(format.begin(), format.end(), item);
	value.write(out, width, strict_width > 0 ? strict_width : width);
	break;
      }

      case '[': {
	++p;
	num = "";
	while (*p && *p != ']')
	  num += *p++;
	assert(*p == ']');

	if (item->date != -1) {
	  char buf[256];
	  std::strftime(buf, 255, num.c_str(), std::gmtime(&item->date));
	  out << (strict_width == 0 ? buf : truncated(buf, strict_width));
	} else {
	  out << " ";
	}
	break;
      }

      case 'd': {
	if (item->date != -1) {
	  char buf[32];
	  std::strftime(buf, 31, "%Y/%m/%d", std::gmtime(&item->date));
	  out << (strict_width == 0 ? buf : truncated(buf, strict_width));
	} else {
	out << " ";
	}
	break;
      }

      case 'p':
	out << (strict_width == 0 ?
		item->payee : truncated(item->payee, strict_width));
	break;

      case 'n':
	if (item->account) {
	  std::string name = maximal_account_name(item, displayed_parent);
	  out << (strict_width == 0 ? name : truncated(name, strict_width));
	} else {
	  out << " ";
	}
	break;

      case 'N':
	if (item->account)
	  out << (strict_width == 0 ?
		  item->account->fullname() :
		  truncated(item->account->fullname(), strict_width));
	else
	  out << " ";
	break;

      case 't':
	if (format.value_style) {
	  balance_t value = format.compute_value(item);
	  value.write(out, width, strict_width > 0 ? strict_width : width);
	}
	break;

      case 'T':
	if (format.total_style) {
	  balance_t value = format.compute_total(item);
	  value.write(out, width, strict_width > 0 ? strict_width : width);
	}
	break;

      case '_': {
	int depth = 0;
	for (const item_t * i = item; i->parent; i = i->parent)
	  depth++;

	for (const item_t * i = item->parent;
	     i && i->account && i != displayed_parent;
	     i = i->parent)
	  depth--;

	while (--depth >= 0) {
	  if (width > 0 || strict_width > 0)
	    out.width(width > strict_width ? width : strict_width);
	  out << " ";
	}
	break;
      }
      }

      result += out.str();
    } else {
      result += *p;
    }
  }

  return result;
}

} // namespace ledger
