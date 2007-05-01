#include "register.h"
#include "journal.h"

namespace ledger {

string abbreviate(const string&	  str,
		  unsigned int	  width,
		  elision_style_t elision_style,
		  const bool	  is_account,
		  int		  abbrev_length)
{
  const unsigned int len = str.length();
  if (len <= width)
    return str;

  assert(width < 4095);

  static char buf[4096];

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
      std::list<string> parts;
      string::size_type beg = 0;
      for (string::size_type pos = str.find(':');
	   pos != string::npos;
	   beg = pos + 1, pos = str.find(':', beg))
	parts.push_back(string(str, beg, pos - beg));
      parts.push_back(string(str, beg));

      string result;
      unsigned int newlen = len;
      for (std::list<string>::iterator i = parts.begin();
	   i != parts.end();
	   i++) {
	// Don't contract the last element
	std::list<string>::iterator x = i;
	if (++x == parts.end()) {
	  result += *i;
	  break;
	}

	if (newlen > width) {
	  result += string(*i, 0, abbrev_length);
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

static void scan_for_transactions(std::ostream& out, const xml::node_t * node)
{
  if (! (node->flags & XML_NODE_IS_PARENT))
    return;
  
  const xml::parent_node_t * parent =
    static_cast<const xml::parent_node_t *>(node);

  for (const xml::node_t * child = parent->children();
       child;
       child = child->next)
    if (child->name_id == xml::document_t::TRANSACTION) {
      const xml::transaction_node_t * xact_node =
	dynamic_cast<const xml::transaction_node_t *>(child);
      assert(xact_node);

      const transaction_t * xact = xact_node->transaction;
      assert(xact);

      out << xact->entry->date() << ' '
	  << std::setw(21) << std::left
	  << abbreviate(xact->entry->payee, 21) << ' '
	  << std::setw(21) << std::left
	  << abbreviate(xact->account->fullname(), 21,
			ABBREVIATE, true) << ' '
	  << std::setw(12) << std::right;
      if (xact->amount)
	out << *xact->amount;
      out << '\n';
    } else {
      scan_for_transactions(out, child);
    }
}

void register_command::print_document(std::ostream&	out,
				      xml::document_t * doc)
{
#if 1
  scan_for_transactions(out, doc->top);
  out.flush();
#else
  value_t nodelist;
  xml::xpath_t::eval(nodelist, "//transaction", doc);

  const value_t::sequence_t * xact_list = nodelist.to_sequence();
  assert(xact_list);

  for (value_t::sequence_t::const_iterator i = xact_list->begin();
       i != xact_list->end();
       i++) {
    const xml::node_t * node = (*i).to_xml_node();
    assert(node);

    const xml::transaction_node_t * xact_node =
      dynamic_cast<const xml::transaction_node_t *>(node);
    assert(xact_node);

    const transaction_t * xact = xact_node->transaction;
    assert(xact);

    std::cout << xact->entry->date() << ' '
	      << std::setw(21) << std::left
	      << abbreviate(xact->entry->payee, 21) << ' '
	      << std::setw(21) << std::left
	      << abbreviate(xact->account->fullname(), 21,
			    ABBREVIATE, true) << ' '
	      << std::setw(12) << std::right
	      << xact->amount
	      << std::endl;
  }
#endif
}

} // namespace ledger
