#include "format.h"
#include "error.h"

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

std::string partial_account_name(const account_t *  account,
				 const unsigned int start_depth)
{
  std::string	    name = account->name;
  const account_t * acct = account->parent;

  for (int i = account->depth - start_depth - 1;
       --i >= 0 && acct->parent; ) {
    assert(acct);
    name = acct->name + ":" + name;
    acct = acct->parent;
  }
  return name;
}

std::auto_ptr<node_t> format_t::value_expr;
std::auto_ptr<node_t> format_t::total_expr;

element_t * format_t::parse_elements(const std::string& fmt)
{
  element_t * result  = NULL;
  element_t * current = NULL;
  std::string str;

  for (const char * p = fmt.c_str(); *p; p++) {
    if (*p == '%') {
      if (! result) {
	current = result = new element_t;
      } else {
	current->next = new element_t;
	current = current->next;
      }

      if (! str.empty()) {
	current->type  = element_t::STRING;
	current->chars = str;
	str = "";

	current->next  = new element_t;
	current = current->next;
      }

      ++p;
      if (*p == '-') {
	current->align_left = true;
	++p;
      }

      std::string num;
      while (*p && std::isdigit(*p))
	num += *p++;
      if (! num.empty())
	current->min_width = std::atol(num.c_str());

      if (*p == '.') {
	++p;
	num = "";
	while (*p && std::isdigit(*p))
	  num += *p++;
	if (! num.empty()) {
	  current->max_width = std::atol(num.c_str());
	  if (current->min_width == 0)
	    current->min_width = current->max_width;
	}
      }

      switch (*p) {
      case '%':
	current->type  = element_t::STRING;
	current->chars = "%";
	break;

      case '(':
	++p;
	num = "";
	while (*p && *p != ')')
	  num += *p++;
	if (*p != ')')
	  throw format_error("Missing ')'");

	current->type     = element_t::VALUE_EXPR;
	current->val_expr = parse_expr(num);
	break;

      case '[':
	++p;
	num = "";
	while (*p && *p != ']')
	  num += *p++;
	if (*p != ']')
	  throw format_error("Missing ']'");

	current->type  = element_t::DATE_STRING;
	current->chars = num;
	break;

      case 'd':
	current->type  = element_t::DATE_STRING;
	current->chars = "%Y/%m/%d";
	break;

      case 'X': current->type = element_t::CLEARED; break;
      case 'C': current->type = element_t::CODE; break;
      case 'p': current->type = element_t::PAYEE; break;
      case 'n': current->type = element_t::ACCOUNT_NAME; break;
      case 'N': current->type = element_t::ACCOUNT_FULLNAME; break;
      case 'o': current->type = element_t::OPT_AMOUNT; break;
      case 't': current->type = element_t::VALUE; break;
      case 'T': current->type = element_t::TOTAL; break;
      case '_': current->type = element_t::SPACER; break;
      }
    } else {
      str += *p;
    }
  }

  if (! str.empty()) {
    if (! result) {
      current = result = new element_t;
    } else {
      current->next = new element_t;
      current = current->next;
    }
    current->type  = element_t::STRING;
    current->chars = str;
  }

  return result;
}

void format_t::format_elements(std::ostream&    out,
			       const details_t& details) const
{
  for (const element_t * elem = elements; elem; elem = elem->next) {
    if (elem->align_left)
      out << std::left;
    else
      out << std::right;

    if (elem->min_width > 0)
      out.width(elem->min_width);

    switch (elem->type) {
    case element_t::STRING:
      out << elem->chars;
      break;

    case element_t::VALUE_EXPR: {
      balance_t value;
      elem->val_expr->compute(value, details);
      value.write(out, elem->min_width, (elem->max_width > 0 ?
					 elem->max_width : elem->min_width));
      break;
    }

    case element_t::DATE_STRING:
      if (details.entry && details.entry->date != -1) {
	char buf[256];
	std::strftime(buf, 255, elem->chars.c_str(),
		      std::gmtime(&details.entry->date));
	out << (elem->max_width == 0 ? buf : truncated(buf, elem->max_width));
      } else {
	out << " ";
      }
      break;

    case element_t::CLEARED:
      if (details.entry && details.entry->state == entry_t::CLEARED)
	out << "* ";
      else
	out << "";
      break;

    case element_t::CODE:
      if (details.entry && ! details.entry->code.empty())
	out << "(" << details.entry->code << ") ";
      else
	out << "";
      break;

    case element_t::PAYEE:
      if (details.entry)
	out << (elem->max_width == 0 ?
		details.entry->payee : truncated(details.entry->payee,
						 elem->max_width));
      break;

    case element_t::ACCOUNT_NAME:
    case element_t::ACCOUNT_FULLNAME:
      if (details.account) {
	std::string name = (elem->type == element_t::ACCOUNT_FULLNAME ?
			    details.account->fullname() :
			    partial_account_name(details.account,
						 details.depth));
	if (elem->max_width > 0)
	  name = truncated(name, elem->max_width);

	if (details.xact && details.xact->flags & TRANSACTION_VIRTUAL) {
	  if (details.xact->flags & TRANSACTION_BALANCE)
	    name = "[" + name + "]";
	  else
	    name = "(" + name + ")";
	}
	out << name;
      } else {
	out << " ";
      }
      break;

    case element_t::OPT_AMOUNT: {
      if (! details.entry || ! details.xact)
	break;

      std::string disp;
      bool        use_disp = false;

      if (std::find(details.entry->transactions.begin(),
		    details.entry->transactions.end(), details.xact) !=
	  details.entry->transactions.end()) {
	if (details.entry->transactions.size() == 2 &&
	    details.xact == details.entry->transactions.back() &&
	    (details.entry->transactions.front()->amount ==
	     details.entry->transactions.front()->cost) &&
	    (details.entry->transactions.front()->amount ==
	     - details.entry->transactions.back()->amount)) {
	  use_disp = true;
	}
	else if (details.entry->transactions.size() != 2 &&
		 details.xact->amount != details.xact->cost) {
	  amount_t unit_cost = details.xact->cost / details.xact->amount;
	  std::ostringstream stream;
	  stream << details.xact->amount << " @ " << unit_cost;
	  disp = stream.str();
	  use_disp = true;
	}
      }

      if (! use_disp)
	disp = std::string(details.xact->amount);
      out << disp;

      // jww (2004-07-31): this should be handled differently
      if (! details.xact->note.empty())
	out << "  ; " << details.xact->note;
      break;
    }

    case element_t::VALUE: {
      balance_t value;
      compute_value(value, details);
      value.write(out, elem->min_width, (elem->max_width > 0 ?
					 elem->max_width : elem->min_width));
      break;
    }

    case element_t::TOTAL: {
      balance_t value;
      compute_total(value, details);
      value.write(out, elem->min_width, (elem->max_width > 0 ?
					 elem->max_width : elem->min_width));
      break;
    }

    case element_t::SPACER:
      for (unsigned int i = 0; i < details.depth; i++) {
	if (elem->min_width > 0 || elem->max_width > 0)
	  out.width(elem->min_width > elem->max_width ?
		    elem->min_width : elem->max_width);
	out << " ";
      }
      break;

    default:
      assert(0);
      break;
    }
  }
}

void format_transaction::operator()(transaction_t * xact)
{
  if (inverted) {
    xact->total.quantity += - xact->amount;
    xact->total.cost     += - xact->cost;
  } else {
    xact->total += *xact;
  }
  xact->index = index++;

  // This makes the assumption that transactions from a single entry
  // will always be grouped together.

  if (last_entry != xact->entry)
    first_line_format.format_elements(output_stream, details_t(xact));
  else
    next_lines_format.format_elements(output_stream, details_t(xact));

  last_entry = xact->entry;
}

void format_account::operator()(const account_t *  account,
				const unsigned int max_depth,
				const bool         report_top)
{
  // Don't output the account if only one child will be displayed
  // which shows the exact same amount.  jww (2004-08-03): How do
  // compute the right figure?  It should a value expression specified
  // by the user, to say, "If this expression is equivalent between a
  // parent account and a lone displayed child, then don't display the
  // parent."

  if (bool output = report_top || account->parent != NULL) {
    int  counted = 0;
    bool display = false;

    for (accounts_map::const_iterator i = account->accounts.begin();
	 i != account->accounts.end();
	 i++) {
      if (! (*i).second->total)
	continue;

      if ((*i).second->total != account->total || counted > 0) {
	display = true;
	break;
      }
      counted++;
    }

    if (counted == 1 && ! display)
      output = false;

    if (output) {
      unsigned int depth = account->depth;
      if (max_depth == 0 || depth <= max_depth) {
	for (const account_t * acct = account;
	     depth > 0 && acct && acct != last_account;
	     acct = acct->parent)
	  depth--;

	format.format_elements(output_stream, details_t(account, depth));
	last_account = account;
      }
    }
  }
}

} // namespace ledger
