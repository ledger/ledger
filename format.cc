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

std::string partial_account_name(const account_t *  account)
{
  std::string name;

  for (const account_t * acct = account;
       acct && acct->parent;
       acct = acct->parent) {
    if (acct->dflags & ACCOUNT_DISPLAYED)
      break;

    if (name.empty())
      name = acct->name;
    else
      name = acct->name + ":" + name;
  }

  return name;
}

std::string format_t::date_format = "%Y/%m/%d";

#ifdef NO_CLEANUP
value_expr_t * format_t::value_expr = NULL;
value_expr_t * format_t::total_expr = NULL;
#else
std::auto_ptr<value_expr_t> format_t::value_expr;
std::auto_ptr<value_expr_t> format_t::total_expr;
#endif

element_t * format_t::parse_elements(const std::string& fmt)
{
  element_t * result  = NULL;
  element_t * current = NULL;

  static char buf[1024];
  char * q = buf;

  for (const char * p = fmt.c_str(); *p; p++) {
    if (*p != '%') {
      *q++ = *p;
      continue;
    }

    if (! result) {
      current = result = new element_t;
    } else {
      current->next = new element_t;
      current = current->next;
    }

    if (q != buf) {
      current->type  = element_t::STRING;
      current->chars = std::string(buf, q);
      q = buf;

      current->next  = new element_t;
      current = current->next;
    }

    ++p;
    if (*p == '-') {
      current->align_left = true;
      ++p;
    }

    int num = 0;
    while (*p && std::isdigit(*p)) {
      num *= 10;
      num += *p++ - '0';
    }
    current->min_width = num;

    if (*p == '.') {
      ++p;
      num = 0;
      while (*p && std::isdigit(*p)) {
	num *= 10;
	num += *p++ - '0';
      }
      current->max_width = num;
      if (current->min_width == 0)
	current->min_width = current->max_width;
    }

    switch (*p) {
    case '%':
      current->type  = element_t::STRING;
      current->chars = "%";
      break;

    case '(': {
      ++p;
      const char * b = p;
      while (*p && *p != ')')
	p++;
      if (*p != ')')
	throw format_error("Missing ')'");

      current->type     = element_t::VALUE_EXPR;
      current->val_expr = parse_value_expr(std::string(b, p));
      break;
    }

    case '[': {
      ++p;
      const char * b = p;
      while (*p && *p != ']')
	p++;
      if (*p != ']')
	throw format_error("Missing ']'");

      current->type  = element_t::DATE_STRING;
      current->chars = std::string(b, p);
      break;
    }

    case 'D':
      current->type  = element_t::DATE_STRING;
      current->chars = format_t::date_format;
      break;

    case 'X': current->type = element_t::CLEARED; break;
    case 'C': current->type = element_t::CODE; break;
    case 'P': current->type = element_t::PAYEE; break;
    case 'n': current->type = element_t::ACCOUNT_NAME; break;
    case 'N': current->type = element_t::ACCOUNT_FULLNAME; break;
    case 'o': current->type = element_t::OPT_AMOUNT; break;
    case 't': current->type = element_t::VALUE; break;
    case 'T': current->type = element_t::TOTAL; break;
    case '_': current->type = element_t::SPACER; break;
    }
  }

  if (q != buf) {
    if (! result) {
      current = result = new element_t;
    } else {
      current->next = new element_t;
      current = current->next;
    }
    current->type  = element_t::STRING;
    current->chars = std::string(buf, q);
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

    case element_t::VALUE:
    case element_t::TOTAL:
    case element_t::VALUE_EXPR: {
      value_expr_t * expr;
      switch (elem->type) {
#ifdef NO_CLEANUP
      case element_t::VALUE: expr = value_expr; break;
      case element_t::TOTAL: expr = total_expr; break;
#else
      case element_t::VALUE: expr = value_expr.get(); break;
      case element_t::TOTAL: expr = total_expr.get(); break;
#endif
      case element_t::VALUE_EXPR: expr = elem->val_expr; break;

      default:
	assert(0);
	break;
      }
      value_t value;
      expr->compute(value, details);
      switch (value.type) {
      case value_t::BOOLEAN:
	out << (*((bool *) value.data) ? "1" : "0");
	break;
      case value_t::INTEGER:
	out << *((unsigned int *) value.data);
	break;
      case value_t::AMOUNT:
	out << std::string(*((amount_t *) value.data));
	break;
      case value_t::BALANCE:
	((balance_t *) value.data)->write(out, elem->min_width,
					  (elem->max_width > 0 ?
					   elem->max_width : elem->min_width));
	break;
      default:
	assert(0);
	break;
      }
      break;
    }

    case element_t::DATE_STRING:
      if (details.entry && details.entry->date != -1) {
	char buf[256];
	std::strftime(buf, 255, elem->chars.c_str(),
		      std::localtime(&details.entry->date));
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

    case element_t::CODE: {
      std::string temp;
      if (details.entry && ! details.entry->code.empty()) {
	temp += "(";
	temp += details.entry->code;
	temp += ") ";
      }
      out << temp;
      break;
    }

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
			    partial_account_name(details.account));

	if (details.xact && details.xact->flags & TRANSACTION_VIRTUAL) {
	  if (elem->max_width > 2)
	    name = truncated(name, elem->max_width - 2);

	  if (details.xact->flags & TRANSACTION_BALANCE)
	    name = "[" + name + "]";
	  else
	    name = "(" + name + ")";
	}
	else if (elem->max_width > 0)
	  name = truncated(name, elem->max_width);

	out << name;
      } else {
	out << " ";
      }
      break;

    case element_t::OPT_AMOUNT:
      if (details.xact) {
	std::string disp;
	bool        use_disp = false;

	if (details.xact->amount != details.xact->cost) {
	  amount_t unit_cost = details.xact->cost / details.xact->amount;
	  std::ostringstream stream;
	  stream << details.xact->amount << " @ " << unit_cost;
	  disp = stream.str();
	  use_disp = true;
	} else {
	  unsigned int xacts_real_count = 0;
	  transaction_t * first = NULL;
	  transaction_t * last  = NULL;

	  for (transactions_list::const_iterator i
		 = details.entry->transactions.begin();
	       i != details.entry->transactions.end();
	       i++)
	    if (! ((*i)->flags & TRANSACTION_AUTO)) {
	      xacts_real_count++;

	      if (! first)
		first = *i;
	      last = *i;
	    }

	  use_disp = (xacts_real_count == 2 &&
		      details.xact == last &&
		      first->amount == - last->amount);
	}

	if (! use_disp)
	  disp = std::string(details.xact->amount);
	out << disp;

	// jww (2004-07-31): this should be handled differently
	if (! details.xact->note.empty())
	  out << "  ; " << details.xact->note;
      }
      break;

    case element_t::SPACER:
      for (const account_t * acct = details.account;
	   acct;
	   acct = acct->parent)
	if (acct->dflags & ACCOUNT_DISPLAYED) {
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

bool format_account::disp_subaccounts_p(const account_t * account,
					const item_predicate<account_t>&
					    disp_pred,
					const account_t *& to_show)
{
  bool	       display  = false;
  unsigned int counted  = 0;
  bool         computed = false;
  bool         matches  = disp_pred(account);
  balance_t    acct_total;

  to_show = NULL;

  for (accounts_map::const_iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++) {
    if (! disp_pred((*i).second))
      continue;

    balance_t result;
    format_t::compute_total(result, details_t((*i).second));

    if (! computed) {
      format_t::compute_total(acct_total, details_t(account));
      computed = true;
    }

    if ((result != acct_total) || counted > 0) {
      display = matches;
      break;
    }
    to_show = (*i).second;
    counted++;
  }

  return display;
}

bool format_account::display_account(const account_t * account,
				     const item_predicate<account_t>& disp_pred,
				     const bool even_top)
{
  // Never display the master account, or an account that has already
  // been displayed.
  if (! (account->parent || even_top) || account->dflags & ACCOUNT_DISPLAYED)
    return false;

  // At this point, one of two possibilities exists: the account is a
  // leaf which matches the predicate restrictions; or it is a parent
  // and two or more children must be subtotaled; or it is a parent
  // and its child has been hidden by the predicate.  So first,
  // determine if it is a parent that must be displayed regardless of
  // the predicate.

  const account_t * account_to_show = NULL;
  if (disp_subaccounts_p(account, disp_pred, account_to_show))
    return true;

  return ! account_to_show && disp_pred(account);
}

} // namespace ledger
