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

std::string partial_account_name(const account_t&  account)
{
  std::string name;

  for (const account_t * acct = &account;
       acct && acct->parent;
       acct = acct->parent) {
    if (account_has_xdata(*acct) &&
	account_xdata(*acct).dflags & ACCOUNT_DISPLAYED)
      break;

    if (name.empty())
      name = acct->name;
    else
      name = acct->name + ":" + name;
  }

  return name;
}

std::string    format_t::date_format = "%Y/%m/%d";
value_expr_t * format_t::value_expr  = NULL;
value_expr_t * format_t::total_expr  = NULL;

static struct _init_format {
  ~_init_format();
} _init_obj;

_init_format::~_init_format()
{
  if (format_t::value_expr)
    delete format_t::value_expr;
  if (format_t::total_expr)
    delete format_t::total_expr;
}

element_t * format_t::parse_elements(const std::string& fmt)
{
  std::auto_ptr<element_t> result;

  element_t * current = NULL;

  static char buf[1024];
  char * q = buf;

  for (const char * p = fmt.c_str(); *p; p++) {
    if (*p != '%' && *p != '\\') {
      *q++ = *p;
      continue;
    }

    if (! result.get()) {
      result.reset(new element_t);
      current = result.get();
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

    if (*p == '\\') {
      p++;
      current->type = element_t::STRING;
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
    case '|': current->type = element_t::SPACER; break;
    case '_': current->type = element_t::DEPTH_SPACER; break;
    }
  }

  if (q != buf) {
    if (! result.get()) {
      result.reset(new element_t);
      current = result.get();
    } else {
      current->next = new element_t;
      current = current->next;
    }
    current->type  = element_t::STRING;
    current->chars = std::string(buf, q);
  }

  return result.release();
}

void format_t::format(std::ostream& out, const details_t& details) const
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
      value_expr_t * expr = NULL;
      switch (elem->type) {
      case element_t::VALUE: expr = value_expr; break;
      case element_t::TOTAL: expr = total_expr; break;
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
	out << *((amount_t *) value.data);
	break;
      case value_t::BALANCE:
	((balance_t *) value.data)->write(out, elem->min_width,
					  (elem->max_width > 0 ?
					   elem->max_width : elem->min_width));
	break;
      case value_t::BALANCE_PAIR:
	((balance_pair_t *) value.data)->quantity.write(out, elem->min_width,
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
			    partial_account_name(*details.account));

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

	if (details.xact->cost) {
	  amount_t unit_cost = *details.xact->cost / details.xact->amount;
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
	  out << details.xact->amount;
	else
	  out << disp;

	// jww (2004-07-31): this should be handled differently
	if (! details.xact->note.empty())
	  out << "  ; " << details.xact->note;
      }
      break;

    case element_t::SPACER:
      out << " ";
      break;

    case element_t::DEPTH_SPACER:
      for (const account_t * acct = details.account;
	   acct;
	   acct = acct->parent)
	if (account_has_xdata(*acct) &&
	    account_xdata(*acct).dflags & ACCOUNT_DISPLAYED) {
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

bool format_account::disp_subaccounts_p(const account_t& account,
					const item_predicate<account_t>&
					    disp_pred,
					const account_t *& to_show)
{
  bool	       display  = false;
  unsigned int counted  = 0;
  bool         matches  = disp_pred(account);
  value_t      acct_total;
  bool         computed = false;
  value_t      result;

  to_show = NULL;

  for (accounts_map::const_iterator i = account.accounts.begin();
       i != account.accounts.end();
       i++) {
    if (! disp_pred(*(*i).second))
      continue;

    format_t::compute_total(result, details_t(*(*i).second));
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

bool format_account::display_account(const account_t& account,
				     const item_predicate<account_t>& disp_pred)
{
  // Never display an account that has already been displayed.
  if (account_has_xdata(account) &&
      account_xdata(account).dflags & ACCOUNT_DISPLAYED)
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

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>

using namespace boost::python;
using namespace ledger;

std::string py_format_1(format_t& format, const details_t& item)
{
  std::ostringstream out;
  format.format(out, item);
  return out.str();
}

template <typename T>
std::string py_format(format_t& format, const T& item)
{
  std::ostringstream out;
  format.format(out, details_t(item));
  return out.str();
}

void export_format()
{
  class_< format_t > ("Format")
    .def(init<std::string>())
    .def("reset", &format_t::reset)
    .def("format", py_format_1)
    .def("format", py_format<account_t>)
    .def("format", py_format<entry_t>)
    .def("format", py_format<transaction_t>)
    ;

  def("truncated", truncated);
#if 0
  def("partial_account_name", partial_account_name);
#endif
}

#endif // USE_BOOST_PYTHON
