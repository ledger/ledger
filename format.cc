#include "format.h"
#include "error.h"
#ifdef USE_BOOST_PYTHON
#include "py_eval.h"
#endif

#include <cstdlib>
#include <ctime>

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

std::string format_t::date_format = "%Y/%m/%d";

element_t * format_t::parse_elements(const std::string& fmt)
{
  std::auto_ptr<element_t> result;

  element_t * current = NULL;

  char   buf[1024];
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
      int depth = 1;
      while (*p) {
	if (*p == ')' && --depth == 0)
	  break;
	else if (*p == '(')
	  ++depth;
	p++;
      }
      if (*p != ')')
	throw format_error("Missing ')'");

      current->type = element_t::VALUE_EXPR;
      try {
	current->val_expr = parse_value_expr(std::string(b, p));
      }
      catch (value_expr_error& err) {
	throw value_expr_error(std::string("In format expression '") +
			       std::string(b, p) + "': " + err.what());
      }
      break;
    }

    case '@': {
      const char * s = ++p;
      while (*p && *p != '(')
	p++;
      if (*p && *++p != ')')
	throw format_error("Missing ')'");

      current->type  = element_t::INTERP_FUNC;
      current->chars = std::string(s, (p - s) - 1);
      break;
    }

    case '[': {
      ++p;
      const char * b = p;
      int depth = 1;
      while (*p) {
	if (*p == ']' && --depth == 0)
	  break;
	else if (*p == '[')
	  ++depth;
	p++;
      }
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
    case 'a': current->type = element_t::ACCOUNT_NAME; break;
    case 'A': current->type = element_t::ACCOUNT_FULLNAME; break;
    case 't': current->type = element_t::AMOUNT; break;
    case 'o': current->type = element_t::OPT_AMOUNT; break;
    case 'T': current->type = element_t::TOTAL; break;
    case 'N': current->type = element_t::NOTE; break;
    case 'n': current->type = element_t::OPT_NOTE; break;
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

    case element_t::AMOUNT:
    case element_t::TOTAL:
    case element_t::VALUE_EXPR: {
      value_expr_t * expr = NULL;
      switch (elem->type) {
      case element_t::AMOUNT:     expr = amount_expr.get(); break;
      case element_t::TOTAL:      expr = total_expr.get(); break;
      case element_t::VALUE_EXPR: expr = elem->val_expr; break;
      default:
	assert(0);
	break;
      }
      if (! expr)
	break;

      value_t     value;
      balance_t * bal = NULL;

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
	bal = (balance_t *) value.data;
	// fall through...

      case value_t::BALANCE_PAIR:
	if (! bal)
	  bal = &((balance_pair_t *) value.data)->quantity;

	bal->write(out, elem->min_width,
		   (elem->max_width > 0 ? elem->max_width : elem->min_width));
	break;
      default:
	assert(0);
	break;
      }
      break;
    }

    case element_t::OPT_AMOUNT:
      if (details.xact) {
	std::string disp;
	bool        use_disp = false;

	if (details.xact->cost && details.xact->amount) {
	  amount_t unit_cost = *details.xact->cost / details.xact->amount;

	  commodity_t& comm(unit_cost.commodity());
	  bool has_flag = comm.flags & COMMODITY_STYLE_VARIABLE;
	  if (! has_flag)
	    unit_cost.commodity().flags |= COMMODITY_STYLE_VARIABLE;

	  std::ostringstream stream;
	  stream << details.xact->amount << " @ " << unit_cost;
	  disp = stream.str();
	  use_disp = true;

	  if (! has_flag)
	    unit_cost.commodity().flags &= ~COMMODITY_STYLE_VARIABLE;
	} else {
	  unsigned int    xacts_count = 0;
	  transaction_t * first = NULL;
	  transaction_t * last  = NULL;

	  for (transactions_list::const_iterator i
		 = details.entry->transactions.begin();
	       i != details.entry->transactions.end();
	       i++)
	    if (transaction_has_xdata(**i) &&
		transaction_xdata_(**i).dflags & TRANSACTION_TO_DISPLAY) {
	      xacts_count++;
	      if (! first)
		first = *i;
	      last = *i;
	    }

	  use_disp = (xacts_count == 2 && details.xact == last &&
		      first->amount == - last->amount);
	}

	if (! use_disp)
	  out << details.xact->amount;
	else
	  out << disp;
      }
      break;

    case element_t::DATE_STRING:
      if (details.entry && details.entry->date) {
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

    case element_t::OPT_NOTE:
      if (details.xact && ! details.xact->note.empty())
	out << "  ; ";
      // fall through...

    case element_t::NOTE:
      if (details.xact)
	out << (elem->max_width == 0 ?
		details.xact->note : truncated(details.xact->note,
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

    case element_t::SPACER:
      out << " ";
      break;

    case element_t::DEPTH_SPACER:
      for (const account_t * acct = details.account;
	   acct;
	   acct = acct->parent)
	if (account_has_xdata(*acct) &&
	    account_xdata_(*acct).dflags & ACCOUNT_DISPLAYED) {
	  if (elem->min_width > 0 || elem->max_width > 0)
	    out.width(elem->min_width > elem->max_width ?
		      elem->min_width : elem->max_width);
	  out << " ";
	}
      break;

    case element_t::INTERP_FUNC:
#ifdef USE_BOOST_PYTHON
      try {
	object func = python_eval(elem->chars);
	out << call<std::string>(func.ptr(), details);
      }
      catch(const boost::python::error_already_set&) {
	PyErr_Print();
	throw format_error(std::string("While calling Python function '") +
			   elem->chars + "'");
      }
#endif
      break;

    default:
      assert(0);
      break;
    }
  }
}

format_transactions::format_transactions(std::ostream&      _output_stream,
					 const std::string& format)
  : output_stream(_output_stream), last_entry(NULL)
{
  const char * f = format.c_str();
  if (const char * p = std::strstr(f, "%/")) {
    first_line_format.reset(std::string(f, 0, p - f));
    next_lines_format.reset(std::string(p + 2));
  } else {
    first_line_format.reset(format);
    next_lines_format.reset(format);
  }
}

void format_transactions::operator()(transaction_t& xact)
{
  if (! transaction_has_xdata(xact) ||
      ! (transaction_xdata_(xact).dflags & TRANSACTION_DISPLAYED)) {
    if (last_entry != xact.entry) {
      first_line_format.format(output_stream, details_t(xact));
      last_entry = xact.entry;
    } else {
      next_lines_format.format(output_stream, details_t(xact));
    }
    transaction_xdata_(xact).dflags |= TRANSACTION_DISPLAYED;
  }
}

void format_entries::format_last_entry()
{
  bool first = true;
  for (transactions_list::const_iterator i = last_entry->transactions.begin();
       i != last_entry->transactions.end();
       i++) {
    if (transaction_has_xdata(**i) &&
	transaction_xdata_(**i).dflags & TRANSACTION_TO_DISPLAY) {
      if (first) {
	first_line_format.format(output_stream, details_t(**i));
	first = false;
      } else {
	next_lines_format.format(output_stream, details_t(**i));
      }
      transaction_xdata_(**i).dflags |= TRANSACTION_DISPLAYED;
    }
  }
}

void format_entries::operator()(transaction_t& xact)
{
  transaction_xdata(xact).dflags |= TRANSACTION_TO_DISPLAY;

  if (last_entry && xact.entry != last_entry)
    format_last_entry();

  last_entry = xact.entry;
}

void xml_write_amount(std::ostream& out, const amount_t& amount,
		      const int depth = 0)
{
  for (int i = 0; i < depth; i++) out << ' ';
  out << "<amount>\n";

  commodity_t& c = amount.commodity();
  for (int i = 0; i < depth + 2; i++) out << ' ';
  out << "<commodity flags=\"";
  if (! (c.flags & COMMODITY_STYLE_SUFFIXED)) out << 'P';
  if (c.flags & COMMODITY_STYLE_SEPARATED)    out << 'S';
  if (c.flags & COMMODITY_STYLE_THOUSANDS)    out << 'T';
  if (c.flags & COMMODITY_STYLE_EUROPEAN)     out << 'E';
  out << "\">" << c.symbol << "</commodity>\n";

  for (int i = 0; i < depth + 2; i++) out << ' ';
  out << "<quantity>";
  out << amount.quantity_string() << "</quantity>\n";

  for (int i = 0; i < depth; i++) out << ' ';
  out << "</amount>\n";
}

void xml_write_value(std::ostream& out, const value_t& value,
		     const int depth = 0)
{
  balance_t * bal = NULL;

  for (int i = 0; i < depth; i++) out << ' ';
  out << "<value type=\"";
  switch (value.type) {
  case value_t::BOOLEAN: out << "boolean"; break;
  case value_t::INTEGER: out << "integer"; break;
  case value_t::AMOUNT: out << "amount"; break;
  case value_t::BALANCE:
  case value_t::BALANCE_PAIR: out << "balance"; break;
  }
  out << "\">\n";

  switch (value.type) {
  case value_t::BOOLEAN:
    for (int i = 0; i < depth + 2; i++) out << ' ';
    out << "<boolean>" << *((bool *) value.data) << "</boolean>\n";
    break;

  case value_t::INTEGER:
    for (int i = 0; i < depth + 2; i++) out << ' ';
    out << "<integer>" << *((long *) value.data) << "</integer>\n";
    break;

  case value_t::AMOUNT:
    xml_write_amount(out, *((amount_t *) value.data), depth + 2);
    break;

  case value_t::BALANCE:
    bal = (balance_t *) value.data;
    // fall through...

  case value_t::BALANCE_PAIR:
    if (! bal)
      bal = &((balance_pair_t *) value.data)->quantity;

    for (int i = 0; i < depth + 2; i++) out << ' ';
    out << "<balance>\n";

    for (amounts_map::const_iterator i = bal->amounts.begin();
	 i != bal->amounts.end();
	 i++)
      xml_write_amount(out, (*i).second, depth + 4);

    for (int i = 0; i < depth + 2; i++) out << ' ';
    out << "</balance>\n";
    break;

  default:
    assert(0);
    break;
  }

  for (int i = 0; i < depth; i++) out << ' ';
  out << "</value>\n";
}

std::string xml_string(const std::string& str)
{
  std::string::size_type pos   = 0;
  std::string::size_type index = str.find('&', pos);
  if (index == std::string::npos)
    return str;

  std::string temp;
  while (index != std::string::npos) {
    temp += std::string(str, pos, index) + "&amp;";
    pos = index + 1;
    index = str.find('&', pos);
  }

  return temp;
}

void format_xml_entries::format_last_entry()
{
  char buf[256];
  std::strftime(buf, 255, format_t::date_format.c_str(),
		std::localtime(&last_entry->date));

  output_stream << "  <entry>\n"
		<< "    <en:date>" << buf << "</en:date>\n";

  if (last_entry->state == entry_t::CLEARED)
    output_stream << "    <en:cleared/>\n";
  else if (last_entry->state == entry_t::PENDING)
    output_stream << "    <en:pending/>\n";

  if (! last_entry->code.empty())
    output_stream << "    <en:code>" << xml_string(last_entry->code)
		  << "</en:code>\n";

  if (! last_entry->payee.empty())
    output_stream << "    <en:payee>" << xml_string(last_entry->payee)
		  << "</en:payee>\n";

  bool first = true;
  for (transactions_list::const_iterator i = last_entry->transactions.begin();
       i != last_entry->transactions.end();
       i++) {
    if (transaction_has_xdata(**i) &&
	transaction_xdata_(**i).dflags & TRANSACTION_TO_DISPLAY) {
      if (first) {
	output_stream << "    <en:transactions>\n";
	first = false;
      }

      output_stream << "      <transaction>\n";

      if ((*i)->flags & TRANSACTION_VIRTUAL)
	output_stream << "        <tr:virtual/>\n";
      if ((*i)->flags & TRANSACTION_AUTO)
	output_stream << "        <tr:generated/>\n";

      if ((*i)->account) {
	std::string name = (*i)->account->fullname();
	if (name == "<Total>")
	  name = "[TOTAL]";
	else if (name == "<Unknown>")
	  name = "[UNKNOWN]";
	output_stream << "        <tr:account>" << name << "</tr:account>\n";
      }

      output_stream << "        <tr:amount>\n";
      if (transaction_xdata_(**i).dflags & TRANSACTION_COMPOSITE)
	xml_write_value(output_stream,
			transaction_xdata_(**i).composite_amount, 10);
      else
	xml_write_value(output_stream, value_t((*i)->amount), 10);
      output_stream << "        </tr:amount>\n";

      if ((*i)->cost)
	output_stream << "        <tr:cost>" << *(*i)->cost << "</tr:cost>\n";

      if (! (*i)->note.empty())
	output_stream << "        <tr:note>" << xml_string((*i)->note)
		      << "</tr:note>\n";

      if (show_totals) {
	output_stream << "        <total>\n";
	xml_write_value(output_stream, transaction_xdata_(**i).total, 10);
	output_stream << "        </total>\n";
      }

      output_stream << "      </transaction>\n";

      transaction_xdata_(**i).dflags |= TRANSACTION_DISPLAYED;
    }
  }

  if (! first)
    output_stream << "    </en:transactions>\n";

  output_stream << "  </entry>\n";
}

void print_entry(std::ostream& out, const entry_t& entry)
{
  const std::string print_format
    = "\n%D %X%C%P\n    %-34A  %12o\n%/    %-34A  %12o\n";

  format_entries formatter(out, print_format);
  walk_transactions(const_cast<transactions_list&>(entry.transactions),
		    formatter);
  formatter.flush();

  clear_transactions_xdata();
  clear_accounts_xdata();
}

bool disp_subaccounts_p(const account_t&		 account,
			const item_predicate<account_t>& disp_pred,
			const account_t *&		 to_show)
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

    compute_total(result, details_t(*(*i).second));
    if (! computed) {
      compute_total(acct_total, details_t(account));
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

bool display_account(const account_t&		      account,
		     const item_predicate<account_t>& disp_pred)
{
  // Never display an account that has already been displayed.
  if (account_has_xdata(account) &&
      account_xdata_(account).dflags & ACCOUNT_DISPLAYED)
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

void format_account::operator()(account_t& account)
{
  if (display_account(account, disp_pred)) {
    if (! account.parent) {
      account_xdata(account).dflags |= ACCOUNT_TO_DISPLAY;
    } else {
      format.format(output_stream, details_t(account));
      account_xdata(account).dflags |= ACCOUNT_DISPLAYED;
    }
  }
}

format_equity::format_equity(std::ostream&      _output_stream,
			     const std::string& _format,
			     const std::string& display_predicate)
  : output_stream(_output_stream), disp_pred(display_predicate)
{
  const char * f = _format.c_str();
  if (const char * p = std::strstr(f, "%/")) {
    first_line_format.reset(std::string(f, 0, p - f));
    next_lines_format.reset(std::string(p + 2));
  } else {
    first_line_format.reset(_format);
    next_lines_format.reset(_format);
  }

  entry_t header_entry;
  header_entry.payee = "Opening Balances";
  header_entry.date  = now;
  first_line_format.format(output_stream, details_t(header_entry));
}

void format_equity::flush()
{
  account_xdata_t xdata;
  xdata.value = total;
  xdata.value.negate();
  account_t summary(NULL, "Equity:Opening Balances");
  summary.data = &xdata;
  next_lines_format.format(output_stream, details_t(summary));
  output_stream.flush();
}

void format_equity::operator()(account_t& account)
{
  if (display_account(account, disp_pred)) {
    next_lines_format.format(output_stream, details_t(account));
    if (account_has_xdata(account))
      total += account_xdata_(account).value;
    account_xdata(account).dflags |= ACCOUNT_DISPLAYED;
  }
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
  def("display_account", display_account);
}

#endif // USE_BOOST_PYTHON
