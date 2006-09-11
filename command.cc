#if 0
namespace {
  inline void mark_red(std::ostream& out, const element_t * elem) {
    out.setf(std::ios::left);
    out.width(0);
    out << "\e[31m";

    if (elem->flags & ELEMENT_ALIGN_LEFT)
      out << std::left;
    else
      out << std::right;

    if (elem->min_width > 0)
      out.width(elem->min_width);
  }

  inline void mark_plain(std::ostream& out) {
    out << "\e[0m";
  }
}

format_transactions::format_transactions(std::ostream& _output_stream,
					 const std::string& format)
  : output_stream(_output_stream), last_entry(NULL), last_xact(NULL)
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
    }
    else if (last_xact && last_xact->date() != xact.date()) {
      first_line_format.format(output_stream, details_t(xact));
    }
    else {
      next_lines_format.format(output_stream, details_t(xact));
    }

    transaction_xdata(xact).dflags |= TRANSACTION_DISPLAYED;
    last_xact = &xact;
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

bool display_account(const account_t& account,
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
  header_entry._date = ledger::terminus;
  first_line_format.format(output_stream, details_t(header_entry));
}

void format_equity::flush()
{
  account_xdata_t xdata;
  xdata.value = total;
  xdata.value.negate();
  account_t summary(NULL, "Equity:Opening Balances");
  summary.data = &xdata;

  if (total.type >= value_t::BALANCE) {
    balance_t * bal;
    if (total.type == value_t::BALANCE)
      bal = (balance_t *) total.data;
    else if (total.type == value_t::BALANCE_PAIR)
      bal = &((balance_pair_t *) total.data)->quantity;
    else
      assert(0);

    for (amounts_map::const_iterator i = bal->amounts.begin();
	 i != bal->amounts.end();
	 i++) {
      xdata.value = (*i).second;
      xdata.value.negate();
      next_lines_format.format(output_stream, details_t(summary));
    }
  } else {
    next_lines_format.format(output_stream, details_t(summary));
  }
  output_stream.flush();
}

void format_equity::operator()(account_t& account)
{
  if (display_account(account, disp_pred)) {
    if (account_has_xdata(account)) {
      value_t val = account_xdata_(account).value;

      if (val.type >= value_t::BALANCE) {
	balance_t * bal;
	if (val.type == value_t::BALANCE)
	  bal = (balance_t *) val.data;
	else if (val.type == value_t::BALANCE_PAIR)
	  bal = &((balance_pair_t *) val.data)->quantity;
	else
	  assert(0);

	for (amounts_map::const_iterator i = bal->amounts.begin();
	     i != bal->amounts.end();
	     i++) {
	  account_xdata_(account).value = (*i).second;
	  next_lines_format.format(output_stream, details_t(account));
	}
	account_xdata_(account).value = val;
      } else {
	next_lines_format.format(output_stream, details_t(account));
      }
      total += val;
    }
    account_xdata(account).dflags |= ACCOUNT_DISPLAYED;
  }
}

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
  typedef
    pystream_handler_wrap<format_transactions, transaction_t, std::string>
    format_transactions_wrap;

  class_< format_transactions_wrap, bases<item_handler<transaction_t> > >
    ("FormatTransactions",
     init<PyObject *, std::string>()[with_custodian_and_ward<1, 2>()])
    .def("flush", &format_transactions_wrap::flush)
    .def("__call__", &format_transactions_wrap::operator())
    ;

  typedef
    pystream_handler_wrap<format_entries, transaction_t, std::string>
    format_entries_wrap;

  class_< format_entries_wrap, bases<item_handler<transaction_t> > >
    ("FormatEntries",
     init<PyObject *, std::string>()[with_custodian_and_ward<1, 2>()])
    .def("flush", &format_entries_wrap::flush)
    .def("__call__", &format_entries_wrap::operator())
    ;

  typedef
    pystream_handler_wrap<format_account, account_t, std::string, std::string>
    format_account_wrap;

  class_< format_account_wrap, bases<item_handler<transaction_t> > >
    ("FormatAccount",
     init<PyObject *, std::string, std::string>()
     [with_custodian_and_ward<1, 2>()])
    .def("flush", &format_account_wrap::flush)
    .def("__call__", &format_account_wrap::operator())
    ;

  typedef
    pystream_handler_wrap<format_equity, account_t, std::string, std::string>
    format_equity_wrap;

  class_< format_equity_wrap, bases<item_handler<transaction_t> > >
    ("FormatEquity",
     init<PyObject *, std::string, std::string>()
     [with_custodian_and_ward<1, 2>()])
    .def("flush", &format_equity_wrap::flush)
    .def("__call__", &format_equity_wrap::operator())
    ;

  class_< format_t > ("Format")
    .def(init<std::string>())
    .def("reset", &format_t::reset)
    .def("format", py_format_1)
    .def("format", py_format<account_t>)
    .def("format", py_format<entry_t>)
    .def("format", py_format<transaction_t>)
    ;

#if 0
  def("truncated", truncated);
  def("partial_account_name", partial_account_name);
#endif
  def("display_account", display_account);
}

#endif // USE_BOOST_PYTHON
#endif
