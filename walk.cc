#include "walk.h"
#include "format.h"

namespace ledger {

std::list<transaction_xdata_t> transactions_xdata;
std::list<void **>	       transactions_xdata_ptrs;

std::list<account_xdata_t>     accounts_xdata;
std::list<void **>	       accounts_xdata_ptrs;

void sort_transactions::flush()
{
  std::stable_sort(transactions.begin(), transactions.end(),
		   compare_items<transaction_t>(sort_order));

  for (transactions_deque::iterator i = transactions.begin();
       i != transactions.end();
       i++)
    (*handler)(**i);

  transactions.clear();

  item_handler<transaction_t>::flush();
}

void calc_transactions::operator()(transaction_t& xact)
{
  if (last_xact && transaction_has_xdata(*last_xact)) {
    transaction_xdata(xact).total += transaction_xdata(*last_xact).total;
    transaction_xdata(xact).index  = transaction_xdata(*last_xact).index + 1;
  } else {
    transaction_xdata(xact).index = 0;
  }

  if (inverted) {
    xact.amount.negate();
    if (xact.cost)
      xact.cost->negate();
  }

  if (! (transaction_xdata(xact).dflags & TRANSACTION_NO_TOTAL))
    add_transaction_to(xact, transaction_xdata(xact).total);

  (*handler)(xact);

  if (inverted) {
    xact.amount.negate();
    if (xact.cost)
      xact.cost->negate();
  }

  last_xact = &xact;
}


static void handle_value(const value_t&		       value,
			 account_t *		       account,
			 entry_t *		       entry,
			 unsigned int		       flags,
			 std::list<transaction_t>&     temps,
			 item_handler<transaction_t> * handler)
{
  balance_t * bal = NULL;

  switch (value.type) {
  case value_t::BOOLEAN:
  case value_t::INTEGER:
  case value_t::AMOUNT: {
    temps.push_back(transaction_t(account));
    transaction_t& xact = temps.back();

    xact.entry = entry;
    switch (value.type) {
    case value_t::BOOLEAN:
      xact.amount  = *((bool *) value.data);
      break;
    case value_t::INTEGER:
      xact.amount  = *((unsigned int *) value.data);
      break;
    case value_t::AMOUNT:
      xact.amount  = *((amount_t *) value.data);
      break;
    default:
      assert(0);
      break;
    }

    if (flags)
      transaction_xdata(xact).dflags |= flags;

    (*handler)(xact);
    break;
  }

  case value_t::BALANCE_PAIR:
    bal = &((balance_pair_t *) value.data)->quantity;
    // fall through...

  case value_t::BALANCE:
    if (! bal)
      bal = (balance_t *) value.data;

    for (amounts_map::const_iterator i = bal->amounts.begin();
	 i != bal->amounts.end();
	 i++) {
      temps.push_back(transaction_t(account));
      transaction_t& xact = temps.back();

      xact.entry  = entry;
      xact.amount = (*i).second;

      if (flags)
	transaction_xdata(xact).dflags |= flags;

      (*handler)(xact);
    }
    break;

  default:
    assert(0);
    break;
  }
}

void collapse_transactions::report_cumulative_subtotal()
{
  if (count == 1) {
    (*handler)(*last_xact);
  } else {
    assert(count > 1);

    account_xdata_t xdata;
    xdata.total = subtotal;
    value_t result;
    totals_account.data = &xdata;
    format_t::compute_total(result, details_t(totals_account));
    handle_value(result, &totals_account, last_entry, 0, xact_temps, handler);
  }

  subtotal = 0;
  count    = 0;
}

void changed_value_transactions::output_diff(const std::time_t current)
{
  value_t     prev_bal;
  value_t     cur_bal;
  std::time_t prev_date = last_xact->entry->date;

  format_t::compute_total(prev_bal, details_t(*last_xact));

  last_xact->entry->date = current;
  format_t::compute_total(cur_bal,  details_t(*last_xact));
  last_xact->entry->date = prev_date;

  cur_bal -= prev_bal;
  if (cur_bal) {
    entry_temps.push_back(entry_t());
    entry_t& entry = entry_temps.back();

    entry.payee = "Commodities revalued";
    entry.date  = current;

    handle_value(cur_bal, NULL, &entry, TRANSACTION_NO_TOTAL, xact_temps,
		 handler);
  }
}

void changed_value_transactions::operator()(transaction_t& xact)
{
  if (last_xact)
    output_diff(xact.entry->date);

  if (changed_values_only)
    transaction_xdata(xact).dflags |= TRANSACTION_DISPLAYED;

  (*handler)(xact);

  last_xact = &xact;
}

void subtotal_transactions::flush(const char * spec_fmt)
{
  char buf[256];

  if (! spec_fmt) {
    std::string fmt = "- ";
    fmt += format_t::date_format;

    // Make sure the end date is inclusive
    if (start != finish)
      finish -= 86400;

    std::strftime(buf, 255, fmt.c_str(), std::localtime(&finish));
  } else {
    std::strftime(buf, 255, spec_fmt, std::localtime(&finish));
  }

  entry_temps.push_back(entry_t());
  entry_t& entry = entry_temps.back();

  entry.payee = buf;

  value_t result;

  for (balances_map::iterator i = balances.begin();
       i != balances.end();
       i++) {
    entry.date = finish;
    {
      transaction_xdata_t xact_data;
      xact_data.total = (*i).second;
      transaction_t temp((*i).first);
      temp.entry = &entry;
      temp.data = &xact_data;
      format_t::compute_total(result, details_t(temp));
    }
    entry.date = start;

    handle_value(result, (*i).first, &entry, 0, xact_temps, handler);
  }

  balances.clear();

  item_handler<transaction_t>::flush();
}

void subtotal_transactions::operator()(transaction_t& xact)
{
  if (balances.size() == 0) {
    start = finish = xact.entry->date;
  } else {
    if (std::difftime(xact.entry->date, start) < 0)
      start = xact.entry->date;
    if (std::difftime(xact.entry->date, finish) > 0)
      finish = xact.entry->date;
  }

  balances_map::iterator i = balances.find(xact.account);
  if (i == balances.end()) {
    balance_pair_t temp;
    add_transaction_to(xact, temp);
    balances.insert(balances_pair(xact.account, temp));
  } else {
    add_transaction_to(xact, (*i).second);
  }
}

void interval_transactions::operator()(transaction_t& xact)
{
  if ((interval.begin &&
       std::difftime(xact.entry->date, interval.begin) < 0) ||
      (interval.end &&
       std::difftime(xact.entry->date, interval.end) >= 0))
    return;

  std::time_t quant = interval.increment(interval.begin);
  if (std::difftime(xact.entry->date, quant) > 0) {
    if (last_xact) {
      start  = interval.begin;
      finish = quant;
      flush();
    }

    if (! interval.seconds) {
      struct std::tm * desc = std::localtime(&xact.entry->date);
      if (interval.years)
	desc->tm_mon = 0;
      desc->tm_mday = 1;
      desc->tm_hour = 0;
      desc->tm_min  = 0;
      desc->tm_sec  = 0;
      quant = std::mktime(desc);
    }

    std::time_t temp;
#if DEBUG_LEVEL >= RELEASE
    int cutoff = 10000;
#endif
    while (std::difftime(xact.entry->date,
			 temp = interval.increment(quant)) > 0) {
      if (quant == temp)
	break;
      quant = temp;
#if DEBUG_LEVEL >= RELEASE
      assert(--cutoff > 0);
#endif
    }
    interval.begin = quant;
  }

  subtotal_transactions::operator()(xact);

  last_xact = &xact;
}

void dow_transactions::flush()
{
  for (int i = 0; i < 7; i++) {
    for (transactions_list::iterator d = days_of_the_week[i].begin();
	 d != days_of_the_week[i].end();
	 d++)
      subtotal_transactions::operator()(**d);
    subtotal_transactions::flush("%As");
    days_of_the_week[i].clear();
  }
}

void sum_accounts(account_t& account)
{
  for (accounts_map::iterator i = account.accounts.begin();
       i != account.accounts.end();
       i++) {
    sum_accounts(*(*i).second);
    account_xdata(account).total += account_xdata(*(*i).second).total;
    account_xdata(account).count += (account_xdata(*(*i).second).count +
				     account_xdata(*(*i).second).subcount);
  }
  account_xdata(account).total += account_xdata(account).value;
  account_xdata(account).count += account_xdata(account).subcount;
}

void sort_accounts(account_t&		account,
		   const value_expr_t * sort_order,
		   accounts_deque&      accounts)
{
  for (accounts_map::iterator i = account.accounts.begin();
       i != account.accounts.end();
       i++)
    accounts.push_back((*i).second);

  std::stable_sort(accounts.begin(), accounts.end(),
		   compare_items<account_t>(sort_order));
}

void walk_accounts(account_t&		    account,
		   item_handler<account_t>& handler,
		   const value_expr_t *     sort_order)
{
  handler(account);

  if (sort_order) {
    accounts_deque accounts;
    sort_accounts(account, sort_order, accounts);
    for (accounts_deque::const_iterator i = accounts.begin();
	 i != accounts.end();
	 i++)
      walk_accounts(**i, handler, sort_order);
  } else {
    for (accounts_map::const_iterator i = account.accounts.begin();
	 i != account.accounts.end();
	 i++)
      walk_accounts(*(*i).second, handler, NULL);
  }
}

void walk_accounts(account_t&		    account,
		   item_handler<account_t>& handler,
		   const std::string&       sort_string)
{
  if (! sort_string.empty()) {
    std::auto_ptr<value_expr_t> sort_order(parse_value_expr(sort_string));
    if (! sort_order.get())
      throw error(std::string("Sort string failed to parse: " + sort_string));
    walk_accounts(account, handler, sort_order.get());
  } else {
    walk_accounts(account, handler);
  }
}

void clear_accounts_xdata()
{
  accounts_xdata.clear();

  for (std::list<void **>::iterator i = accounts_xdata_ptrs.begin();
       i != accounts_xdata_ptrs.end();
       i++)
    **i = NULL;
}

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>

using namespace boost::python;
using namespace ledger;

template <typename T>
struct item_handler_wrap : public item_handler<T>
{
  PyObject* self;

  item_handler_wrap(PyObject * self_) : self(self_) {}
  item_handler_wrap(PyObject * self_, const item_handler<T>& handler)
    : item_handler<T>(const_cast<item_handler<T> *>(&handler)), self(self_) {}

  virtual void flush() {
    call_method<void>(self, "flush");
  }
  void default_flush() {
    item_handler<T>::flush();
  }

  virtual void operator()(T& item) {
    call_method<void>(self, "__call__", ptr(&item));
  }
  void default_call(T& item) {
    item_handler<T>::operator()(item);
  }
};

void (subtotal_transactions::*subtotal_transactions_flush)() =
  &subtotal_transactions::flush;

void py_walk_entries(journal_t& journal, item_handler<transaction_t>& handler)
{
  walk_entries(journal.entries, handler);
}

void py_walk_transactions(entry_t& entry, item_handler<transaction_t>& handler)
{
  walk_transactions(entry.transactions, handler);
}

void py_walk_accounts_1(account_t&		 account,
			item_handler<account_t>& handler)
{
  walk_accounts(account, handler);
}

void py_walk_accounts_2(account_t&		 account,
			item_handler<account_t>& handler,
			const value_expr_t *     sort_order)
{
  walk_accounts(account, handler, sort_order);
}

void py_walk_accounts_3(account_t&		 account,
			item_handler<account_t>& handler,
			const std::string&       sort_string)
{
  walk_accounts(account, handler, sort_string);
}

void export_walk()
{
  typedef item_handler<transaction_t> xact_handler_t;

  scope().attr("TRANSACTION_HANDLED")	= TRANSACTION_HANDLED;
  scope().attr("TRANSACTION_DISPLAYED") = TRANSACTION_DISPLAYED;
  scope().attr("TRANSACTION_NO_TOTAL")	= TRANSACTION_NO_TOTAL;

  class_< transaction_xdata_t > ("TransactionXData")
    .def_readwrite("total", &transaction_xdata_t::total)
    .def_readwrite("index", &transaction_xdata_t::index)
    .def_readwrite("dflags", &transaction_xdata_t::dflags)
    ;

  def("transaction_has_xdata", transaction_has_xdata);
  def("transaction_xdata", transaction_xdata, return_internal_reference<1>());

  class_< xact_handler_t, item_handler_wrap<transaction_t> >
    ("TransactionHandler")
    .def(init<xact_handler_t *>())

    .def("flush", &xact_handler_t::flush,
	 &item_handler_wrap<transaction_t>::default_flush)
    .def("__call__", &xact_handler_t::operator(),
	 &item_handler_wrap<transaction_t>::default_call)
    ;

  class_< ignore_transactions, bases<xact_handler_t> >
    ("IgnoreTransactions")
    .def("flush", &xact_handler_t::flush)
    .def("__call__", &ignore_transactions::operator());
    ;

  class_< set_account_value, bases<xact_handler_t> > ("SetAccountValue")
    .def(init<xact_handler_t *>()[with_custodian_and_ward<1, 2>()])
    .def("flush", &xact_handler_t::flush)
    .def("__call__", &set_account_value::operator());
    ;

  class_< sort_transactions, bases<xact_handler_t> >
    ("SortTransactions", init<xact_handler_t *, const value_expr_t *>()
     [with_custodian_and_ward<1, 2>()])
    .def(init<xact_handler_t *, const std::string&>()
	 [with_custodian_and_ward<1, 2>()])
    .def("flush", &sort_transactions::flush)
    .def("__call__", &sort_transactions::operator());
    ;

  class_< filter_transactions, bases<xact_handler_t> >
    ("FilterTransactions", init<xact_handler_t *, std::string>()
     [with_custodian_and_ward<1, 2>()])
    .def(init<xact_handler_t *, const std::string&>()
	 [with_custodian_and_ward<1, 2>()])
    .def("flush", &xact_handler_t::flush)
    .def("__call__", &filter_transactions::operator());
    ;

  class_< calc_transactions, bases<xact_handler_t> >
    ("CalcTransactions", init<xact_handler_t *, optional<bool> >()
     [with_custodian_and_ward<1, 2>()])
    .def("flush", &xact_handler_t::flush)
    .def("__call__", &calc_transactions::operator());
    ;

  class_< collapse_transactions, bases<xact_handler_t> >
    ("CollapseTransactions", init<xact_handler_t *>()
     [with_custodian_and_ward<1, 2>()])
    .def("flush", &collapse_transactions::flush)
    .def("__call__", &collapse_transactions::operator());
    ;

  class_< changed_value_transactions, bases<xact_handler_t> >
    ("ChangeValueTransactions", init<xact_handler_t *, bool>()
     [with_custodian_and_ward<1, 2>()])
    .def("flush", &changed_value_transactions::flush)
    .def("__call__", &changed_value_transactions::operator());
    ;

  class_< subtotal_transactions, bases<xact_handler_t> >
    ("SubtotalTransactions", init<xact_handler_t *>()
     [with_custodian_and_ward<1, 2>()])
    .def("flush", subtotal_transactions_flush)
    .def("__call__", &subtotal_transactions::operator());
    ;

  class_< interval_transactions, bases<xact_handler_t> >
    ("IntervalTransactions", init<xact_handler_t *, interval_t>()
     [with_custodian_and_ward<1, 2>()])
    .def(init<xact_handler_t *, const std::string&>()
	 [with_custodian_and_ward<1, 2>()])
    .def("flush", &xact_handler_t::flush)
    .def("__call__", &interval_transactions::operator());
    ;

  class_< dow_transactions, bases<xact_handler_t> >
    ("DowTransactions", init<xact_handler_t *>()
     [with_custodian_and_ward<1, 2>()])
    .def("flush", &dow_transactions::flush)
    .def("__call__", &dow_transactions::operator());
    ;

  class_< related_transactions, bases<xact_handler_t> >
    ("RelatedTransactions", init<xact_handler_t *, optional<bool> >()
     [with_custodian_and_ward<1, 2>()])
    .def("flush", &xact_handler_t::flush)
    .def("__call__", &related_transactions::operator());
    ;

  def("walk_entries", py_walk_entries);
  def("walk_transactions", py_walk_transactions);

  typedef item_handler<account_t> account_handler_t;

  scope().attr("ACCOUNT_DISPLAYED")  = ACCOUNT_DISPLAYED;
  scope().attr("ACCOUNT_TO_DISPLAY") = ACCOUNT_TO_DISPLAY;

  class_< account_xdata_t > ("AccountXData")
    .def_readwrite("value", &account_xdata_t::value)
    .def_readwrite("total", &account_xdata_t::total)
    .def_readwrite("count", &account_xdata_t::count)
    .def_readwrite("subcount", &account_xdata_t::subcount)
    .def_readwrite("dflags", &account_xdata_t::dflags)
    ;

  def("account_has_xdata", account_has_xdata);
  def("account_xdata", account_xdata, return_internal_reference<1>());

  class_< account_handler_t, item_handler_wrap<account_t> > ("AccountHandler")
    .def(init<account_handler_t *>())

    .def("flush", &account_handler_t::flush,
	 &item_handler_wrap<account_t>::default_flush)
    .def("__call__", &account_handler_t::operator(),
	 &item_handler_wrap<account_t>::default_call)
    ;

  def("sum_accounts", sum_accounts);
  def("walk_accounts", py_walk_accounts_1);
  def("walk_accounts", py_walk_accounts_2);
  def("walk_accounts", py_walk_accounts_3);
}

#endif // USE_BOOST_PYTHON
