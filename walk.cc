#if 0
#include "walk.h"
#include "format.h"
#include "textual.h"
#include "util.h"

#include <algorithm>

namespace ledger {

template <>
bool compare_items<transaction_t>::operator()(const transaction_t * left,
					      const transaction_t * right)
{
  assert(left);
  assert(right);

  transaction_xdata_t& lxdata(transaction_xdata(*left));
  if (! (lxdata.dflags & TRANSACTION_SORT_CALC)) {
    guarded_compute(sort_order, lxdata.sort_value, details_t(*left));
    lxdata.sort_value.reduce();
    lxdata.dflags |= TRANSACTION_SORT_CALC;
  }

  transaction_xdata_t& rxdata(transaction_xdata(*right));
  if (! (rxdata.dflags & TRANSACTION_SORT_CALC)) {
    guarded_compute(sort_order, rxdata.sort_value, details_t(*right));
    rxdata.sort_value.reduce();
    rxdata.dflags |= TRANSACTION_SORT_CALC;
  }

  return lxdata.sort_value < rxdata.sort_value;
}

transaction_xdata_t& transaction_xdata(const transaction_t& xact)
{
  if (! xact.data)
    xact.data = new transaction_xdata_t();
  return *((transaction_xdata_t *) xact.data);
}

void truncate_entries::flush()
{
  if (! xacts.size())
    return;

  entry_t * last_entry = (*xacts.begin())->entry;

  int l = 0;
  for (transactions_list::iterator x = xacts.begin();
       x != xacts.end();
       x++)
    if (last_entry != (*x)->entry) {
      l++;
      last_entry = (*x)->entry;
    }
  l++;

  last_entry = (*xacts.begin())->entry;

  int i = 0;
  for (transactions_list::iterator x = xacts.begin();
       x != xacts.end();
       x++) {
    if (last_entry != (*x)->entry) {
      last_entry = (*x)->entry;
      i++;
    }

    bool print = false;
    if (head_count) {
      if (head_count > 0 && i < head_count)
	print = true;
      else if (head_count < 0 && i >= - head_count)
	print = true;
    }

    if (! print && tail_count) {
      if (tail_count > 0 && l - i <= tail_count)
	print = true;
      else if (tail_count < 0 && l - i > - tail_count)
	print = true;
    }

    if (print)
      item_handler<transaction_t>::operator()(**x);
  }
  xacts.clear();

  item_handler<transaction_t>::flush();
}

void set_account_value::operator()(transaction_t& xact)
{
  account_t * acct = xact_account(xact);
  assert(acct);

  account_xdata_t& xdata = account_xdata(*acct);
  add_transaction_to(xact, xdata.value);

  xdata.count++;
  if (xact.flags & TRANSACTION_VIRTUAL)
    xdata.virtuals++;

  item_handler<transaction_t>::operator()(xact);
}

void sort_transactions::post_accumulated_xacts()
{
  std::stable_sort(transactions.begin(), transactions.end(),
		   compare_items<transaction_t>(sort_order));

  for (transactions_deque::iterator i = transactions.begin();
       i != transactions.end();
       i++) {
    transaction_xdata(**i).dflags &= ~TRANSACTION_SORT_CALC;
    item_handler<transaction_t>::operator()(**i);
  }

  transactions.clear();
}

void calc_transactions::operator()(transaction_t& xact)
{
  try {

  transaction_xdata_t& xdata(transaction_xdata(xact));

  if (last_xact && transaction_has_xdata(*last_xact)) {
    xdata.total += transaction_xdata_(*last_xact).total;
    xdata.index  = transaction_xdata_(*last_xact).index + 1;
  } else {
    xdata.index = 0;
  }

  if (! (xdata.dflags & TRANSACTION_NO_TOTAL))
    add_transaction_to(xact, xdata.total);

  item_handler<transaction_t>::operator()(xact);

  last_xact = &xact;

  }
  catch (error * err) {
    err->context.push_front
      (new xact_context(xact, "Calculating transaction at"));
    throw err;
  }
}

void invert_transactions::operator()(transaction_t& xact)
{
  if (transaction_has_xdata(xact) &&
      transaction_xdata_(xact).dflags & TRANSACTION_COMPOUND) {
    transaction_xdata_(xact).value.negate();
  } else {
    xact.amount.negate();
    if (xact.cost)
      xact.cost->negate();
  }

  item_handler<transaction_t>::operator()(xact);
}


static inline
void handle_value(const value_t&	       value,
		  account_t *		       account,
		  entry_t *		       entry,
		  unsigned int		       flags,
		  std::list<transaction_t>&    temps,
		  item_handler<transaction_t>& handler,
		  const datetime_t&            date = datetime_t(),
		  transactions_list *          component_xacts = NULL)
{
  temps.push_back(transaction_t(account));
  transaction_t& xact(temps.back());
  xact.entry = entry;
  xact.flags |= TRANSACTION_BULK_ALLOC;
  entry->add_transaction(&xact);

  // If there are component transactions to associate with this
  // temporary, do so now.

  if (component_xacts)
    transaction_xdata(xact).copy_component_xacts(*component_xacts);

  // If the account for this transaction is all virtual, then report
  // the transaction as such.  This allows subtotal reports to show
  // "(Account)" for accounts that contain only virtual transactions.

  if (account && account_has_xdata(*account))
    if (! (account_xdata_(*account).dflags & ACCOUNT_HAS_NON_VIRTUALS)) {
      xact.flags |= TRANSACTION_VIRTUAL;
      if (! (account_xdata_(*account).dflags & ACCOUNT_HAS_UNB_VIRTUALS))
	xact.flags |= TRANSACTION_BALANCE;
    }

  transaction_xdata_t& xdata(transaction_xdata(xact));

  if (date)
    xdata.date = date;
  if (flags)
    xdata.dflags |= flags;

  value_t temp(value);

  switch (value.type) {
  case value_t::BOOLEAN:
  case value_t::DATETIME:
  case value_t::INTEGER:
    temp.cast(value_t::AMOUNT);
    // fall through...

  case value_t::AMOUNT:
    xact.amount = *((amount_t *) temp.data);
    break;

  case value_t::BALANCE:
  case value_t::BALANCE_PAIR:
    xdata.value = temp;
    flags |= TRANSACTION_COMPOUND;
    break;
  }

  handler(xact);
}

void collapse_transactions::report_subtotal()
{
  assert(count >= 1);

  if (count == 1) {
    item_handler<transaction_t>::operator()(*last_xact);
  } else {
    entry_temps.push_back(entry_t());
    entry_t& entry = entry_temps.back();
    entry.payee = last_entry->payee;
    entry._date = last_entry->_date;

    handle_value(subtotal, &totals_account, last_entry, 0, xact_temps,
		 *handler);
  }

  last_entry = NULL;
  last_xact  = NULL;
  subtotal   = 0L;
  count      = 0;
}

void collapse_transactions::operator()(transaction_t& xact)
{
  // If we've reached a new entry, report on the subtotal
  // accumulated thus far.

  if (last_entry && last_entry != xact.entry && count > 0)
    report_subtotal();

  add_transaction_to(xact, subtotal);
  count++;

  last_entry = xact.entry;
  last_xact  = &xact;
}

void related_transactions::flush()
{
  if (transactions.size() > 0) {
    for (transactions_list::iterator i = transactions.begin();
	 i != transactions.end();
	 i++) {
      if ((*i)->entry) {
	for (transactions_list::iterator j = (*i)->entry->transactions.begin();
	     j != (*i)->entry->transactions.end();
	     j++) {
	  transaction_xdata_t& xdata = transaction_xdata(**j);
	  if (! (xdata.dflags & TRANSACTION_HANDLED) &&
	      (! (xdata.dflags & TRANSACTION_RECEIVED) ?
	       ! ((*j)->flags & (TRANSACTION_AUTO | TRANSACTION_VIRTUAL)) :
	       also_matching)) {
	    xdata.dflags |= TRANSACTION_HANDLED;
	    item_handler<transaction_t>::operator()(**j);
	  }
	}
      } else {
	// This code should only be reachable from the "output"
	// command, since that is the only command which attempts to
	// output auto or period entries.
	transaction_xdata_t& xdata = transaction_xdata(**i);
	if (! (xdata.dflags & TRANSACTION_HANDLED) &&
	    ! ((*i)->flags & TRANSACTION_AUTO)) {
	  xdata.dflags |= TRANSACTION_HANDLED;
	  item_handler<transaction_t>::operator()(**i);
	}
      }
    }
  }

  item_handler<transaction_t>::flush();
}

void changed_value_transactions::output_diff(const datetime_t& current)
{
  value_t cur_bal;

  transaction_xdata(*last_xact).date = current;
  compute_total(cur_bal, details_t(*last_xact));
  cur_bal.round();
  transaction_xdata(*last_xact).date = 0;

  if (value_t diff = cur_bal - last_balance) {
    entry_temps.push_back(entry_t());
    entry_t& entry = entry_temps.back();
    entry.payee = "Commodities revalued";
    entry._date = current;

    handle_value(diff, NULL, &entry, TRANSACTION_NO_TOTAL, xact_temps,
		 *handler);
  }
}

void changed_value_transactions::operator()(transaction_t& xact)
{
  if (last_xact) {
    datetime_t moment;
    if (transaction_has_xdata(*last_xact))
      moment = transaction_xdata_(*last_xact).date;
    else
      moment = xact.date();
    output_diff(moment);
  }

  if (changed_values_only)
    transaction_xdata(xact).dflags |= TRANSACTION_DISPLAYED;

  item_handler<transaction_t>::operator()(xact);

  compute_total(last_balance, details_t(xact));
  last_balance.round();

  last_xact = &xact;
}

void component_transactions::operator()(transaction_t& xact)
{
  if (handler && pred(xact)) {
    if (transaction_has_xdata(xact) &&
	transaction_xdata_(xact).have_component_xacts())
      transaction_xdata_(xact).walk_component_xacts(*handler);
    else
      (*handler)(xact);
  }
}

void subtotal_transactions::report_subtotal(const char * spec_fmt)
{
  std::ostringstream out_date;
  if (! spec_fmt) {
    std::string fmt = "- ";
    fmt += date_t::output_format;
    finish.write(out_date, fmt);
  } else {
    finish.write(out_date, spec_fmt);
  }

  entry_temps.push_back(entry_t());
  entry_t& entry = entry_temps.back();
  entry.payee = out_date.str();
  entry._date = start;

  for (values_map::iterator i = values.begin();
       i != values.end();
       i++)
    handle_value((*i).second.value, (*i).second.account, &entry, 0,
		 xact_temps, *handler, finish, &(*i).second.components);

  values.clear();
}

void subtotal_transactions::operator()(transaction_t& xact)
{
  if (! start || xact.date() < start)
    start = xact.date();
  if (! finish || xact.date() > finish)
    finish = xact.date();

  account_t * acct = xact_account(xact);
  assert(acct);

  values_map::iterator i = values.find(acct->fullname());
  if (i == values.end()) {
    value_t temp;
    add_transaction_to(xact, temp);
    std::pair<values_map::iterator, bool> result
      = values.insert(values_pair(acct->fullname(),
				  acct_value_t(acct, temp)));
    assert(result.second);

    if (remember_components)
      (*result.first).second.components.push_back(&xact);
  } else {
    add_transaction_to(xact, (*i).second.value);

    if (remember_components)
      (*i).second.components.push_back(&xact);
  }

  // If the account for this transaction is all virtual, mark it as
  // such, so that `handle_value' can show "(Account)" for accounts
  // that contain only virtual transactions.

  if (! (xact.flags & TRANSACTION_VIRTUAL))
    account_xdata(*xact_account(xact)).dflags |= ACCOUNT_HAS_NON_VIRTUALS;
  else if (! (xact.flags & TRANSACTION_BALANCE))
    account_xdata(*xact_account(xact)).dflags |= ACCOUNT_HAS_UNB_VIRTUALS;
}

void interval_transactions::report_subtotal(const datetime_t& moment)
{
  assert(last_xact);

  start = interval.begin;
  if (moment)
    finish = moment - 86400L;
  else
    finish = last_xact->date();

  subtotal_transactions::report_subtotal();

  last_xact = NULL;
}

void interval_transactions::operator()(transaction_t& xact)
{
  const datetime_t date = xact.date();

  if ((interval.begin && date < interval.begin) ||
      (interval.end   && date >= interval.end))
    return;

  if (interval) {
    if (! started) {
      if (! interval.begin)
	interval.start(date);
      start   = interval.begin;
      started = true;
    }

    datetime_t quant = interval.increment(interval.begin);
    if (date >= quant) {
      if (last_xact)
	report_subtotal(quant);

      datetime_t temp;
      while (date >= (temp = interval.increment(quant))) {
	if (quant == temp)
	  break;
	quant = temp;
      }
      start = interval.begin = quant;
    }

    subtotal_transactions::operator()(xact);
  } else {
    item_handler<transaction_t>::operator()(xact);
  }

  last_xact = &xact;
}

by_payee_transactions::~by_payee_transactions()
{
  for (payee_subtotals_map::iterator i = payee_subtotals.begin();
       i != payee_subtotals.end();
       i++)
    delete (*i).second;
}

void by_payee_transactions::flush()
{
  for (payee_subtotals_map::iterator i = payee_subtotals.begin();
       i != payee_subtotals.end();
       i++)
    (*i).second->report_subtotal((*i).first.c_str());

  item_handler<transaction_t>::flush();

  payee_subtotals.clear();
}

void by_payee_transactions::operator()(transaction_t& xact)
{
  payee_subtotals_map::iterator i = payee_subtotals.find(xact.entry->payee);
  if (i == payee_subtotals.end()) {
    payee_subtotals_pair
      temp(xact.entry->payee,
	   new subtotal_transactions(handler, remember_components));
    std::pair<payee_subtotals_map::iterator, bool> result
      = payee_subtotals.insert(temp);

    assert(result.second);
    if (! result.second)
      return;
    i = result.first;
  }

  if (xact.date() > (*i).second->start)
    (*i).second->start = xact.date();

  (*(*i).second)(xact);
}

void set_comm_as_payee::operator()(transaction_t& xact)
{
  entry_temps.push_back(*xact.entry);
  entry_t& entry = entry_temps.back();
  entry._date = xact.date();
  entry.code  = xact.entry->code;

  if (xact.amount.commodity())
    entry.payee = xact.amount.commodity().symbol();
  else
    entry.payee = "<none>";

  xact_temps.push_back(xact);
  transaction_t& temp = xact_temps.back();
  temp.entry = &entry;
  temp.state = xact.state;
  temp.flags |= TRANSACTION_BULK_ALLOC;

  entry.add_transaction(&temp);

  item_handler<transaction_t>::operator()(temp);
}

void set_code_as_payee::operator()(transaction_t& xact)
{
  entry_temps.push_back(*xact.entry);
  entry_t& entry = entry_temps.back();
  entry._date = xact.date();

  if (! xact.entry->code.empty())
    entry.payee = xact.entry->code;
  else
    entry.payee = "<none>";

  xact_temps.push_back(xact);
  transaction_t& temp = xact_temps.back();
  temp.entry = &entry;
  temp.state = xact.state;
  temp.flags |= TRANSACTION_BULK_ALLOC;

  entry.add_transaction(&temp);

  item_handler<transaction_t>::operator()(temp);
}

void dow_transactions::flush()
{
  for (int i = 0; i < 7; i++) {
    start = finish = 0;
    for (transactions_list::iterator d = days_of_the_week[i].begin();
	 d != days_of_the_week[i].end();
	 d++)
      subtotal_transactions::operator()(**d);
    subtotal_transactions::report_subtotal("%As");
    days_of_the_week[i].clear();
  }

  subtotal_transactions::flush();
}

void generate_transactions::add_period_entries
  (period_entries_list& period_entries)
{
  for (period_entries_list::iterator i = period_entries.begin();
       i != period_entries.end();
       i++)
    for (transactions_list::iterator j = (*i)->transactions.begin();
	 j != (*i)->transactions.end();
	 j++)
      add_transaction((*i)->period, **j);
}

void generate_transactions::add_transaction(const interval_t& period,
					    transaction_t& xact)
{
  pending_xacts.push_back(pending_xacts_pair(period, &xact));
}

void budget_transactions::report_budget_items(const datetime_t& moment)
{
  if (pending_xacts.size() == 0)
    return;

  bool reported;
  do {
    reported = false;
    for (pending_xacts_list::iterator i = pending_xacts.begin();
	 i != pending_xacts.end();
	 i++) {
      datetime_t& begin = (*i).first.begin;
      if (! begin) {
	(*i).first.start(moment);
	begin = (*i).first.begin;
      }

      if (begin < moment &&
	  (! (*i).first.end || begin < (*i).first.end)) {
	transaction_t& xact = *(*i).second;

	DEBUG_PRINT("ledger.walk.budget", "Reporting budget for "
		    << xact_account(xact)->fullname());
	DEBUG_PRINT_TIME("ledger.walk.budget", begin);
	DEBUG_PRINT_TIME("ledger.walk.budget", moment);

	entry_temps.push_back(entry_t());
	entry_t& entry = entry_temps.back();
	entry.payee = "Budget entry";
	entry._date = begin;

	xact_temps.push_back(xact);
	transaction_t& temp = xact_temps.back();
	temp.entry   = &entry;
	temp.flags  |= TRANSACTION_AUTO | TRANSACTION_BULK_ALLOC;
	temp.amount.negate();
	entry.add_transaction(&temp);

	begin = (*i).first.increment(begin);

	item_handler<transaction_t>::operator()(temp);

	reported = true;
      }
    }
  } while (reported);
}

void budget_transactions::operator()(transaction_t& xact)
{
  bool xact_in_budget = false;

  for (pending_xacts_list::iterator i = pending_xacts.begin();
       i != pending_xacts.end();
       i++)
    for (account_t * acct = xact_account(xact);
	 acct;
	 acct = acct->parent) {
      if (acct == xact_account(*(*i).second)) {
	xact_in_budget = true;
	// Report the transaction as if it had occurred in the parent
	// account.
	if (xact_account(xact) != acct)
	  transaction_xdata(xact).account = acct;
	goto handle;
      }
    }

 handle:
  if (xact_in_budget && flags & BUDGET_BUDGETED) {
    report_budget_items(xact.date());
    item_handler<transaction_t>::operator()(xact);
  }
  else if (! xact_in_budget && flags & BUDGET_UNBUDGETED) {
    item_handler<transaction_t>::operator()(xact);
  }
}

void forecast_transactions::add_transaction(const interval_t& period,
					    transaction_t&    xact)
{
  generate_transactions::add_transaction(period, xact);

  interval_t& i = pending_xacts.back().first;
  if (! i.begin) {
    i.start(datetime_t::now);
    i.begin = i.increment(i.begin);
  } else {
    while (i.begin < datetime_t::now)
      i.begin = i.increment(i.begin);
  }
}

void forecast_transactions::flush()
{
  transactions_list passed;
  datetime_t last;

  while (pending_xacts.size() > 0) {
    pending_xacts_list::iterator least = pending_xacts.begin();
    for (pending_xacts_list::iterator i = ++pending_xacts.begin();
	 i != pending_xacts.end();
	 i++)
      if ((*i).first.begin < (*least).first.begin)
	least = i;

    datetime_t& begin = (*least).first.begin;

    if ((*least).first.end && begin >= (*least).first.end) {
      pending_xacts.erase(least);
      passed.remove((*least).second);
      continue;
    }

    transaction_t& xact = *(*least).second;

    entry_temps.push_back(entry_t());
    entry_t& entry = entry_temps.back();
    entry.payee = "Forecast entry";
    entry._date = begin;

    xact_temps.push_back(xact);
    transaction_t& temp = xact_temps.back();
    temp.entry = &entry;
    temp.flags |= TRANSACTION_AUTO;
    temp.flags |= TRANSACTION_BULK_ALLOC;
    entry.add_transaction(&temp);

    datetime_t next = (*least).first.increment(begin);
    if (next < begin || // wraparound
	(last && (next - last) > 365 * 5 * 24 * 3600))
      break;
    begin = next;

    item_handler<transaction_t>::operator()(temp);

    if (transaction_has_xdata(temp) &&
	transaction_xdata_(temp).dflags & TRANSACTION_MATCHES) {
      if (! pred(temp))
	break;
      last = temp.date();
      passed.clear();
    } else {
      bool found = false;
      for (transactions_list::iterator i = passed.begin();
	   i != passed.end();
	   i++)
	if (*i == &xact) {
	  found = true;
	  break;
	}

      if (! found) {
	passed.push_back(&xact);
	if (passed.size() >= pending_xacts.size())
	  break;
      }
    }
  }

  item_handler<transaction_t>::flush();
}

template <>
bool compare_items<account_t>::operator()(const account_t * left,
					  const account_t * right)
{
  assert(left);
  assert(right);

  account_xdata_t& lxdata(account_xdata(*left));
  if (! (lxdata.dflags & ACCOUNT_SORT_CALC)) {
    guarded_compute(sort_order, lxdata.sort_value, details_t(*left));
    lxdata.dflags |= ACCOUNT_SORT_CALC;
  }

  account_xdata_t& rxdata(account_xdata(*right));
  if (! (rxdata.dflags & ACCOUNT_SORT_CALC)) {
    guarded_compute(sort_order, rxdata.sort_value, details_t(*right));
    rxdata.dflags |= ACCOUNT_SORT_CALC;
  }

  return lxdata.sort_value < rxdata.sort_value;
}

account_xdata_t& account_xdata(const account_t& account)
{
  if (! account.data)
    account.data = new account_xdata_t();

  return *((account_xdata_t *) account.data);
}

void sum_accounts(account_t& account)
{
  account_xdata_t& xdata(account_xdata(account));

  for (accounts_map::iterator i = account.accounts.begin();
       i != account.accounts.end();
       i++) {
    sum_accounts(*(*i).second);

    xdata.total += account_xdata_(*(*i).second).total;
    xdata.total_count += (account_xdata_(*(*i).second).total_count +
			  account_xdata_(*(*i).second).count);
  }

  value_t result;
  compute_amount(result, details_t(account));
  if (! result.realzero())
    xdata.total += result;
  xdata.total_count += xdata.count;
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
	 i++) {
      account_xdata(**i).dflags &= ~ACCOUNT_SORT_CALC;
      walk_accounts(**i, handler, sort_order);
    }
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
    value_expr sort_order;
    sort_order.reset(parse_value_expr(sort_string));
    walk_accounts(account, handler, sort_order.get());
  } else {
    walk_accounts(account, handler);
  }
}

void walk_commodities(commodities_map& commodities,
		      item_handler<transaction_t>& handler)
{
  std::list<transaction_t> xact_temps;
  std::list<entry_t>       entry_temps;
  std::list<account_t>     acct_temps;

  for (commodities_map::iterator i = commodities.begin();
       i != commodities.end();
       i++) {
    if ((*i).second->flags() & COMMODITY_STYLE_NOMARKET)
      continue;

    entry_temps.push_back(entry_t());
    acct_temps.push_back(account_t(NULL, (*i).second->symbol()));

    if ((*i).second->history())
      for (history_map::iterator j = (*i).second->history()->prices.begin();
	   j != (*i).second->history()->prices.end();
	   j++) {
	entry_temps.back()._date = (*j).first;

	xact_temps.push_back(transaction_t(&acct_temps.back()));
	transaction_t& temp = xact_temps.back();
	temp.entry  = &entry_temps.back();
	temp.amount = (*j).second;
	temp.flags |= TRANSACTION_BULK_ALLOC;
	entry_temps.back().add_transaction(&temp);

	handler(xact_temps.back());
      }
  }

  handler.flush();

  clear_entries_transactions(entry_temps);
}

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>

using namespace boost::python;
using namespace ledger;

template <typename T>
struct item_handler_wrap : public item_handler<T>
{
  PyObject * self;

  item_handler_wrap(PyObject * self_) : self(self_) {}
  item_handler_wrap(PyObject * self_, const item_handler<T>& _handler)
    : item_handler<T>(const_cast<item_handler<T> *>(&_handler)),
      self(self_) {}
  item_handler_wrap(PyObject * self_, item_handler<T> * _handler)
    : item_handler<T>(_handler), self(self_) {}

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

#endif
#endif
