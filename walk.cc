#include "walk.h"
#include "format.h"
#include "textual.h"
#include "util.h"

#include <algorithm>

namespace ledger {

std::list<transaction_xdata_t> transactions_xdata;
std::list<void **>	       transactions_xdata_ptrs;

std::list<account_xdata_t>     accounts_xdata;
std::list<void **>	       accounts_xdata_ptrs;

template <>
bool compare_items<transaction_t>::operator()(const transaction_t * left,
					      const transaction_t * right)
{
  assert(left);
  assert(right);

  transaction_xdata_t& lxdata(transaction_xdata(*left));
  if (! (lxdata.dflags & TRANSACTION_SORT_CALC)) {
    sort_order->compute(lxdata.sort_value, details_t(*left));
    lxdata.dflags |= TRANSACTION_SORT_CALC;
  }

  transaction_xdata_t& rxdata(transaction_xdata(*right));
  if (! (rxdata.dflags & TRANSACTION_SORT_CALC)) {
    sort_order->compute(rxdata.sort_value, details_t(*right));
    rxdata.dflags |= TRANSACTION_SORT_CALC;
  }

  return lxdata.sort_value < rxdata.sort_value;
}

transaction_xdata_t& transaction_xdata(const transaction_t& xact)
{
  if (! xact.data) {
    transactions_xdata.push_back(transaction_xdata_t());
    xact.data = &transactions_xdata.back();
    transactions_xdata_ptrs.push_back(&xact.data);
  }
  return *((transaction_xdata_t *) xact.data);
}

void add_transaction_to(const transaction_t& xact, value_t& value)
{
  if (transaction_has_xdata(xact) &&
      transaction_xdata_(xact).dflags & TRANSACTION_COMPOSITE)
    value += transaction_xdata_(xact).composite_amount;
  else if (xact.cost || value)
    value.add(xact.amount, xact.cost);
  else
    value = xact.amount;
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

void invert_transactions::operator()(transaction_t& xact)
{
  if (transaction_has_xdata(xact) &&
      transaction_xdata_(xact).dflags & TRANSACTION_COMPOSITE) {
    transaction_xdata_(xact).composite_amount.negate();
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
		  const std::time_t            date = 0)
{
  temps.push_back(transaction_t(account));
  transaction_t& xact(temps.back());
  xact.entry = entry;
  xact.flags |= TRANSACTION_BULK_ALLOC;
  entry->add_transaction(&xact);

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

  switch (value.type) {
  case value_t::BOOLEAN:
    xact.amount = *((bool *) value.data);
    break;
  case value_t::INTEGER:
    xact.amount = *((long *) value.data);
    break;
  case value_t::AMOUNT:
    xact.amount = *((amount_t *) value.data);
    break;

  case value_t::BALANCE:
  case value_t::BALANCE_PAIR:
    xdata.composite_amount = value;
    flags |= TRANSACTION_COMPOSITE;
    break;
  }

  if (date)
    xdata.date = date;
  if (flags)
    xdata.dflags |= flags;

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
    entry.date  = last_entry->date;

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

void changed_value_transactions::output_diff(const std::time_t current)
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
    entry.date  = current;

    handle_value(diff, NULL, &entry, TRANSACTION_NO_TOTAL, xact_temps,
		 *handler);
  }
}

void changed_value_transactions::operator()(transaction_t& xact)
{
  if (last_xact) {
    std::time_t moment = 0;
    if (transaction_has_xdata(*last_xact))
      moment = transaction_xdata_(*last_xact).date;
    if (! moment)
      moment = xact.entry->date;
    output_diff(moment);
  }

  if (changed_values_only)
    transaction_xdata(xact).dflags |= TRANSACTION_DISPLAYED;

  item_handler<transaction_t>::operator()(xact);

  compute_total(last_balance, details_t(xact));
  last_balance.round();

  last_xact = &xact;
}

void subtotal_transactions::report_subtotal(const char * spec_fmt)
{
  char buf[256];

  if (! spec_fmt) {
    std::string fmt = "- ";
    fmt += format_t::date_format;
    std::strftime(buf, 255, fmt.c_str(), std::localtime(&finish));
  } else {
    std::strftime(buf, 255, spec_fmt, std::localtime(&finish));
  }

  entry_temps.push_back(entry_t());
  entry_t& entry = entry_temps.back();
  entry.payee = buf;
  entry.date  = start;

  for (values_map::iterator i = values.begin();
       i != values.end();
       i++)
    handle_value((*i).second.value, (*i).second.account, &entry, 0,
		 xact_temps, *handler, finish);

  values.clear();
}

void subtotal_transactions::operator()(transaction_t& xact)
{
  if (! start || std::difftime(xact.entry->date, start) < 0)
    start = xact.entry->date;
  if (! finish || std::difftime(xact.entry->date, finish) > 0)
    finish = xact.entry->date;

  account_t * acct = xact.account;
  assert(acct);

  values_map::iterator i = values.find(acct->fullname());
  if (i == values.end()) {
    value_t temp;
    add_transaction_to(xact, temp);
    values.insert(values_pair(acct->fullname(), acct_value_t(acct, temp)));
  } else {
    add_transaction_to(xact, (*i).second.value);
  }

  // If the account for this transaction is all virtual, mark it as
  // such, so that `handle_value' can show "(Account)" for accounts
  // that contain only virtual transactions.

  if (! (xact.flags & TRANSACTION_VIRTUAL))
    account_xdata(*xact.account).dflags |= ACCOUNT_HAS_NON_VIRTUALS;
  else if (! (xact.flags & TRANSACTION_BALANCE))
    account_xdata(*xact.account).dflags |= ACCOUNT_HAS_UNB_VIRTUALS;
}

void interval_transactions::report_subtotal(const std::time_t moment)
{
  assert(last_xact);

  start = interval.begin;
  if (moment)
    finish = moment - 86400;
  else
    finish = last_xact->entry->date;

  subtotal_transactions::report_subtotal();

  if (sorter)
    sorter->post_accumulated_xacts();

  last_xact = NULL;
}

void interval_transactions::operator()(transaction_t& xact)
{
  const std::time_t date = xact.entry->date;

  if ((interval.begin && std::difftime(date, interval.begin) < 0) ||
      (interval.end   && std::difftime(date, interval.end) >= 0))
    return;

  if (interval) {
    if (! started) {
      if (! interval.begin)
	interval.start(date);
      start   = interval.begin;
      started = true;
    }

    std::time_t quant = interval.increment(interval.begin);
    if (std::difftime(date, quant) >= 0) {
      if (last_xact)
	report_subtotal(quant);

      std::time_t temp;
      while (std::difftime(date, temp = interval.increment(quant)) >= 0) {
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
    payee_subtotals_pair temp(xact.entry->payee,
			      new subtotal_transactions(handler));
    std::pair<payee_subtotals_map::iterator, bool> result
      = payee_subtotals.insert(temp);

    assert(result.second);
    if (! result.second)
      return;
    i = result.first;
  }

  if (std::difftime(xact.entry->date, (*i).second->start) > 0)
    (*i).second->start = xact.entry->date;

  (*(*i).second)(xact);
}

void set_comm_as_payee::operator()(transaction_t& xact)
{
  entry_temps.push_back(*xact.entry);
  entry_t& entry = entry_temps.back();
  entry.date  = xact.entry->date;
  entry.state = xact.entry->state;
  entry.code  = xact.entry->code;
  entry.payee = xact.amount.commodity().symbol;

  xact_temps.push_back(xact);
  transaction_t& temp = xact_temps.back();
  temp.entry = &entry;
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

void budget_transactions::report_budget_items(const std::time_t moment)
{
  if (pending_xacts.size() == 0)
    return;

  bool reported;
  do {
    reported = false;
    for (pending_xacts_list::iterator i = pending_xacts.begin();
	 i != pending_xacts.end();
	 i++) {
      std::time_t& begin = (*i).first.begin;
      if (! begin) {
	(*i).first.start(moment);
	begin = (*i).first.begin;
      }

      if (std::difftime(begin, moment) < 0 &&
	  (! (*i).first.end || std::difftime(begin, (*i).first.end) < 0)) {
	transaction_t& xact  = *(*i).second;

	DEBUG_PRINT("ledger.walk.budget", "Reporting budget for "
		    << xact.account->fullname());
	DEBUG_PRINT_TIME("ledger.walk.budget", begin);
	DEBUG_PRINT_TIME("ledger.walk.budget", moment);

	entry_temps.push_back(entry_t());
	entry_t& entry = entry_temps.back();
	entry.payee = "Budget entry";
	entry.date  = begin;

	xact_temps.push_back(xact);
	transaction_t& temp = xact_temps.back();
	temp.entry = &entry;
	temp.flags |= TRANSACTION_AUTO;
	temp.amount.negate();
	temp.flags |= TRANSACTION_BULK_ALLOC;
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
    for (account_t * acct = xact.account; acct; acct = acct->parent) {
      if (acct == (*i).second->account) {
	xact_in_budget = true;

	// Report the transaction as if it had occurred in the parent
	// account.  jww (2005-07-13): Note that this assignment will
	// irrevocably change the underlying transaction.
	if (xact.account != acct)
	  xact.account = acct;
	goto handle;
      }
    }

 handle:
  if (xact_in_budget && flags & BUDGET_BUDGETED) {
    report_budget_items(xact.entry->date);
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
    i.start(now);
    i.begin = i.increment(i.begin);
  } else {
    while (std::difftime(i.begin, now) < 0)
      i.begin = i.increment(i.begin);
  }
}

void forecast_transactions::flush()
{
  transactions_list passed;
  std::time_t       last = 0;

  while (pending_xacts.size() > 0) {
    pending_xacts_list::iterator least = pending_xacts.begin();
    for (pending_xacts_list::iterator i = ++pending_xacts.begin();
	 i != pending_xacts.end();
	 i++)
      if (std::difftime((*i).first.begin, (*least).first.begin) < 0)
	least = i;

    std::time_t& begin = (*least).first.begin;

    if ((*least).first.end &&
	std::difftime(begin, (*least).first.end) >= 0) {
      pending_xacts.erase(least);
      passed.remove((*least).second);
      continue;
    }

    transaction_t& xact = *(*least).second;

    entry_temps.push_back(entry_t());
    entry_t& entry = entry_temps.back();
    entry.payee = "Forecast entry";
    entry.date  = begin;

    xact_temps.push_back(xact);
    transaction_t& temp = xact_temps.back();
    temp.entry = &entry;
    temp.flags |= TRANSACTION_AUTO;
    temp.flags |= TRANSACTION_BULK_ALLOC;
    entry.add_transaction(&temp);

    std::time_t next = (*least).first.increment(begin);
    if (std::difftime(next, begin) < 0 || // wraparound
	(last && std::difftime(next, last) > 5 * 365 * 24 * 60 * 60))
      break;
    begin = next;

    item_handler<transaction_t>::operator()(temp);

    if (transaction_has_xdata(temp) &&
	transaction_xdata_(temp).dflags & TRANSACTION_MATCHES) {
      if (! pred(temp))
	break;
      last = temp.entry->date;
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

void clear_transactions_xdata()
{
  transactions_xdata.clear();

  for (std::list<void **>::iterator i = transactions_xdata_ptrs.begin();
       i != transactions_xdata_ptrs.end();
       i++)
    **i = NULL;
  transactions_xdata_ptrs.clear();
}

template <>
bool compare_items<account_t>::operator()(const account_t * left,
					  const account_t * right)
{
  assert(left);
  assert(right);

  account_xdata_t& lxdata(account_xdata(*left));
  if (! (lxdata.dflags & ACCOUNT_SORT_CALC)) {
    sort_order->compute(lxdata.sort_value, details_t(*left));
    lxdata.dflags |= ACCOUNT_SORT_CALC;
  }

  account_xdata_t& rxdata(account_xdata(*right));
  if (! (rxdata.dflags & ACCOUNT_SORT_CALC)) {
    sort_order->compute(rxdata.sort_value, details_t(*right));
    rxdata.dflags |= ACCOUNT_SORT_CALC;
  }

  return lxdata.sort_value < rxdata.sort_value;
}

account_xdata_t& account_xdata(const account_t& account)
{
  if (! account.data) {
    accounts_xdata.push_back(account_xdata_t());
    account.data = &accounts_xdata.back();
    accounts_xdata_ptrs.push_back(&account.data);
  }
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
  if (result)
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
    std::auto_ptr<value_expr_t> sort_order;
    try {
      sort_order.reset(parse_value_expr(sort_string));
    }
    catch (value_expr_error& err) {
      throw error(std::string("In sort string '" + sort_string + "': " +
			      err.what()));
    }
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
  accounts_xdata_ptrs.clear();
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
    if ((*i).second->flags & COMMODITY_STYLE_NOMARKET)
      continue;

    entry_temps.push_back(entry_t());
    acct_temps.push_back(account_t(NULL, (*i).second->symbol));

    if ((*i).second->history)
      for (history_map::iterator j = (*i).second->history->prices.begin();
	   j != (*i).second->history->prices.end();
	   j++) {
	entry_temps.back().date = (*j).first;

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

void (subtotal_transactions::*subtotal_transactions_flush)() =
  &subtotal_transactions::flush;

void py_walk_entries(journal_t& journal,
		     item_handler<transaction_t>& handler) {
  walk_entries(journal.entries, handler);
}

void py_walk_transactions(entry_t& entry,
			  item_handler<transaction_t>& handler) {
  walk_transactions(entry.transactions, handler);
}

void py_walk_accounts_1(account_t&		 account,
			item_handler<account_t>& handler) {
  walk_accounts(account, handler);
}

void py_walk_accounts_2(account_t&		 account,
			item_handler<account_t>& handler,
			const value_expr_t *     sort_order) {
  walk_accounts(account, handler, sort_order);
}

void py_walk_accounts_3(account_t&		 account,
			item_handler<account_t>& handler,
			const std::string&       sort_string) {
  walk_accounts(account, handler, sort_string);
}

void py_walk_commodities(item_handler<transaction_t>& handler) {
  walk_commodities(commodity_t::commodities, handler);
}

void py_add_period_entries(generate_transactions& handler,
			   journal_t * journal) {
  handler.add_period_entries(journal->period_entries);
}

void export_walk()
{
  typedef item_handler<transaction_t> xact_handler_t;

  scope().attr("TRANSACTION_RECEIVED")	 = TRANSACTION_RECEIVED;
  scope().attr("TRANSACTION_HANDLED")	 = TRANSACTION_HANDLED;
  scope().attr("TRANSACTION_TO_DISPLAY") = TRANSACTION_TO_DISPLAY;
  scope().attr("TRANSACTION_DISPLAYED")  = TRANSACTION_DISPLAYED;
  scope().attr("TRANSACTION_NO_TOTAL")	 = TRANSACTION_NO_TOTAL;
  scope().attr("TRANSACTION_SORT_CALC")	 = TRANSACTION_SORT_CALC;
  scope().attr("TRANSACTION_COMPOSITE")	 = TRANSACTION_COMPOSITE;
  scope().attr("TRANSACTION_MATCHES")	 = TRANSACTION_MATCHES;

  class_< transaction_xdata_t > ("TransactionXData")
    .def_readwrite("total", &transaction_xdata_t::total)
    .def_readwrite("sort_value", &transaction_xdata_t::sort_value)
    .def_readwrite("composite_amount", &transaction_xdata_t::composite_amount)
    .def_readwrite("index", &transaction_xdata_t::index)
    .def_readwrite("dflags", &transaction_xdata_t::dflags)
    ;

  def("transaction_has_xdata", transaction_has_xdata);
  def("transaction_xdata", transaction_xdata, return_internal_reference<1>());
  def("clear_transactions_xdata", clear_transactions_xdata);
  def("add_transaction_to", add_transaction_to);

  class_< xact_handler_t, item_handler_wrap<transaction_t> >
    ("TransactionHandler")
    .def(init<xact_handler_t *>()[with_custodian_and_ward<1, 2>()])

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

  class_< truncate_entries, bases<xact_handler_t> >
    ("TruncateEntries", init<xact_handler_t *, int, int>()
     [with_custodian_and_ward<1, 2>()])
    .def("flush", &truncate_entries::flush)
    .def("__call__", &truncate_entries::operator());
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
    ("CalcTransactions", init<xact_handler_t *>()
     [with_custodian_and_ward<1, 2>()])
    .def("flush", &xact_handler_t::flush)
    .def("__call__", &calc_transactions::operator());
    ;

  class_< invert_transactions, bases<xact_handler_t> >
    ("InvertTransactions", init<xact_handler_t *>()
     [with_custodian_and_ward<1, 2>()])
    .def("flush", &xact_handler_t::flush)
    .def("__call__", &invert_transactions::operator());
    ;

  class_< collapse_transactions, bases<xact_handler_t> >
    ("CollapseTransactions", init<xact_handler_t *>()
     [with_custodian_and_ward<1, 2>()])
    .def("flush", &collapse_transactions::flush)
    .def("__call__", &collapse_transactions::operator());
    ;

  class_< related_transactions, bases<xact_handler_t> >
    ("RelatedTransactions", init<xact_handler_t *, optional<bool> >()
     [with_custodian_and_ward<1, 2>()])
    .def("flush", &xact_handler_t::flush)
    .def("__call__", &related_transactions::operator());
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
    ("IntervalTransactions",
     init<xact_handler_t *, interval_t, optional<value_expr_t *> >()
     [with_custodian_and_ward<1, 2>()])
    .def(init<xact_handler_t *, const std::string&,
	 optional<const std::string&> >()
	 [with_custodian_and_ward<1, 2>()])
    .def("flush", &xact_handler_t::flush)
    .def("__call__", &interval_transactions::operator());
    ;

  class_< by_payee_transactions, bases<xact_handler_t> >
    ("ByPayeeTransactions", init<xact_handler_t *>()
     [with_custodian_and_ward<1, 2>()])
    .def("flush", &by_payee_transactions::flush)
    .def("__call__", &by_payee_transactions::operator());
    ;

  class_< set_comm_as_payee, bases<xact_handler_t> >
    ("SetCommAsPayee", init<xact_handler_t *>()
     [with_custodian_and_ward<1, 2>()])
    .def("flush", &xact_handler_t::flush)
    .def("__call__", &xact_handler_t::operator());
    ;

  class_< dow_transactions, bases<xact_handler_t> >
    ("DowTransactions", init<xact_handler_t *>()
     [with_custodian_and_ward<1, 2>()])
    .def("flush", &dow_transactions::flush)
    .def("__call__", &dow_transactions::operator());
    ;

  scope().attr("BUDGET_BUDGETED")   = BUDGET_BUDGETED;
  scope().attr("BUDGET_UNBUDGETED") = BUDGET_UNBUDGETED;

  class_< generate_transactions, bases<xact_handler_t> >
    ("GenerateTransactions", init<xact_handler_t *>()
     [with_custodian_and_ward<1, 2>()])
    .def("add_transaction", &generate_transactions::add_transaction)
    .def("add_period_entries", py_add_period_entries)
    .def("flush", &xact_handler_t::flush)
    .def("__call__", &xact_handler_t::operator());
    ;

  class_< budget_transactions, bases<generate_transactions> >
    ("BudgetTransactions",
     init<xact_handler_t *, unsigned long>()
     [with_custodian_and_ward<1, 2>()])
    .def("add_transaction", &generate_transactions::add_transaction)
    .def("add_period_entries", py_add_period_entries)
    .def("flush", &budget_transactions::flush)
    .def("__call__", &xact_handler_t::operator());
    ;

  class_< forecast_transactions, bases<generate_transactions> >
    ("ForecastTransactions",
     init<xact_handler_t *, std::string>()
     [with_custodian_and_ward<1, 2>()])
    .def("add_transaction", &forecast_transactions::add_transaction)
    .def("add_period_entries", py_add_period_entries)
    .def("flush", &forecast_transactions::flush)
    .def("__call__", &xact_handler_t::operator());
    ;

  def("walk_entries", py_walk_entries);
  def("walk_transactions", py_walk_transactions);

  typedef item_handler<account_t> account_handler_t;

  scope().attr("ACCOUNT_TO_DISPLAY") = ACCOUNT_TO_DISPLAY;
  scope().attr("ACCOUNT_DISPLAYED")  = ACCOUNT_DISPLAYED;
  scope().attr("ACCOUNT_SORT_CALC")  = ACCOUNT_SORT_CALC;

  class_< account_xdata_t > ("AccountXData")
    .def_readwrite("value", &account_xdata_t::value)
    .def_readwrite("total", &account_xdata_t::total)
    .def_readwrite("sort_value", &account_xdata_t::sort_value)
    .def_readwrite("count", &account_xdata_t::count)
    .def_readwrite("total_count", &account_xdata_t::total_count)
    .def_readwrite("virtuals", &account_xdata_t::virtuals)
    .def_readwrite("dflags", &account_xdata_t::dflags)
    ;

  def("account_has_xdata", account_has_xdata);
  def("account_xdata", account_xdata, return_internal_reference<1>());
  def("clear_accounts_xdata", clear_accounts_xdata);
  def("clear_all_xdata", clear_all_xdata);

  class_< account_handler_t, item_handler_wrap<account_t> >
    ("AccountHandler")
    .def(init<account_handler_t *>()[with_custodian_and_ward<1, 2>()])

    .def("flush", &account_handler_t::flush,
	 &item_handler_wrap<account_t>::default_flush)
    .def("__call__", &account_handler_t::operator(),
	 &item_handler_wrap<account_t>::default_call)
    ;

  def("sum_accounts", sum_accounts);
  def("walk_accounts", py_walk_accounts_1);
  def("walk_accounts", py_walk_accounts_2);
  def("walk_accounts", py_walk_accounts_3);

  def("walk_commodities", py_walk_commodities);
}

#endif // USE_BOOST_PYTHON
