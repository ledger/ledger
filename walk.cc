#include "walk.h"
#include "session.h"
#include "format.h"
#include "textual.h"

#include <algorithm>

namespace ledger {

template <>
bool compare_items<xact_t>::operator()(const xact_t * left,
					      const xact_t * right)
{
  assert(left);
  assert(right);

#if 0
  xact_xdata_t& lxdata(xact_xdata(*left));
  if (! (lxdata.dflags & XACT_SORT_CALC)) {
    sort_order.compute(lxdata.sort_value, details_t(*left));
    lxdata.sort_value.reduce();
    lxdata.dflags |= XACT_SORT_CALC;
  }

  xact_xdata_t& rxdata(xact_xdata(*right));
  if (! (rxdata.dflags & XACT_SORT_CALC)) {
    sort_order.compute(rxdata.sort_value, details_t(*right));
    rxdata.sort_value.reduce();
    rxdata.dflags |= XACT_SORT_CALC;
  }

  DEBUG("ledger.walk.compare_items_xact",
	"lxdata.sort_value = " << lxdata.sort_value);
  DEBUG("ledger.walk.compare_items_xact",
	"rxdata.sort_value = " << rxdata.sort_value);

  return lxdata.sort_value < rxdata.sort_value;
#else
  return false;
#endif
}

xact_xdata_t& xact_xdata(const xact_t& xact)
{
  if (! xact.data)
    xact.data = new xact_xdata_t();
  return *static_cast<xact_xdata_t *>(xact.data);
}

void add_xact_to(const xact_t& xact, value_t& value)
{
  if (xact_has_xdata(xact) &&
      xact_xdata_(xact).dflags & XACT_COMPOUND) {
    value += xact_xdata_(xact).value;
  }
  else if (xact.cost || (! value.is_null() && ! value.is_realzero())) {
    value.add(xact.amount, xact.cost);
  }
  else {
    value = xact.amount;
  }
}

void entries_iterator::reset(session_t& session)
{
  journals_i   = session.journals.begin();
  journals_end = session.journals.end();

  journals_uninitialized = false;

  if (journals_i != journals_end) {
    entries_i   = (*journals_i).entries.begin();
    entries_end = (*journals_i).entries.end();

    entries_uninitialized = false;
  } else {
    entries_uninitialized = true;
  }
}

entry_t * entries_iterator::operator()()
{
  if (entries_i == entries_end) {
    journals_i++;
    if (journals_i == journals_end)
      return NULL;

    entries_i	= (*journals_i).entries.begin();
    entries_end = (*journals_i).entries.end();
  }
  return *entries_i++;
}

void session_xacts_iterator::reset(session_t& session)
{
  entries.reset(session);
  entry_t * entry = entries();
  if (entry != NULL)
    xacts.reset(*entry);
}

xact_t * session_xacts_iterator::operator()()
{
  xact_t * xact = xacts();
  if (xact == NULL) {
    entry_t * entry = entries();
    if (entry != NULL) {
      xacts.reset(*entry);
      xact = xacts();
    }
  }
  return xact;
}

void truncate_entries::flush()
{
  if (! xacts.size())
    return;

  entry_t * last_entry = (*xacts.begin())->entry;

  int l = 0;
  foreach (xact_t * xact, xacts)
    if (last_entry != xact->entry) {
      l++;
      last_entry = xact->entry;
    }
  l++;

  last_entry = (*xacts.begin())->entry;

  int i = 0;
  foreach (xact_t * xact, xacts) {
    if (last_entry != xact->entry) {
      last_entry = xact->entry;
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
      item_handler<xact_t>::operator()(*xact);
  }
  xacts.clear();

  item_handler<xact_t>::flush();
}

void set_account_value::operator()(xact_t& xact)
{
  account_t * acct = xact_account(xact);
  assert(acct);

  account_xdata_t& xdata = account_xdata(*acct);
  add_xact_to(xact, xdata.value);

  xdata.count++;
  if (xact.has_flags(XACT_VIRTUAL))
    xdata.virtuals++;

  item_handler<xact_t>::operator()(xact);
}

void sort_xacts::post_accumulated_xacts()
{
  std::stable_sort(xacts.begin(), xacts.end(),
		   compare_items<xact_t>(sort_order));

  foreach (xact_t * xact, xacts) {
    xact_xdata(*xact).dflags &= ~XACT_SORT_CALC;
    item_handler<xact_t>::operator()(*xact);
  }

  xacts.clear();
}

void calc_xacts::operator()(xact_t& xact)
{
  try {

  xact_xdata_t& xdata(xact_xdata(xact));

  if (last_xact && xact_has_xdata(*last_xact)) {
    if (xdata.total.is_null())
      xdata.total = xact_xdata_(*last_xact).total;
    else
      xdata.total += xact_xdata_(*last_xact).total;
    xdata.index  = xact_xdata_(*last_xact).index + 1;
  } else {
    xdata.index = 0;
  }

  if (! (xdata.dflags & XACT_NO_TOTAL))
    add_xact_to(xact, xdata.total);

  item_handler<xact_t>::operator()(xact);

  last_xact = &xact;

  }
  catch (const std::exception& err) {
    add_error_context("Calculating transaction at");
#if 0
    add_error_context(xact_context(xact));
#endif
    throw err;
  }
}

void invert_xacts::operator()(xact_t& xact)
{
  if (xact_has_xdata(xact) &&
      xact_xdata_(xact).dflags & XACT_COMPOUND) {
    xact_xdata_(xact).value.negate();
  } else {
    xact.amount.negate();
    if (xact.cost)
      xact.cost->negate();
  }

  item_handler<xact_t>::operator()(xact);
}


static inline
void handle_value(const value_t&	value,
		  account_t *		account,
		  entry_t *		entry,
		  unsigned int		flags,
		  std::list<xact_t>&    temps,
		  item_handler<xact_t>& handler,
		  const date_t&         date = date_t(),
		  xacts_list *          component_xacts = NULL)
{
  temps.push_back(xact_t(account));
  xact_t& xact(temps.back());
  xact.entry = entry;
  xact.add_flags(XACT_TEMP);
  entry->add_xact(&xact);

  // If there are component xacts to associate with this
  // temporary, do so now.

  if (component_xacts)
    xact_xdata(xact).copy_component_xacts(*component_xacts);

  // If the account for this xact is all virtual, then report
  // the xact as such.  This allows subtotal reports to show
  // "(Account)" for accounts that contain only virtual xacts.

  if (account && account_has_xdata(*account))
    if (! (account_xdata_(*account).dflags & ACCOUNT_HAS_NON_VIRTUALS)) {
      xact.add_flags(XACT_VIRTUAL);
      if (! (account_xdata_(*account).dflags & ACCOUNT_HAS_UNB_VIRTUALS))
	xact.add_flags(XACT_BALANCE);
    }

  xact_xdata_t& xdata(xact_xdata(xact));

  if (is_valid(date))
    xdata.date = date;

  value_t temp(value);

  switch (value.type()) {
  case value_t::BOOLEAN:
  case value_t::DATETIME:
  case value_t::DATE:
  case value_t::INTEGER:
    temp.cast(value_t::AMOUNT);
    // fall through...

  case value_t::AMOUNT:
    xact.amount = temp.as_amount();
    break;

  case value_t::BALANCE:
  case value_t::BALANCE_PAIR:
    xdata.value = temp;
    flags |= XACT_COMPOUND;
    break;

  default:
    assert(false);		// jww (2008-04-24): What to do here?
    break;
  }

  if (flags)
    xdata.dflags |= flags;

  handler(xact);
}

void collapse_xacts::report_subtotal()
{
  assert(count >= 1);

  if (count == 1) {
    item_handler<xact_t>::operator()(*last_xact);
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

void collapse_xacts::operator()(xact_t& xact)
{
  // If we've reached a new entry, report on the subtotal
  // accumulated thus far.

  if (last_entry && last_entry != xact.entry && count > 0)
    report_subtotal();

  add_xact_to(xact, subtotal);
  count++;

  last_entry = xact.entry;
  last_xact  = &xact;
}

void related_xacts::flush()
{
  if (xacts.size() > 0) {
    foreach (xact_t * xact, xacts) {
      if (xact->entry) {
	foreach (xact_t * r_xact, xact->entry->xacts) {
	  xact_xdata_t& xdata = xact_xdata(*r_xact);
	  if (! (xdata.dflags & XACT_HANDLED) &&
	      (! (xdata.dflags & XACT_RECEIVED) ?
	       ! r_xact->has_flags(XACT_AUTO | XACT_VIRTUAL) :
	       also_matching)) {
	    xdata.dflags |= XACT_HANDLED;
	    item_handler<xact_t>::operator()(*r_xact);
	  }
	}
      } else {
	// This code should only be reachable from the "output"
	// command, since that is the only command which attempts to
	// output auto or period entries.
	xact_xdata_t& xdata = xact_xdata(*xact);
	if (! (xdata.dflags & XACT_HANDLED) &&
	    ! xact->has_flags(XACT_AUTO)) {
	  xdata.dflags |= XACT_HANDLED;
	  item_handler<xact_t>::operator()(*xact);
	}
      }
    }
  }

  item_handler<xact_t>::flush();
}

void changed_value_xacts::output_diff(const date_t& date)
{
  value_t cur_bal;

  xact_xdata(*last_xact).date = date;
#if 0
  compute_total(cur_bal, details_t(*last_xact));
#endif
  cur_bal.round();
  // jww (2008-04-24): What does this do?
#if 0
  xact_xdata(*last_xact).date = 0;
#endif

  if (value_t diff = cur_bal - last_balance) {
    entry_temps.push_back(entry_t());
    entry_t& entry = entry_temps.back();
    entry.payee = "Commodities revalued";
    entry._date = date;

    handle_value(diff, NULL, &entry, XACT_NO_TOTAL, xact_temps,
		 *handler);
  }
}

void changed_value_xacts::operator()(xact_t& xact)
{
  if (last_xact) {
    date_t date;
    if (xact_has_xdata(*last_xact))
      date = xact_xdata_(*last_xact).date;
    else
      date = xact.date();
    output_diff(date);
  }

  if (changed_values_only)
    xact_xdata(xact).dflags |= XACT_DISPLAYED;

  item_handler<xact_t>::operator()(xact);

#if 0
  compute_total(last_balance, details_t(xact));
#endif
  last_balance.round();

  last_xact = &xact;
}

void component_xacts::operator()(xact_t& xact)
{
  if (handler && pred(xact)) {
    if (xact_has_xdata(xact) &&
	xact_xdata_(xact).have_component_xacts())
      xact_xdata_(xact).walk_component_xacts(*handler);
    else
      (*handler)(xact);
  }
}

void subtotal_xacts::report_subtotal(const char * spec_fmt)
{
  std::ostringstream out_date;
  if (! spec_fmt) {
    string fmt = "- ";
    fmt += output_date_format;
    out_date << format_date(finish, string(fmt));
  } else {
    out_date << format_date(finish, string(spec_fmt));
  }

  entry_temps.push_back(entry_t());
  entry_t& entry = entry_temps.back();
  entry.payee = out_date.str();
  entry._date = start;

  foreach (values_map::value_type& pair, values)
    handle_value(pair.second.value, pair.second.account, &entry, 0,
		 xact_temps, *handler, finish, &pair.second.components);

  values.clear();
}

void subtotal_xacts::operator()(xact_t& xact)
{
  if (! is_valid(start) || xact.date() < start)
    start = xact.date();
  if (! is_valid(finish) || xact.date() > finish)
    finish = xact.date();

  account_t * acct = xact_account(xact);
  assert(acct);

  values_map::iterator i = values.find(acct->fullname());
  if (i == values.end()) {
    value_t temp;
    add_xact_to(xact, temp);
    std::pair<values_map::iterator, bool> result
      = values.insert(values_pair(acct->fullname(), acct_value_t(acct, temp)));
    assert(result.second);

    if (remember_components)
      (*result.first).second.components.push_back(&xact);
  } else {
    add_xact_to(xact, (*i).second.value);

    if (remember_components)
      (*i).second.components.push_back(&xact);
  }

  // If the account for this xact is all virtual, mark it as
  // such, so that `handle_value' can show "(Account)" for accounts
  // that contain only virtual xacts.

  if (! xact.has_flags(XACT_VIRTUAL))
    account_xdata(*xact_account(xact)).dflags |= ACCOUNT_HAS_NON_VIRTUALS;
  else if (! xact.has_flags(XACT_BALANCE))
    account_xdata(*xact_account(xact)).dflags |= ACCOUNT_HAS_UNB_VIRTUALS;
}

void interval_xacts::report_subtotal(const date_t& date)
{
  assert(last_xact);

  start = interval.begin;
  if (is_valid(date))
    finish = date - gregorian::days(1);
  else
    finish = last_xact->date();

  subtotal_xacts::report_subtotal();

  last_xact = NULL;
}

void interval_xacts::operator()(xact_t& xact)
{
  const date_t& date(xact.date());

  if ((is_valid(interval.begin) && date < interval.begin) ||
      (is_valid(interval.end)   && date >= interval.end))
    return;

  if (interval) {
    if (! started) {
      if (! is_valid(interval.begin))
	interval.start(date);
      start   = interval.begin;
      started = true;
    }

    date_t quant = interval.increment(interval.begin);
    if (date >= quant) {
      if (last_xact)
	report_subtotal(quant);

      date_t temp;
      while (date >= (temp = interval.increment(quant))) {
	if (quant == temp)
	  break;
	quant = temp;
      }
      start = interval.begin = quant;
    }

    subtotal_xacts::operator()(xact);
  } else {
    item_handler<xact_t>::operator()(xact);
  }

  last_xact = &xact;
}

by_payee_xacts::~by_payee_xacts()
{
  TRACE_DTOR(by_payee_xacts);

  foreach (payee_subtotals_map::value_type& pair, payee_subtotals)
    checked_delete(pair.second);
}

void by_payee_xacts::flush()
{
  foreach (payee_subtotals_map::value_type& pair, payee_subtotals)
    pair.second->report_subtotal(pair.first.c_str());

  item_handler<xact_t>::flush();

  payee_subtotals.clear();
}

void by_payee_xacts::operator()(xact_t& xact)
{
  payee_subtotals_map::iterator i = payee_subtotals.find(xact.entry->payee);
  if (i == payee_subtotals.end()) {
    payee_subtotals_pair
      temp(xact.entry->payee,
	   new subtotal_xacts(handler, remember_components));
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

void set_comm_as_payee::operator()(xact_t& xact)
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
  xact_t& temp = xact_temps.back();
  temp.entry = &entry;
  temp.state = xact.state;
  temp.add_flags(XACT_TEMP);

  entry.add_xact(&temp);

  item_handler<xact_t>::operator()(temp);
}

void set_code_as_payee::operator()(xact_t& xact)
{
  entry_temps.push_back(*xact.entry);
  entry_t& entry = entry_temps.back();
  entry._date = xact.date();

  if (xact.entry->code)
    entry.payee = *xact.entry->code;
  else
    entry.payee = "<none>";

  xact_temps.push_back(xact);
  xact_t& temp = xact_temps.back();
  temp.entry = &entry;
  temp.state = xact.state;
  temp.add_flags(XACT_TEMP);

  entry.add_xact(&temp);

  item_handler<xact_t>::operator()(temp);
}

void dow_xacts::flush()
{
  for (int i = 0; i < 7; i++) {
    // jww (2008-04-24): What to use here?
#if 0
    start = finish = 0;
#endif
    foreach (xact_t * xact, days_of_the_week[i])
      subtotal_xacts::operator()(*xact);
    subtotal_xacts::report_subtotal("%As");
    days_of_the_week[i].clear();
  }

  subtotal_xacts::flush();
}

void generate_xacts::add_period_entries
  (period_entries_list& period_entries)
{
  foreach (period_entry_t * entry, period_entries)
    foreach (xact_t * xact, entry->xacts)
      add_xact(entry->period, *xact);
}

void generate_xacts::add_xact(const interval_t& period,
					    xact_t& xact)
{
  pending_xacts.push_back(pending_xacts_pair(period, &xact));
}

void budget_xacts::report_budget_items(const date_t& date)
{
  if (pending_xacts.size() == 0)
    return;

  bool reported;
  do {
    reported = false;
    foreach (pending_xacts_list::value_type& pair, pending_xacts) {
      date_t& begin = pair.first.begin;
      if (! is_valid(begin)) {
	pair.first.start(date);
	begin = pair.first.begin;
      }

      if (begin < date &&
	  (! is_valid(pair.first.end) || begin < pair.first.end)) {
	xact_t& xact = *pair.second;

	DEBUG("ledger.walk.budget", "Reporting budget for "
		    << xact_account(xact)->fullname());
#if 0
	// jww (2008-04-24): Need a new debug macro here
	DEBUG_TIME("ledger.walk.budget", begin);
	DEBUG_TIME("ledger.walk.budget", date);
#endif

	entry_temps.push_back(entry_t());
	entry_t& entry = entry_temps.back();
	entry.payee = "Budget entry";
	entry._date = begin;

	xact_temps.push_back(xact);
	xact_t& temp = xact_temps.back();
	temp.entry = &entry;
	temp.add_flags(XACT_AUTO | XACT_TEMP);
	temp.amount.negate();
	entry.add_xact(&temp);

	begin = pair.first.increment(begin);

	item_handler<xact_t>::operator()(temp);

	reported = true;
      }
    }
  } while (reported);
}

void budget_xacts::operator()(xact_t& xact)
{
  bool xact_in_budget = false;

  foreach (pending_xacts_list::value_type& pair, pending_xacts)
    for (account_t * acct = xact_account(xact);
	 acct;
	 acct = acct->parent) {
      if (acct == xact_account(*pair.second)) {
	xact_in_budget = true;
	// Report the xact as if it had occurred in the parent
	// account.
	if (xact_account(xact) != acct)
	  xact_xdata(xact).account = acct;
	goto handle;
      }
    }

 handle:
  if (xact_in_budget && flags & BUDGET_BUDGETED) {
    report_budget_items(xact.date());
    item_handler<xact_t>::operator()(xact);
  }
  else if (! xact_in_budget && flags & BUDGET_UNBUDGETED) {
    item_handler<xact_t>::operator()(xact);
  }
}

void forecast_xacts::add_xact(const interval_t& period, xact_t& xact)
{
  generate_xacts::add_xact(period, xact);

  interval_t& i = pending_xacts.back().first;
  if (! is_valid(i.begin)) {
    i.start(current_date);
    i.begin = i.increment(i.begin);
  } else {
    while (i.begin < current_date)
      i.begin = i.increment(i.begin);
  }
}

void forecast_xacts::flush()
{
  xacts_list passed;
  date_t last;

  while (pending_xacts.size() > 0) {
    pending_xacts_list::iterator least = pending_xacts.begin();
    for (pending_xacts_list::iterator i = ++pending_xacts.begin();
	 i != pending_xacts.end();
	 i++)
      if ((*i).first.begin < (*least).first.begin)
	least = i;

    date_t& begin = (*least).first.begin;

    if (is_valid((*least).first.end) && begin >= (*least).first.end) {
      pending_xacts.erase(least);
      passed.remove((*least).second);
      continue;
    }

    xact_t& xact = *(*least).second;

    entry_temps.push_back(entry_t());
    entry_t& entry = entry_temps.back();
    entry.payee = "Forecast entry";
    entry._date = begin;

    xact_temps.push_back(xact);
    xact_t& temp = xact_temps.back();
    temp.entry = &entry;
    temp.add_flags(XACT_AUTO | XACT_TEMP);
    entry.add_xact(&temp);

    date_t next = (*least).first.increment(begin);
#if 0
    // jww (2008-04-24): Does seconds() here give the total seconds?
    if (next < begin || // wraparound
	(is_valid(last) && (next - last).seconds() > 365 * 5 * 24 * 3600))
      break;
#endif
    begin = next;

    item_handler<xact_t>::operator()(temp);

    if (xact_has_xdata(temp) &&
	xact_xdata_(temp).dflags & XACT_MATCHES) {
      if (! pred(temp))
	break;
      last = temp.date();
      passed.clear();
    } else {
      bool found = false;
      foreach (xact_t * x, passed)
	if (x == &xact) {
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

  item_handler<xact_t>::flush();
}

template <>
bool compare_items<account_t>::operator()(const account_t * left,
					  const account_t * right)
{
  assert(left);
  assert(right);

#if 0
  account_xdata_t& lxdata(account_xdata(*left));
  if (! (lxdata.dflags & ACCOUNT_SORT_CALC)) {
    sort_order.compute(lxdata.sort_value, details_t(*left));
    lxdata.dflags |= ACCOUNT_SORT_CALC;
  }

  account_xdata_t& rxdata(account_xdata(*right));
  if (! (rxdata.dflags & ACCOUNT_SORT_CALC)) {
    sort_order.compute(rxdata.sort_value, details_t(*right));
    rxdata.dflags |= ACCOUNT_SORT_CALC;
  }

  return lxdata.sort_value < rxdata.sort_value;
#else
  return false;
#endif
}

account_xdata_t& account_xdata(const account_t& account)
{
  if (! account.data)
    account.data = new account_xdata_t();

  return *static_cast<account_xdata_t *>(account.data);
}

void sum_accounts(account_t& account)
{
  account_xdata_t& xdata(account_xdata(account));

  foreach (accounts_map::value_type& pair, account.accounts) {
    sum_accounts(*pair.second);

    xdata.total += account_xdata_(*pair.second).total;
    xdata.total_count += (account_xdata_(*pair.second).total_count +
			  account_xdata_(*pair.second).count);
  }

  value_t result;
#if 0
  compute_amount(result, details_t(account));
#endif
  if (! result.is_realzero())
    xdata.total += result;
  xdata.total_count += xdata.count;
}

account_t * accounts_iterator::operator()()
{
  while (! accounts_i.empty() &&
	 accounts_i.back() == accounts_end.back()) {
    accounts_i.pop_back();
    accounts_end.pop_back();
  }
  if (accounts_i.empty())
    return NULL;

  account_t * account = (*(accounts_i.back()++)).second;
  assert(account);

  // If this account has children, queue them up to be iterated next.
  if (! account->accounts.empty())
    push_back(*account);

  return account;
}

void sorted_accounts_iterator::sort_accounts(account_t& account,
					     accounts_deque_t& deque)
{
  foreach (accounts_map::value_type& pair, account.accounts)
    deque.push_back(pair.second);

  std::stable_sort(deque.begin(), deque.end(),
		   compare_items<account_t>(sort_cmp));
}

account_t * sorted_accounts_iterator::operator()()
{
  while (! sorted_accounts_i.empty() &&
	 sorted_accounts_i.back() == sorted_accounts_end.back()) {
    sorted_accounts_i.pop_back();
    sorted_accounts_end.pop_back();
    assert(! accounts_list.empty());
    accounts_list.pop_back();
  }
  if (sorted_accounts_i.empty())
    return NULL;

  account_t * account = *sorted_accounts_i.back()++;
  assert(account);

  // If this account has children, queue them up to be iterated next.
  if (! account->accounts.empty())
    push_back(*account);

  account_xdata(*account).dflags &= ~ACCOUNT_SORT_CALC;
  return account;
}

void walk_commodities(commodity_pool_t::commodities_by_ident& commodities,
		      item_handler<xact_t>& handler)
{
  std::list<xact_t> xact_temps;
  std::list<entry_t>       entry_temps;
  std::list<account_t>     acct_temps;

  for (commodity_pool_t::commodities_by_ident::iterator
	 i = commodities.begin();
       i != commodities.end();
       i++) {
    if ((*i)->has_flags(COMMODITY_STYLE_NOMARKET))
      continue;

    entry_temps.push_back(entry_t());
    acct_temps.push_back(account_t(NULL, (*i)->symbol()));

    if ((*i)->history())
      foreach (const commodity_t::history_map::value_type& pair,
	       (*i)->history()->prices) {
	entry_temps.back()._date = pair.first.date();

	xact_temps.push_back(xact_t(&acct_temps.back()));
	xact_t& temp = xact_temps.back();
	temp.entry  = &entry_temps.back();
	temp.amount = pair.second;
	temp.add_flags(XACT_TEMP);
	entry_temps.back().add_xact(&temp);

	handler(xact_temps.back());
      }
  }

  handler.flush();

  clear_entries_xacts(entry_temps);
}

void journals_iterator::reset(session_t& session)
{
  journals_i   = session.journals.begin();
  journals_end = session.journals.end();
}

journal_t * journals_iterator::operator()()
{
  if (journals_i == journals_end)
    return NULL;
  return &(*journals_i++);
}

} // namespace ledger
