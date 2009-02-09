/*
 * Copyright (c) 2003-2009, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of New Artisans LLC nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "filters.h"
#include "iterators.h"
#include "compare.h"
#include "session.h"
#include "format.h"

namespace ledger {

pass_down_xacts::pass_down_xacts(xact_handler_ptr handler,
				 xacts_iterator& iter)
  : item_handler<xact_t>(handler)
{
  TRACE_CTOR(pass_down_xacts, "xact_handler_ptr, xacts_iterator");

  for (xact_t * xact = iter(); xact; xact = iter())
    item_handler<xact_t>::operator()(*xact);

  item_handler<xact_t>::flush();
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
  account_t * acct = xact.reported_account();

  account_t::xdata_t& xdata(acct->xdata());
  DEBUG("account.sums", "Account value was = " << xdata.value);
  xact.add_to_value(xdata.value);
  DEBUG("account.sums", "Account value is  = " << xdata.value);

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
    xact->xdata().drop_flags(XACT_EXT_SORT_CALC);
    item_handler<xact_t>::operator()(*xact);
  }

  xacts.clear();
}

namespace {
  string to_hex(uint_least32_t * message_digest)
  {
    std::ostringstream buf;

    for(int i = 0; i < 5 ; i++) {
      buf.width(8);
      buf.fill('0');
      buf << std::hex << message_digest[i];
    }
    return buf.str();
  }
}

void anonymize_xacts::operator()(xact_t& xact)
{
  SHA1		 sha;
  uint_least32_t message_digest[5];
  bool		 copy_entry_details = false;

  if (last_entry != xact.entry) {
    entry_temps.push_back(*xact.entry);
    last_entry = xact.entry;
    copy_entry_details = true;
  }
  entry_t& entry = entry_temps.back();

  if (copy_entry_details) {
    entry.copy_details(*xact.entry);

    sha.Reset();
    sha << xact.entry->payee.c_str();
    sha.Result(message_digest);

    entry.payee = to_hex(message_digest);
    entry.note  = none;
  }

  xact_temps.push_back(xact);
  xact_t& temp = xact_temps.back();
  temp.entry = &entry;

  sha.Reset();
  sha << xact.account->fullname().c_str();
  sha.Result(message_digest);

  temp.copy_details(xact);

  temp.account = xact.entry->journal->find_account(to_hex(message_digest));
  temp.note    = none;
  temp.add_flags(ITEM_TEMP);

  entry.add_xact(&temp);

  (*handler)(temp);
}

void calc_xacts::operator()(xact_t& xact)
{
  try {
    xact_t::xdata_t& xdata(xact.xdata());

    if (last_xact && last_xact->has_xdata())
      add_or_set_value(xdata.total, last_xact->xdata().total);

    if (! xdata.has_flags(XACT_EXT_NO_TOTAL)) {
      bind_scope_t bound_scope(*amount_expr.get_context(), xact);
      xdata.total += amount_expr.calc();
    }

    item_handler<xact_t>::operator()(xact);

    last_xact = &xact;
  }
  catch (const std::exception& err) {
    add_error_context(item_context(xact, "While calculating transaction"));
    throw;
  }
}

void invert_xacts::operator()(xact_t& xact)
{
  if (xact.has_xdata() &&
      xact.xdata().has_flags(XACT_EXT_COMPOUND)) {
    xact.xdata().value.negate();
  } else {
    xact.amount.negate();
    if (xact.cost)
      xact.cost->negate();
  }

  item_handler<xact_t>::operator()(xact);
}


namespace {
  void handle_value(const value_t&	  value,
		    account_t *		  account,
		    entry_t *		  entry,
		    unsigned int	  flags,
		    std::list<xact_t>&    temps,
		    item_handler<xact_t>& handler,
		    const date_t&         date		  = date_t())
  {
    temps.push_back(xact_t(account));
    xact_t& xact(temps.back());
    xact.entry = entry;
    xact.add_flags(ITEM_TEMP);
    entry->add_xact(&xact);

    // If the account for this xact is all virtual, then report the xact as
    // such.  This allows subtotal reports to show "(Account)" for accounts
    // that contain only virtual xacts.

    if (account && account->has_xdata())
      if (! account->xdata().has_flags(ACCOUNT_EXT_HAS_NON_VIRTUALS)) {
	xact.add_flags(XACT_VIRTUAL);
	if (! account->xdata().has_flags(ACCOUNT_EXT_HAS_UNB_VIRTUALS))
	  xact.add_flags(XACT_MUST_BALANCE);
      }

    xact_t::xdata_t& xdata(xact.xdata());

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
      flags |= XACT_EXT_COMPOUND;
      break;

    default:
      assert(false);		// jww (2008-04-24): What to do here?
      break;
    }

    if (flags)
      xdata.add_flags(flags);

    handler(xact);
  }
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

  if (last_entry != xact.entry && count > 0)
    report_subtotal();

  xact.add_to_value(subtotal);
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
	  xact_t::xdata_t& xdata(r_xact->xdata());
	  if (! xdata.has_flags(XACT_EXT_HANDLED) &&
	      (! xdata.has_flags(XACT_EXT_RECEIVED) ?
	       ! r_xact->has_flags(XACT_AUTO | XACT_VIRTUAL) :
	       also_matching)) {
	    xdata.add_flags(XACT_EXT_HANDLED);
	    item_handler<xact_t>::operator()(*r_xact);
	  }
	}
      } else {
	// This code should only be reachable from the "output"
	// command, since that is the only command which attempts to
	// output auto or period entries.
	xact_t::xdata_t& xdata(xact->xdata());
	if (! xdata.has_flags(XACT_EXT_HANDLED) &&
	    ! xact->has_flags(XACT_AUTO)) {
	  xdata.add_flags(XACT_EXT_HANDLED);
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

  last_xact->xdata().date = date;
  cur_bal = total_expr.calc(*last_xact).rounded();

  if (value_t diff = cur_bal - last_balance) {
    entry_temps.push_back(entry_t());
    entry_t& entry = entry_temps.back();
    entry.payee = "Commodities revalued";
    entry._date = date;

    handle_value(diff, NULL, &entry, XACT_EXT_NO_TOTAL, xact_temps,
		 *handler);
  }
}

void changed_value_xacts::operator()(xact_t& xact)
{
  if (last_xact)
    output_diff(last_xact->reported_date());

  if (changed_values_only)
    xact.xdata().add_flags(XACT_EXT_DISPLAYED);

  item_handler<xact_t>::operator()(xact);

  last_balance = total_expr.calc(xact).rounded();
  last_xact    = &xact;
}

void subtotal_xacts::report_subtotal(const char * spec_fmt)
{
  std::ostringstream out_date;
  if (spec_fmt) {
    out_date << format_date(finish, string(spec_fmt));
  }
  else if (date_format) {
    string fmt = "- ";
    fmt += *date_format;
    out_date << format_date(finish, string(fmt));
  }
  else {
    out_date << format_date(finish);
  }

  entry_temps.push_back(entry_t());
  entry_t& entry = entry_temps.back();
  entry.payee = out_date.str();
  entry._date = start;

  foreach (values_map::value_type& pair, values)
    handle_value(pair.second.value, pair.second.account, &entry, 0,
		 xact_temps, *handler, finish);

  values.clear();
}

void subtotal_xacts::operator()(xact_t& xact)
{
  if (! is_valid(start) || xact.date() < start)
    start = xact.date();
  if (! is_valid(finish) || xact.date() > finish)
    finish = xact.date();

  account_t * acct = xact.reported_account();
  assert(acct);

  values_map::iterator i = values.find(acct->fullname());
  if (i == values.end()) {
    value_t temp;
    xact.add_to_value(temp);
    std::pair<values_map::iterator, bool> result
      = values.insert(values_pair(acct->fullname(), acct_value_t(acct, temp)));
    assert(result.second);
  } else {
    xact.add_to_value((*i).second.value);
  }

  // If the account for this xact is all virtual, mark it as
  // such, so that `handle_value' can show "(Account)" for accounts
  // that contain only virtual xacts.

  if (! xact.has_flags(XACT_VIRTUAL))
    xact.reported_account()->xdata().add_flags(ACCOUNT_EXT_HAS_NON_VIRTUALS);
  else if (! xact.has_flags(XACT_MUST_BALANCE))
    xact.reported_account()->xdata().add_flags(ACCOUNT_EXT_HAS_UNB_VIRTUALS);
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
	interval.set_start(date);
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
    payee_subtotals_pair temp(xact.entry->payee, new subtotal_xacts(handler));
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
  temp.set_state(xact.state());
  temp.add_flags(ITEM_TEMP);

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
  temp.set_state(xact.state());
  temp.add_flags(ITEM_TEMP);

  entry.add_xact(&temp);

  item_handler<xact_t>::operator()(temp);
}

void dow_xacts::flush()
{
  for (int i = 0; i < 7; i++) {
    start = finish = date_t();
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
	pair.first.set_start(date);
	begin = pair.first.begin;
      }

      if (begin < date &&
	  (! is_valid(pair.first.end) || begin < pair.first.end)) {
	xact_t& xact = *pair.second;

	DEBUG("ledger.walk.budget", "Reporting budget for "
	      << xact.reported_account()->fullname());

	entry_temps.push_back(entry_t());
	entry_t& entry = entry_temps.back();
	entry.payee = "Budget entry";
	entry._date = begin;

	xact_temps.push_back(xact);
	xact_t& temp = xact_temps.back();
	temp.entry = &entry;
	temp.add_flags(XACT_AUTO | ITEM_TEMP);
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
    for (account_t * acct = xact.reported_account();
	 acct;
	 acct = acct->parent) {
      if (acct == (*pair.second).reported_account()) {
	xact_in_budget = true;
	// Report the xact as if it had occurred in the parent
	// account.
	if (xact.reported_account() != acct)
	  xact.xdata().account = acct;
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
    i.set_start(CURRENT_DATE());
    i.begin = i.increment(i.begin);
  } else {
    while (i.begin < CURRENT_DATE())
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
    temp.add_flags(XACT_AUTO | ITEM_TEMP);
    entry.add_xact(&temp);

    date_t next = (*least).first.increment(begin);
    if (next < begin || (is_valid(last) && (next - last).days() > 365 * 5))
      break;
    begin = next;

    item_handler<xact_t>::operator()(temp);

    if (temp.has_xdata() &&
	temp.xdata().has_flags(XACT_EXT_MATCHES)) {
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

pass_down_accounts::pass_down_accounts
  (acct_handler_ptr   handler,
   accounts_iterator& iter,
   const optional<item_predicate<account_t> >& predicate)
  : item_handler<account_t>(handler), pred(predicate)
{
  TRACE_CTOR(pass_down_accounts,
	     "acct_handler_ptr, accounts_iterator");

  for (account_t * account = iter(); account; account = iter())
    if (! pred || (*pred)(*account))
      item_handler<account_t>::operator()(*account);

  item_handler<account_t>::flush();
}

} // namespace ledger
