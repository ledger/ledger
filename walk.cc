#include "walk.h"
#include "format.h"

namespace ledger {

void sort_transactions::flush()
{
  std::stable_sort(transactions.begin(), transactions.end(),
		   compare_items<transaction_t>(sort_order));

  for (transactions_deque::iterator i = transactions.begin();
       i != transactions.end();
       i++)
    (*handler)(*i);

  transactions.clear();

  item_handler<transaction_t>::flush();
}

void calc_transactions::operator()(transaction_t * xact)
{
  if (last_xact) {
    xact->total += last_xact->total;
    xact->index = last_xact->index + 1;
  } else {
    xact->index = 0;
  }

  if (inverted) {
    xact->amount.negate();
    xact->cost.negate();
  }

  if (! (xact->dflags & TRANSACTION_NO_TOTAL))
    xact->total += *xact;

  (*handler)(xact);

  if (inverted) {
    xact->amount.negate();
    xact->cost.negate();
  }

  last_xact = xact;
}

void collapse_transactions::report_cumulative_subtotal()
{
  if (count == 1) {
    (*handler)(last_xact);
  } else {
    assert(count > 1);

    totals_account->total = subtotal;
    balance_t result;
    format_t::compute_total(result, details_t(totals_account));

    for (amounts_map::const_iterator i = result.amounts.begin();
	 i != result.amounts.end();
	 i++) {
      transaction_t * total_xact
	= new transaction_t(last_entry, totals_account);
      xact_temps.push_back(total_xact);

      total_xact->amount = (*i).second;
      total_xact->cost   = (*i).second;

      (*handler)(total_xact);
    }
  }

  subtotal = 0;
  count    = 0;
}

void changed_value_transactions::operator()(transaction_t * xact)
{
  if (last_xact) {
    balance_t	prev_bal;
    balance_t	cur_bal;
    std::time_t current   = xact ? xact->entry->date : std::time(NULL);
    std::time_t prev_date = last_xact->entry->date;

    format_t::compute_total(prev_bal, details_t(last_xact));

    last_xact->entry->date = current;
    format_t::compute_total(cur_bal,  details_t(last_xact));
    last_xact->entry->date = prev_date;

    if (balance_t diff = cur_bal - prev_bal) {
      entry_t * entry = new entry_t;

      entry->payee = "Commodities revalued";
      entry->date  = current;

      entry_temps.push_back(entry);

      for (amounts_map::const_iterator i = diff.amounts.begin();
	   i != diff.amounts.end();
	   i++) {
	transaction_t * temp_xact = new transaction_t(entry, NULL);
	xact_temps.push_back(temp_xact);

	temp_xact->amount  = (*i).second;
	temp_xact->dflags |= TRANSACTION_NO_TOTAL;

	(*handler)(temp_xact);
      }
    }
  }

  if (xact) {
    if (changed_values_only)
      xact->dflags |= TRANSACTION_DISPLAYED;

    (*handler)(xact);
  }

  last_xact = xact;
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

  entry_t * entry = new entry_t;
  entry->payee = buf;

  entry_temps.push_back(entry);

  for (balances_map::iterator i = balances.begin();
       i != balances.end();
       i++) {
    entry->date = finish;
    transaction_t temp(entry, (*i).first);
    temp.total = (*i).second;
    balance_t result;
    format_t::compute_total(result, details_t(&temp));
    entry->date = start;

    for (amounts_map::const_iterator j = result.amounts.begin();
	 j != result.amounts.end();
	 j++) {
      transaction_t * xact = new transaction_t(entry, (*i).first);
      xact_temps.push_back(xact);

      xact->amount = (*j).second;
      xact->cost   = (*j).second;

      (*handler)(xact);
    }
  }

  balances.clear();

  item_handler<transaction_t>::flush();
}

void subtotal_transactions::operator()(transaction_t * xact)
{
  if (balances.size() == 0) {
    start = finish = xact->entry->date;
  } else {
    if (std::difftime(xact->entry->date, start) < 0)
      start = xact->entry->date;
    if (std::difftime(xact->entry->date, finish) > 0)
      finish = xact->entry->date;
  }

  balances_map::iterator i = balances.find(xact->account);
  if (i == balances.end())
    balances.insert(balances_pair(xact->account, *xact));
  else
    (*i).second += *xact;
}

void interval_transactions::operator()(transaction_t * xact)
{
  std::time_t quant = interval.increment(begin);
  if (std::difftime(xact->entry->date, quant) > 0) {
    if (last_xact) {
      start  = begin;
      finish = quant;
      flush();
    }

    if (! interval.seconds) {
      struct std::tm * desc = std::localtime(&xact->entry->date);
      if (interval.years)
	desc->tm_mon = 0;
      desc->tm_mday = 1;
      desc->tm_hour = 0;
      desc->tm_min  = 0;
      desc->tm_sec  = 0;
      quant = std::mktime(desc);
    }

    std::time_t temp;
    while (std::difftime(xact->entry->date,
			 temp = interval.increment(quant)) > 0)
      quant = temp;
    begin = quant;
  }

  subtotal_transactions::operator()(xact);

  last_xact = xact;
}

void dow_transactions::flush()
{
  for (int i = 0; i < 7; i++) {
    for (transactions_deque::iterator d = days_of_the_week[i].begin();
	 d != days_of_the_week[i].end();
	 d++)
      subtotal_transactions::operator()(*d);
    subtotal_transactions::flush("%As");
    days_of_the_week[i].clear();
  }
}

} // namespace ledger
