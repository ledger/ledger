#include "walk.h"
#include "format.h"

namespace ledger {

void calc_transactions::operator()(transaction_t * xact)
{
  if (last_xact)
    xact->total += last_xact->total;

  if (inverted) {
    xact->amount.negate();
    xact->cost.negate();
  }

  xact->total += *xact;
  xact->index = last_xact ? last_xact->index + 1 : 0;

  (*handler)(xact);

  if (inverted) {
    xact->amount.negate();
    xact->cost.negate();
  }

  last_xact  = xact;
}

void collapse_transactions::report_cumulative_subtotal()
{
  if (count == 1) {
    (*handler)(last_xact);
    return;
  }

  assert(count > 1);

  transaction_t * total_xact = new transaction_t(NULL, totals_account);

  balance_t value;
  total_xact->total = subtotal;
  format_t::compute_total(value, details_t(total_xact));
  total_xact->total = 0;

  total_xact->entry = last_entry;

  for (amounts_map::const_iterator i = value.amounts.begin();
       i != value.amounts.end();
       i++) {
    total_xact->amount = (*i).second;
    total_xact->cost   = (*i).second;

    (*handler)(total_xact);
  }

  xact_temps.push_back(total_xact);
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

	temp_xact->amount = (*i).second;
	temp_xact->total  = (*i).second;
	temp_xact->total.negate();

	xact_temps.push_back(temp_xact);

	(*handler)(temp_xact);
      }
    }
  }

  if (xact)
    (*handler)(xact);

  last_xact = xact;
}

void subtotal_transactions::flush()
{
  entry_t * entry = new entry_t;

  char buf[256];
  // jww (2004-08-10): allow for a format string here
  std::strftime(buf, 255, "- %Y/%m/%d", std::gmtime(&finish));
  entry->payee = buf;

  entry_temps.push_back(entry);

  for (balances_map::iterator i = balances.begin();
       i != balances.end();
       i++) {
    entry->date = finish;
    transaction_t * xact = new transaction_t(entry, (*i).first);
    xact->total = (*i).second;
    balance_t result;
    format_t::compute_total(result, details_t(xact));
    xact->total = 0;
    entry->date = start;

    xact_temps.push_back(xact);

    for (amounts_map::const_iterator j = result.amounts.begin();
	 j != result.amounts.end();
	 j++) {
      xact->amount = xact->cost = (*j).second;
      (*handler)(xact);
    }
  }

  balances.clear();
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

struct sum_in_account : public item_handler<transaction_t>
{
  virtual void operator()(transaction_t * xact) {
    xact->account->value += *xact;
  }
};

void calc__accounts(account_t * account,
		    const item_predicate<transaction_t>& pred,
		    unsigned int flags)
{
  sum_in_account handler;

  for (transactions_list::iterator i = account->transactions.begin();
       i != account->transactions.end();
       i++)
    if (pred(*i))
      handle_transaction(*i, handler, flags);

  for (accounts_map::iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++)
    calc__accounts((*i).second, pred, flags);
}

} // namespace ledger
