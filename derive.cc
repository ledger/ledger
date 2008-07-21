#include "derive.h"
#include "datetime.h"
#include "error.h"
#include "mask.h"
#include "walk.h"

#include <memory>

namespace ledger {

entry_t * derive_new_entry(journal_t& journal,
			   strings_list::iterator i,
			   strings_list::iterator end)
{
  std::auto_ptr<entry_t> added(new entry_t);

  entry_t * matching = NULL;

  added->_date = *i++;
  if (i == end)
    throw new error("Too few arguments to 'entry'");

  mask_t regexp(*i++);

  entries_list::reverse_iterator j;
  for (j = journal.entries.rbegin();
       j != journal.entries.rend();
       j++)
    if (regexp.match((*j)->payee)) {
      matching = *j;
      break;
    }

  added->payee = matching ? matching->payee : regexp.pattern;

  if (! matching) {
    account_t * acct;
    if (i == end || ((*i)[0] == '-' || std::isdigit((*i)[0]))) {
      acct = journal.find_account("Expenses");
    }
    else if (i != end) {
      acct = journal.find_account_re(*i);
      if (! acct)
	acct = journal.find_account(*i);
      assert(acct);
      i++;
    }

    if (i == end) {
      added->add_transaction(new transaction_t(acct));
    } else {
      transaction_t * xact = new transaction_t(acct, amount_t(*i++));
      added->add_transaction(xact);

      if (! xact->amount.commodity()) {
	// If the amount has no commodity, we can determine it given
	// the account by creating a final for the account and then
	// checking if it contains only a single commodity.  An
	// account to which only dollars are applied would imply that
	// dollars are wanted now too.

	std::auto_ptr<item_handler<transaction_t> > formatter;
	formatter.reset(new set_account_value);
	walk_entries(journal.entries, *formatter.get());
	formatter->flush();

	sum_accounts(*journal.master);

	value_t total = account_xdata(*acct).total;
	if (total.type == value_t::AMOUNT)
	  xact->amount.set_commodity(((amount_t *) total.data)->commodity());
      }
    }

    acct = NULL;

    if (i != end) {
      if (! acct)
	acct = journal.find_account_re(*i);
      if (! acct)
	acct = journal.find_account(*i);
    }

    if (! acct) {
      if (journal.basket)
	acct = journal.basket;
      else
	acct = journal.find_account("Equity");
    }   

    added->add_transaction(new transaction_t(acct));
  }
  else if (i == end) {
    // If no argument were given but the payee, assume the user wants
    // to see the same transaction as last time.
    added->code = matching->code;

    for (transactions_list::iterator k = matching->transactions.begin();
	 k != matching->transactions.end();
	 k++)
      added->add_transaction(new transaction_t(**k));
  }
  else if ((*i)[0] == '-' || std::isdigit((*i)[0])) {
    transaction_t * m_xact, * xact, * first;
    m_xact = matching->transactions.front();

    first = xact = new transaction_t(m_xact->account, amount_t(*i++));
    added->add_transaction(xact);

    if (! xact->amount.commodity())
      xact->amount.set_commodity(m_xact->amount.commodity());

    m_xact = matching->transactions.back();

    xact = new transaction_t(m_xact->account, - first->amount);
    added->add_transaction(xact);

    if (i != end) {
      account_t * acct = journal.find_account_re(*i);
      if (! acct)
	acct = journal.find_account(*i);
      assert(acct);
      added->transactions.back()->account = acct;
    }
  }
  else {
    account_t * draw_acct = NULL;

    while (i != end) {
      std::string& re_pat(*i++);
      account_t *  acct = NULL;
      amount_t *   amt  = NULL;

      mask_t acct_regex(re_pat);

      for (; j != journal.entries.rend(); j++)
	if (regexp.match((*j)->payee)) {
	  entry_t * entry = *j;
	  for (transactions_list::const_iterator x =
		 entry->transactions.begin();
	       x != entry->transactions.end();
	       x++)
	    if (acct_regex.match((*x)->account->fullname())) {
	      acct = (*x)->account;
	      amt  = &(*x)->amount;
	      matching = entry;
	      goto found;
	    }
	}

    found:
      transaction_t * xact;
      if (i == end) {
	if (amt)
	  xact = new transaction_t(acct, *amt);
	else
	  xact = new transaction_t(acct);
      } else {
	amount_t amount(*i++);

	strings_list::iterator x = i;
	if (i != end && ++x == end) {
	  draw_acct = journal.find_account_re(*i);
	  if (! draw_acct)
	    draw_acct = journal.find_account(*i);
	  i++;
	}

	if (! acct)
	  acct = journal.find_account_re(re_pat);
	if (! acct)
	  acct = journal.find_account(re_pat);

	xact = new transaction_t(acct, amount);
	if (! xact->amount.commodity()) {
	  if (amt)
	    xact->amount.set_commodity(amt->commodity());
	  else if (commodity_t::default_commodity)
	    xact->amount.set_commodity(*commodity_t::default_commodity);
	}
      }
      added->add_transaction(xact);
    }

    if (! draw_acct) {
      assert(matching->transactions.back()->account);
      draw_acct = matching->transactions.back()->account;
    }
    if (draw_acct)
      added->add_transaction(new transaction_t(draw_acct));
  }

 done:
  if (! run_hooks(journal.entry_finalize_hooks, *added, false) ||
      ! added->finalize() ||
      ! run_hooks(journal.entry_finalize_hooks, *added, true))
    throw new error("Failed to finalize derived entry (check commodities)");

  return added.release();
}

} // namespace ledger
