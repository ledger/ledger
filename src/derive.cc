/*
 * Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
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

#include "derive.h"
#include "mask.h"

namespace ledger {

void derive_command::operator()
  (value_t& result, xml::xpath_t::scope_t * locals)
{
#if 0
  std::ostream& out   = *get_ptr<std::ostream>(locals, 0);
  repitem_t *	items = get_ptr<repitem_t>(locals, 1);
  strings_list& args  = *get_ptr<strings_list *>(locals, 2);

  std::auto_ptr<entry_t> added(new entry_t);

  entry_t * matching = NULL;

  strings_list::iterator i = args.begin();

  added->_date = *i++;
  if (i == args.end())
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
    if (i == args.end() || ((*i)[0] == '-' || std::isdigit((*i)[0]))) {
      acct = journal.find_account("Expenses");
    }
    else if (i != args.end()) {
      acct = journal.find_account_re(*i);
      if (! acct)
	acct = journal.find_account(*i);
      assert(acct);
      i++;
    }

    if (i == args.end()) {
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

    if (journal.basket)
      acct = journal.basket;
    else
      acct = journal.find_account("Equity");

    added->add_transaction(new transaction_t(acct));
  }
  else if (i == args.end()) {
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

    if (i != args.end()) {
      account_t * acct = journal.find_account_re(*i);
      if (! acct)
	acct = journal.find_account(*i);
      assert(acct);
      added->transactions.back()->account = acct;
    }
  }
  else {
    while (i != args.end()) {
      string& re_pat(*i++);
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
      if (! acct)
	acct = journal.find_account_re(re_pat);
      if (! acct)
	acct = journal.find_account(re_pat);

      transaction_t * xact;
      if (i == args.end()) {
	if (amt)
	  xact = new transaction_t(acct, *amt);
	else
	  xact = new transaction_t(acct);
      } else {
	xact = new transaction_t(acct, amount_t(*i++));
	if (! xact->amount.commodity()) {
	  if (amt)
	    xact->amount.set_commodity(amt->commodity());
	  else if (commodity_t::default_commodity)
	    xact->amount.set_commodity(*commodity_t::default_commodity);
	}
      }
      added->add_transaction(xact);
    }

    assert(matching->transactions.back()->account);
    if (account_t * draw_acct = matching->transactions.back()->account)
      added->add_transaction(new transaction_t(draw_acct));
  }

 done:
  if (! run_hooks(journal.entry_finalize_hooks, *added, false) ||
      ! added->finalize() ||
      ! run_hooks(journal.entry_finalize_hooks, *added, true))
    throw new error("Failed to finalize derived entry (check commodities)");

  return added.release();
#endif
}

} // namespace ledger
