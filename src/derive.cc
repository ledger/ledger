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

#include "derive.h"
#include "session.h"
#include "iterators.h"

namespace ledger {

entry_t * derive_new_entry(report_t&		  report,
			   strings_list::iterator i,
			   strings_list::iterator end)
{
  session_t& session(report.session);

  std::auto_ptr<entry_t> added(new entry_t);

  entry_t * matching = NULL;

  added->_date = parse_date(*i++);
  if (i == end)
    throw std::runtime_error("Too few arguments to 'entry'");

  mask_t regexp(*i++);

  entries_list::reverse_iterator j;

  for (j = report.session.journal->entries.rbegin();
       j != report.session.journal->entries.rend();
       j++) {
    if (regexp.match((*j)->payee)) {
      matching = *j;
      break;
    }
  }

  added->payee = matching ? matching->payee : regexp.expr.str();

  if (! matching) {
    account_t * acct;
    if (i == end || ((*i)[0] == '-' || std::isdigit((*i)[0]))) {
      acct = session.master->find_account("Expenses");
    }
    else if (i != end) {
      acct = session.master->find_account_re(*i);
      if (! acct)
	acct = session.master->find_account(*i);
      assert(acct);
      i++;
    }

    if (i == end) {
      added->add_xact(new xact_t(acct));
    } else {
      xact_t * xact = new xact_t(acct, amount_t(*i++));
      added->add_xact(xact);

      if (! xact->amount.commodity()) {
	// If the amount has no commodity, we can determine it given
	// the account by creating a final for the account and then
	// checking if it contains only a single commodity.  An
	// account to which only dollars are applied would imply that
	// dollars are wanted now too.

	report.sum_all_accounts();

	value_t total = acct->xdata().total;
	if (total.is_type(value_t::AMOUNT))
	  xact->amount.set_commodity(total.as_amount().commodity());
      }
    }

    acct = NULL;

    if (i != end) {
      if (! acct)
	acct = session.master->find_account_re(*i);
      if (! acct)
	acct = session.master->find_account(*i);
    }

    if (! acct) {
      if (matching && matching->journal->basket)
	acct = matching->journal->basket;
      else
	acct = session.master->find_account("Equity");
    }   

    added->add_xact(new xact_t(acct));
  }
  else if (i == end) {
    // If no argument were given but the payee, assume the user wants
    // to see the same xact as last time.
    added->code = matching->code;

    foreach (xact_t * xact, matching->xacts)
      added->add_xact(new xact_t(*xact));
  }
  else if ((*i)[0] == '-' || std::isdigit((*i)[0])) {
    xact_t * m_xact, * xact, * first;
    m_xact = matching->xacts.front();

    first = xact = new xact_t(m_xact->account, amount_t(*i++));
    added->add_xact(xact);

    if (! xact->amount.commodity())
      xact->amount.set_commodity(m_xact->amount.commodity());

    m_xact = matching->xacts.back();

    xact = new xact_t(m_xact->account, - first->amount);
    added->add_xact(xact);

    if (i != end) {
      account_t * acct = session.master->find_account_re(*i);
      if (! acct)
	acct = session.master->find_account(*i);
      assert(acct);
      added->xacts.back()->account = acct;
    }
  }
  else {
    account_t * draw_acct = NULL;

    while (i != end) {
      string&	  re_pat(*i++);
      account_t * acct = NULL;
      amount_t *  amt  = NULL;

      mask_t acct_regex(re_pat);

      for (; j != matching->journal->entries.rend(); j++)
	if (regexp.match((*j)->payee)) {
	  entry_t * entry = *j;
	  foreach (xact_t * xact, entry->xacts)
	    if (acct_regex.match(xact->account->fullname())) {
	      acct = xact->account;
	      amt  = &xact->amount;
	      matching = entry;
	      goto found;
	    }
	}

    found:
      xact_t * xact;
      if (i == end) {
	if (amt)
	  xact = new xact_t(acct, *amt);
	else
	  xact = new xact_t(acct);
      } else {
	amount_t amount(*i++);

	strings_list::iterator x = i;
	if (i != end && ++x == end) {
	  draw_acct = session.master->find_account_re(*i);
	  if (! draw_acct)
	    draw_acct = session.master->find_account(*i);
	  i++;
	}

	if (! acct)
	  acct = session.master->find_account_re(re_pat);
	if (! acct)
	  acct = session.master->find_account(re_pat);

	xact = new xact_t(acct, amount);
	if (! xact->amount.commodity()) {
	  if (amt)
	    xact->amount.set_commodity(amt->commodity());
	  else if (amount_t::current_pool->default_commodity)
	    xact->amount.set_commodity(*amount_t::current_pool->default_commodity);
	}
      }
      added->add_xact(xact);
    }

    if (! draw_acct) {
      assert(matching->xacts.back()->account);
      draw_acct = matching->xacts.back()->account;
    }
    if (draw_acct)
      added->add_xact(new xact_t(draw_acct));
  }

  if ((matching &&
       ! matching->journal->entry_finalize_hooks.run_hooks(*added, false)) ||
      ! added->finalize() ||
      (matching &&
       ! matching->journal->entry_finalize_hooks.run_hooks(*added, true)))
    throw std::runtime_error("Failed to finalize derived entry (check commodities)");

  return added.release();
}

} // namespace ledger
