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
#include "output.h"

namespace ledger {

namespace {
  struct entry_template_t
  {
    optional<date_t> date;
    optional<date_t> eff_date;
    optional<string> code;
    optional<string> note;
    mask_t           payee_mask;

    struct xact_template_t {
      bool               from;
      optional<mask_t>   account_mask;
      optional<amount_t> amount;

      xact_template_t() : from(false) {}
    };

    std::list<xact_template_t> xacts;

    entry_template_t() {}

    void dump(std::ostream& out) const
    {
      if (date)
	out << "Date:       " << *date << std::endl;
      else
	out << "Date:       <today>" << std::endl;

      if (eff_date)
	out << "Effective:  " << *eff_date << std::endl;

      if (code)
	out << "Code:       " << *code << std::endl;
      if (note)
	out << "Note:       " << *note << std::endl;

      if (payee_mask.empty())
	out << "Payee mask: INVALID (template expression will cause an error)"
	    << std::endl;
      else
	out << "Payee mask: " << payee_mask << std::endl;

      if (xacts.empty()) {
	out << std::endl
	    << "<Transaction copied from last related entry>"
	    << std::endl;
      } else {
	bool has_only_from = true;
	bool has_only_to   = true;

	foreach (const xact_template_t& xact, xacts) {
	  if (xact.from)
	    has_only_to = false;
	  else
	    has_only_from = false;
	}

	foreach (const xact_template_t& xact, xacts) {
	  out << std::endl
	      << "[Transaction \"" << (xact.from ? "from" : "to")
	      << "\"]" << std::endl;

	  if (xact.account_mask)
	    out << "  Account mask: " << *xact.account_mask << std::endl;
	  else if (xact.from)
	    out << "  Account mask: <use last of last related accounts>" << std::endl;
	  else
	    out << "  Account mask: <use first of last related accounts>" << std::endl;

	  if (xact.amount)
	    out << "  Amount:       " << *xact.amount << std::endl;
	}
      }
    }
  };

  entry_template_t
  args_to_entry_template(value_t::sequence_t::const_iterator begin,
			 value_t::sequence_t::const_iterator end)
  {
    regex  date_mask("([0-9]+(?:[-/.][0-9]+)?(?:[-/.][0-9]+))?(?:=.*)?");
    regex  dow_mask("(sun|mon|tue|wed|thu|fri|sat)");
    smatch what;

    entry_template_t tmpl;
    bool	     check_for_date = true;

    entry_template_t::xact_template_t * xact = NULL;

    for (; begin != end; begin++) {
      if (check_for_date &&
	  regex_match((*begin).to_string(), what, date_mask)) {
	tmpl.date = parse_date(what[0]);
	if (what.size() == 2)
	  tmpl.eff_date = parse_date(what[1]);
	check_for_date = false;
      }
      else if (check_for_date &&
	  regex_match((*begin).to_string(), what, dow_mask)) {
	short  dow  = string_to_day_of_week(what[0]);
	date_t date = CURRENT_DATE() - date_duration(1);
	while (date.day_of_week() != dow)
	  date -= date_duration(1);
	tmpl.date = date;
	check_for_date = false;
      }
      else {
	string arg = (*begin).to_string();

	if (arg == "at") {
	  tmpl.payee_mask = (*++begin).to_string();
	}
	else if (arg == "to" || arg == "from") {
	  if (! xact || xact->account_mask) {
	    tmpl.xacts.push_back(entry_template_t::xact_template_t());
	    xact = &tmpl.xacts.back();
	  }
	  xact->account_mask = mask_t((*++begin).to_string());
	  xact->from = arg == "from";
	}
	else if (arg == "on") {
	  tmpl.date = parse_date((*++begin).to_string());
	  check_for_date = false;
	}
	else if (arg == "code") {
	  tmpl.code = (*++begin).to_string();
	}
	else if (arg == "note") {
	  tmpl.note = (*++begin).to_string();
	}
	else if (arg == "rest") {
	  ;			// just ignore this argument
	}
	else {
	  // Without a preposition, it is either:
	  //
	  //  A payee, if we have not seen one
	  //  An account or an amount, if we have
	  //  An account if an amount has just been seen
	  //  An amount if an account has just been seen

	  if (tmpl.payee_mask.empty()) {
	    tmpl.payee_mask = arg;
	  }
	  else {
	    amount_t	     amt;
	    optional<mask_t> account;

	    if (! amt.parse(arg, amount_t::PARSE_SOFT_FAIL |
			    amount_t::PARSE_NO_MIGRATE))
	      account = mask_t(arg);

	    if (! xact ||
		(account && xact->account_mask) ||
		(! account && xact->amount)) {
	      tmpl.xacts.push_back(entry_template_t::xact_template_t());
	      xact = &tmpl.xacts.back();
	    }

	    if (account) {
	      xact->from = false;
	      xact->account_mask = account;
	    } else {
	      xact->amount = amt;
	    }
	  }
	}
      }
    }

    if (! tmpl.xacts.empty()) {
      bool has_only_from = true;
      bool has_only_to   = true;

      foreach (entry_template_t::xact_template_t& xact, tmpl.xacts) {
	if (xact.from)
	  has_only_to = false;
	else
	  has_only_from = false;
      }

      if (has_only_from) {
	tmpl.xacts.push_front(entry_template_t::xact_template_t());
      }
      else if (has_only_to) {
	tmpl.xacts.push_back(entry_template_t::xact_template_t());
	tmpl.xacts.back().from = true;
      }
    }

    return tmpl;
  }

  entry_t * derive_entry_from_template(entry_template_t& tmpl,
				       report_t&	 report)
  {
    if (tmpl.payee_mask.empty())
      throw std::runtime_error("'entry' command requires at least a payee");

    entry_t *  matching = NULL;
    journal_t& journal(*report.session.journal.get());

    std::auto_ptr<entry_t> added(new entry_t);

    entries_list::reverse_iterator j;

    for (j = journal.entries.rbegin();
	 j != journal.entries.rend();
	 j++) {
      if (tmpl.payee_mask.match((*j)->payee)) {
	matching = *j;
	break;
      }
    }

    if (! tmpl.date)
      added->_date = CURRENT_DATE();
    else
      added->_date = tmpl.date;

    if (tmpl.eff_date)
      added->_date_eff = tmpl.eff_date;

    if (matching) {
      added->payee = matching->payee;
      added->code  = matching->code;
      added->note  = matching->note;
    } else {
      added->payee = tmpl.payee_mask.expr.str();
    }

    if (tmpl.code)
      added->code = tmpl.code;
    if (tmpl.note)
      added->note = tmpl.note;

    if (tmpl.xacts.empty()) {
      if (matching) {
	foreach (xact_t * xact, matching->xacts)
	  added->add_xact(new xact_t(*xact));
      } else {
	throw_(std::runtime_error,
	       "No accounts, and no past entry matching '" << tmpl.payee_mask <<"'");
      }
    } else {
      bool any_xact_has_amount = false;
      foreach (entry_template_t::xact_template_t& xact, tmpl.xacts) {
	if (xact.amount) {
	  any_xact_has_amount = true;
	  break;
	}
      }

      foreach (entry_template_t::xact_template_t& xact, tmpl.xacts) {
	std::auto_ptr<xact_t> new_xact;

	commodity_t * found_commodity = NULL;

	if (matching) {
	  if (xact.account_mask) {
	    foreach (xact_t * x, matching->xacts) {
	      if (xact.account_mask->match(x->account->fullname())) {
		new_xact.reset(new xact_t(*x));
		break;
	      }
	    }
	  } else {
	    if (xact.from)
	      new_xact.reset(new xact_t(*matching->xacts.back()));
	    else
	      new_xact.reset(new xact_t(*matching->xacts.front()));
	  }
	}

	if (! new_xact.get())
	  new_xact.reset(new xact_t);

	if (! new_xact->account) {
	  if (xact.account_mask) {
	    account_t * acct = NULL;
	    if (! acct)
	      acct = journal.find_account_re(xact.account_mask->expr.str());
	    if (! acct)
	      acct = journal.find_account(xact.account_mask->expr.str());

	    // Find out the default commodity to use by looking at the last
	    // commodity used in that account
	    entries_list::reverse_iterator j;

	    for (j = journal.entries.rbegin();
		 j != journal.entries.rend();
		 j++) {
	      foreach (xact_t * x, (*j)->xacts) {
		if (x->account == acct && ! x->amount.is_null()) {
		  new_xact.reset(new xact_t(*x));
		  break;
		}
	      }
	    }
	    if (! new_xact.get())
	      new_xact.reset(new xact_t);

	    new_xact->account = acct;
	  } else {

	    if (xact.from)
	      new_xact->account = journal.find_account("Liabilities:Unknown");
	    else
	      new_xact->account = journal.find_account("Expenses:Unknown");
	  }
	}

	if (new_xact.get() && ! new_xact->amount.is_null()) {
	  found_commodity = &new_xact->amount.commodity();

	  if (any_xact_has_amount)
	    new_xact->amount = amount_t();
	  else
	    any_xact_has_amount = true;
	}

	if (xact.amount) {
	  new_xact->amount = *xact.amount;
	  if (xact.from)
	    new_xact->amount.in_place_negate();
	}

	if (found_commodity &&
	    ! new_xact->amount.is_null() &&
	    ! new_xact->amount.has_commodity()) {
	  new_xact->amount.set_commodity(*found_commodity);
	  new_xact->amount = new_xact->amount.rounded();
	}

	added->add_xact(new_xact.release());
      }
    }

    if (! journal.entry_finalize_hooks.run_hooks(*added.get(), false) ||
	! added->finalize() ||
	! journal.entry_finalize_hooks.run_hooks(*added.get(), true))
      throw std::runtime_error("Failed to finalize derived entry (check commodities)");

    return added.release();
  }
}

value_t template_command(call_scope_t& args)
{
  report_t& report(find_scope<report_t>(args));
  std::ostream& out(report.output_stream);

  value_t::sequence_t::const_iterator begin = args.value().begin();
  value_t::sequence_t::const_iterator end   = args.value().end();

  out << "--- Input arguments ---" << std::endl;
  args.value().dump(out);
  out << std::endl << std::endl;

  entry_template_t tmpl = args_to_entry_template(begin, end);

  out << "--- Entry template ---" << std::endl;
  tmpl.dump(out);

  return true;
}

value_t entry_command(call_scope_t& args)
{
  value_t::sequence_t::const_iterator begin = args.value().begin();
  value_t::sequence_t::const_iterator end   = args.value().end();

  report_t&		 report(find_scope<report_t>(args));
  entry_template_t	 tmpl = args_to_entry_template(begin, end);
  std::auto_ptr<entry_t> new_entry(derive_entry_from_template(tmpl, report));

  report.entry_report(xact_handler_ptr
		      (new format_xacts(report,
					report.HANDLER(print_format_).str())),
		      *new_entry.get());
  return true;
}

} // namespace ledger
