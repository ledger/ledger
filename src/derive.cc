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

#include <system.hh>

#include "derive.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "journal.h"
#include "session.h"
#include "report.h"
#include "output.h"

namespace ledger {

namespace {
  struct xact_template_t
  {
    optional<date_t> date;
    optional<string> code;
    optional<string> note;
    mask_t           payee_mask;

    struct post_template_t {
      bool               from;
      optional<mask_t>   account_mask;
      optional<amount_t> amount;

      post_template_t() : from(false) {}
    };

    std::list<post_template_t> posts;

    xact_template_t() {}

    void dump(std::ostream& out) const
    {
      if (date)
	out << _("Date:       ") << *date << std::endl;
      else
	out << _("Date:       <today>") << std::endl;

      if (code)
	out << _("Code:       ") << *code << std::endl;
      if (note)
	out << _("Note:       ") << *note << std::endl;

      if (payee_mask.empty())
	out << _("Payee mask: INVALID (template expression will cause an error)")
	    << std::endl;
      else
	out << _("Payee mask: ") << payee_mask << std::endl;

      if (posts.empty()) {
	out << std::endl
	    << _("<Posting copied from last related transaction>")
	    << std::endl;
      } else {
	bool has_only_from = true;
	bool has_only_to   = true;

	foreach (const post_template_t& post, posts) {
	  if (post.from)
	    has_only_to = false;
	  else
	    has_only_from = false;
	}

	foreach (const post_template_t& post, posts) {
	  straccstream accum;
	  out << std::endl
	      << ACCUM(accum << _("[Posting \"%1\"]")
		       << (post.from ? _("from") : _("to")))
	      << std::endl;

	  if (post.account_mask)
	    out << _("  Account mask: ") << *post.account_mask << std::endl;
	  else if (post.from)
	    out << _("  Account mask: <use last of last related accounts>") << std::endl;
	  else
	    out << _("  Account mask: <use first of last related accounts>") << std::endl;

	  if (post.amount)
	    out << _("  Amount:       ") << *post.amount << std::endl;
	}
      }
    }
  };

  xact_template_t
  args_to_xact_template(value_t::sequence_t::const_iterator begin,
			 value_t::sequence_t::const_iterator end)
  {
    regex  date_mask(_("([0-9]+(?:[-/.][0-9]+)?(?:[-/.][0-9]+))?"));
    smatch what;

    xact_template_t tmpl;
    bool	    check_for_date = true;

    optional<date_time::weekdays>      weekday;
    xact_template_t::post_template_t * post = NULL;

    for (; begin != end; begin++) {
      if (check_for_date &&
	  regex_match((*begin).to_string(), what, date_mask)) {
	tmpl.date = parse_date(what[0]);
	check_for_date = false;
      }
      else if (check_for_date &&
	       bool(weekday = string_to_day_of_week(what[0]))) {
	short  dow  = static_cast<short>(*weekday);
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
	  if (! post || post->account_mask) {
	    tmpl.posts.push_back(xact_template_t::post_template_t());
	    post = &tmpl.posts.back();
	  }
	  post->account_mask = mask_t((*++begin).to_string());
	  post->from = arg == "from";
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

	    if (! post ||
		(account && post->account_mask) ||
		(! account && post->amount)) {
	      tmpl.posts.push_back(xact_template_t::post_template_t());
	      post = &tmpl.posts.back();
	    }

	    if (account) {
	      post->from = false;
	      post->account_mask = account;
	    } else {
	      post->amount = amt;
	    }
	  }
	}
      }
    }

    if (! tmpl.posts.empty()) {
      bool has_only_from = true;
      bool has_only_to   = true;

      // A single account at the end of the line is the "from" account
      if (tmpl.posts.size() > 1 &&
	  tmpl.posts.back().account_mask && ! tmpl.posts.back().amount)
	tmpl.posts.back().from = true;

      foreach (xact_template_t::post_template_t& post, tmpl.posts) {
	if (post.from)
	  has_only_to = false;
	else
	  has_only_from = false;
      }

      if (has_only_from) {
	tmpl.posts.push_front(xact_template_t::post_template_t());
      }
      else if (has_only_to) {
	tmpl.posts.push_back(xact_template_t::post_template_t());
	tmpl.posts.back().from = true;
      }
    }

    return tmpl;
  }

  xact_t * derive_xact_from_template(xact_template_t& tmpl,
				     report_t&	      report)
  {
    if (tmpl.payee_mask.empty())
      throw std::runtime_error(_("xact' command requires at least a payee"));

    xact_t *		  matching = NULL;
    journal_t&		  journal(*report.session.journal.get());
    std::auto_ptr<xact_t> added(new xact_t);

    for (xacts_list::reverse_iterator j = journal.xacts.rbegin();
	 j != journal.xacts.rend();
	 j++) {
      if (tmpl.payee_mask.match((*j)->payee)) {
	matching = *j;
	DEBUG("derive.xact",
	      "Found payee match: transaction on line " << (*j)->beg_line);
	break;
      }
    }

    if (! tmpl.date) {
      added->_date = CURRENT_DATE();
      DEBUG("derive.xact", "Setting date to current date");
    } else {
      added->_date = tmpl.date;
      DEBUG("derive.xact", "Setting date to template date: " << *tmpl.date);
    }

    added->set_state(item_t::UNCLEARED);

    if (matching) {
      added->payee = matching->payee;
      added->code  = matching->code;
      added->note  = matching->note;

      DEBUG("derive.xact", "Setting payee from match: " << added->payee);
      if (added->code)
	DEBUG("derive.xact", "Setting code  from match: " << *added->code);
      if (added->note)
	DEBUG("derive.xact", "Setting note  from match: " << *added->note);
    } else {
      added->payee = tmpl.payee_mask.expr.str();
      DEBUG("derive.xact", "Setting payee from template: " << added->payee);
    }

    if (tmpl.code) {
      added->code = tmpl.code;
      DEBUG("derive.xact", "Now setting code from template: " << *added->code);
    }
    if (tmpl.note) {
      added->note = tmpl.note;
      DEBUG("derive.xact", "Now setting note from template: " << *added->note);
    }

    if (tmpl.posts.empty()) {
      if (matching) {
	DEBUG("derive.xact", "Template had no postings, copying from match");

	foreach (post_t * post, matching->posts) {
	  added->add_post(new post_t(*post));
	  added->posts.back()->set_state(item_t::UNCLEARED);
	}
      } else {
	throw_(std::runtime_error,
	       _("No accounts, and no past transaction matching '%1'")
	       << tmpl.payee_mask);
      }
    } else {
      DEBUG("derive.xact", "Template had postings");

      bool any_post_has_amount = false;
      foreach (xact_template_t::post_template_t& post, tmpl.posts) {
	if (post.amount) {
	  DEBUG("derive.xact", "  and at least one has an amount specified");
	  any_post_has_amount = true;
	  break;
	}
      }

      foreach (xact_template_t::post_template_t& post, tmpl.posts) {
	std::auto_ptr<post_t> new_post;

	commodity_t * found_commodity = NULL;

	if (matching) {
	  if (post.account_mask) {
	    DEBUG("derive.xact",
		  "Looking for matching posting based on account mask");

	    foreach (post_t * x, matching->posts) {
	      if (post.account_mask->match(x->account->fullname())) {
		new_post.reset(new post_t(*x));
		DEBUG("derive.xact",
		      "Founding posting from line " << x->beg_line);
		break;
	      }
	    }
	  } else {
	    if (post.from) {
	      for (posts_list::reverse_iterator j = matching->posts.rbegin();
		   j != matching->posts.rend();
		   j++) {
		if ((*j)->must_balance()) {
		  new_post.reset(new post_t(**j));
		  DEBUG("derive.xact",
			"Copied last real posting from matching");
		  break;
		}
	      }
	    } else {
	      for (posts_list::iterator j = matching->posts.begin();
		   j != matching->posts.end();
		   j++) {
		if ((*j)->must_balance()) {
		  new_post.reset(new post_t(**j));
		  DEBUG("derive.xact",
			"Copied first real posting from matching");
		  break;
		}
	      }
	    }
	  }
	}

	if (! new_post.get()) {
	  new_post.reset(new post_t);
	  DEBUG("derive.xact", "New posting was NULL, creating a blank one");
	}

	if (! new_post->account) {
	  DEBUG("derive.xact", "New posting still needs an account");

	  if (post.account_mask) {
	    DEBUG("derive.xact", "The template has an account mask");

	    account_t * acct = NULL;
	    if (! acct) {
	      acct = journal.find_account_re(post.account_mask->expr.str());
	      if (acct)
		DEBUG("derive.xact", "Found account as a regular expression");
	    }
	    if (! acct) {
	      acct = journal.find_account(post.account_mask->expr.str());
	      if (acct)
		DEBUG("derive.xact", "Found (or created) account by name");
	    }

	    // Find out the default commodity to use by looking at the last
	    // commodity used in that account
	    for (xacts_list::reverse_iterator j = journal.xacts.rbegin();
		 j != journal.xacts.rend();
		 j++) {
	      foreach (post_t * x, (*j)->posts) {
		if (x->account == acct && ! x->amount.is_null()) {
		  new_post.reset(new post_t(*x));
		  DEBUG("derive.xact",
			"Found account in journal postings, setting new posting");
		  break;
		}
	      }
	    }

	    new_post->account = acct;
	    DEBUG("derive.xact",
		  "Set new posting's account to: " << acct->fullname());
	  } else {
	    if (post.from) {
	      new_post->account = journal.find_account(_("Liabilities:Unknown"));
	      DEBUG("derive.xact",
		    "Set new posting's account to: Liabilities:Unknown");
	    } else {
	      new_post->account = journal.find_account(_("Expenses:Unknown"));
	      DEBUG("derive.xact",
		    "Set new posting's account to: Expenses:Unknown");
	    }
	  }
	}

	if (new_post.get() && ! new_post->amount.is_null()) {
	  found_commodity = &new_post->amount.commodity();

	  if (any_post_has_amount) {
	    new_post->amount = amount_t();
	    DEBUG("derive.xact", "New posting has an amount, but we cleared it");
	  } else {
	    any_post_has_amount = true;
	    DEBUG("derive.xact", "New posting has an amount, and we're using it");
	  }
	}

	if (post.amount) {
	  new_post->amount = *post.amount;
	  DEBUG("derive.xact", "Copied over posting amount");

	  if (post.from) {
	    new_post->amount.in_place_negate();
	    DEBUG("derive.xact", "Negated new posting amount");
	  }
	}

	if (found_commodity &&
	    ! new_post->amount.is_null() &&
	    ! new_post->amount.has_commodity()) {
	  new_post->amount.set_commodity(*found_commodity);
	  DEBUG("derive.xact", "Set posting amount commodity to: "
		<< new_post->amount.commodity());

	  new_post->amount = new_post->amount.rounded();
	  DEBUG("derive.xact",
		"Rounded posting amount to: " << new_post->amount);
	}

	added->add_post(new_post.release());
	added->posts.back()->set_state(item_t::UNCLEARED);

	DEBUG("derive.xact", "Added new posting to derived entry");
      }
    }

    if (! journal.xact_finalize_hooks.run_hooks(*added.get(), false) ||
	! added->finalize() ||
	! journal.xact_finalize_hooks.run_hooks(*added.get(), true)) {
      throw_(std::runtime_error,
	     _("Failed to finalize derived transaction (check commodities)"));
    }

    return added.release();
  }
}

value_t template_command(call_scope_t& args)
{
  report_t& report(find_scope<report_t>(args));
  std::ostream& out(report.output_stream);

  value_t::sequence_t::const_iterator begin = args.value().begin();
  value_t::sequence_t::const_iterator end   = args.value().end();

  out << _("--- Input arguments ---") << std::endl;
  args.value().dump(out);
  out << std::endl << std::endl;

  xact_template_t tmpl = args_to_xact_template(begin, end);

  out << _("--- Transaction template ---") << std::endl;
  tmpl.dump(out);

  return true;
}

value_t xact_command(call_scope_t& args)
{
  value_t::sequence_t::const_iterator begin = args.value().begin();
  value_t::sequence_t::const_iterator end   = args.value().end();

  report_t&		report(find_scope<report_t>(args));
  xact_template_t	tmpl = args_to_xact_template(begin, end);
  std::auto_ptr<xact_t> new_xact(derive_xact_from_template(tmpl, report));

  // jww (2009-02-27): make this more general
  report.HANDLER(limit_).on(string("#xact"), "actual");

  report.xact_report(post_handler_ptr
		      (new format_posts(report,
					report.HANDLER(print_format_).str())),
		      *new_xact.get());
  return true;
}

} // namespace ledger
