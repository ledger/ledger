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
  struct xact_template_t
  {
    optional<date_t> date;
    optional<date_t> eff_date;
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

      if (posts.empty()) {
	out << std::endl
	    << "<Posting copied from last related transaction>"
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
	  out << std::endl
	      << "[Posting \"" << (post.from ? "from" : "to")
	      << "\"]" << std::endl;

	  if (post.account_mask)
	    out << "  Account mask: " << *post.account_mask << std::endl;
	  else if (post.from)
	    out << "  Account mask: <use last of last related accounts>" << std::endl;
	  else
	    out << "  Account mask: <use first of last related accounts>" << std::endl;

	  if (post.amount)
	    out << "  Amount:       " << *post.amount << std::endl;
	}
      }
    }
  };

  xact_template_t
  args_to_xact_template(value_t::sequence_t::const_iterator begin,
			 value_t::sequence_t::const_iterator end)
  {
    regex  date_mask("([0-9]+(?:[-/.][0-9]+)?(?:[-/.][0-9]+))?(?:=.*)?");
    regex  dow_mask("(sun|mon|tue|wed|thu|fri|sat)");
    smatch what;

    xact_template_t tmpl;
    bool	     check_for_date = true;

    xact_template_t::post_template_t * post = NULL;

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
				       report_t&	 report)
  {
    if (tmpl.payee_mask.empty())
      throw std::runtime_error("'xact' command requires at least a payee");

    xact_t *  matching = NULL;
    journal_t& journal(*report.session.journal.get());

    std::auto_ptr<xact_t> added(new xact_t);

    xacts_list::reverse_iterator j;

    for (j = journal.xacts.rbegin();
	 j != journal.xacts.rend();
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

    if (tmpl.posts.empty()) {
      if (matching) {
	foreach (post_t * post, matching->posts)
	  added->add_post(new post_t(*post));
      } else {
	throw_(std::runtime_error,
	       "No accounts, and no past transaction matching '"
	       << tmpl.payee_mask <<"'");
      }
    } else {
      bool any_post_has_amount = false;
      foreach (xact_template_t::post_template_t& post, tmpl.posts) {
	if (post.amount) {
	  any_post_has_amount = true;
	  break;
	}
      }

      foreach (xact_template_t::post_template_t& post, tmpl.posts) {
	std::auto_ptr<post_t> new_post;

	commodity_t * found_commodity = NULL;

	if (matching) {
	  if (post.account_mask) {
	    foreach (post_t * x, matching->posts) {
	      if (post.account_mask->match(x->account->fullname())) {
		new_post.reset(new post_t(*x));
		break;
	      }
	    }
	  } else {
	    if (post.from)
	      new_post.reset(new post_t(*matching->posts.back()));
	    else
	      new_post.reset(new post_t(*matching->posts.front()));
	  }
	}

	if (! new_post.get())
	  new_post.reset(new post_t);

	if (! new_post->account) {
	  if (post.account_mask) {
	    account_t * acct = NULL;
	    if (! acct)
	      acct = journal.find_account_re(post.account_mask->expr.str());
	    if (! acct)
	      acct = journal.find_account(post.account_mask->expr.str());

	    // Find out the default commodity to use by looking at the last
	    // commodity used in that account
	    xacts_list::reverse_iterator j;

	    for (j = journal.xacts.rbegin();
		 j != journal.xacts.rend();
		 j++) {
	      foreach (post_t * x, (*j)->posts) {
		if (x->account == acct && ! x->amount.is_null()) {
		  new_post.reset(new post_t(*x));
		  break;
		}
	      }
	    }
	    if (! new_post.get())
	      new_post.reset(new post_t);

	    new_post->account = acct;
	  } else {

	    if (post.from)
	      new_post->account = journal.find_account("Liabilities:Unknown");
	    else
	      new_post->account = journal.find_account("Expenses:Unknown");
	  }
	}

	if (new_post.get() && ! new_post->amount.is_null()) {
	  found_commodity = &new_post->amount.commodity();

	  if (any_post_has_amount)
	    new_post->amount = amount_t();
	  else
	    any_post_has_amount = true;
	}

	if (post.amount) {
	  new_post->amount = *post.amount;
	  if (post.from)
	    new_post->amount.in_place_negate();
	}

	if (found_commodity &&
	    ! new_post->amount.is_null() &&
	    ! new_post->amount.has_commodity()) {
	  new_post->amount.set_commodity(*found_commodity);
	  new_post->amount = new_post->amount.rounded();
	}

	added->add_post(new_post.release());
      }
    }

    if (! journal.xact_finalize_hooks.run_hooks(*added.get(), false) ||
	! added->finalize() ||
	! journal.xact_finalize_hooks.run_hooks(*added.get(), true))
      throw_(std::runtime_error,
	     "Failed to finalize derived transaction (check commodities)");

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

  xact_template_t tmpl = args_to_xact_template(begin, end);

  out << "--- Transaction template ---" << std::endl;
  tmpl.dump(out);

  return true;
}

value_t xact_command(call_scope_t& args)
{
  value_t::sequence_t::const_iterator begin = args.value().begin();
  value_t::sequence_t::const_iterator end   = args.value().end();

  report_t&		 report(find_scope<report_t>(args));
  xact_template_t	 tmpl = args_to_xact_template(begin, end);
  std::auto_ptr<xact_t> new_xact(derive_xact_from_template(tmpl, report));

  report.xact_report(post_handler_ptr
		      (new format_posts(report,
					report.HANDLER(print_format_).str())),
		      *new_xact.get());
  return true;
}

} // namespace ledger
