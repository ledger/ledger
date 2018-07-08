/*
 * Copyright (c) 2003-2018, John Wiegley.  All rights reserved.
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

#include "draft.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "journal.h"
#include "session.h"
#include "report.h"
#include "lookup.h"
#include "print.h"

namespace ledger {

void draft_t::xact_template_t::dump(std::ostream& out) const
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
    foreach (const post_template_t& post, posts) {
      out << std::endl
          << _f("[Posting \"%1\"]") % (post.from ? _("from") : _("to"))
          << std::endl;

      if (post.account_mask)
        out << _("  Account mask: ") << *post.account_mask << std::endl;
      else if (post.from)
        out << _("  Account mask: <use last of last related accounts>") << std::endl;
      else
        out << _("  Account mask: <use first of last related accounts>") << std::endl;

      if (post.amount)
        out << _("  Amount:       ") << *post.amount << std::endl;

      if (post.cost)
        out << _("  Cost:         ") << *post.cost_operator
            << " " << *post.cost << std::endl;
    }
  }
}

void draft_t::parse_args(const value_t& args)
{
  regex  date_mask(_("([0-9]+(?:[-/.][0-9]+)?(?:[-/.][0-9]+))?"));
  smatch what;
  bool   check_for_date = true;

  tmpl = xact_template_t();

  optional<date_time::weekdays>       weekday;
  xact_template_t::post_template_t *  post  = NULL;
  value_t::sequence_t::const_iterator begin = args.begin();
  value_t::sequence_t::const_iterator end   = args.end();

  for (; begin != end; begin++) {
    if (check_for_date &&
        regex_match((*begin).to_string(), what, date_mask)) {
      tmpl->date = parse_date(what[0]);
      check_for_date = false;
    }
    else if (check_for_date &&
             bool(weekday = string_to_day_of_week(what[0]))) {
#if defined(__GNUC__) && __GNUC__ >= 4 && __GNUC_MINOR__ >= 7
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#endif
      short  dow  = static_cast<short>(*weekday);
#if defined(__GNUC__) && __GNUC__ >= 4 && __GNUC_MINOR__ >= 7
#pragma GCC diagnostic pop
#endif
      date_t date = CURRENT_DATE() - date_duration(1);
      while (date.day_of_week() != dow)
        date -= date_duration(1);
      tmpl->date = date;
      check_for_date = false;
    }
    else {
      string arg = (*begin).to_string();

      if (arg == "at") {
        if (begin == end)
          throw std::runtime_error(_("Invalid xact command arguments"));
        tmpl->payee_mask = (*++begin).to_string();
      }
      else if (arg == "to" || arg == "from") {
        if (! post || post->account_mask) {
          tmpl->posts.push_back(xact_template_t::post_template_t());
          post = &tmpl->posts.back();
        }
        if (begin == end)
          throw std::runtime_error(_("Invalid xact command arguments"));
        post->account_mask = mask_t((*++begin).to_string());
        post->from = arg == "from";
      }
      else if (arg == "on") {
        if (begin == end)
          throw std::runtime_error(_("Invalid xact command arguments"));
        tmpl->date = parse_date((*++begin).to_string());
        check_for_date = false;
      }
      else if (arg == "code") {
        if (begin == end)
          throw std::runtime_error(_("Invalid xact command arguments"));
        tmpl->code = (*++begin).to_string();
      }
      else if (arg == "note") {
        if (begin == end)
          throw std::runtime_error(_("Invalid xact command arguments"));
        tmpl->note = (*++begin).to_string();
      }
      else if (arg == "rest") {
        ;                       // just ignore this argument
      }
      else if (arg == "@" || arg == "@@") {
        amount_t cost;
        post->cost_operator = arg;
        if (begin == end)
          throw std::runtime_error(_("Invalid xact command arguments"));
        arg = (*++begin).to_string();
        if (! cost.parse(arg, PARSE_SOFT_FAIL | PARSE_NO_MIGRATE))
          throw std::runtime_error(_("Invalid xact command arguments"));
        post->cost = cost;
      }
      else {
        // Without a preposition, it is either:
        //
        //  A payee, if we have not seen one
        //  An account or an amount, if we have
        //  An account if an amount has just been seen
        //  An amount if an account has just been seen

        if (tmpl->payee_mask.empty()) {
          tmpl->payee_mask = arg;
        } else {
          amount_t           amt;
          optional<mask_t> account;

          if (! amt.parse(arg, PARSE_SOFT_FAIL | PARSE_NO_MIGRATE))
            account = mask_t(arg);

          if (! post ||
              (account && post->account_mask) ||
              (! account && post->amount)) {
            tmpl->posts.push_back(xact_template_t::post_template_t());
            post = &tmpl->posts.back();
          }

          if (account) {
            post->account_mask = account;
          } else {
            post->amount = amt;
            post = NULL;        // an amount concludes this posting
          }
        }
      }
    }
  }

  if (! tmpl->posts.empty()) {
    bool has_only_from = true;
    bool has_only_to   = true;

    // A single account at the end of the line is the "from" account
    if (tmpl->posts.size() > 1 &&
        tmpl->posts.back().account_mask && ! tmpl->posts.back().amount)
      tmpl->posts.back().from = true;

    foreach (xact_template_t::post_template_t& post_tmpl, tmpl->posts) {
      if (post_tmpl.from)
        has_only_to = false;
      else
        has_only_from = false;
    }

    if (has_only_from) {
      tmpl->posts.push_front(xact_template_t::post_template_t());
    }
    else if (has_only_to) {
      tmpl->posts.push_back(xact_template_t::post_template_t());
      tmpl->posts.back().from = true;
    }
  }
}

xact_t * draft_t::insert(journal_t& journal)
{
  if (! tmpl)
    return NULL;

  if (tmpl->payee_mask.empty())
    throw std::runtime_error(_("'xact' command requires at least a payee"));

  xact_t *              matching = NULL;
  unique_ptr<xact_t> added(new xact_t);

  if (xact_t * xact =
      lookup_probable_account(tmpl->payee_mask.str(), journal.xacts.rbegin(),
                              journal.xacts.rend()).first) {
    DEBUG("draft.xact", "Found payee by lookup: transaction on line "
          << xact->pos->beg_line);
    matching = xact;
  } else {
    for (xacts_list::reverse_iterator j = journal.xacts.rbegin();
         j != journal.xacts.rend();
         j++) {
      if (tmpl->payee_mask.match((*j)->payee)) {
        matching = *j;
        DEBUG("draft.xact",
              "Found payee match: transaction on line " << (*j)->pos->beg_line);
        break;
      }
    }
  }

  if (! tmpl->date) {
    added->_date = CURRENT_DATE();
    DEBUG("draft.xact", "Setting date to current date");
  } else {
    added->_date = tmpl->date;
    DEBUG("draft.xact", "Setting date to template date: " << *tmpl->date);
  }

  added->set_state(item_t::UNCLEARED);

  if (matching) {
    added->payee = matching->payee;
    //added->code  = matching->code;
    //added->note  = matching->note;

#if DEBUG_ON
    DEBUG("draft.xact", "Setting payee from match: " << added->payee);
    //if (added->code)
    //  DEBUG("draft.xact", "Setting code  from match: " << *added->code);
    //if (added->note)
    //  DEBUG("draft.xact", "Setting note  from match: " << *added->note);
#endif
  } else {
    added->payee = tmpl->payee_mask.str();
    DEBUG("draft.xact", "Setting payee from template: " << added->payee);
  }

  if (tmpl->code) {
    added->code = tmpl->code;
    DEBUG("draft.xact", "Now setting code from template: " << *added->code);
  }
  if (tmpl->note) {
    added->note = tmpl->note;
    DEBUG("draft.xact", "Now setting note from template: " << *added->note);
  }

  if (tmpl->posts.empty()) {
    if (matching) {
      DEBUG("draft.xact", "Template had no postings, copying from match");

      foreach (post_t * post, matching->posts) {
        added->add_post(new post_t(*post));
        added->posts.back()->set_state(item_t::UNCLEARED);
      }
    } else {
      throw_(std::runtime_error,
             _f("No accounts, and no past transaction matching '%1%'")
             % tmpl->payee_mask);
    }
  } else {
    DEBUG("draft.xact", "Template had postings");

    bool any_post_has_amount = false;
    foreach (xact_template_t::post_template_t& post, tmpl->posts) {
      if (post.amount) {
        DEBUG("draft.xact", "  and at least one has an amount specified");
        any_post_has_amount = true;
        break;
      }
    }

    foreach (xact_template_t::post_template_t& post, tmpl->posts) {
      unique_ptr<post_t> new_post;

      commodity_t * found_commodity = NULL;

      if (matching) {
        if (post.account_mask) {
          DEBUG("draft.xact",
                "Looking for matching posting based on account mask");

          foreach (post_t * x, matching->posts) {
            if (post.account_mask->match(x->account->fullname())) {
              new_post.reset(new post_t(*x));
              DEBUG("draft.xact",
                    "Founding posting from line " << x->pos->beg_line);
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
                DEBUG("draft.xact",
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
                DEBUG("draft.xact",
                      "Copied first real posting from matching");
                break;
              }
            }
          }
        }
      }

      if (! new_post.get()) {
        new_post.reset(new post_t);
        DEBUG("draft.xact", "New posting was NULL, creating a blank one");
      }

      if (! new_post->account) {
        DEBUG("draft.xact", "New posting still needs an account");

        if (post.account_mask) {
          DEBUG("draft.xact", "The template has an account mask");

          account_t * acct = NULL;
          if (! acct) {
            acct = journal.find_account_re(post.account_mask->str());
#if DEBUG_ON
            if (acct)
              DEBUG("draft.xact", "Found account as a regular expression");
#endif
          }
          if (! acct) {
            acct = journal.find_account(post.account_mask->str());
#if DEBUG_ON
            if (acct)
              DEBUG("draft.xact", "Found (or created) account by name");
#endif
          }

          // Find out the default commodity to use by looking at the last
          // commodity used in that account
          for (xacts_list::reverse_iterator j = journal.xacts.rbegin();
               j != journal.xacts.rend();
               j++) {
            foreach (post_t * x, (*j)->posts) {
              if (x->account == acct && ! x->amount.is_null()) {
                new_post.reset(new post_t(*x));
                DEBUG("draft.xact",
                      "Found account in journal postings, setting new posting");
                break;
              }
            }
          }

          new_post->account = acct;
          DEBUG("draft.xact",
                "Set new posting's account to: " << acct->fullname());
        } else {
          if (post.from) {
            new_post->account = journal.find_account(_("Liabilities:Unknown"));
            DEBUG("draft.xact",
                  "Set new posting's account to: Liabilities:Unknown");
          } else {
            new_post->account = journal.find_account(_("Expenses:Unknown"));
            DEBUG("draft.xact",
                  "Set new posting's account to: Expenses:Unknown");
          }
        }
      }
      assert(new_post->account);

      if (new_post.get() && ! new_post->amount.is_null()) {
        found_commodity = &new_post->amount.commodity();

        if (any_post_has_amount) {
          new_post->amount = amount_t();
          DEBUG("draft.xact", "New posting has an amount, but we cleared it");
        } else {
          any_post_has_amount = true;
          DEBUG("draft.xact", "New posting has an amount, and we're using it");
        }
      }

      if (post.amount) {
        new_post->amount = *post.amount;
        DEBUG("draft.xact", "Copied over posting amount");

        if (post.from) {
          new_post->amount.in_place_negate();
          DEBUG("draft.xact", "Negated new posting amount");
        }
      }

      if (post.cost) {
        if (post.cost->sign() < 0)
          throw parse_error(_("A posting's cost may not be negative"));

        post.cost->in_place_unround();

        if (*post.cost_operator == "@") {
          // For the sole case where the cost might be uncommoditized,
          // guarantee that the commodity of the cost after multiplication
          // is the same as it was before.
          commodity_t& cost_commodity(post.cost->commodity());
          *post.cost *= new_post->amount;
          post.cost->set_commodity(cost_commodity);
        }
        else if (new_post->amount.sign() < 0) {
          new_post->cost->in_place_negate();
        }

        new_post->cost = *post.cost;
        DEBUG("draft.xact", "Copied over posting cost");
      }

      if (found_commodity &&
          ! new_post->amount.is_null() &&
          ! new_post->amount.has_commodity()) {
        new_post->amount.set_commodity(*found_commodity);
        DEBUG("draft.xact", "Set posting amount commodity to: "
              << new_post->amount.commodity());

        new_post->amount = new_post->amount.rounded();
        DEBUG("draft.xact",
              "Rounded posting amount to: " << new_post->amount);
      }

      added->add_post(new_post.release());
      added->posts.back()->account->add_post(added->posts.back());
      added->posts.back()->set_state(item_t::UNCLEARED);

      DEBUG("draft.xact", "Added new posting to derived entry");
    }
  }

  if (! journal.add_xact(added.get()))
    throw_(std::runtime_error,
           _("Failed to finalize derived transaction (check commodities)"));

  return added.release();
}

value_t template_command(call_scope_t& args)
{
  report_t& report(find_scope<report_t>(args));
  std::ostream& out(report.output_stream);

  out << _("--- Input arguments ---") << std::endl;
  args.value().dump(out);
  out << std::endl << std::endl;

  draft_t draft(args.value());
  out << _("--- Transaction template ---") << std::endl;
  draft.dump(out);

  return true;
}

value_t xact_command(call_scope_t& args)
{
  report_t& report(find_scope<report_t>(args));
  draft_t   draft(args.value());

  unique_ptr<xact_t> new_xact(draft.insert(*report.session.journal.get()));
  if (new_xact.get()) {
    // Only consider actual postings for the "xact" command
    report.HANDLER(limit_).on("#xact", "actual");

    report.xact_report(post_handler_ptr(new print_xacts(report)), *new_xact.get());
  }

  return true;
}

} // namespace ledger
