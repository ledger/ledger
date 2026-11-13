/*
 * Copyright (c) 2003-2025, John Wiegley.  All rights reserved.
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

/**
 * @file   draft.cc
 * @author John Wiegley
 *
 * @ingroup report
 *
 * @brief Transaction drafting: parsing template arguments, matching
 *        historical transactions, and inserting derived entries.
 *
 * This file implements the "xact" and "template" commands.  The core
 * algorithm works in three phases:
 *
 *   1. **Argument parsing** (parse_args): Tokenize the command-line
 *      arguments into an xact_template_t, recognizing dates, prepositions,
 *      payee patterns, account patterns, amounts, and costs.
 *
 *   2. **Historical matching** (insert): Search the journal's transaction
 *      list (most recent first) for a payee that matches the template's
 *      payee mask.  Two strategies are tried: first a probabilistic lookup
 *      (via lookup_probable_account), then a simple regex match.
 *
 *   3. **Transaction construction** (insert): Build a new xact_t by
 *      merging the template's explicit fields with details from the
 *      matching historical transaction.  The result is finalized and
 *      inserted into the journal.
 */

#include <system.hh>

#include "draft.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "journal.h"
#include "pool.h"
#include "session.h"
#include "report.h"
#include "lookup.h"
#include "print.h"
#include "times.h"

namespace ledger {

/*--- Template Dump ---*/

void draft_t::xact_template_t::dump(std::ostream& out) const {
  if (date)
    out << _("Date:       ") << *date << '\n';
  else
    out << _("Date:       <today>") << '\n';

  if (code)
    out << _("Code:       ") << *code << '\n';
  if (note)
    out << _("Note:       ") << *note << '\n';

  if (payee_mask.empty())
    out << _("Payee mask: INVALID (template expression will cause an error)") << '\n';
  else
    out << _("Payee mask: ") << payee_mask << '\n';

  if (posts.empty()) {
    out << '\n' << _("<Posting copied from last related transaction>") << '\n';
  } else {
    for (const post_template_t& post : posts) {
      out << '\n' << _f("[Posting \"%1%\"]") % (post.from ? _("from") : _("to")) << '\n';

      if (post.account_mask)
        out << _("  Account mask: ") << *post.account_mask << '\n';
      else if (post.from)
        out << _("  Account mask: <use last of last related accounts>") << '\n';
      else
        out << _("  Account mask: <use first of last related accounts>") << '\n';

      if (post.amount)
        out << _("  Amount:       ") << *post.amount << '\n';

      if (post.cost)
        out << _("  Cost:         ") << *post.cost_operator << " " << *post.cost << '\n';
    }
  }
}

/*--- Argument Parsing ---*/

/**
 * @brief Parse the command-line arguments into an xact_template_t.
 *
 * The parser processes arguments left-to-right using the following rules:
 *
 *   - The first argument that looks like a date (digits with separators)
 *     or a day-of-week name becomes the transaction date.
 *   - Prepositions ("at", "to", "from", "on", "code", "note", "@", "@@")
 *     introduce their respective fields.
 *   - Without a preposition, the first non-date token becomes the payee.
 *     Subsequent tokens become either accounts or amounts depending on
 *     whether they parse as valid amount_t values.
 *   - A trailing account with no amount is assumed to be the "from"
 *     (source) account.
 *   - If all postings are "from" or all are "to", a balancing posting
 *     is added automatically.
 */
void draft_t::parse_args(const value_t& args) {
  regex date_mask(_("([0-9]+(?:[-/.][0-9]+)?(?:[-/.][0-9]+))?"));
  smatch what;
  bool check_for_date = true;

  tmpl = xact_template_t();

  optional<date_time::weekdays> weekday;
  optional<date_t> rel_date;
  xact_template_t::post_template_t* post = nullptr;
  value_t::sequence_t::const_iterator begin = args.begin();
  value_t::sequence_t::const_iterator end = args.end();

  for (; begin != end; begin++) {
    string arg = (*begin).to_string();

    if (check_for_date && regex_match(arg, what, date_mask)) {
      tmpl->date_string = what[0];
      check_for_date = false;
    } else if (check_for_date && (rel_date = string_to_relative_date(arg))) {
      // "today", "yesterday", "tomorrow" (and the short forms "tday",
      // "yday", "tmrw") as a positional first argument.  Multi-word
      // expressions such as "last month" must be passed with the `on`
      // preposition so they can be supplied as a single argument.
      tmpl->date = *rel_date;
      check_for_date = false;
    } else {
      weekday = string_to_day_of_week(arg);
      if (check_for_date && weekday) {
#if defined(__GNUC__) && __GNUC__ >= 4 && __GNUC_MINOR__ >= 7
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#endif
        short dow = static_cast<short>(*weekday);
#if defined(__GNUC__) && __GNUC__ >= 4 && __GNUC_MINOR__ >= 7
#pragma GCC diagnostic pop
#endif
        date_t date = CURRENT_DATE() - date_duration(1);
        while (date.day_of_week() != dow)
          date -= date_duration(1);
        tmpl->date = date;
        check_for_date = false;
      } else {
        // NOLINTBEGIN(bugprone-branch-clone)

        // Handle preposition-based argument syntax
        if (arg == "at") {
          if (++begin == end)
            throw std::runtime_error(_("Invalid xact command arguments"));
          tmpl->payee_mask = (*begin).to_string();
        } else if (arg == "to" || arg == "from") {
          if (!post || post->account_mask) {
            tmpl->posts.push_back(xact_template_t::post_template_t());
            post = &tmpl->posts.back();
          }
          if (++begin == end)
            throw std::runtime_error(_("Invalid xact command arguments"));
          post->account_mask = mask_t((*begin).to_string());
          post->from = arg == "from";
        } else if (arg == "on") {
          if (++begin == end)
            throw std::runtime_error(_("Invalid xact command arguments"));
          tmpl->date_string = (*begin).to_string();
          check_for_date = false;
        } else if (arg == "code") {
          if (++begin == end)
            throw std::runtime_error(_("Invalid xact command arguments"));
          tmpl->code = (*begin).to_string();
        } else if (arg == "note") {
          if (++begin == end)
            throw std::runtime_error(_("Invalid xact command arguments"));
          tmpl->note = (*begin).to_string();
        } else if (arg == "rest") {
          ; // just ignore this argument
        } else if (arg == "@" || arg == "@@") {
          amount_t cost;
          post->cost_operator = arg;
          if (++begin == end)
            throw std::runtime_error(_("Invalid xact command arguments"));
          arg = (*begin).to_string();
          if (!cost.parse(arg, PARSE_SOFT_FAIL | PARSE_NO_MIGRATE))
            throw std::runtime_error(_("Invalid xact command arguments"));
          post->cost = cost;
        } else {
          // Without a preposition, it is either:
          //
          //  A payee, if we have not seen one
          //  An account or an amount, if we have
          //  An account if an amount has just been seen
          //  An amount if an account has just been seen

          if (tmpl->payee_mask.empty()) {
            tmpl->payee_mask = arg;
          } else {
            amount_t amt;
            optional<mask_t> account;

            if (!amt.parse(arg, PARSE_SOFT_FAIL | PARSE_NO_MIGRATE)) {
              // Before treating as an account, check if this argument
              // is a known commodity symbol that should attach to the
              // previous posting's uncommoditized amount (e.g., "1000
              // EUR" passed as separate arguments). (#1593)
              if (!tmpl->posts.empty()) {
                auto& last_post = tmpl->posts.back();
                if (last_post.amount && !last_post.amount->has_commodity()) {
                  if (commodity_t* comm = commodity_pool_t::current_pool->find(arg)) {
                    last_post.amount->set_commodity(*comm);
                    continue;
                  }
                }
              }
              account = mask_t(arg);
            }

            if (!post || (account && post->account_mask) || (!account && post->amount)) {
              tmpl->posts.push_back(xact_template_t::post_template_t());
              post = &tmpl->posts.back();
            }

            if (account) {
              post->account_mask = account;
            } else {
              post->amount = amt;
              post = nullptr; // an amount concludes this posting
            }
          }
        }
        // NOLINTEND(bugprone-branch-clone)
      }
    }
  }

  // Post-processing: ensure the template has balanced "to" and "from"
  // postings, adding placeholders as needed.
  if (!tmpl->posts.empty()) {
    bool has_only_from = true;
    bool has_only_to = true;

    // A single account at the end of the line is the "from" account
    if (tmpl->posts.size() > 1 && tmpl->posts.back().account_mask && !tmpl->posts.back().amount)
      tmpl->posts.back().from = true;

    for (xact_template_t::post_template_t& post_tmpl : tmpl->posts) {
      if (post_tmpl.from)
        has_only_to = false;
      else
        has_only_from = false;
    }

    // NOLINTBEGIN(bugprone-branch-clone)
    if (has_only_from) {
      tmpl->posts.push_front(xact_template_t::post_template_t());
    } else if (has_only_to) {
      tmpl->posts.push_back(xact_template_t::post_template_t());
      tmpl->posts.back().from = true;
    }
    // NOLINTEND(bugprone-branch-clone)
  }
}

/*--- Transaction Construction and Insertion ---*/

namespace {
/**
 * @brief Give a posting copied from history a new amount (null to let
 *        finalize infer it), discarding what described the old one.
 *
 * The copy carries the history's cost, balance assertion and the flags
 * that say how that amount and cost were written or computed.  Kept
 * beside a new amount they would print or book something the draft
 * never said: a hidden amount, a stale assertion, a stale @@ or (@)=.
 */
void replace_copied_amount(post_t& post, const amount_t& amount) {
  post.amount = amount;
  post.cost = std::nullopt;
  post.given_cost = std::nullopt;
  post.assigned_amount = std::nullopt;
  post.drop_flags(POST_CALCULATED | POST_COST_CALCULATED | POST_COST_IN_FULL | POST_COST_FIXATED |
                  POST_COST_VIRTUAL | POST_AMOUNT_USER_ANNOTATED | POST_AMOUNT_USER_DATE);
}

/**
 * @brief Remove any `UUID: ...` tag lines from a note copied off a
 *        matched transaction.
 *
 * A generated draft is a new transaction, not a restatement of the one
 * it was derived from, so it must not claim the original's UUID: the
 * UUID deduplication in journal_t::add_xact() would otherwise reject or
 * silently discard it -- once when the draft is inserted, and again when
 * the user adds the generated text to the journal (issue #3244).
 */
optional<string> strip_uuid_lines(const optional<string>& note) {
  if (!note)
    return note;

  std::istringstream in(*note);
  std::ostringstream out;
  string line;
  bool first = true;
  bool changed = false;

  while (std::getline(in, line)) {
    string::size_type b = line.find_first_not_of(" \t");
    if (b != string::npos && line.compare(b, 5, "UUID:") == 0) {
      changed = true;
      continue;
    }
    if (!first)
      out << '\n';
    out << line;
    first = false;
  }

  if (!changed)
    return note;

  string result = out.str();
  if (result.empty())
    return none;
  return result;
}
} // namespace

/**
 * @brief Build a new transaction from the template and insert it into
 *        the journal.
 *
 * The algorithm proceeds in several phases:
 *
 *   1. **Payee matching**: Search for the most recent historical
 *      transaction whose payee matches the template's payee_mask.
 *      First try probabilistic matching (lookup_probable_account),
 *      then fall back to simple regex matching.
 *
 *   2. **Header construction**: Set the date, payee, code, note, and
 *      metadata from the template, falling back to the matching
 *      transaction for any field the template leaves unspecified.
 *
 *   3. **Posting construction**: For each posting in the template,
 *      find a matching posting in the historical transaction (by
 *      account pattern), or create a new one.  Apply any explicit
 *      amount, cost, and commodity from the template.
 *
 *   4. **Finalization**: Add the transaction to the journal, which
 *      runs the standard balance-checking and amount-inference logic.
 *
 * @return The newly created xact_t, owned by the journal, or nullptr.
 */
xact_t* draft_t::insert(journal_t& journal) {
  if (!tmpl)
    return nullptr;

  if (tmpl->payee_mask.empty())
    throw std::runtime_error(_("'xact' command requires at least a payee"));

  xact_t* matching = nullptr;
  unique_ptr<xact_t> added(new xact_t);

  // There is no need to check drafts for errors, because we generated them.
  journal.checking_style = journal_t::CHECK_PERMISSIVE;

  // Phase 1: Search for a matching historical transaction
  if (xact_t* xact = lookup_probable_account(tmpl->payee_mask.str(), journal.xacts.rbegin(),
                                             journal.xacts.rend())
                         .first) {
    DEBUG("draft.xact", "Found payee by lookup: transaction on line " << xact->pos->beg_line);
    matching = xact;
  } else {
    for (xacts_list::reverse_iterator j = journal.xacts.rbegin(); j != journal.xacts.rend(); j++) {
      if (tmpl->payee_mask.match((*j)->payee)) {
        matching = *j;
        DEBUG("draft.xact", "Found payee match: transaction on line " << (*j)->pos->beg_line);
        break;
      }
      bool found_via_tag = false;
      for (post_t* post : (*j)->posts) {
        string tag_payee = post->payee_from_tag();
        if (!tag_payee.empty() && tmpl->payee_mask.match(tag_payee)) {
          matching = *j;
          found_via_tag = true;
          DEBUG("draft.xact",
                "Found payee match via Payee tag: transaction on line " << (*j)->pos->beg_line);
          break;
        }
      }
      if (found_via_tag)
        break;
    }
  }

  // Phase 2: Set the transaction date
  if (!tmpl->date && !tmpl->date_string) {
    added->_date = CURRENT_DATE();
    DEBUG("draft.xact", "Setting date to current date");
  } else if (tmpl->date) {
    added->_date = tmpl->date;
    DEBUG("draft.xact", "Setting date to template date: " << *tmpl->date);
  } else if (tmpl->date_string) {
    // Convert separators to slashes for consistent parsing.  parse_date_expr
    // first tries the registered date formats and falls back to the full
    // period-expression grammar (yesterday, last month, 3 days ago, etc.)
    // so the `on` preposition accepts the same date vocabulary as --period.
    string date_str = *tmpl->date_string;
    for (char& c : date_str) {
      if (c == '-' || c == '.')
        c = '/';
    }

    added->_date = parse_date_expr(date_str);
    DEBUG("draft.xact",
          "Setting date to parsed date string: " << *tmpl->date_string << " -> " << added->_date);
  }

  added->set_state(item_t::UNCLEARED);

  // Phase 2 (continued): Set header fields from the match or template
  if (matching) {
    added->payee = matching->payee;
    added->code = matching->code;
    // The draft must not inherit the matched transaction's UUID (see
    // strip_uuid_lines above), so drop it both from the raw note text
    // and from the parsed metadata.
    added->note = strip_uuid_lines(matching->note);

    if (added->note) {
      if (matching->has_flags(ITEM_NOTE_ON_NEXT_LINE))
        added->add_flags(ITEM_NOTE_ON_NEXT_LINE);
    }
    if (matching->metadata) {
      added->metadata = matching->metadata;
      added->metadata->erase(_("UUID"));
      if (added->metadata->empty())
        added->metadata = none;
    }

#if DEBUG_ON
    DEBUG("draft.xact", "Setting payee from match: " << added->payee);
    if (added->code)
      DEBUG("draft.xact", "Setting code  from match: " << *added->code);
    if (added->note)
      DEBUG("draft.xact", "Setting note  from match: " << *added->note);
#endif
  } else {
    added->payee = tmpl->payee_mask.str();
    DEBUG("draft.xact", "Setting payee from template: " << added->payee);
  }

  // Template overrides for code and note take precedence over the match
  if (tmpl->code) {
    added->code = *tmpl->code;
    DEBUG("draft.xact", "Now setting code from template: " << *added->code);
  }
  if (tmpl->note) {
    added->note = tmpl->note;
    DEBUG("draft.xact", "Now setting note from template: " << *added->note);
  }

  // Phase 3: Construct postings
  if (tmpl->posts.empty()) {
    // No postings in the template -- copy all postings from the match
    if (matching) {
      DEBUG("draft.xact", "Template had no postings, copying from match");

      for (post_t* post : matching->posts) {
        added->add_post(new post_t(*post));
        added->posts.back()->set_state(item_t::UNCLEARED);
      }
    } else {
      throw_(std::runtime_error,
             _f("No accounts, and no past transaction matching '%1%'") % tmpl->payee_mask);
    }
  } else {
    DEBUG("draft.xact", "Template had postings");

    bool any_post_has_amount = false;
    for (xact_template_t::post_template_t& post : tmpl->posts) {
      if (post.amount) {
        DEBUG("draft.xact", "  and at least one has an amount specified");
        any_post_has_amount = true;
        break;
      }
    }

    for (xact_template_t::post_template_t& post : tmpl->posts) {
      unique_ptr<post_t> new_post;

      commodity_t* found_commodity = nullptr;

      // Try to find a matching posting from the historical transaction
      if (matching) {
        if (post.account_mask) {
          DEBUG("draft.xact", "Looking for matching posting based on account mask");

          for (post_t* x : matching->posts) {
            if (post.account_mask->match(x->account->fullname())) {
              new_post.reset(new post_t(*x));
              DEBUG("draft.xact", "Founding posting from line " << x->pos->beg_line);
              break;
            }
          }
        } else {
          if (post.from) {
            for (posts_list::reverse_iterator j = matching->posts.rbegin();
                 j != matching->posts.rend(); j++) {
              if ((*j)->must_balance()) {
                new_post.reset(new post_t(**j));
                DEBUG("draft.xact", "Copied last real posting from matching");
                break;
              }
            }
          } else {
            for (posts_list::iterator j = matching->posts.begin(); j != matching->posts.end();
                 j++) {
              if ((*j)->must_balance()) {
                new_post.reset(new post_t(**j));
                DEBUG("draft.xact", "Copied first real posting from matching");
                break;
              }
            }
          }
        }
      }

      if (!new_post.get()) {
        new_post.reset(new post_t);
        DEBUG("draft.xact", "New posting was NULL, creating a blank one");
      }

      // Resolve the account for the posting if not already set
      if (!new_post->account) {
        DEBUG("draft.xact", "New posting still needs an account");

        if (post.account_mask) {
          DEBUG("draft.xact", "The template has an account mask");

          account_t* acct = journal.find_account_re(post.account_mask->str());
          if (!acct)
            acct = journal.find_account(post.account_mask->str());

          // Take the default commodity (and, with no amount given, the
          // amount) from the newest posting to that account.
          for (auto j = acct->posts.rbegin(); j != acct->posts.rend(); ++j) {
            if (!(*j)->amount.is_null()) {
              new_post.reset(new post_t(**j));
              DEBUG("draft.xact", "Found account in journal postings, setting new posting");
              break;
            }
          }

          new_post->account = acct;
          DEBUG("draft.xact", "Set new posting's account to: " << acct->fullname());
        } else {
          // Default accounts when no account mask is provided
          if (post.from) {
            new_post->account = journal.find_account(_("Liabilities:Unknown"));
            DEBUG("draft.xact", "Set new posting's account to: Liabilities:Unknown");
          } else {
            new_post->account = journal.find_account(_("Expenses:Unknown"));
            DEBUG("draft.xact", "Set new posting's account to: Expenses:Unknown");
          }
        }
      }
      assert(new_post->account);

      // Handle amount and commodity assignment
      if (!new_post->amount.is_null()) {
        found_commodity = &new_post->amount.commodity().strip_annotations(keep_details_t());

        if (any_post_has_amount) {
          replace_copied_amount(*new_post, amount_t());
          DEBUG("draft.xact", "New posting has an amount, but we cleared it");
        } else {
          any_post_has_amount = true;
          DEBUG("draft.xact", "New posting has an amount, and we're using it");
        }
      }

      if (post.amount) {
        replace_copied_amount(*new_post, *post.amount);
        DEBUG("draft.xact", "Copied over posting amount");

        if (post.from) {
          new_post->amount.in_place_negate();
          DEBUG("draft.xact", "Negated new posting amount");
        }
      }

      // If the posting has a commodity from history but the template
      // amount was uncommoditized, apply the historical commodity
      if (found_commodity && !new_post->amount.is_null() && !new_post->amount.has_commodity()) {
        new_post->amount.set_commodity(*found_commodity);
        DEBUG("draft.xact", "Set posting amount commodity to: " << new_post->amount.commodity());

        new_post->amount = new_post->amount.rounded();
        DEBUG("draft.xact", "Rounded posting amount to: " << new_post->amount);
      }

      // If the amount is still uncommoditized, apply the default
      // commodity from the D directive so that the display format
      // (precision, symbol) is preserved.
      if (!new_post->amount.is_null() && !new_post->amount.has_commodity()) {
        commodity_t* default_commodity = commodity_pool_t::current_pool->default_commodity;
        if (default_commodity && *default_commodity) {
          new_post->amount.set_commodity(*default_commodity);
          DEBUG("draft.xact", "Applied default commodity: " << new_post->amount.commodity());

          new_post->amount = new_post->amount.rounded();
          DEBUG("draft.xact", "Rounded to default commodity precision: " << new_post->amount);
        }
      }

      // Apply the template cost (@ per unit, @@ in full) as the parser does.
      if (post.cost) {
        if (new_post->amount.is_null())
          throw std::runtime_error(_("Invalid xact command arguments"));
        // An amount kept from history (no template amount) still carries
        // how that posting's cost was written or computed, and its lot.
        if (!post.amount)
          replace_copied_amount(*new_post, new_post->amount.strip_annotations(keep_details_t()));
        new_post->set_given_cost(*post.cost, *post.cost_operator == "@@");
        DEBUG("draft.xact", "Set posting cost: " << *new_post->cost);
      }

      added->add_post(new_post.release());
      added->posts.back()->set_state(item_t::UNCLEARED);

      DEBUG("draft.xact", "Added new posting to derived entry");
    }
  }

  // Phase 4: Finalize and insert into the journal
  if (!journal.add_xact(added.get()))
    throw_(std::runtime_error, _("Failed to finalize derived transaction (check commodities)"));

  return added.release();
}

namespace {

/// Hides the journal's year directive while a command drafts a
/// transaction, so that its dates resolve against now (--now or the
/// clock), not the directive's year (#707), and puts it back on every
/// exit path, including a throw from draft_t or insert().
class year_directive_suspension_t : public noncopyable {
  optional<int> saved_;

public:
  year_directive_suspension_t() : saved_(year_directive_year) { year_directive_year = none; }
  ~year_directive_suspension_t() { year_directive_year = saved_; }
};

} // namespace

/*--- Command Handlers ---*/

value_t template_command(call_scope_t& args) {
  report_t& report(find_scope<report_t>(args));
  std::ostream& out(report.output_stream);

  out << _("--- Input arguments ---") << '\n';
  args.value().dump(out);
  out << '\n' << '\n';

  year_directive_suspension_t suspension; // Draft against now, not the year directive (#707).
  draft_t draft(args.value());

  out << _("--- Transaction template ---") << '\n';
  draft.dump(out);

  return true;
}

value_t xact_command(call_scope_t& args) {
  report_t& report(find_scope<report_t>(args));

  // The drafted transaction is dated against now (--now or the clock),
  // not against any year directive in the journal (#707).
  xact_t* new_xact;
  {
    year_directive_suspension_t suspension;
    draft_t draft(args.value());

    // Once insert() succeeds the generated transaction lives in the journal,
    // which owns it (and destroys it in ~journal_t); deleting it here as well
    // freed it out from under the journal, crashing later commands in the
    // REPL and the --verify teardown (issue #3244).
    new_xact = draft.insert(*report.session.journal.get());
  }

  if (new_xact) {
    // Only consider actual postings for the "xact" command
    report.HANDLER(limit_).on("#xact", "actual");

    report.xact_report(post_handler_ptr(new print_xacts(report)), *new_xact);
  }

  return true;
}

} // namespace ledger
