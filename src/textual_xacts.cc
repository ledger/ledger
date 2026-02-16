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

#include "textual_internal.h"

namespace ledger {

using detail::instance_t;
using detail::application_t;
using detail::fixed_rate_t;
using detail::parse_amount_expr;

namespace {
void check_command_has_match(parse_context_t& context, bool has_match, const string& command,
                             const mask_t& name_mask) {
  if (!has_match) {
    context.warning(_f("Automated transaction command '%1%' with mask '%2%' didn't match any "
                       "automated transaction.") %
                    command % name_mask);
  }
}
} // namespace

void instance_t::automated_xact_directive(char* line) {
  std::istream::pos_type pos = context.line_beg_pos;

  bool reveal_context = true;

  string name;
  char* p = skip_ws(line + 1);
  switch (*p) {
  case '\'':
  case '"':
  case '/': {
    char closing = *p;
    bool found_closing = false;
    for (p++; *p; p++) {
      if (*p == '\\') {
        if (*(p++) == '\0')
          throw_(parse_error, _("Unexpected '\\' at end of pattern"));
      } else if (*p == closing) {
        p++;
        found_closing = true;
        break;
      }
      name.push_back(*p);
    }
    if (!found_closing)
      throw_(parse_error, _f("Expected '%1%' at end of pattern") % closing);
    if (name.empty())
      throw_(parse_error, _("Match pattern is empty"));
    break;
  }
  default:
    for (; *p; p++) {
      if (*p == ' ' || *p == '\t')
        break;
      name.push_back(*p);
    }
  }

  p = skip_ws(p);
  string command;
  for (; *p; p++) {
    if (*p == ' ' || *p == '\t')
      break;
    command.push_back(*p);
  }

  mask_t name_mask(name);
  bool has_match = false;

  if (command == "enable" || command == "disable") {
    DEBUG("textual.autoxact", command << " automated transaction matching '" << name << "'");
    bool enabled = command == "enable";
    for (unique_ptr<auto_xact_t>& xact : context.journal->auto_xacts) {
      if (xact->name && name_mask.match(xact->name.get())) {
        DEBUG("textual.autoxact", command << "d '" << xact->name.get() << "'");
        xact->enabled = enabled;
        has_match = true;
      }
    }
    check_command_has_match(context, has_match, command, name_mask);
    return;
  } else if (command == "delete") {
    DEBUG("textual.autoxact", "deleting automated transaction matching '" << name << "'");
    auto_xacts_list::iterator it = context.journal->auto_xacts_begin();
    auto_xacts_list::iterator end = context.journal->auto_xacts_end();

    while (it != end) {
      unique_ptr<auto_xact_t>& xact = *it;
      if (xact->name && name_mask.match(xact->name.get())) {
        DEBUG("textual.autoxact", "deleted '" << xact->name.get() << "'");
        it = context.journal->auto_xacts.erase(it);
        has_match = true;
        continue;
      } else {
        it++;
      }
    }
    check_command_has_match(context, has_match, command, name_mask);
    return;
  }

  optional<string> xact_name = none;

  // if command is :: it means we are defining a new query, so we need to start
  // parsing the query at `p`
  char* query_start = line + 1;
  if (command == "::") {
    query_start = p;
    xact_name = name;
    DEBUG("textual.autoxact", "defining named autoxact '" << name << "'");
  }
  query_start = skip_ws(query_start);

  try {
    query_t query;
    keep_details_t keeper(true, true, true);
    expr_t::ptr_op_t expr =
        query.parse_args(string_value(query_start).to_sequence(), keeper, false, true);
    if (!expr) {
      throw parse_error(_("Expected predicate after '='"));
    }

    for (unique_ptr<auto_xact_t>& xact : context.journal->auto_xacts) {
      if (xact->name && name == xact->name.get()) {
        throw_(parse_error, _f("Automated transaction with name '%1%' already exists") % name);
      }
    }

    unique_ptr<auto_xact_t> ae(new auto_xact_t(predicate_t(expr, keeper), xact_name));
    ae->pos = position_t();
    ae->pos->pathname = context.pathname;
    ae->pos->beg_pos = context.line_beg_pos;
    ae->pos->beg_line = context.linenum;
    ae->pos->sequence = context.sequence++;

    post_t* last_post = NULL;

    while (peek_whitespace_line()) {
      std::streamsize len = read_line(line);
      char* p = skip_ws(line);
      if (!*p)
        break;

      const std::size_t remlen = std::strlen(p);

      if (*p == ';') {
        item_t* item;
        if (last_post)
          item = last_post;
        else
          item = ae.get();

        // This is a trailing note, and possibly a metadata info tag
        ae->append_note(p + 1, *context.scope, true);
        item->add_flags(ITEM_NOTE_ON_NEXT_LINE);
        item->pos->end_pos = context.curr_pos;
        item->pos->end_line++;
      } else if ((remlen > 7 && *p == 'a' && std::strncmp(p, "assert", 6) == 0 &&
                  std::isspace(static_cast<unsigned char>(p[6]))) ||
                 (remlen > 6 && *p == 'c' && std::strncmp(p, "check", 5) == 0 &&
                  std::isspace(static_cast<unsigned char>(p[5]))) ||
                 (remlen > 5 && *p == 'e' &&
                  ((std::strncmp(p, "expr", 4) == 0 &&
                    std::isspace(static_cast<unsigned char>(p[4]))) ||
                   (std::strncmp(p, "eval", 4) == 0 &&
                    std::isspace(static_cast<unsigned char>(p[4])))))) {
        const char c = *p;
        p = skip_ws(&p[*p == 'a' ? 6 : (*p == 'c' ? 5 : 4)]);
        if (!ae->check_exprs)
          ae->check_exprs = expr_t::check_expr_list();
        ae->check_exprs->push_back(expr_t::check_expr_pair(
            expr_t(p), c == 'a' ? expr_t::EXPR_ASSERTION
                                : (c == 'c' ? expr_t::EXPR_CHECK : expr_t::EXPR_GENERAL)));
      } else {
        reveal_context = false;

        if (post_t* post = parse_post(p, len - (p - line), top_account(), NULL, true)) {
          reveal_context = true;
          ae->add_post(post);
          ae->active_post = last_post = post;
        }
        reveal_context = true;
      }
    }

    ae->journal = context.journal;
    ae->pos->end_pos = context.curr_pos;
    ae->pos->end_line = context.linenum;

    context.journal->auto_xacts.push_back(std::move(ae));
  } catch (const std::exception&) {
    if (reveal_context) {
      add_error_context(_("While parsing automated transaction:"));
      add_error_context(source_context(context.pathname, pos, context.curr_pos, "> "));
    }
    throw;
  }
}

void instance_t::period_xact_directive(char* line) {
  std::istream::pos_type pos = context.line_beg_pos;

  bool reveal_context = true;

  try {

    unique_ptr<period_xact_t> pe(new period_xact_t(skip_ws(line + 1)));
    pe->pos = position_t();
    pe->pos->pathname = context.pathname;
    pe->pos->beg_pos = context.line_beg_pos;
    pe->pos->beg_line = context.linenum;
    pe->pos->sequence = context.sequence++;

    reveal_context = false;

    if (parse_posts(top_account(), *pe.get())) {
      reveal_context = true;
      pe->journal = context.journal;

      if (pe->finalize()) {
        context.journal->extend_xact(pe.get());
        context.journal->period_xacts.push_back(pe.get());

        pe->pos->end_pos = context.curr_pos;
        pe->pos->end_line = context.linenum;

        pe.release();
      } else {
        reveal_context = true;
        pe->journal = NULL;
        throw parse_error(_("Period transaction failed to balance"));
      }
    }

  } catch (const std::exception&) {
    if (reveal_context) {
      add_error_context(_("While parsing periodic transaction:"));
      add_error_context(source_context(context.pathname, pos, context.curr_pos, "> "));
    }
    throw;
  }
}

xact_t* instance_t::xact_directive(char* line, std::streamsize len, xact_t* previous_xact) {
  TRACE_START(xacts, 1, "Time spent handling transactions:");

  if (xact_t* xact = parse_xact(line, len, top_account(), previous_xact)) {
    unique_ptr<xact_t> manager(xact);

    if (context.journal->add_xact(xact)) {
      context.count++;
      return manager.release(); // it's owned by the journal now
    }
    // It's perfectly valid for the journal to reject the xact, which it
    // will do if the xact has no substantive effect (for example, a
    // checking xact, all of whose postings have null amounts).
  } else {
    throw parse_error(_("Failed to parse transaction"));
  }

  TRACE_STOP(xacts, 1);

  return NULL;
}

void detail::parse_amount_expr(std::istream& in, scope_t& scope, post_t& post, amount_t& amount,
                               const parse_flags_t& flags, const bool defer_expr,
                               optional<expr_t>* amount_expr) {
  expr_t expr(in, flags.plus_flags(PARSE_PARTIAL));

  DEBUG("textual.parse", "Parsed an amount expression");

  if (expr) {
    if (amount_expr)
      *amount_expr = expr;
    if (!defer_expr)
      amount = post.resolve_expr(scope, expr);
  }
}

post_t* instance_t::parse_post(char* line, std::streamsize len, account_t* account, xact_t* xact,
                               bool defer_expr) {
  TRACE_START(post_details, 1, "Time spent parsing postings:");

  unique_ptr<post_t> post(new post_t);

  post->xact = xact; // this could be NULL
  post->pos = position_t();
  post->pos->pathname = context.pathname;
  post->pos->beg_pos = context.line_beg_pos;
  post->pos->beg_line = context.linenum;
  post->pos->sequence = context.sequence++;

  std::string buf(line);
  std::streamsize beg = 0;

  try {

    // Parse the state flag

    assert(line);
    assert(*line);

    char* p = skip_ws(line);

    switch (*p) {
    case '*':
      post->set_state(item_t::CLEARED);
      p = skip_ws(p + 1);
      DEBUG("textual.parse", "line " << context.linenum << ": "
                                     << "Parsed the CLEARED flag");
      break;

    case '!':
      post->set_state(item_t::PENDING);
      p = skip_ws(p + 1);
      DEBUG("textual.parse", "line " << context.linenum << ": "
                                     << "Parsed the PENDING flag");
      break;
    }

    if (xact && (xact->_state != item_t::UNCLEARED && post->_state == item_t::UNCLEARED))
      post->set_state(xact->_state);

    // Parse the account name

    if (!*p || *p == ';')
      throw parse_error(_("Posting has no account"));

    char* next = next_element(p, true);
    char* e = p + std::strlen(p);

    while (e > p && std::isspace(static_cast<unsigned char>(*(e - 1))))
      e--;

    if ((*p == '[' && *(e - 1) == ']') || (*p == '(' && *(e - 1) == ')')) {
      post->add_flags(POST_VIRTUAL);
      DEBUG("textual.parse", "line " << context.linenum << ": "
                                     << "Parsed a virtual account name");

      if (*p == '[') {
        post->add_flags(POST_MUST_BALANCE);
        DEBUG("textual.parse", "line " << context.linenum << ": "
                                       << "Posting must balance");
      }
      p++;
      e--;
    } else if (*p == '<' && *(e - 1) == '>') {
      post->add_flags(POST_DEFERRED);
      DEBUG("textual.parse", "line " << context.linenum << ": "
                                     << "Parsed a deferred account name");
      p++;
      e--;
    }

    string name(p, static_cast<string::size_type>(e - p));
    DEBUG("textual.parse", "line " << context.linenum << ": "
                                   << "Parsed account name " << name);

    post->account = context.journal->register_account(name, post.get(), account);

    // Parse the optional amount

    if (next && *next && (*next != ';' && *next != '=')) {
      beg = static_cast<std::streamsize>(next - line);
      ptristream stream(next, static_cast<std::size_t>(len - beg));

      if (*next != '(') // indicates a value expression
        post->amount.parse(stream, PARSE_NO_REDUCE);
      else
        parse_amount_expr(stream, *context.scope, *post.get(), post->amount,
                          PARSE_NO_REDUCE | PARSE_SINGLE | PARSE_NO_ASSIGN, defer_expr,
                          &post->amount_expr);

      DEBUG("textual.parse", "line " << context.linenum << ": "
                                     << "post amount = " << post->amount);

      if (!post->amount.is_null() && post->amount.has_commodity()) {
        if (post->amount.has_annotation()) {
          post->add_flags(POST_AMOUNT_USER_ANNOTATED);
          if (post->amount.annotation().date)
            post->add_flags(POST_AMOUNT_USER_DATE);
        }

        context.journal->register_commodity(post->amount.commodity(), post.get());

        if (!post->amount.has_annotation()) {
          std::vector<fixed_rate_t> rates;
          get_applications<fixed_rate_t>(rates);
          for (fixed_rate_t& rate : rates) {
            if (*rate.first == post->amount.commodity()) {
              annotation_t details(rate.second);
              details.add_flags(ANNOTATION_PRICE_FIXATED);
              post->amount.annotate(details);
              DEBUG("textual.parse", "line " << context.linenum << ": "
                                             << "applied rate = " << post->amount);
              break;
            }
          }
        }
      }

      if (stream.eof()) {
        next = NULL;
      } else {
        next = skip_ws(next + static_cast<std::ptrdiff_t>(stream.tellg()));

        // Parse the optional cost (@ PER-UNIT-COST, @@ TOTAL-COST)

        if (*next == '@' || (*next == '(' && *(next + 1) == '@')) {
          DEBUG("textual.parse", "line " << context.linenum << ": "
                                         << "Found a price indicator");

          if (*next == '(') {
            post->add_flags(POST_COST_VIRTUAL);
            ++next;
          }

          bool per_unit = true;
          if (*++next == '@') {
            per_unit = false;
            post->add_flags(POST_COST_IN_FULL);
            DEBUG("textual.parse", "line " << context.linenum << ": "
                                           << "And it's for a total price");
            next++;
          }

          if (post->has_flags(POST_COST_VIRTUAL) && *next == ')')
            ++next;

          p = skip_ws(next);
          if (*p) {
            post->cost = amount_t();

            bool fixed_cost = false;
            if (*p == '=') {
              p++;
              fixed_cost = true;
              if (*p == '\0')
                throw parse_error(_("Posting is missing a cost amount"));
            }

            beg = static_cast<std::streamsize>(p - line);
            ptristream cstream(p, static_cast<std::size_t>(len - beg));

            if (*p != '(') // indicates a value expression
              post->cost->parse(cstream, PARSE_NO_MIGRATE);
            else
              parse_amount_expr(cstream, *context.scope, *post.get(), *post->cost,
                                PARSE_NO_MIGRATE | PARSE_SINGLE | PARSE_NO_ASSIGN);

            if (post->cost->sign() < 0)
              throw parse_error(_("A posting's cost may not be negative"));

            post->cost->in_place_unround();

            if (per_unit) {
              // For the sole case where the cost might be uncommoditized,
              // guarantee that the commodity of the cost after multiplication
              // is the same as it was before.
              commodity_t& cost_commodity(post->cost->commodity());
              *post->cost *= post->amount;
              post->cost->set_commodity(cost_commodity);
            } else if (post->amount.sign() < 0) {
              post->cost->in_place_negate();
            }

            if (fixed_cost)
              post->add_flags(POST_COST_FIXATED);

            post->given_cost = post->cost;

            DEBUG("textual.parse", "line " << context.linenum << ": "
                                           << "Total cost is " << *post->cost);
            DEBUG("textual.parse", "line " << context.linenum << ": "
                                           << "Annotated amount is " << post->amount);

            if (cstream.eof())
              next = NULL;
            else
              next = skip_ws(p + static_cast<std::ptrdiff_t>(cstream.tellg()));
          } else {
            throw parse_error(_("Expected a cost amount"));
          }
        }
      }
    }

    // Parse the optional balance assignment

    if (xact && next && *next == '=') {
      DEBUG("textual.parse", "line " << context.linenum << ": "
                                     << "Found a balance assignment indicator");

      beg = static_cast<std::streamsize>(++next - line);

      p = skip_ws(next);
      if (*p) {
        post->assigned_amount = amount_t();

        beg = static_cast<std::streamsize>(p - line);
        ptristream stream(p, static_cast<std::size_t>(len - beg));

        if (*p != '(') // indicates a value expression
          post->assigned_amount->parse(stream);
        else
          parse_amount_expr(stream, *context.scope, *post.get(), *post->assigned_amount,
                            PARSE_SINGLE | PARSE_NO_MIGRATE);

        if (post->assigned_amount->is_null()) {
          if (post->amount.is_null())
            throw parse_error(_("Balance assignment must evaluate to a constant"));
          else
            throw parse_error(_("Balance assertion must evaluate to a constant"));
        }

        DEBUG("textual.parse",
              "line " << context.linenum << ": "
                      << "POST assign: parsed balance amount = " << *post->assigned_amount);

        const amount_t& amt(*post->assigned_amount);

        // When the assertion/assignment amount has lot annotations,
        // preserve them in the comparison so that annotated balance
        // assertions like "= 1.00 XXX {=1.01 USD}" work correctly
        // (fixes #2318, #2355).  When the amount has no annotations,
        // strip annotations from the account total so that assertions
        // like "= 2 AAA" match regardless of lot cost basis or date
        // (the behavior that bug #1055 depends on).
        bool strip = !amt.has_annotation();

        value_t account_total(
            post->account
                ->self_total(!(post->has_flags(POST_VIRTUAL) || post->has_flags(POST_IS_TIMELOG))));
        if (strip)
          account_total = account_total.strip_annotations(keep_details_t());

        DEBUG("post.assign", "line " << context.linenum << ": "
                                     << "account balance = " << account_total);
        DEBUG("post.assign", "line " << context.linenum << ": "
                                     << "post amount = " << post->amount
                                     << " (is_zero = " << post->amount.is_zero() << ")");
        DEBUG("post.assign", "line " << context.linenum << ": "
                                     << "post assertion = " << amt
                                     << " (is_zero = " << amt.is_zero() << ")");

        balance_t diff = amt;

        switch (account_total.type()) {
        case value_t::AMOUNT: {
          amount_t acct_amt(account_total.as_amount());
          diff -= acct_amt.reduced();
          DEBUG("textual.parse", "line " << context.linenum << ": "
                                         << "Subtracting amount " << acct_amt << " from diff, yielding "
                                         << diff);
          break;
        }
        case value_t::BALANCE: {
          balance_t bal(account_total.as_balance());
          diff -= bal;
          DEBUG("textual.parse", "line " << context.linenum << ": "
                                         << "Subtracting balance " << bal << " from diff, yielding "
                                         << diff);
          break;
        }
        default:
          break;
        }

        DEBUG("post.assign", "line " << context.linenum << ": " << "diff = " << diff);
        DEBUG("textual.parse", "line " << context.linenum << ": "
                                       << "POST assign: diff = " << diff);

        // Subtract amounts from previous posts to this account in the xact.
        for (post_t* p : xact->posts) {
          if (p->account == post->account &&
              ((p->has_flags(POST_VIRTUAL) || p->has_flags(POST_IS_TIMELOG)) ==
               (post->has_flags(POST_VIRTUAL) || post->has_flags(POST_IS_TIMELOG)))) {
            amount_t p_amt(strip ? p->amount.strip_annotations(keep_details_t()) : p->amount);
            diff -= p_amt;
            DEBUG("textual.parse", "line " << context.linenum << ": "
                                           << "Subtracting " << p_amt << ", diff = " << diff);
          }
        }

        // If amt has a commodity, restrict balancing to that. Otherwise, it's the blanket '0' and
        // check that all of them are zero.
        if (amt.has_commodity()) {
          DEBUG("textual.parse", "line " << context.linenum << ": "
                                         << "Finding commodity " << amt.commodity() << " (" << amt
                                         << ") in balance " << diff);
          optional<amount_t> wanted_commodity = diff.commodity_amount(amt.commodity());
          if (!wanted_commodity) {
            diff = amt - amt; // this is '0' with the correct commodity.
          } else {
            diff = *wanted_commodity;
          }
          DEBUG("textual.parse", "line " << context.linenum << ": "
                                         << "Diff is now " << diff);
        }

        if (post->amount.is_null()) {
          // balance assignment
          if (!diff.is_zero()) {
            if (strip) {
              // Recompute the diff preserving lot annotations (cost basis
              // and lot date) so that the assigned amount retains them.
              value_t ann_total(
                  post->account
                      ->self_total(!(post->has_flags(POST_VIRTUAL) ||
                                     post->has_flags(POST_IS_TIMELOG))));
              balance_t ann_diff = amt;
              switch (ann_total.type()) {
              case value_t::AMOUNT:
                ann_diff -= ann_total.as_amount().reduced();
                break;
              case value_t::BALANCE:
                ann_diff -= ann_total.as_balance();
                break;
              default:
                break;
              }
              for (post_t* p : xact->posts) {
                if (p->account == post->account &&
                    ((p->has_flags(POST_VIRTUAL) || p->has_flags(POST_IS_TIMELOG)) ==
                     (post->has_flags(POST_VIRTUAL) || post->has_flags(POST_IS_TIMELOG)))) {
                  ann_diff -= p->amount;
                }
              }
              // Use annotated diff if it can be represented as a single
              // amount; fall back to the stripped diff otherwise (e.g.
              // when multiple lots with different annotations exist).
              try {
                post->amount = ann_diff.to_amount();
              } catch (...) {
                post->amount = diff.to_amount();
              }
            } else {
              // Diff already has annotations; use it directly.
              try {
                post->amount = diff.to_amount();
              } catch (...) {
                post->amount = diff.strip_annotations(keep_details_t()).to_amount();
              }
            }
            DEBUG("textual.parse", "line " << context.linenum << ": "
                                           << "Overwrite null posting with " << post->amount);
          } else {
            post->amount = amt - amt; // this is '0' with the correct commodity.
            DEBUG("textual.parse", "line " << context.linenum << ": "
                                           << "Overwrite null posting with zero diff with "
                                           << amt - amt);
          }
          post->add_flags(POST_CALCULATED);
        } else {
          // balance assertion
          amount_t post_amt(strip ? post->amount.reduced().strip_annotations(keep_details_t())
                                  : post->amount.reduced());
          diff -= post_amt;
          if (!no_assertions && !diff.is_zero()) {
            balance_t tot = strip ? (-diff + amt).strip_annotations(keep_details_t()) : (-diff + amt);
            DEBUG("textual.parse",
                  "Balance assertion: off by " << diff << " (expected to see " << tot << ")");
            throw_(parse_error, _f("Balance assertion off by %1% (expected to see %2%)") %
                                    diff.to_string() % tot.to_string());
          }
        }

        if (stream.eof())
          next = NULL;
        else
          next = skip_ws(p + static_cast<std::ptrdiff_t>(stream.tellg()));
      } else {
        throw parse_error(_("Expected an balance assignment/assertion amount"));
      }
    }

    // Parse the optional note

    if (next && *next == ';') {
      post->append_note(++next, *context.scope, true);
      next = line + len;
      DEBUG("textual.parse", "line " << context.linenum << ": "
                                     << "Parsed a posting note");
    }

    // There should be nothing more to read

    if (next && *next)
      throw_(parse_error,
             _f("Unexpected char '%1%' (Note: inline math requires parentheses)") % *next);

    post->pos->end_pos = context.curr_pos;
    post->pos->end_line = context.linenum;

    std::vector<string> tags;
    get_applications<string>(tags);
    for (string& tag : tags)
      post->parse_tags(tag.c_str(), *context.scope, true);

    string post_payee = post->payee_from_tag();
    if (post_payee != "")
      post->set_payee(context.journal->validate_payee(post_payee));

    TRACE_STOP(post_details, 1);

    return post.release();

  } catch (const std::exception&) {
    add_error_context(_("While parsing posting:"));
    add_error_context(line_context(buf, static_cast<string::size_type>(beg),
                                   static_cast<string::size_type>(len)));
    throw;
  }
}

bool instance_t::parse_posts(account_t* account, xact_base_t& xact, const bool defer_expr) {
  TRACE_START(xact_posts, 1, "Time spent parsing postings:");

  bool added = false;

  while (peek_whitespace_line()) {
    char* line;
    std::streamsize len = read_line(line);
    char* p = skip_ws(line);
    if (*p != ';') {
      if (post_t* post = parse_post(line, len, account, NULL, defer_expr)) {
        xact.add_post(post);
        added = true;
      }
    }
  }

  TRACE_STOP(xact_posts, 1);

  return added;
}

xact_t* instance_t::parse_xact(char* line, std::streamsize len, account_t* account,
                               xact_t* previous_xact) {
  TRACE_START(xact_text, 1, "Time spent parsing transaction text:");

  unique_ptr<xact_t> xact(new xact_t);

  xact->pos = position_t();
  xact->pos->pathname = context.pathname;
  xact->pos->beg_pos = context.line_beg_pos;
  xact->pos->beg_line = context.linenum;
  xact->pos->sequence = context.sequence++;

  bool reveal_context = true;

  try {

    // Parse the date

    char* next = next_element(line);

    if (char* p = std::strchr(line, '=')) {
      *p++ = '\0';
      xact->_date_aux = parse_date(p);
    }
    xact->_date = parse_date(line);

    // Parse the optional cleared flag: *

    if (next) {
      switch (*next) {
      case '*':
        xact->_state = item_t::CLEARED;
        next = skip_ws(++next);
        break;
      case '!':
        xact->_state = item_t::PENDING;
        next = skip_ws(++next);
        break;
      }
    }

    // Parse the optional code: (TEXT)

    if (next && *next == '(') {
      if (char* p = std::strchr(next++, ')')) {
        *p++ = '\0';
        xact->code = next;
        next = skip_ws(p);
      }
    }

    // Parse the description text

    if (next && *next) {
      char* p = next;
      std::size_t spaces = 0;
      std::size_t tabs = 0;
      while (*p) {
        if (*p == ' ') {
          ++spaces;
        } else if (*p == '\t') {
          ++tabs;
        } else if (*p == ';' && (tabs > 0 || spaces > 1)) {
          char* q = p - 1;
          while (q > next && std::isspace(static_cast<unsigned char>(*q)))
            --q;
          if (q >= next)
            *(q + 1) = '\0';
          break;
        } else {
          spaces = 0;
          tabs = 0;
        }
        ++p;
      }
      xact->payee = context.journal->validate_payee(next);
      next = p;
    } else {
      xact->payee = _("<Unspecified payee>");
    }

    // Parse the xact note

    if (next && *next == ';')
      xact->append_note(++next, *context.scope, false);

    TRACE_STOP(xact_text, 1);

    // Parse all of the posts associated with this xact

    TRACE_START(xact_details, 1, "Time spent parsing transaction details:");

    post_t* last_post = NULL;

    while (peek_whitespace_line()) {
      len = read_line(line);
      char* p = skip_ws(line);
      if (!*p)
        break;

      const std::size_t remlen = std::strlen(p);

      item_t* item;
      if (last_post)
        item = last_post;
      else
        item = xact.get();

      if (*p == ';') {
        // This is a trailing note, and possibly a metadata info tag
        item->append_note(p + 1, *context.scope, true);
        item->add_flags(ITEM_NOTE_ON_NEXT_LINE);
        item->pos->end_pos = context.curr_pos;
        item->pos->end_line++;
      } else if ((remlen > 7 && *p == 'a' && std::strncmp(p, "assert", 6) == 0 &&
                  std::isspace(static_cast<unsigned char>(p[6]))) ||
                 (remlen > 6 && *p == 'c' && std::strncmp(p, "check", 5) == 0 &&
                  std::isspace(static_cast<unsigned char>(p[5]))) ||
                 (remlen > 5 && *p == 'e' && std::strncmp(p, "expr", 4) == 0 &&
                  std::isspace(static_cast<unsigned char>(p[4])))) {
        const char c = *p;
        p = skip_ws(&p[*p == 'a' ? 6 : (*p == 'c' ? 5 : 4)]);
        expr_t expr(p);
        bind_scope_t bound_scope(*context.scope, *item);
        if (c == 'e') {
          expr.calc(bound_scope);
        } else if (!expr.calc(bound_scope).to_boolean()) {
          if (c == 'a') {
            throw_(parse_error, _f("Transaction assertion failed: %1%") % p);
          } else {
            context.warning(_f("Transaction check failed: %1%") % p);
          }
        }
      } else {
        reveal_context = false;

        if (!last_post) {
          if (xact->has_tag(_("UUID"))) {
            string uuid = xact->get_tag(_("UUID"))->to_string();
            auto it = context.journal->payee_uuid_mappings.find(uuid);
            if (it != context.journal->payee_uuid_mappings.end()) {
              xact->payee = it->second;
            }
          }
        }

        if (post_t* post = parse_post(p, len - (p - line), account, xact.get())) {
          reveal_context = true;
          xact->add_post(post);
          last_post = post;
        }
        reveal_context = true;
      }
    }

#if 0
  if (xact->_state == item_t::UNCLEARED) {
    item_t::application_t result = item_t::CLEARED;

    for (post_t * post : xact->posts) {
      if (post->_state == item_t::UNCLEARED) {
        result = item_t::UNCLEARED;
        break;
      }
      else if (post->_state == item_t::PENDING) {
        result = item_t::PENDING;
      }
    }
  }
#endif

    xact->pos->end_pos = context.curr_pos;
    xact->pos->end_line = context.linenum;

    std::vector<string> tags;
    get_applications<string>(tags);
    for (string& tag : tags)
      xact->parse_tags(tag.c_str(), *context.scope, false);

    TRACE_STOP(xact_details, 1);

    if (hash_type != NO_HASHES) {
      string expected_hash = xact->hash(previous_xact && previous_xact->has_tag("Hash")
                                            ? previous_xact->get_tag("Hash")->to_string()
                                            : "",
                                        hash_type);
      if (xact->has_tag("Hash")) {
        string current_hash = xact->get_tag("Hash")->to_string();
        if (!std::equal(expected_hash.begin(),
                        expected_hash.begin() + std::min(expected_hash.size(), current_hash.size()),
                        current_hash.begin()))
          throw_(parse_error, _f("Expected hash %1% != %2%") % expected_hash % current_hash);
      } else {
        xact->set_tag("Hash", string_value(expected_hash));
      }
    }

    return xact.release();

  } catch (const std::exception&) {
    if (reveal_context) {
      add_error_context(_("While parsing transaction:"));
      add_error_context(
          source_context(xact->pos->pathname, xact->pos->beg_pos, context.curr_pos, "> "));
    }
    throw;
  }
}

} // namespace ledger
