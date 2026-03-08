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
 * @file   filters.cc
 * @author John Wiegley
 *
 * @ingroup report
 *
 * @brief Implementations of the filter handlers declared in filters.h.
 *
 * Each filter in the posting pipeline implements operator()(post_t&) to
 * process incoming postings and flush() to emit accumulated results.
 * This file is organized by filter category, matching the inventory in
 * the filters.h module documentation.
 */

#include <system.hh>
#include <utility>

#include "filters.h"
#include "iterators.h"
#include "journal.h"
#include "report.h"
#include "compare.h"
#include "pool.h"

namespace ledger {

/*--- Collection and Splitting ---*/

/// Print a group title to the output handler (unless --no-titles is active).
void post_splitter::print_title(const value_t& val) {
  if (!report.HANDLED(no_titles)) {
    std::ostringstream buf;
    val.print(buf);
    post_chain->title(buf.str());
  }
}

/**
 * Flush all accumulated groups through the post_chain.
 *
 * For each group (keyed by group_by_expr result), the preflush callback is
 * invoked (typically to print a title), then all postings in the group are
 * forwarded through the post_chain, which is flushed and optionally cleared
 * (unless --group-by-cumulative is active, which preserves running totals
 * across groups).
 */
void post_splitter::flush() {
  bool cumulative = report.HANDLED(group_by_cumulative);

  for (value_to_posts_map::value_type& pair : posts_map) {
    preflush_func(pair.first);

    for (post_t* post : pair.second)
      (*post_chain)(*post);

    post_chain->flush();

    // In cumulative mode, don't clear the chain state between groups
    // so that account totals accumulate across groups.
    if (!cumulative)
      post_chain->clear();

    if (postflush_func)
      (*postflush_func)(pair.first);
  }
}

/// Evaluate group_by_expr and insert the posting into the appropriate bucket.
void post_splitter::operator()(post_t& post) {
  bind_scope_t bound_scope(report, post);
  value_t result(group_by_expr.calc(bound_scope));

  if (!result.is_null()) {
    value_to_posts_map::iterator i = posts_map.find(result);
    if (i != posts_map.end()) {
      (*i).second.push_back(&post);
    } else {
      std::pair<value_to_posts_map::iterator, bool> inserted =
          posts_map.insert(value_to_posts_map::value_type(result, posts_list()));
      assert(inserted.second);
      (*inserted.first).second.push_back(&post);
    }
  }
}

/*--- Truncation ---*/

/**
 * Flush accumulated postings, forwarding only those within the head/tail
 * window.
 *
 * First counts the total number of distinct transactions, then iterates
 * through all accumulated postings.  A posting is forwarded if its
 * transaction index falls within the head_count window from the start
 * and/or the tail_count window from the end.  Negative counts invert
 * the selection (e.g., head_count=-2 skips the first 2 transactions).
 */
void truncate_xacts::flush() {
  if (!posts.size())
    return;

  xact_t* xact = (*posts.begin())->xact;

  int l = 0;
  for (post_t* post : posts)
    if (xact != post->xact) {
      l++;
      xact = post->xact;
    }
  l++;

  xact = (*posts.begin())->xact;

  int i = 0;
  for (post_t* post : posts) {
    if (xact != post->xact) {
      xact = post->xact;
      i++;
    }

    bool print = false;
    if (head_count) {
      if (head_count > 0 && i < head_count)
        print = true; // NOLINT(bugprone-branch-clone)
      else if (head_count < 0 && i >= -head_count)
        print = true;
    }

    if (!print && tail_count) {
      if (tail_count > 0 && l - i <= tail_count)
        print = true; // NOLINT(bugprone-branch-clone)
      else if (tail_count < 0 && l - i > -tail_count)
        print = true;
    }

    if (print)
      item_handler<post_t>::operator()(*post);
  }
  posts.clear();

  item_handler<post_t>::flush();
}

/**
 * Accumulate postings and track transaction boundaries.
 *
 * When only --head is active (no --tail), an early termination optimization
 * triggers flush() as soon as head_count transactions have been seen,
 * avoiding accumulation of the entire posting set.
 */
void truncate_xacts::operator()(post_t& post) {
  if (completed)
    return;

  if (last_xact != post.xact) {
    if (last_xact)
      xacts_seen++;
    last_xact = post.xact;
  }

  if (tail_count == 0 && head_count > 0 && static_cast<int>(xacts_seen) >= head_count) {
    flush();
    completed = true;
    return;
  }

  posts.push_back(&post);
}

/*--- Sorting ---*/

/**
 * Sort accumulated postings by sort_order and forward them downstream.
 *
 * Uses std::stable_sort with compare_items to preserve the relative order
 * of postings with equal sort keys.  After sorting, the POST_EXT_SORT_CALC
 * flag is cleared from each posting's xdata to allow re-evaluation by
 * downstream handlers.
 */
void sort_posts::post_accumulated_posts() {
  std::stable_sort(posts.begin(), posts.end(), compare_items<post_t>(sort_order, report));

  for (post_t* post : posts) {
    post->xdata().drop_flags(POST_EXT_SORT_CALC);
    item_handler<post_t>::operator()(*post);
  }

  posts.clear();
}

/*--- Helper utilities ---*/

namespace {
/// Split a string by a delimiter character into a list of substrings.
void split_string(const string& str, const char ch, std::list<string>& strings) {
  const char* b = str.c_str();
  for (const char* p = b; *p; p++) {
    if (*p == ch) {
      strings.push_back(string(b, static_cast<std::string::size_type>(p - b)));
      b = p + 1;
    }
  }
  strings.push_back(string(b));
}

/// Create or find a temporary account from a list of path components.
account_t* create_temp_account_from_path(std::list<string>& account_names, temporaries_t& temps,
                                         account_t* master) {
  account_t* new_account = nullptr;
  for (const string& name : account_names) {
    if (new_account) {
      new_account = new_account->find_account(name);
    } else {
      new_account = master->find_account(name, false);
      if (!new_account)
        new_account = &temps.create_account(name, master);
    }
  }

  assert(new_account != nullptr);
  return new_account;
}
} // namespace

/*--- Anonymization ---*/

/**
 * Replace a commodity symbol with a sequential letter label (A, B, C, ..., AA, AB, ...).
 *
 * The mapping is stable: the same original commodity always maps to the same
 * label within a single anonymization pass.  Commodity flags and precision
 * are preserved from the original.
 */
void anonymize_posts::render_commodity(amount_t& amt) {
  commodity_t& comm(amt.commodity());

  std::size_t id;
  bool newly_added = false;

  commodity_index_map::iterator i = comms.find(&comm);
  if (i == comms.end()) {
    id = next_comm_id++;
    newly_added = true;
    comms.insert(commodity_index_map::value_type(&comm, id));
  } else {
    id = (*i).second;
  }

  std::ostringstream buf;
  do {
    buf << static_cast<char>('A' + (id % 26));
    id /= 26;
  } while (id > 0);

  if (amt.has_annotation())
    amt.set_commodity(*commodity_pool_t::current_pool->find_or_create(buf.str(), amt.annotation()));
  else
    amt.set_commodity(*commodity_pool_t::current_pool->find_or_create(buf.str()));

  if (newly_added) {
    amt.commodity().set_flags(comm.flags());
    amt.commodity().set_precision(comm.precision());
  }
}

/**
 * Anonymize a posting by replacing its payee, account names, and commodity
 * symbols with hashed/sequential substitutes.
 *
 * A new temporary transaction is created for each distinct xact, with the
 * payee replaced by a SHA1 hash.  Account names at each level are similarly
 * hashed.  Amounts retain their numeric value but receive relabeled
 * commodities.  Notes, codes, and tags are stripped entirely.
 */
void anonymize_posts::operator()(post_t& post) {
  bool copy_xact_details = false;

  if (last_xact != post.xact) {
    temps.copy_xact(*post.xact);
    last_xact = post.xact;
    copy_xact_details = true;
  }
  xact_t& xact = temps.last_xact();
  xact.code = std::nullopt;

  if (copy_xact_details) {
    xact.copy_details(*post.xact);

    std::ostringstream buf;
    buf << reinterpret_cast<boost::uintmax_t>(post.xact->payee.c_str()) << integer_gen()
        << post.xact->payee.c_str();
    xact.payee = sha1sum(buf.str(), 8);
    xact.note = none;
  } else {
    xact.journal = post.xact->journal;
  }

  std::list<string> account_names;

  for (account_t* acct = post.account; acct; acct = acct->parent) {
    std::ostringstream buf;
    buf << integer_gen() << acct << acct->fullname();
    account_names.push_front(sha1sum(buf.str(), 8));
  }

  account_t* new_account =
      create_temp_account_from_path(account_names, temps, xact.journal->master);
  post_t& temp = temps.copy_post(post, xact, new_account);
  temp.note = none;
  temp.add_flags(POST_ANONYMIZED);

  render_commodity(temp.amount);
  if (temp.amount.has_annotation()) {
    temp.amount.annotation().tag = std::nullopt;
    if (temp.amount.annotation().price)
      render_commodity(*temp.amount.annotation().price);
  }

  if (temp.cost)
    render_commodity(*temp.cost);
  if (temp.assigned_amount)
    render_commodity(*temp.assigned_amount);

  (*handler)(temp);
}

/*--- Calculation ---*/

/**
 * Evaluate the amount expression for this posting and update running totals.
 *
 * For each posting, this handler:
 * 1. Propagates the running total and sequence count from the previous posting.
 * 2. Evaluates amount_expr and stores the result in xdata.visited_value.
 * 3. Marks the posting and its account as visited.
 * 4. If calc_running_total is true, adds the visited_value to the running total.
 * 5. If maintain_stripped_total is true, incrementally maintains a pre-stripped
 *    display total (O(1) per posting) for downstream display_filter_posts.
 */
void calc_posts::operator()(post_t& post) {
  post_t::xdata_t& xdata(post.xdata());

  if (last_post) {
    assert(last_post->has_xdata());
    if (calc_running_total)
      xdata.total = last_post->xdata().total;
    xdata.count = last_post->xdata().count + 1;
  } else {
    xdata.count = 1;
  }

  post.add_to_value(xdata.visited_value, amount_expr);
  xdata.add_flags(POST_EXT_VISITED);

  account_t* acct = post.reported_account();
  acct->xdata().add_flags(ACCOUNT_EXT_VISITED);

  if (calc_running_total) {
    add_or_set_value(acct->xdata().running_total, xdata.visited_value);
    add_or_set_value(xdata.total, xdata.visited_value);

    // Incrementally maintain a stripped display total alongside the
    // regular total.  Instead of stripping the entire running total
    // (O(K) per posting where K = annotated commodities), we strip
    // only this posting's visited_value (O(1)) and add it to the
    // previous stripped total.  This relies on strip_annotations
    // being distributive over balance addition.
    if (maintain_stripped_total) {
      if (last_post && last_post->xdata().has_flags(POST_EXT_DISPLAY_TOTAL_CACHED))
        xdata.display_total = last_post->xdata().display_total;

      value_t stripped_value = xdata.visited_value.strip_annotations(what_to_keep);
      add_or_set_value(xdata.display_total, stripped_value);
      xdata.add_flags(POST_EXT_DISPLAY_TOTAL_CACHED);
    }
  }

  item_handler<post_t>::operator()(post);

  last_post = &post;
}

/*--- Synthetic posting generation helper ---*/

namespace {
/**
 * Create a synthetic (generated) posting and forward it through the handler.
 *
 * This helper is used by multiple filters (collapse_posts, subtotal_posts,
 * changed_value_posts, display_filter_posts, posts_as_equity) to create
 * temporary postings that represent computed values rather than journal entries.
 * The posting is attached to the given xact and account, with its amount set
 * from the value parameter.  Balance and sequence values are stored as compound
 * values in xdata.
 *
 * @param value            The amount/balance to assign to the posting.
 * @param account          The account for the synthetic posting.
 * @param xact             The transaction to attach the posting to.
 * @param temps            Temporary storage owning the posting's lifetime.
 * @param handler          The downstream handler to receive the posting.
 * @param date             Optional date override for the posting.
 * @param act_date_p       If true, date is an actual date; if false, a value date.
 * @param total            Optional total to store in xdata.
 * @param direct_amount    If true, mark as POST_EXT_DIRECT_AMT.
 * @param mark_visited     If true, mark posting and account as visited.
 * @param bidir_link       If true, create bidirectional xact/post links.
 * @param source_post      Optional source posting to copy metadata from.
 * @param force_virtual    If true, force the posting to be virtual.
 * @param force_must_balance If true, force virtual posting to require balance.
 */
void handle_value(const value_t& value, account_t* account, xact_t* xact, temporaries_t& temps,
                  const post_handler_ptr& handler, const date_t& date = date_t(),
                  const bool act_date_p = true, const value_t& total = value_t(),
                  const bool direct_amount = false, const bool mark_visited = false,
                  const bool bidir_link = true, post_t* source_post = nullptr,
                  const bool force_virtual = false, const bool force_must_balance = false) {
  post_t& post = temps.create_post(*xact, account, bidir_link);
  post.add_flags(ITEM_GENERATED);

  // Copy tags/metadata from the source posting so that revaluation posts
  // inherit tags and can be properly filtered by predicates like --limit
  if (source_post && source_post->metadata)
    post.metadata = source_post->metadata;

  // If the caller explicitly specifies that this posting is virtual (e.g.,
  // when subtotal_posts accumulates virtual and non-virtual postings for the
  // same account as separate entries), use that flag directly.  Otherwise,
  // infer virtual status from account xdata flags as before.
  if (force_virtual) {
    post.add_flags(POST_VIRTUAL);
    if (force_must_balance)
      post.add_flags(POST_MUST_BALANCE);
  } else if (account && account->has_xdata() &&
             account->xdata().has_flags(ACCOUNT_EXT_AUTO_VIRTUALIZE)) {
    if (!account->xdata().has_flags(ACCOUNT_EXT_HAS_NON_VIRTUALS)) {
      post.add_flags(POST_VIRTUAL);
      if (!account->xdata().has_flags(ACCOUNT_EXT_HAS_UNB_VIRTUALS))
        post.add_flags(POST_MUST_BALANCE);
    }
  }

  post_t::xdata_t& xdata(post.xdata());

  if (is_valid(date)) {
    if (act_date_p)
      xdata.date = date;
    else
      xdata.value_date = date;
  }

  value_t temp(value);

  switch (value.type()) {
  case value_t::BOOLEAN:
  case value_t::INTEGER:
    temp.in_place_cast(value_t::AMOUNT);
    [[fallthrough]];

  case value_t::AMOUNT:
    post.amount = temp.as_amount();
    break;

  case value_t::BALANCE:
  case value_t::SEQUENCE:
    xdata.compound_value = temp;
    xdata.add_flags(POST_EXT_COMPOUND);
    break;

  case value_t::DATETIME:
  case value_t::DATE:
  default:
    assert(false);
    break;
  }

  if (!total.is_null())
    xdata.total = total;

  if (direct_amount)
    xdata.add_flags(POST_EXT_DIRECT_AMT);

  DEBUG("filters.changed_value.rounding", "post.amount = " << post.amount);

  (*handler)(post);

  if (mark_visited) {
    post.xdata().add_flags(POST_EXT_VISITED);
    post.account->xdata().add_flags(ACCOUNT_EXT_VISITED);
  }
}
} // namespace

/*--- Collapsing ---*/

/**
 * Emit the collapsed representation of the current transaction's postings.
 *
 * The behavior depends on flags and counts:
 * - **--collapse-if-zero with non-zero subtotal**: pass all component postings
 *   through uncollapsed.
 * - **Regular collapse, single displayed post**: optimize by passing the
 *   original posting through directly.
 * - **Regular collapse, multiple posts**: create synthetic postings with
 *   per-account totals (or grouped by date+account when --depth with --effective).
 * - **--collapse-if-zero with zero subtotal**: suppress entirely.
 */
void collapse_posts::report_subtotal() {
  if (!count)
    return;

  std::size_t displayed_count = 0;
  for (post_t* post : component_posts) {
    bind_scope_t bound_scope(report, *post);
    if (only_predicate(bound_scope) && display_predicate(bound_scope))
      displayed_count++;
  }

  // NOLINTBEGIN(bugprone-branch-clone)
  if (only_collapse_if_zero && !subtotal.is_zero()) {
    // --collapse-if-zero with non-zero subtotal: pass all component posts
    // through uncollapsed.  Must be checked before the displayed_count == 1
    // optimisation because display_predicate evaluates on posts that have not
    // yet had their running totals computed by calc_posts, so the predicate
    // result is unreliable at this point.
    for (post_t* post : component_posts)
      item_handler<post_t>::operator()(*post);
  } else if (!only_collapse_if_zero && collapse_depth == 0 && displayed_count == 1) {
    // Regular --collapse optimisation: only one post matched the display
    // predicate, so pass it through directly rather than building a synthetic
    // posting.
    item_handler<post_t>::operator()(*last_post);
  } else if (!only_collapse_if_zero) {
    // Regular --collapse: collapse all component posts into synthetic
    // posting(s).
    if (collapse_depth > 0 && item_t::use_aux_date) {
      // When using --depth with --effective, group posts by both the
      // depth-truncated account and effective date, so that postings with
      // different effective dates are not incorrectly collapsed together.
      typedef std::pair<date_t, account_t*> date_acct_key;
      typedef std::map<date_acct_key, value_t> grouped_totals_map;
      grouped_totals_map grouped;

      for (post_t* post : component_posts) {
        account_t* acct = post->account;
        while (acct->depth > collapse_depth && acct->parent)
          acct = acct->parent;

        date_t d = post->date();
        date_acct_key key(d, acct);
        post->add_to_value(grouped[key], amount_expr);
      }

      for (const grouped_totals_map::value_type& entry : grouped) {
        date_t d = entry.first.first;
        account_t* acct = entry.first.second;

        xact_t& xact = temps.create_xact();
        xact.payee = last_xact->payee;
        xact._date = d;

        handle_value(/* value=      */ entry.second,
                     /* account=    */ acct,
                     /* xact=       */ &xact,
                     /* temps=      */ temps,
                     /* handler=    */ handler,
                     /* date=       */ d,
                     /* act_date_p= */ true);
      }
    } else {
      date_t earliest_date;
      date_t latest_date;

      for (post_t* post : component_posts) {
        date_t date = post->date();
        date_t value_date = post->value_date();
        if (!is_valid(earliest_date) || date < earliest_date)
          earliest_date = date;
        if (!is_valid(latest_date) || value_date > latest_date)
          latest_date = value_date;
      }

      xact_t& xact = temps.create_xact();
      xact.payee = last_xact->payee;
      xact._date = (is_valid(earliest_date) ? earliest_date : last_xact->_date);

      DEBUG("filters.collapse", "Pseudo-xact date = " << *xact._date);
      DEBUG("filters.collapse", "earliest date    = " << earliest_date);
      DEBUG("filters.collapse", "latest date      = " << latest_date);

      for (totals_map::value_type& pat : totals) {
        handle_value(/* value=      */ pat.second,
                     /* account=    */ pat.first,
                     /* xact=       */ &xact,
                     /* temps=      */ temps,
                     /* handler=    */ handler,
                     /* date=       */ latest_date,
                     /* act_date_p= */ false);
      }
    }
  }
  // NOLINTEND(bugprone-branch-clone)
  // When only_collapse_if_zero && subtotal.is_zero(): suppress entirely
  // (fall through to cleanup below).

  totals.clear();
  component_posts.clear();

  last_xact = nullptr;
  last_post = nullptr;
  subtotal = 0L;
  count = 0;
}

/// Find the totals bucket for an account, walking up to collapse_depth.
value_t& collapse_posts::find_totals(account_t* account) {
  if (collapse_depth == 0)
    return totals[global_totals_account];

  if (account->depth <= collapse_depth)
    return totals[account];

  // else recurse
  return find_totals(account->parent);
}

/**
 * Accumulate a posting into the current transaction's collapse group.
 *
 * When a new transaction boundary is detected, the previous transaction's
 * accumulated postings are flushed via report_subtotal().
 */
void collapse_posts::operator()(post_t& post) {
  // If we've reached a new xact, report on the subtotal
  // accumulated thus far.

  if (last_xact != post.xact && count > 0)
    report_subtotal();

  post.add_to_value(subtotal, amount_expr);
  post.add_to_value(find_totals(post.account), amount_expr);

  component_posts.push_back(&post);

  last_xact = post.xact;
  last_post = &post;
  count++;
}

/*--- Related posts ---*/

/**
 * For each collected posting, forward the sibling postings from its transaction.
 *
 * A posting is forwarded if it has not been handled yet and either:
 * - It was not originally received (it is a "related" sibling), or
 * - also_matching is true (include the originally matched postings too).
 * Generated postings are excluded unless they are calculated (POST_CALCULATED).
 */
void related_posts::flush() {
  if (posts.size() > 0) {
    for (post_t* post : posts) {
      assert(post->xact);
      for (post_t* r_post : post->xact->posts) {
        post_t::xdata_t& xdata(r_post->xdata());
        if (!xdata.has_flags(POST_EXT_HANDLED) &&
            (!xdata.has_flags(POST_EXT_RECEIVED)
                 ? (!r_post->has_flags(ITEM_GENERATED) || r_post->has_flags(POST_CALCULATED) ||
                    also_matching)
                 : also_matching)) {
          xdata.add_flags(POST_EXT_HANDLED);
          item_handler<post_t>::operator()(*r_post);
        }
      }
    }
  }

  item_handler<post_t>::flush();
}

/*--- Display filtering and rounding ---*/

display_filter_posts::display_filter_posts(post_handler_ptr handler, report_t& _report,
                                           bool _show_rounding)
    : item_handler<post_t>(std::move(handler)), report(_report),
      display_amount_expr(report.HANDLER(display_amount_).expr),
      display_total_expr(report.HANDLER(display_total_).expr), show_rounding(_show_rounding),
      has_stripped_cache(false), what_to_keep(report.what_to_keep()) {
  // The incremental stripped total optimization is only safe when the
  // display_total expression is the simple running total accumulation
  // (i.e., display_total_ = total_expr, total_ = total).  Options like
  // --market, --exchange, --average, etc. modify these expressions,
  // breaking the distributive property: strip(f(total)) != f(strip(total)).
  incremental_strip_eligible = report.HANDLER(display_total_).expr.exprs.empty() &&
                               report.HANDLER(display_total_).expr.base_expr == "total_expr" &&
                               report.HANDLER(total_).expr.exprs.empty() &&
                               report.HANDLER(total_).expr.base_expr == "total" &&
                               !what_to_keep.keep_all();
  create_accounts();
  TRACE_CTOR(display_filter_posts, "post_handler_ptr, report_t&, bool");
}

/**
 * Compute the rounding adjustment and decide whether to display the posting.
 *
 * Compares the new display total (after this posting) with the previous one.
 * If they differ due to display rounding, emits a synthetic <Adjustment>
 * posting to compensate.  Returns true if the posting should be displayed
 * (non-zero display amount or --empty), false otherwise.
 *
 * Uses an incremental optimization when possible: instead of stripping the
 * full running total (O(K) GMP ops), strips only this posting's contribution
 * and adds it to the cached stripped total from the previous posting.
 */
bool display_filter_posts::output_rounding(post_t& post) {
  bind_scope_t bound_scope(report, post);
  value_t new_display_total;

  if (show_rounding) {
    // Optimization: use incremental stripped total computation when
    // possible.  Instead of stripping the full running total (O(K) GMP
    // operations where K = number of annotated commodities), we strip
    // only the posting's contribution and add it to the cached stripped
    // total from the previous posting (O(1) GMP operations).
    //
    // strip_annotations is distributive over balance addition:
    //   strip(total[n]) = strip(total[n-1]) + strip(visited_value[n])
    //
    // We fall back to full stripping when:
    //   - This is the first posting (no cache yet)
    //   - The display_total_expr is non-default (--market, --exchange, etc.)
    //   - The posting is a revalued/generated posting
    //   - The posting doesn't have visited_value set by calc_posts
    if (has_stripped_cache && incremental_strip_eligible && post.account != revalued_account &&
        post.has_xdata() && post.xdata().has_flags(POST_EXT_VISITED) &&
        !post.xdata().visited_value.is_null()) {
      // Incremental path: strip just the posting's contribution
      value_t stripped_delta = post.xdata().visited_value.strip_annotations(what_to_keep);
      new_display_total = last_stripped_display_total;
      add_or_set_value(new_display_total, stripped_delta);
    } else {
      // Full path: strip the entire display total from scratch
      new_display_total = (display_total_expr.calc(bound_scope).strip_annotations(what_to_keep));
    }

    DEBUG("filters.changed_value.rounding",
          "rounding.new_display_total     = " << new_display_total);
  }

  // Allow the posting to be displayed if:
  //  1. Its display_amount would display as non-zero, or
  //  2. The --empty option was specified, or
  //  3. a) The account of the posting is <Revalued>, and
  //     b) the revalued option is specified, and
  //     c) the --no-rounding option is not specified.

  if (post.account == revalued_account) {
    if (show_rounding)
      last_display_total = new_display_total;
    // Revalued postings break the incremental chain since they
    // modify the total in non-standard ways.
    has_stripped_cache = false;
    return true;
  }

  if (value_t repriced_amount =
          (display_amount_expr.calc(bound_scope).strip_annotations(what_to_keep))) {
    if (!last_display_total.is_null()) {
      DEBUG("filters.changed_value.rounding",
            "rounding.repriced_amount       = " << repriced_amount);

      value_t precise_display_total(new_display_total.display_rounded() -
                                    repriced_amount.display_rounded());

      DEBUG("filters.changed_value.rounding",
            "rounding.precise_display_total = " << precise_display_total);
      DEBUG("filters.changed_value.rounding",
            "rounding.last_display_total    = " << last_display_total);

      // In period+plot mode (-M with -j/-J), suppress the revaluation
      // adjustment posting.  Market value changes between periods are already
      // reflected in the final posting's display_total via market(), so the
      // intermediate adjustment would only create an unwanted extra data point
      // for each period in the plot output (issue #984).
      bool suppress_period_plot_adjustment =
          report.HANDLED(period_) && (report.HANDLED(amount_data) || report.HANDLED(total_data));

      if (!suppress_period_plot_adjustment) {
        if (value_t diff = precise_display_total - last_display_total) {
          DEBUG("filters.changed_value.rounding", "rounding.diff                  = " << diff);

          handle_value(/* value=         */ diff,
                       /* account=       */ rounding_account,
                       /* xact=          */ post.xact,
                       /* temps=         */ temps,
                       /* handler=       */ handler,
                       /* date=          */ date_t(),
                       /* act_date_p=    */ true,
                       /* total=         */ precise_display_total,
                       /* direct_amount= */ true,
                       /* mark_visited=  */ false,
                       /* bidir_link=    */ false);
        }
      }
    }
    if (show_rounding) {
      last_display_total = new_display_total;
      // Update the incremental cache and store in xdata for the
      // format's scrub(display_total) to reuse.
      last_stripped_display_total = new_display_total;
      has_stripped_cache = true;
      if (post.has_xdata()) {
        post.xdata().display_total = new_display_total;
        post.xdata().add_flags(POST_EXT_DISPLAY_TOTAL_CACHED);
      }
    }
    return true;
  } else {
    return report.HANDLED(empty);
  }
}

/// Forward the posting downstream only if output_rounding() approves it.
void display_filter_posts::operator()(post_t& post) {
  if (output_rounding(post))
    item_handler<post_t>::operator()(post);
}

/*--- Revaluation (changed value) ---*/

changed_value_posts::changed_value_posts(post_handler_ptr handler, report_t& _report,
                                         bool _for_accounts_report, bool _show_unrealized,
                                         display_filter_posts* _display_filter)
    : item_handler<post_t>(std::move(handler)), report(_report),
      total_expr(report.HANDLED(revalued_total_) ? report.HANDLER(revalued_total_).expr
                                                 : report.HANDLER(display_total_).expr),
      display_total_expr(report.HANDLER(display_total_).expr),
      changed_values_only(report.HANDLED(revalued_only)),
      historical_prices_only(report.HANDLED(historical)), for_accounts_report(_for_accounts_report),
      show_unrealized(_show_unrealized), last_post(nullptr), display_filter(_display_filter) {
  string gains_equity_account_name;
  if (report.HANDLED(unrealized_gains_))
    gains_equity_account_name = report.HANDLER(unrealized_gains_).str();
  else
    gains_equity_account_name = _("Equity:Unrealized Gains");
  gains_equity_account = report.session.journal->master->find_account(gains_equity_account_name);
  gains_equity_account->add_flags(ACCOUNT_GENERATED);

  string losses_equity_account_name;
  if (report.HANDLED(unrealized_losses_))
    losses_equity_account_name = report.HANDLER(unrealized_losses_).str();
  else
    losses_equity_account_name = _("Equity:Unrealized Losses");
  losses_equity_account = report.session.journal->master->find_account(losses_equity_account_name);
  losses_equity_account->add_flags(ACCOUNT_GENERATED);

  create_accounts();

  TRACE_CTOR(changed_value_posts,
             "post_handler_ptr, report_t&, bool, bool, display_filter_posts *");
}

/**
 * Emit final revaluation up to the report terminus date.
 *
 * After all real postings have been processed, this generates one last
 * revaluation for any price changes between the last posting and the
 * report's terminus date.
 */
void changed_value_posts::flush() {
  if (last_post && last_post->date() <= report.terminus.date()) {
    if (!historical_prices_only) {
      if (!for_accounts_report)
        output_intermediate_prices(*last_post, report.terminus.date());
      output_revaluation(*last_post, report.terminus.date());
    }
    last_post = nullptr;
  }
  item_handler<post_t>::flush();
}

/**
 * Generate a revaluation posting if the repriced total differs from last_total.
 *
 * Temporarily overrides the posting's xdata date to @p date, evaluates the
 * total expression, and compares with last_total.  If they differ, a
 * synthetic posting is emitted to either the <Revalued> account (register
 * reports) or the equity gain/loss accounts (balance reports with --unrealized).
 */
void changed_value_posts::output_revaluation(post_t& post, const date_t& date) {
  if (is_valid(date))
    post.xdata().date = date;

  try {
    bind_scope_t bound_scope(report, post);
    repriced_total = total_expr.calc(bound_scope);
  } catch (...) {
    post.xdata().date = date_t();
    throw;
  }
  post.xdata().date = date_t();

  DEBUG("filters.changed_value", "output_revaluation(last_total)     = " << last_total);
  DEBUG("filters.changed_value", "output_revaluation(repriced_total) = " << repriced_total);

  if (!last_total.is_null()) {
    if (value_t diff = repriced_total - last_total) {
      DEBUG("filters.changed_value",
            "output_revaluation(strip(diff)) = " << diff.strip_annotations(report.what_to_keep()));

      xact_t& xact = temps.create_xact();
      xact.payee = _("Commodities revalued");
      xact._date = is_valid(date) ? date : post.value_date();

      // NOLINTBEGIN(bugprone-branch-clone)
      if (!for_accounts_report) {
        handle_value(/* value=         */ diff,
                     /* account=       */ revalued_account,
                     /* xact=          */ &xact,
                     /* temps=         */ temps,
                     /* handler=       */ handler,
                     /* date=          */ *xact._date,
                     /* act_date_p=    */ true,
                     /* total=         */ repriced_total,
                     /* direct_amount= */ false,
                     /* mark_visited=  */ false,
                     /* bidir_link=    */ true,
                     /* source_post=   */ &post);
      } else if (show_unrealized) {
        handle_value(
            /* value=         */ -diff,
            /* account=       */ (diff < 0L ? losses_equity_account : gains_equity_account),
            /* xact=          */ &xact,
            /* temps=         */ temps,
            /* handler=       */ handler,
            /* date=          */ *xact._date,
            /* act_date_p=    */ true,
            /* total=         */ value_t(),
            /* direct_amount= */ false,
            /* mark_visited=  */ true,
            /* bidir_link=    */ true,
            /* source_post=   */ &post);
      }
      // NOLINTEND(bugprone-branch-clone)
    }
  }
}

namespace {
struct insert_prices_in_map {
  price_map_t& all_prices;

  insert_prices_in_map(price_map_t& _all_prices) : all_prices(_all_prices) {}

  void operator()(const datetime_t& date, const amount_t& price) {
    all_prices.insert(price_map_t::value_type(date, price));
  }
};
} // namespace

/**
 * Scan for commodity price changes between the last posting and the current one,
 * generating a revaluation for each distinct pricing date.
 *
 * This handles the case where the commodity price database contains price
 * entries between two postings.  The running total's balance is examined to
 * find all commodities present, then each commodity's price history is
 * queried for changes in the relevant date range.  A revaluation posting is
 * emitted for each date where a price changed.
 */
void changed_value_posts::output_intermediate_prices(post_t& post, const date_t& current) {
  // To fix BZ#199, examine the balance of last_post and determine whether the
  // price of that amount changed after its date and before the new post's
  // date.  If so, generate an output_revaluation for that price change.
  // Mostly this is only going to occur if the user has a series of pricing
  // entries, since a posting-based revaluation would be seen here as a post.

  value_t display_total(last_total);

  if (display_total.type() == value_t::SEQUENCE) {
    xact_t& xact(temps.create_xact());

    xact.payee = _("Commodities revalued");
    xact._date = is_valid(current) ? current : post.value_date();

    post_t& temp(temps.copy_post(post, xact));
    temp.add_flags(ITEM_GENERATED);

    post_t::xdata_t& xdata(temp.xdata());
    if (is_valid(current))
      xdata.date = current;

    DEBUG("filters.revalued", "intermediate last_total = " << last_total);

    switch (last_total.type()) {
    case value_t::BOOLEAN:
    case value_t::INTEGER:
      last_total.in_place_cast(value_t::AMOUNT);
      [[fallthrough]];

    case value_t::AMOUNT:
      temp.amount = last_total.as_amount();
      break;

    case value_t::BALANCE:
    case value_t::SEQUENCE:
      xdata.compound_value = last_total;
      xdata.add_flags(POST_EXT_COMPOUND);
      break;

    case value_t::DATETIME:
    case value_t::DATE:
    default:
      assert(false);
      break;
    }

    bind_scope_t inner_scope(report, temp);
    display_total = display_total_expr.calc(inner_scope);

    DEBUG("filters.revalued", "intermediate display_total = " << display_total);
  }

  switch (display_total.type()) {
  case value_t::VOID:
  case value_t::INTEGER:
  case value_t::SEQUENCE:
    break;

  case value_t::AMOUNT:
    display_total.in_place_cast(value_t::BALANCE);
    [[fallthrough]];

  case value_t::BALANCE: {
    price_map_t all_prices;

    for (const balance_t::amounts_map::value_type& amt_comm : display_total.as_balance().amounts)
      amt_comm.first->map_prices(insert_prices_in_map(all_prices), datetime_t(current),
                                 datetime_t(post.value_date()), true);

    // Choose the last price from each day as the price to use
    typedef std::map<const date_t, bool> date_map;
    date_map pricing_dates;

    for (auto it = all_prices.rbegin(); it != all_prices.rend(); ++it) {
      const auto& price = *it;
      // This insert will fail if a later price has already been inserted
      // for that date.
      DEBUG("filters.revalued", "re-inserting " << price.second << " at " << price.first.date());
      pricing_dates.insert(date_map::value_type(price.first.date(), true));
    }

    // Go through the time-sorted prices list, outputting a revaluation for
    // each price difference.
    for (const date_map::value_type& price : pricing_dates) {
      output_revaluation(post, price.first);
      last_total = repriced_total;
    }
    break;
  }
  default:
    assert(false);
    break;
  }
}

/**
 * Process a posting, generating revaluations for price changes since the
 * previous posting.
 *
 * Between consecutive postings from different transactions:
 * 1. Intermediate price changes are scanned and revaluations emitted.
 * 2. A direct revaluation is generated when using --historical or when
 *    postings share the same value date.
 *
 * The posting is then forwarded downstream, and the current total is
 * captured as last_total for the next comparison.
 */
void changed_value_posts::operator()(post_t& post) {
  if (last_post) {
    // Don't generate spurious revaluations when postings are from the same
    // transaction (e.g., stock splits with multiple prices on the same date).
    // Within a transaction, postings represent a complete operation.
    if (last_post->xact != post.xact) {
      if (!for_accounts_report && !historical_prices_only)
        output_intermediate_prices(*last_post, post.value_date());

      // Generate direct revaluation when:
      // 1. Using historical prices (output_intermediate_prices is skipped), OR
      // 2. Staying on the same date (output_intermediate_prices won't handle it)
      // When moving between dates with intermediate prices enabled,
      // output_intermediate_prices handles the revaluation.
      if (historical_prices_only || last_post->value_date() == post.value_date())
        output_revaluation(*last_post, post.value_date());
    }
  }

  if (changed_values_only)
    post.xdata().add_flags(POST_EXT_DISPLAYED);

  item_handler<post_t>::operator()(post);

  bind_scope_t bound_scope(report, post);
  last_total = total_expr.calc(bound_scope);
  last_post = &post;
}

/*--- Subtotaling ---*/

/**
 * Emit a synthetic transaction containing one posting per accumulated
 * account/commodity pair.
 *
 * The synthetic transaction's payee is formatted as a date range (e.g.,
 * "- 2024/12/31").  If an interval is provided, its boundaries are used;
 * otherwise, the date range is computed from the component postings.
 *
 * @param spec_fmt  Optional strftime format string for the payee date.
 * @param interval  Optional interval providing start/finish dates.
 */
void subtotal_posts::report_subtotal(const char* spec_fmt,
                                     const optional<date_interval_t>& interval) {
  if (component_posts.empty())
    return;

  optional<date_t> range_start = interval ? interval->start : none;
  optional<date_t> range_finish = interval ? interval->inclusive_end() : none;

  if (!range_start || !range_finish) {
    for (post_t* post : component_posts) {
      date_t date = post->date();
      date_t value_date = post->value_date();
#if defined(__GNUC__) && __GNUC__ >= 4 && __GNUC_MINOR__ >= 7
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#endif
      if (!range_start || date < *range_start)
        range_start = date;
      if (!range_finish || value_date > *range_finish)
        range_finish = value_date;
#if defined(__GNUC__) && __GNUC__ >= 4 && __GNUC_MINOR__ >= 7
#pragma GCC diagnostic pop
#endif
    }
  }
  component_posts.clear();

  std::ostringstream out_date;
  // NOLINTBEGIN(bugprone-branch-clone)
  if (spec_fmt) {
    out_date << format_date(*range_finish, FMT_CUSTOM, spec_fmt);
  } else if (date_format) {
    out_date << "- " << format_date(*range_finish, FMT_CUSTOM, date_format->c_str());
  } else {
    out_date << "- " << format_date(*range_finish);
  }
  // NOLINTEND(bugprone-branch-clone)

  xact_t& xact = temps.create_xact();
  xact.payee = out_date.str();
  xact._date = *range_start;

  for (values_map::value_type& pair : values)
    handle_value(/* value=              */ pair.second.value,
                 /* account=            */ pair.second.account,
                 /* xact=               */ &xact,
                 /* temps=              */ temps,
                 /* handler=            */ handler,
                 /* date=               */ *range_finish,
                 /* act_date_p=         */ false,
                 /* total=              */ value_t(),
                 /* direct_amount=      */ false,
                 /* mark_visited=       */ false,
                 /* bidir_link=         */ true,
                 /* source_post=        */ nullptr,
                 /* force_virtual=      */ pair.second.is_virtual,
                 /* force_must_balance= */ pair.second.must_balance);

  values.clear();
}

/**
 * Accumulate a posting into the subtotal.
 *
 * The posting's amount is evaluated via amount_expr and added to the
 * values_map entry for its account.  Virtual and non-virtual postings to the
 * same account are tracked separately (using a sentinel suffix in the key)
 * so they can be emitted with correct virtual flags in report_subtotal.
 */
void subtotal_posts::operator()(post_t& post) {
  component_posts.push_back(&post);

  account_t* acct = post.reported_account();
  assert(acct);

  // Evaluate amount_expr before marking the post as compound so that compound
  // expressions like "(amount, cost)" used by --gain produce values with a
  // consistent type in the accumulator.  Using post.amount directly causes
  // multi-commodity accounts to accumulate as BALANCE values; those compound
  // posts then bypass expression evaluation in calc_posts, which leads to
  // mismatched sequence lengths in the running total (issue #965).
  value_t amount;
  post.add_to_value(amount, amount_expr);

  post.xdata().compound_value = value_t(post.amount);
  post.xdata().add_flags(POST_EXT_COMPOUND);

  // Use a composite key to distinguish virtual from non-virtual postings to
  // the same account.  A sentinel suffix '\x01' is appended for virtual
  // postings so they accumulate separately and do not trigger the error that
  // previously fired when the same account appeared with both virtual and
  // non-virtual postings within a single reporting period (issue #2051).
  string key = acct->fullname();
  if (post.has_flags(POST_VIRTUAL))
    key += '\x01';

  values_map::iterator i = values.find(key);
  if (i == values.end()) {
#if DEBUG_ON
    std::pair<values_map::iterator, bool> result =
#endif
        values.insert(values_pair(key, acct_value_t(acct, amount, post.has_flags(POST_VIRTUAL),
                                                    post.has_flags(POST_MUST_BALANCE))));
#if DEBUG_ON
    assert(result.second);
#endif
  } else {
    add_or_set_value((*i).second.value, amount);
  }

  // If the account for this post is all virtual, mark it as
  // such, so that `handle_value' can show "(Account)" for accounts
  // that contain only virtual posts.

  post.reported_account()->xdata().add_flags(ACCOUNT_EXT_AUTO_VIRTUALIZE);

  if (!post.has_flags(POST_VIRTUAL))
    post.reported_account()->xdata().add_flags(ACCOUNT_EXT_HAS_NON_VIRTUALS);
  else if (!post.has_flags(POST_MUST_BALANCE))
    post.reported_account()->xdata().add_flags(ACCOUNT_EXT_HAS_UNB_VIRTUALS);
}

/*--- Interval (periodic) reporting ---*/

/// Emit the subtotal for one interval period.
void interval_posts::report_subtotal(const date_interval_t& ival) {
  if (exact_periods)
    subtotal_posts::report_subtotal();
  else
    subtotal_posts::report_subtotal(nullptr, ival);
}

namespace {
struct sort_posts_by_date {
  bool operator()(post_t* left, post_t* right) const { return left->date() < right->date(); }
};
} // namespace

/**
 * Collect or forward a posting depending on whether intervals have a duration.
 *
 * When a duration is present (e.g., monthly), postings are accumulated for
 * a two-pass sort-then-group in flush().  Without a duration (bare date range),
 * postings within the range are forwarded immediately.
 */
void interval_posts::operator()(post_t& post) {
  // If there is a duration (such as weekly), we must generate the
  // report in two passes.  Otherwise, we only have to check whether the
  // post falls within the reporting period.

  // NOLINTBEGIN(bugprone-branch-clone,bugprone-parent-virtual-call)
  if (interval.duration) {
    all_posts.push_back(&post);
  } else if (interval.find_period(post.date(), align_intervals)) {
    item_handler<post_t>::operator()(post);
  }
  // NOLINTEND(bugprone-branch-clone,bugprone-parent-virtual-call)
}

/**
 * Sort all accumulated postings by date and walk through intervals,
 * subtotaling each period.
 *
 * The algorithm:
 * 1. Sort all_posts by date.
 * 2. Determine the first interval from the earliest posting (or explicit start).
 * 3. Walk through postings: those within the current interval are subtotaled;
 *    when a posting falls outside, the interval is reported and advanced.
 * 4. Empty intervals generate zero-amount postings when --empty is active.
 * 5. The last group of postings is reported after the loop.
 */
void interval_posts::flush() {
  if (!interval.duration) {
    item_handler<post_t>::flush(); // NOLINT(bugprone-parent-virtual-call)
    return;
  }

  // Sort all the postings we saw by date ascending
  std::stable_sort(all_posts.begin(), all_posts.end(), sort_posts_by_date());

  // only if the interval has no start use the earliest post
  if (!(interval.begin() && interval.find_period(*interval.begin(), align_intervals)))
    // Determine the beginning interval by using the earliest post
    if (all_posts.size() > 0 && all_posts.front() &&
        !interval.find_period(all_posts.front()->date(), align_intervals))
      throw_(std::logic_error, _("Failed to find period for interval report"));

  // Walk the interval forward reporting all posts within each one
  // before moving on, until we reach the end of all_posts
  bool saw_posts = false;
  for (std::deque<post_t*>::iterator i = all_posts.begin(); i != all_posts.end();) {
    post_t* post(*i);

    DEBUG("filters.interval", "Considering post " << post->date() << " = " << post->amount);
#if DEBUG_ON
    DEBUG("filters.interval", "interval is:");
    debug_interval(interval);
#endif
    assert(!interval.finish || post->date() < *interval.finish);

    if (interval.within_period(post->date())) {
      DEBUG("filters.interval", "Calling subtotal_posts::operator()");
      subtotal_posts::operator()(*post);
      ++i;
      saw_posts = true;
    } else {
      if (saw_posts) {
        DEBUG("filters.interval", "Calling subtotal_posts::report_subtotal()");
        report_subtotal(interval);
        saw_posts = false;
      } else if (generate_empty_posts) {
        // Generate a null posting, so the intervening periods can be
        // seen when -E is used, or if the calculated amount ends up
        // being non-zero
        xact_t& null_xact = temps.create_xact();
        null_xact._date = interval.inclusive_end();

        post_t& null_post = temps.create_post(null_xact, empty_account);
        null_post.add_flags(POST_CALCULATED);
        null_post.amount = 0L;

        subtotal_posts::operator()(null_post);
        report_subtotal(interval);
      }

      DEBUG("filters.interval", "Advancing interval");
      ++interval;
    }
  }

  // If the last postings weren't reported, do so now.
  if (saw_posts) {
    DEBUG("filters.interval", "Calling subtotal_posts::report_subtotal() at end");
    report_subtotal(interval);
  }

  // Tell our parent class to flush
  subtotal_posts::flush();
}

namespace {
struct create_post_from_amount {
  post_handler_ptr handler;
  xact_t& xact;
  account_t& balance_account;
  temporaries_t& temps;

  explicit create_post_from_amount(post_handler_ptr _handler, xact_t& _xact,
                                   account_t& _balance_account, temporaries_t& _temps)
      : handler(std::move(_handler)), xact(_xact), balance_account(_balance_account),
        temps(_temps) {
    TRACE_CTOR(create_post_from_amount, "post_handler_ptr, xact_t&, account_t&, temporaries_t&");
  }
  create_post_from_amount(const create_post_from_amount& other)
      : handler(other.handler), xact(other.xact), balance_account(other.balance_account),
        temps(other.temps) {
    TRACE_CTOR(create_post_from_amount, "copy");
  }
  ~create_post_from_amount() noexcept { TRACE_DTOR(create_post_from_amount); }

  void operator()(const amount_t& amount) {
    if (amount.is_zero())
      return;
    post_t& balance_post = temps.create_post(xact, &balance_account);
    balance_post.amount = -amount;
    (*handler)(balance_post);
  }
};
} // namespace

/*--- Equity conversion ---*/

/**
 * Emit accumulated postings as equity opening balances.
 *
 * For each account with a non-zero balance, a posting is created.  A
 * balancing posting to Equity:Opening Balances is emitted for the total.
 * Virtual postings that don't require balancing are excluded from the
 * equity total.
 */
void posts_as_equity::report_subtotal() {
  date_t finish;
  for (post_t* post : component_posts) {
    date_t date = post->date();
    if (!is_valid(finish) || date > finish)
      finish = date;
  }
  component_posts.clear();

  xact_t& xact = temps.create_xact();
  xact.payee = _("Opening Balances");
  xact._date = finish;

  value_t total = 0L;
  for (values_map::value_type& pair : values) {
    value_t value(pair.second.value.strip_annotations(report.what_to_keep()));
    if (unround)
      value.in_place_unround();
    if (!value.is_zero()) {
      if (value.is_balance()) {
        value.as_balance_lval().map_sorted_amounts([&](const amount_t& amt) {
          if (!amt.is_zero())
            handle_value(/* value=      */ amt,
                         /* account=    */ pair.second.account,
                         /* xact=       */ &xact,
                         /* temps=      */ temps,
                         /* handler=    */ handler,
                         /* date=       */ finish,
                         /* act_date_p= */ false);
        });
      } else {
        handle_value(/* value=      */ value.to_amount(),
                     /* account=    */ pair.second.account,
                     /* xact=       */ &xact,
                     /* temps=      */ temps,
                     /* handler=    */ handler,
                     /* date=       */ finish,
                     /* act_date_p= */ false);
      }
    }

    if (!pair.second.is_virtual || pair.second.must_balance)
      total += value;
  }
  values.clear();

  // This last part isn't really needed, since an Equity:Opening
  // Balances posting with a null amount will automatically balance with
  // all the other postings generated.  But it does make the full
  // balancing amount clearer to the user.
  if (!total.is_zero()) {
    create_post_from_amount post_creator(handler, xact, *balance_account, temps);
    if (total.is_balance())
      total.as_balance_lval().map_sorted_amounts(post_creator);
    else
      post_creator(total.to_amount());
  }
}

/*--- By-payee subtotaling ---*/

/// Flush each payee's subtotal_posts instance and forward results downstream.
void by_payee_posts::flush() {
  for (payee_subtotals_map::value_type& pair : payee_subtotals)
    pair.second->report_subtotal(pair.first.c_str());

  item_handler<post_t>::flush();

  payee_subtotals.clear();
}

/// Route each posting to the subtotal_posts instance for its payee.
void by_payee_posts::operator()(post_t& post) {
  payee_subtotals_map::iterator i = payee_subtotals.find(post.payee());
  if (i == payee_subtotals.end()) {
    payee_subtotals_pair temp(
        post.payee(), std::shared_ptr<subtotal_posts>(new subtotal_posts(handler, amount_expr)));
    auto [iter, inserted] = payee_subtotals.insert(temp);

    assert(inserted);
    if (!inserted)
      return;
    i = iter;
  }

  (*(*i).second)(post);
}

/*--- Detail transfer ---*/

/**
 * Rewrite one element (date, account, or payee) of a posting based on an expression.
 *
 * A temporary copy of the posting and its transaction is created.  The
 * expression is evaluated in the posting's scope, and the result replaces
 * the designated element:
 * - SET_DATE: the posting's date is overridden.
 * - SET_ACCOUNT: the expression result is prepended to the account hierarchy.
 * - SET_PAYEE: the transaction payee is replaced.
 */
void transfer_details::operator()(post_t& post) {
  xact_t& xact = temps.copy_xact(*post.xact);
  xact._date = post.date();

  post_t& temp = temps.copy_post(post, xact);
  temp.set_state(post.state());

  bind_scope_t bound_scope(scope, temp);
  value_t substitute(expr.calc(bound_scope));

  if (!substitute.is_null()) {
    switch (which_element) {
    case SET_DATE:
      temp._date = substitute.to_date();
      break;

    case SET_ACCOUNT: {
      string account_name = substitute.to_string();
      if (!account_name.empty() && account_name[account_name.length() - 1] != ':') {
        account_t* prev_account = temp.account;
        temp.account->remove_post(&temp);

        account_name += ':';
        account_name += prev_account->fullname();

        std::list<string> account_names;
        split_string(account_name, ':', account_names);
        temp.account = create_temp_account_from_path(account_names, temps, xact.journal->master);
        temp.account->add_post(&temp);

        temp.account->add_flags(prev_account->flags());
        if (prev_account->has_xdata())
          temp.account->xdata().add_flags(prev_account->xdata().flags());
      }
      break;
    }

    case SET_PAYEE:
      xact.payee = substitute.to_string();
      break;
    }
  }

  item_handler<post_t>::operator()(temp);
}

/*--- Day-of-week subtotaling ---*/

/**
 * Subtotal each day's accumulated postings and emit them with the
 * abbreviated day name as the payee (format "%As").
 */
void day_of_week_posts::flush() {
  for (int i = 0; i < 7; i++) {
    for (post_t* post : days_of_the_week[i])
      subtotal_posts::operator()(*post);
    subtotal_posts::report_subtotal("%As");
    days_of_the_week[i].clear();
  }

  subtotal_posts::flush();
}

/*--- Synthetic posting generation (budget and forecast) ---*/

/**
 * Decompose periodic transactions into individual pending postings.
 *
 * Each posting from each periodic transaction is paired with the
 * transaction's date interval and added to the pending_posts list.
 * Auto-generated postings (ITEM_GENERATED without POST_CALCULATED) are
 * skipped because they lack a valid xact back-pointer.
 */
void generate_posts::add_period_xacts(period_xacts_list& period_xacts) {
  for (period_xact_t* xact : period_xacts)
    for (post_t* post : xact->posts)
      // Skip auto-transaction-generated posts (ITEM_GENERATED without
      // POST_CALCULATED); they have no xact back-pointer and should not
      // produce independent forecast/budget entries.
      if (!post->has_flags(ITEM_GENERATED) || post->has_flags(POST_CALCULATED))
        add_post(xact->period, *post);
}

/// Add a single periodic posting to the pending list.
void generate_posts::add_post(const date_interval_t& period, post_t& post) {
  pending_posts.push_back(pending_posts_pair(period, &post));
}

/**
 * Generate budget postings for all pending periodic postings up to @p date.
 *
 * First, expired periodic postings (whose finish date has passed) are cleaned
 * up.  Then, for each remaining periodic posting whose next occurrence is at
 * or before @p date, a synthetic "Budget transaction" posting with a negated
 * amount is emitted.  The loop repeats until no more budget items can be
 * reported (since advancing one period may reveal additional occurrences).
 */
void budget_posts::report_budget_items(const date_t& date) {
  { // Cleanup pending items that finished before date
    // We have to keep them until the last day they apply because operator() needs them to see if a
    // posting is budgeted or not
    std::list<pending_posts_list::iterator> posts_to_erase;
    for (pending_posts_list::iterator i = pending_posts.begin(); i != pending_posts.end(); i++) {
      pending_posts_list::value_type& pair(*i);
      if (pair.first.finish && !pair.first.start && pair.first.finish < date) {
        posts_to_erase.push_back(i);
      }
    }
    for (pending_posts_list::iterator& i : posts_to_erase)
      pending_posts.erase(i);
  }

  if (pending_posts.size() == 0)
    return;

  bool reported;
  do {
    reported = false;
    for (pending_posts_list::iterator i = pending_posts.begin(); i != pending_posts.end(); i++) {
      pending_posts_list::value_type& pair(*i);

      if (pair.first.finish && !pair.first.start)
        continue; // skip expired posts

      optional<date_t> begin = pair.first.start;
      if (!begin) {
        optional<date_t> range_begin;
        if (pair.first.range)
          range_begin = pair.first.range->begin();

        DEBUG("budget.generate", "Finding period for pending post");
        if (!pair.first.find_period(range_begin ? *range_begin : date))
          continue;
        if (!pair.first.start)
          throw_(std::logic_error, _("Failed to find period for periodic transaction"));
        begin = pair.first.start;
      }

#if DEBUG_ON
      DEBUG("budget.generate", "begin = " << *begin);
      DEBUG("budget.generate", "date  = " << date);
      if (pair.first.finish)
        DEBUG("budget.generate", "pair.first.finish = " << *pair.first.finish);
#endif

      if (*begin <= date && (!pair.first.finish || *begin < *pair.first.finish)) {
        post_t& post = *pair.second;

        ++pair.first;
        DEBUG("budget.generate", "Reporting budget for " << post.reported_account()->fullname());

        xact_t& xact = temps.create_xact();
        xact.payee = _("Budget transaction");
        xact._date = begin;

        post_t& temp = temps.copy_post(post, xact);
        temp.amount.in_place_negate();

        if (flags & BUDGET_WRAP_VALUES) {
          value_t seq;
          seq.push_back(0L);
          seq.push_back(temp.amount);

          temp.xdata().compound_value = seq;
          temp.xdata().add_flags(POST_EXT_COMPOUND);
        }

        item_handler<post_t>::operator()(temp);

        reported = true;
      }
    }
  } while (reported);
}

/**
 * Process a real posting against the budget.
 *
 * Determines whether the posting's account (or any ancestor) matches a
 * pending budget account.  Based on the flags:
 * - BUDGET_BUDGETED: generate budget items up to this date, then forward
 *   the posting (showing actual vs. budget).
 * - BUDGET_UNBUDGETED: forward only non-budget postings, optionally wrapping
 *   values with a VOID budget slot.
 */
void budget_posts::operator()(post_t& post) {
  bool post_in_budget = false;

  for (pending_posts_list::value_type& pair : pending_posts) {
    for (account_t* acct = post.reported_account(); acct; acct = acct->parent) {
      if (acct == (*pair.second).reported_account()) {
        post_in_budget = true;
        // Report the post as if it had occurred in the parent account.
        if (post.reported_account() != acct)
          post.set_reported_account(acct);
        goto handle;
      }
    }
  }

handle:
  if (post_in_budget && flags & BUDGET_BUDGETED) {
    report_budget_items(post.date());
    item_handler<post_t>::operator()(post);
  } else if (!post_in_budget && flags & BUDGET_UNBUDGETED) {
    if (flags & BUDGET_WRAP_VALUES) {
      // Wrap the unbudgeted post's value so that the budget slot is VOID
      // rather than the 0 produced by the amount expression "(amount, 0)".
      // VOID displays as blank in the budget column, correctly indicating
      // that no budget exists for this account (issue #1023).
      value_t seq;
      seq.push_back(post.amount);
      seq.push_back(value_t());

      post.xdata().compound_value = seq;
      post.xdata().add_flags(POST_EXT_COMPOUND);
    }
    item_handler<post_t>::operator()(post);
  }
}

/// Generate remaining budget items up to the terminus and flush downstream.
void budget_posts::flush() {
  if (flags & BUDGET_BUDGETED)
    report_budget_items(terminus);

  item_handler<post_t>::flush();
}

/**
 * Add a periodic posting for forecasting, advancing its interval to CURRENT_DATE.
 *
 * Unlike generate_posts::add_post, this override initializes the interval
 * and advances it past all historical periods so that only future occurrences
 * are generated during flush().
 */
void forecast_posts::add_post(const date_interval_t& period, post_t& post) {
  date_interval_t i(period);
  if (!i.start && !i.find_period(CURRENT_DATE()))
    return;

  generate_posts::add_post(i, post);

  // Advance the period's interval until it is at or beyond the current
  // date.
  while (*i.start < CURRENT_DATE())
    ++i;
}

/**
 * Generate all forecast postings into the future.
 *
 * Iterates through pending periodic postings, always selecting the one with
 * the earliest next occurrence.  For each, a synthetic "Forecast transaction"
 * posting is emitted.  A periodic posting is removed from consideration when:
 * - Its next date exceeds forecast_years beyond the last generated date, or
 * - The generated posting matches the report query but fails the continuation
 *   predicate (--forecast-while), or
 * - Its interval has no more occurrences.
 */
void forecast_posts::flush() {
  posts_list passed;
  date_t last = CURRENT_DATE();

  // If there are period transactions to apply in a continuing series until
  // the forecast condition is met, generate those transactions now.  Note
  // that no matter what, we abandon forecasting beyond the next 5 years.
  //
  // It works like this:
  //
  // Earlier, in forecast_posts::add_period_xacts, we cut up all the periodic
  // transactions into their components postings, so that we have N "periodic
  // postings".  For example, if the user had this:
  //
  // ~ daily
  //   Expenses:Food       $10
  //   Expenses:Auto:Gas   $20
  // ~ monthly
  //   Expenses:Food       $100
  //   Expenses:Auto:Gas   $200
  //
  // We now have 4 periodic postings in `pending_posts'.
  //
  // Each periodic postings gets its own copy of its parent transaction's
  // period, which is modified as we go.  This is found in the second member
  // of the pending_posts_list for each posting.
  //
  // The algorithm below works by iterating through the N periodic postings
  // over and over, until each of them meets the termination criteria for the
  // forecast and is removed from the set.

  while (pending_posts.size() > 0) {
    // At each step through the loop, we find the first periodic posting whose
    // period contains the earliest starting date.
    pending_posts_list::iterator least = pending_posts.begin();
    for (pending_posts_list::iterator i = ++pending_posts.begin(); i != pending_posts.end(); i++) {
      assert((*i).first.start);
      assert((*least).first.start);
      if (*(*i).first.start < *(*least).first.start)
        least = i;
    }

    if ((*least).first.finish)
      assert(*(*least).first.start < *(*least).first.finish);

    // If the next date in the series for this periodic posting is more than 5
    // years beyond the last valid post we generated, drop it from further
    // consideration.
    date_t& next(*(*least).first.next);
    assert(next > *(*least).first.start);

    if (static_cast<std::size_t>((next - last).days()) >
        static_cast<std::size_t>(365U) * forecast_years) {
      DEBUG("filters.forecast",
            "Forecast transaction exceeds " << forecast_years << " years beyond today");
      pending_posts.erase(least);
      continue;
    }

    // `post' refers to the posting defined in the period transaction.  We
    // make a copy of it within a temporary transaction with the payee
    // "Forecast transaction".
    post_t& post = *(*least).second;
    xact_t& xact = temps.create_xact();
    xact.payee = _("Forecast transaction");
    xact._date = next;
    post_t& temp = temps.copy_post(post, xact);

    // Submit the generated posting
    DEBUG("filters.forecast", "Forecast transaction: " << temp.date() << " "
                                                       << temp.account->fullname() << " "
                                                       << temp.amount);
    item_handler<post_t>::operator()(temp);

    // If the generated posting matches the user's report query, check whether
    // it also fails to match the continuation condition for the forecast.  If
    // it does, drop this periodic posting from consideration.
    if (temp.has_xdata() && temp.xdata().has_flags(POST_EXT_MATCHES)) {
      DEBUG("filters.forecast", "  matches report query");
      bind_scope_t bound_scope(context, temp);
      if (!pred(bound_scope)) {
        DEBUG("filters.forecast", "  fails to match continuation criteria");
        pending_posts.erase(least);
        continue;
      }
    }

    // Increment the 'least', but remove it from pending_posts if it
    // exceeds its own boundaries.
    ++(*least).first;
    if (!(*least).first.start) {
      pending_posts.erase(least);
      continue;
    }
  }

  item_handler<post_t>::flush();
}

/*--- Tag injection ---*/

/**
 * Construct the inject_posts handler by parsing the comma-separated tag list.
 *
 * Each tag name is split by ':' to form an account path, and a temporary
 * account is created for it.  The tags_list stores each tag name paired with
 * its target account and a set tracking which transactions have already been
 * injected (to avoid duplicates).
 */
inject_posts::inject_posts(post_handler_ptr handler, const string& tag_list, account_t* master)
    : item_handler<post_t>(std::move(handler)) {
  scoped_array<char> buf(new char[tag_list.length() + 1]);
  std::strcpy(buf.get(), tag_list.c_str());

  for (char* q = std::strtok(buf.get(), ","); q; q = std::strtok(nullptr, ",")) {
    std::list<string> account_names;
    split_string(q, ':', account_names);

    account_t* account = create_temp_account_from_path(account_names, temps, master);
    account->add_flags(ACCOUNT_GENERATED);

    tags_list.push_back(tags_list_pair(q, tag_mapping_pair(account, tag_injected_set())));
  }

  TRACE_CTOR(inject_posts, "post_handler_ptr, string, account_t *");
}

/**
 * For each configured tag, check whether the posting or its transaction
 * carries that tag.  If found, inject a synthetic posting with the tag's
 * value as the amount, then forward the original posting.
 */
void inject_posts::operator()(post_t& post) {
  for (tags_list_pair& pair : tags_list) {
    std::optional<value_t> tag_value = post.get_tag(pair.first, false);
    // When checking if the transaction has the tag, only inject once
    // per transaction.
    if (!tag_value && pair.second.second.find(post.xact) == pair.second.second.end() &&
        (tag_value = post.xact->get_tag(pair.first)))
      pair.second.second.insert(post.xact);

    if (tag_value) {
      xact_t& xact = temps.copy_xact(*post.xact);
      xact._date = post.date();
      xact.add_flags(ITEM_GENERATED);
      post_t& temp = temps.copy_post(post, xact);

      temp.account = pair.second.first;
      temp.amount = tag_value->to_amount();
      temp.add_flags(ITEM_GENERATED);

      item_handler<post_t>::operator()(temp);
    }
  }

  item_handler<post_t>::operator()(post);
}

/*--- Payee/account rewriting ---*/

/**
 * Apply payee and account rewrite rules from the journal to each posting.
 *
 * Checks the posting against all payee_rewrite_mappings (matching on the
 * transaction payee) and account_rewrite_mappings (matching on the account
 * fullname).  If either matches, a temporary copy is created with the
 * rewritten payee and/or account.  Non-matching postings pass through
 * unchanged.
 */
void rewrite_posts::operator()(post_t& post) {
  journal_t* journal = report.session.journal.get();

  // Check whether any payee or account rewrite rule matches this posting.
  string new_payee;
  for (const payee_rewrite_mapping_t& pair : journal->payee_rewrite_mappings) {
    if (pair.first.match(post.xact->payee)) {
      new_payee = pair.second;
      break;
    }
  }

  string new_account;
  for (const account_rewrite_mapping_t& pair : journal->account_rewrite_mappings) {
    if (pair.first.match(post.account->fullname())) {
      new_account = pair.second;
      break;
    }
  }

  // If neither rule matched, forward the original posting unchanged.
  if (new_payee.empty() && new_account.empty()) {
    item_handler<post_t>::operator()(post);
    return;
  }

  // At least one rule matched: create a temporary copy and apply rewrites.
  xact_t& xact = temps.copy_xact(*post.xact);
  xact._date = post.date();
  post_t& temp = temps.copy_post(post, xact);
  temp.set_state(post.state());

  if (!new_payee.empty())
    xact.payee = new_payee;

  if (!new_account.empty()) {
    account_t* prev_account = temp.account;
    temp.account->remove_post(&temp);

    std::list<string> account_names;
    split_string(new_account, ':', account_names);
    temp.account = create_temp_account_from_path(account_names, temps, journal->master);
    temp.account->add_post(&temp);

    temp.account->add_flags(prev_account->flags());
    if (prev_account->has_xdata())
      temp.account->xdata().add_flags(prev_account->xdata().flags());
  }

  item_handler<post_t>::operator()(temp);
}

} // namespace ledger
