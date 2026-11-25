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
 * across groups).  In that mode the chain is flushed once per group without
 * a clear(), so each filter's flush() must leave the filter ready for another
 * stream, holding no posting it has already reported and no count of the
 * transactions it has seen; running totals and temporaries are what carries
 * over.  The postings keep their xdata too, and with --related a later group
 * can be handed a posting an earlier group reported: a stage that marks a
 * posting once per stream drops the mark when the stream ends or sets it
 * afresh on every visit, as calc_posts does a posting's value.
 *
 * The postflush callback runs before post_chain->clear() so that an
 * accounts-report flusher can walk the account tree while any temporary
 * postings created by transfer_details (--payee, --account, --date) are
 * still attached to their accounts.  Calling clear() first would remove
 * those temps via temporaries_t::clear(), leaving the account totals
 * at zero for the group (issue #3207).
 */
void post_splitter::flush() {
  bool cumulative = report.HANDLED(group_by_cumulative);

  for (value_to_posts_map::value_type& pair : posts_map) {
    preflush_func(pair.first);

    for (post_t* post : pair.second)
      (*post_chain)(*post);

    post_chain->flush();

    if (postflush_func)
      (*postflush_func)(pair.first);

    // In cumulative mode, don't clear the chain state between groups
    // so that account totals accumulate across groups.
    if (!cumulative)
      post_chain->clear();
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
  // A flush ends the stream being truncated.  The next one (the next
  // group under --group-by-cumulative, which no clear() precedes)
  // counts its transactions from zero.  operator() sets `completed`
  // again right after its early flush.
  completed = false;
  xacts_seen = 0;
  last_xact = nullptr;

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

  if (tail_count == 0 && head_count > 0 && xacts_seen >= static_cast<std::size_t>(head_count)) {
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
/// The child @p name of @p parent, for postings a report moves there: the
/// journal's own account when it has one, otherwise created in @p accounts
/// (the journal's report-account pool, report_t::temp_accounts()), so the
/// paths built here never add an account to the journal.  An empty @p name
/// is an error (find_account throws).
account_t* report_child_account(account_t* parent, string_view name, temporaries_t& accounts) {
  // Below a temporary account, find_account creates the child itself: it
  // inherits ACCOUNT_TEMP and is freed with its parent (~account_t), so it
  // must not also be owned by @p accounts.  A temporary account found at this
  // level may have been made by another live report; the pool they share
  // keeps it until the last of them ends.
  account_t* child = parent->find_account(name, parent->has_flags(ACCOUNT_TEMP));
  return child ? child : &accounts.create_account(string(name), parent);
}

/// The account at the path @p account_names (one name per component) under
/// @p master, each component found or created by report_child_account.
account_t* create_temp_account_from_path(const std::list<string>& account_names,
                                         temporaries_t& accounts, account_t* master) {
  account_t* account = master;
  for (const string& name : account_names)
    account = report_child_account(account, name, accounts);
  return account;
}

/// The account at the ':'-separated @p path under @p master, as the
/// list overload gives it for the path's components (an empty component,
/// including a leading or trailing ':', is an error).  The components are
/// looked up in place: this runs once per posting --account, --pivot and
/// account rewrites move, and copying them into a list of strings first
/// (boost::split) costs several times the lookups themselves.
account_t* create_temp_account_from_path(string_view path, temporaries_t& accounts,
                                         account_t* master) {
  account_t* account = master;
  for (;;) {
    const string_view::size_type sep = path.find(':');
    account = report_child_account(account, path.substr(0, sep), accounts);
    if (sep == string_view::npos)
      return account;
    path.remove_prefix(sep + 1);
  }
}

/// Give @p account, which a posting has been moved onto while reporting, the
/// flags of the @p original account it came from.  ACCOUNT_KNOWN stays
/// behind: the copy was never declared, and --empty lists declared accounts
/// even when no posting reaches them, so it would show the copy too (#3281).
/// ACCOUNT_TEMP stays behind as well: @p account is temporary already when
/// the report made it, and an account the journal has must not become one
/// (temporaries_t::clear() would no longer unlink the pool's accounts beneath
/// it, and lookups would skip it).
void transfer_account_flags(account_t& account, account_t& original) {
  account.add_flags(original.flags() & ~(ACCOUNT_KNOWN | ACCOUNT_TEMP));
  if (original.has_xdata())
    account.xdata().add_flags(original.xdata().flags());
}

/// Move the report-time copy @p temp from its account to the account at
/// @p path under @p master (created in @p accounts, the journal's
/// report-account pool, where the journal lacks it), which takes the old
/// account's flags as transfer_account_flags gives them.
void retarget_temp_post(post_t& temp, const string& path, temporaries_t& accounts,
                        account_t* master) {
  account_t* prev_account = temp.account;
  prev_account->remove_post(&temp);

  temp.account = create_temp_account_from_path(path, accounts, master);
  temp.account->add_post(&temp);

  transfer_account_flags(*temp.account, *prev_account);
}
} // namespace

/*--- Anonymization ---*/

/**
 * Replace a commodity symbol with a sequential letter label (A, B, C, ..., AA, AB, ...).
 *
 * The mapping is stable: the same original commodity always maps to the same
 * label within a single anonymization pass.  Commodity flags and precision
 * are preserved from the original.  Annotated commodities (e.g.
 * AAA{1.00 EUR} and AAA{2.00 EUR}) share the same label as their base
 * commodity (AAA), since the index key is the referent (base) rather than
 * the per-annotation pointer.  (#756)
 */
void anonymize_posts::render_commodity(amount_t& amt) {
  commodity_t& comm(amt.commodity());
  commodity_t* base = &comm.referent();

  std::size_t id;
  bool newly_added = false;

  commodity_index_map::iterator i = comms.find(base);
  if (i == comms.end()) {
    id = next_comm_id++;
    newly_added = true;
    comms.insert(commodity_index_map::value_type(base, id));
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

  for (account_t* acct = post.account; acct && acct->parent; acct = acct->parent) {
    auto it = acct_hashes.find(acct);
    if (it != acct_hashes.end()) {
      account_names.push_front(it->second);
    } else {
      std::ostringstream buf;
      buf << integer_gen() << acct << acct->fullname();
      string hashed = sha1sum(buf.str(), 8);
      acct_hashes[acct] = hashed;
      account_names.push_front(hashed);
    }
  }

  account_t* new_account =
      create_temp_account_from_path(account_names, accounts, xact.journal->master);
  post_t& temp = temps.copy_post(post, xact, new_account);
  temp.note = none;
  temp.add_flags(POST_ANONYMIZED);

  render_commodity(temp.amount);
  if (temp.amount.has_annotation()) {
    // The annotation is the pooled lot's own, shared by every amount of
    // the lot, so anonymize a copy and look the result up rather than edit
    // it.  The lookup is exact: the fuzzy price matching behind
    // amount_t::annotate(), which merges a user-typed lot price with a
    // computed one, must not merge two lots whose rendered prices merely
    // round alike.
    annotation_t anonymized(temp.amount.annotation());
    anonymized.tag = std::nullopt;
    if (anonymized.price)
      render_commodity(*anonymized.price);
    temp.amount.set_commodity(*commodity_pool_t::current_pool->find_or_create(
        temp.amount.commodity().base_symbol(), anonymized));
  }
  if (temp.given_lot_total)
    render_commodity(*temp.given_lot_total);

  if (temp.cost)
    render_commodity(*temp.cost);
  if (temp.given_cost)
    render_commodity(*temp.given_cost);
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
 *    display total (O(1) per posting) for display_filter_posts and
 *    scrubbed_display_total.
 */
void calc_posts::operator()(post_t& post) {
  post_t::xdata_t& xdata(post.xdata());

  // A grouping filter (subtotal_posts and its subclasses, including
  // posts_as_equity, and collapse_posts) emits one row per account per
  // group, flagged POST_EXT_SUBTOTAL_ROW, all sharing the same synthetic
  // xact.  Two adjustments are needed for those rows:
  //
  // 1. Count stability: keep the counter the same for all accounts within
  //    the same group, so --average divides by the number of groups (for
  //    example, elapsed intervals) rather than the number of account rows.
  //
  // 2. Per-account total (only when period_average_ is enabled): seed each
  //    account's running total from its own accumulator rather than from the
  //    global running total.  Without this, the second and later accounts in
  //    a group inherit the blended total of all preceding accounts, making
  //    each account's average wrong.
  //
  // Budget, forecast, automated-transaction and balance-split postings are
  // ITEM_GENERATED too, but they are ordinary register rows and continue the
  // global running total and count.
  const bool subtotal_row = xdata.has_flags(POST_EXT_SUBTOTAL_ROW);
  bool same_subtotal_group = false;
  if (last_post) {
    assert(last_post->has_xdata());
    same_subtotal_group = (subtotal_row && last_post->xdata().has_flags(POST_EXT_SUBTOTAL_ROW) &&
                           post.xact == last_post->xact);
    if (same_subtotal_group)
      xdata.count = last_post->xdata().count;
    else
      xdata.count = last_post->xdata().count + 1;
    if (calc_running_total) {
      if (period_average_ && subtotal_row) {
        // Seed each account's running total from its own per-account
        // accumulator so that multi-account groups (rows flagged
        // POST_EXT_SUBTOTAL_ROW by a grouping filter) each show their own
        // independent average.
        // When the account has no prior history (first appearance), leave
        // xdata.total unset so add_or_set_value() below initialises it
        // to just this post's value, giving the account a clean running
        // total independent of other accounts in the same period.
        auto it = account_period_totals_.find(post.account);
        if (it != account_period_totals_.end())
          xdata.total = it->second;
      } else {
        xdata.total = last_post->xdata().total;
      }
    }
  } else {
    xdata.count = 1;
  }

  // The stripped display total this visit continues from, read before this
  // posting's own is reset below: under --group-by-cumulative -r the posting
  // can be last_post itself (see the next comment).
  value_t carried_display_total;
  const bool carry_display_total = maintain_stripped_total && calc_running_total && last_post &&
                                   last_post->xdata().has_flags(POST_EXT_DISPLAY_TOTAL_CACHED);
  if (carry_display_total)
    carried_display_total = last_post->xdata().display_total;

  // The value, and the stripped display total cached from it (here, or by
  // display_filter_posts when no stripped total is kept here: -V, -X,
  // --lots), are this visit's.  Under --group-by-cumulative, which keeps
  // the postings' xdata between groups, --related can hand on to a later
  // group a posting an earlier group already reported; what it holds from
  // that visit is not a part of this one.  Nor is the mark of its account
  // totals having counted it: this visit is one of the later group's own
  // postings, so the group's account totals count it again, as its
  // running total does.
  xdata.visited_value = value_t();
  xdata.display_total = value_t();
  xdata.drop_flags(POST_EXT_DISPLAY_TOTAL_CACHED | POST_EXT_CONSIDERED);

  post.add_to_value(xdata.visited_value, amount_expr);
  xdata.add_flags(POST_EXT_VISITED);

  account_t* acct = post.reported_account();
  acct->xdata().add_flags(ACCOUNT_EXT_VISITED);

  if (calc_running_total) {
    add_or_set_value(xdata.total, xdata.visited_value);
    // Update the per-account accumulator for rows flagged
    // POST_EXT_SUBTOTAL_ROW by a grouping filter so that subsequent groups
    // for the same account carry the correct baseline.
    if (period_average_ && subtotal_row)
      account_period_totals_[post.account] = xdata.total;

    // Incrementally maintain a stripped display total alongside the
    // regular total.  Instead of stripping the entire running total
    // (O(K) per posting where K = annotated commodities), we strip
    // only this posting's visited_value (O(1)) and add it to the
    // previous stripped total.  This relies on strip_annotations
    // being distributive over balance addition.
    if (maintain_stripped_total) {
      if (carry_display_total)
        xdata.display_total = std::move(carried_display_total);

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
/// Optional settings for a posting that handle_value() generates.
struct handle_value_options_t {
  date_t date;                     ///< Date override; ignored when not valid.
  bool act_date_p = true;          ///< date is the posting date (true) or its value date.
  value_t total;                   ///< Running total stored in xdata, if not null.
  bool direct_amount = false;      ///< Mark the posting POST_EXT_DIRECT_AMT.
  bool mark_visited = false;       ///< Mark posting and account visited after handling.
  bool bidir_link = true;          ///< Link the posting into its xact both ways.
  post_t* source_post = nullptr;   ///< Copy tags/metadata from this posting.
  bool force_virtual = false;      ///< Make the posting virtual regardless of account flags.
  bool force_must_balance = false; ///< With force_virtual, make it a [balanced] posting.
  bool subtotal_row = false;       ///< Mark it POST_EXT_SUBTOTAL_ROW (one row of a group).
};

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
 * @param opts             Date, total, flags and source posting for the generated posting.
 */
void handle_value(const value_t& value, account_t* account, xact_t* xact, temporaries_t& temps,
                  const post_handler_ptr& handler,
                  const handle_value_options_t& opts = handle_value_options_t()) {
  post_t& post = temps.create_post(*xact, account, opts.bidir_link);
  post.add_flags(ITEM_GENERATED);

  // Copy tags/metadata from the source posting so that revaluation posts
  // inherit tags and can be properly filtered by predicates like --limit
  if (opts.source_post && opts.source_post->metadata)
    post.metadata = opts.source_post->metadata;

  // If the caller explicitly specifies that this posting is virtual (e.g.,
  // when subtotal_posts accumulates virtual and non-virtual postings for the
  // same account as separate entries), use that flag directly.  Otherwise,
  // infer virtual status from account xdata flags as before.
  if (opts.force_virtual) {
    post.add_flags(POST_VIRTUAL);
    if (opts.force_must_balance)
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

  if (is_valid(opts.date)) {
    if (opts.act_date_p)
      xdata.date = opts.date;
    else
      xdata.value_date = opts.date;
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

  if (!opts.total.is_null())
    xdata.total = opts.total;

  if (opts.direct_amount)
    xdata.add_flags(POST_EXT_DIRECT_AMT);
  if (opts.subtotal_row)
    xdata.add_flags(POST_EXT_SUBTOTAL_ROW);

  DEBUG("filters.changed_value.rounding", "post.amount = " << post.amount);

  (*handler)(post);

  if (opts.mark_visited) {
    post.xdata().add_flags(POST_EXT_VISITED);
    post.account->xdata().add_flags(ACCOUNT_EXT_VISITED);
  }
}
} // namespace

/*--- Collapsing ---*/

void collapse_posts::create_accounts() {
  global_totals_account = &temps.create_account(_("<Total>"), report.session.journal->master);
  // <Total> is parented to the journal master so that get_account() can walk
  // up to the real journal root for account() lookups (issue #2985).  But
  // <Total> is a synthetic top-level summary account; like the other
  // synthetics (<None>, <Adjustment>, <Revalued>) it must report depth 0 so
  // that --depth 0 reports include it instead of filtering it out (#3198).
  global_totals_account->depth = 0;
}

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

        handle_value_options_t opts;
        opts.date = d;
        opts.subtotal_row = true;
        handle_value(entry.second, acct, &xact, temps, handler, opts);
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

      handle_value_options_t opts;
      opts.date = latest_date;
      opts.act_date_p = false;
      opts.subtotal_row = true;
      for (totals_map::value_type& pat : totals)
        handle_value(pat.second, pat.first, &xact, temps, handler, opts);
    }
  }
  // NOLINTEND(bugprone-branch-clone)
  // When only_collapse_if_zero && subtotal.is_zero(): suppress entirely
  // (fall through to cleanup below).

  totals.clear();
  component_posts.clear();

  last_xact = nullptr;
  last_post = nullptr;
  subtotal = value_t();
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
 * All sibling postings are forwarded, including those generated by automated
 * transactions (ITEM_GENERATED), so that --related shows the complete picture
 * of what a matched transaction touches.
 */
void related_posts::flush() {
  if (posts.size() > 0) {
    for (post_t* post : posts) {
      assert(post->xact);
      for (post_t* r_post : post->xact->posts) {
        post_t::xdata_t& xdata(r_post->xdata());
        if (!xdata.has_flags(POST_EXT_HANDLED) &&
            (!xdata.has_flags(POST_EXT_RECEIVED) || also_matching)) {
          xdata.add_flags(POST_EXT_HANDLED);
          item_handler<post_t>::operator()(*r_post);
        }
      }
    }
  }

  item_handler<post_t>::flush();

  // The stream is reported.  The next one (the next group under
  // --group-by-cumulative, which no clear() precedes and which keeps the
  // postings' xdata) relates its own postings afresh and may hand on a
  // sibling this one already handed on.  Forget this stream's postings,
  // and drop the marks that hold only for one stream from them and their
  // siblings: received and handled here, displayed by the report at the
  // end of the chain, and sort_posts' cached sort keys, to which the next
  // sort would otherwise append a second set.
  for (post_t* post : posts)
    for (post_t* r_post : post->xact->posts)
      if (r_post->has_xdata()) {
        post_t::xdata_t& xdata(r_post->xdata());
        xdata.drop_flags(POST_EXT_RECEIVED | POST_EXT_HANDLED | POST_EXT_DISPLAYED |
                         POST_EXT_SORT_CALC);
        xdata.sort_values.clear();
      }
  posts.clear();
}

/*--- Display filtering and rounding ---*/

display_filter_posts::display_filter_posts(post_handler_ptr handler, report_t& _report,
                                           bool _show_rounding)
    : item_handler<post_t>(std::move(handler)), report(_report),
      display_amount_expr(report.HANDLER(display_amount_).expr),
      display_total_expr(report.HANDLER(display_total_).expr), show_rounding(_show_rounding),
      what_to_keep(report.what_to_keep()) {
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
 * The new display total is the stripped running total calc_posts cached for
 * the posting; when calc_posts kept none (non-default total expressions,
 * --lots, generated <Revalued> postings) it is computed and cached here.
 */
bool display_filter_posts::output_rounding(post_t& post) {
  bind_scope_t bound_scope(report, post);
  value_t new_display_total;

  if (show_rounding) {
    // calc_posts caches the stripped running total when the total
    // expressions are the defaults; otherwise compute and cache it here.
    post_t::xdata_t& xdata(post.xdata());
    if (!xdata.has_flags(POST_EXT_DISPLAY_TOTAL_CACHED)) {
      xdata.display_total = display_total_expr.calc(bound_scope).strip_annotations(what_to_keep);
      xdata.add_flags(POST_EXT_DISPLAY_TOTAL_CACHED);
    }
    new_display_total = xdata.display_total;

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

      if (value_t diff = precise_display_total - last_display_total) {
        DEBUG("filters.changed_value.rounding", "rounding.diff                  = " << diff);

        handle_value_options_t opts;
        opts.total = precise_display_total;
        opts.direct_amount = true;
        opts.bidir_link = false;
        handle_value(diff, rounding_account, post.xact, temps, handler, opts);
      }
    }
    if (show_rounding)
      last_display_total = new_display_total;
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

/*--- Revaluation (changed value) ---*/

namespace {
/**
 * Whether the report's amount expression is one that the report builds from
 * its own options and that passes a posting's amount on as it is, so that
 * with the posting's cost read in place of the amount it yields the cost of
 * the posting's contribution to the running total (see cost_baseline()).
 *
 * The base expression is that of the default report, of --basis, of --price
 * or of --dc, and each piece appended to it is one that --round or --unround
 * appends, the valuation at today that --round appends under -V, or the pair
 * of an amount and its cleared part that the cleared command appends.  The
 * test compares the expressions as strings, as report_t::default_amount_exprs()
 * does for the rounding decision in chain_post_handlers.
 */
bool amount_expr_reads_cost(const report_t& report) {
  // The base expression that the --dc handler in report.h installs.
  static const char* const dc_expr =
      "is_seq(amount) ? amount : (amount > 0 ? amount : 0, amount < 0 ? amount : 0)";
  static const char* const bases[] = {"amount", "rounded(cost)", "price", dc_expr};
  static const char* const pieces[] = {"roundto_commodity(amount_expr)", "unrounded(amount_expr)",
                                       "market(amount_expr, today, exchange)",
                                       "(amount, cleared ? amount : 0)"};
  auto listed = [](const string& expr, const auto& list) {
    return std::any_of(std::begin(list), std::end(list),
                       [&expr](const char* entry) { return expr == entry; });
  };
  const merged_expr_t& expr(report.HANDLER(amount_).expr);
  return listed(expr.base_expr, bases) &&
         std::all_of(expr.exprs.begin(), expr.exprs.end(),
                     [&listed](const string& piece) { return listed(piece, pieces); });
}

/**
 * Whether -X, when given, names a single commodity: the form that
 * value_t::exchange_commodities() values directly, with no list of
 * commodities, price expression or selective conversion.
 */
bool exchange_names_one_commodity(const report_t& report) {
  return !report.HANDLED(exchange_) ||
         report.HANDLER(exchange_).str().find_first_of(",=:") == string::npos;
}

/**
 * Whether changed_value_posts builds its report on a cost baseline (see the
 * class note): an accounts report (balance or cleared) under -V or -X with
 * --unrealized whose running total is the sum of the report's amount
 * expression over the journal's postings, and whose display shows that
 * total.
 *
 * Every other report keeps the between-posting revaluation:
 *
 * - Under -V or -X, --immediate values each posting on its own date
 *   already, so a cost baseline would book the difference between that value
 *   and the cost as an unrealized gain; for a lot sale that difference is the
 *   gain already realized, booked again as an unrealized loss
 *   (regress/unrealized-excluded-reports; regress/1821_2 guards against the
 *   same symptom).
 * - -H nails each posting at its value on its own date, and flush() books no
 *   revaluation under it, so a cost baseline would be compared with nothing;
 *   valuing from cost there omitted a balancing amount below the display
 *   precision under -X (regress/1692).
 * - -G (which sets --revalued-total) reports the gain itself, market value
 *   less cost; a cost baseline booked its unrealized row as the whole change
 *   in value since the purchase (regress/unrealized-excluded-reports).
 * - --plopen makes the amount a pair of amount and cost and values it at
 *   today, so no revaluation is booked; a cost baseline of plain amounts
 *   cannot be added to that pair.
 * - --percent shows each account's share of its parent, not a running total,
 *   and never values a holding; valuing one here could evaluate a lot's value
 *   expression that fails (regress/cov9-annotate-value-expr).
 * - --average shows a running average: it sets the display total to
 *   display_total/count, and the revaluation prices that display total, so
 *   the total revalued is not the sum over the postings seen here.
 *   --deviation shows each amount less the total.
 * - --period (and -M and the rest), --dow and --by-payee pass on subtotals
 *   that carry no cost; valuing them books the gain that regress/1821_2
 *   realizes a second time under -M.
 * - Budget reports add generated budget postings to the running total,
 *   which is then no sum over the journal's postings, and the budget
 *   command pairs each amount with its budget.
 * - Under --forecast the forecast posting that ends the forecast passes this
 *   handler and enters the running total, but forecast_posts::flush then
 *   withholds it from the account totals (it drops POST_EXT_VISITED), so it
 *   is never displayed.
 * - Under --anon, anonymize_posts relabels a posting's cost and the price in
 *   its lot annotation separately and can give one commodity two labels (in
 *   regress/1190 the cost is in B and the fixated price in C), so the cost is
 *   not in the terms in which the lot is valued.
 * - --pivot, --pivot-only, --account, --payee and --date (transfer_details)
 *   and --inject (inject_posts) copy a posting into a transaction of its
 *   own, dated by the posting.  A posting that carries its own date then no
 *   longer shares the date on which cost_baseline values the costs of the
 *   rest of its transaction, so under -X the two sides of a balanced
 *   transaction are converted at different rates
 *   (regress/unrealized-footing-posting-date).
 *   regress/unrealized-excluded-reports runs the --inject report.  A rewrite
 *   rule is no such case: rewrite_posts copies a rewritten transaction
 *   whole, under the transaction's own date
 *   (regress/unrealized-account-rewrite, regress/unrealized-payee-rewrite
 *   and regress/unrealized-rewrite-option).
 * - An amount expression of the user's own (--amount or -t, or the collect
 *   clause of select), and any other than those that amount_expr_reads_cost
 *   accepts: cost_baseline reads the cost in place of the amount, which gives
 *   the cost of the posting's contribution only for an expression that
 *   passes the amount on as it is, as the report's own expressions do.  An
 *   expression that values the amount, tests its commodity or drops the
 *   commodity does not.  market(amount, value_date, exchange), given by hand,
 *   is the expression that --immediate installs, and a baseline at cost
 *   booked the gain realized by a sale a second time
 *   (regress/unrealized-excluded-reports).
 * - -X naming more than one commodity (a list, a price expression or a
 *   selective conversion): such a report can value a holding into one of
 *   its commodities and leave the cost of the holding in another, which it
 *   does not convert, so the holding would book no gain where the
 *   revaluation between postings books one (under -X 'EUR,$' in
 *   regress/unrealized-unconvertible-cost).
 */
bool report_revalues_between_postings(const report_t& report) {
  // The reports for which the footing machinery keeps the between-posting
  // revaluation rather than a cost baseline or a booked unrealized gain: the
  // exclusions of uses_cost_baseline(), each with its witness in the class
  // note above.  A balance report in one of these shapes keeps the
  // revaluation between postings (and books no whole-report unrealized gain),
  // as every non-accounts report does.
  return !report.HANDLED(historical) && !report.HANDLED(immediate) &&
         !report.HANDLED(revalued_total_) && !report.HANDLED(plopen) && !report.HANDLED(percent) &&
         !report.HANDLED(average) && !report.HANDLED(deviation) && !report.HANDLED(period_) &&
         !report.HANDLED(dow) && !report.HANDLED(by_payee) &&
         report.budget_flags == BUDGET_NO_BUDGET && !report.HANDLED(forecast_while_) &&
         !report.HANDLED(anon) && !report.HANDLED(pivot_) && !report.HANDLED(pivot_only_) &&
         !report.HANDLED(account_) && !report.HANDLED(payee_) && !report.HANDLED(date_) &&
         !report.HANDLED(inject_);
}

bool uses_cost_baseline(const report_t& report, bool for_accounts_report) {
  return for_accounts_report && report.HANDLED(market) && amount_expr_reads_cost(report) &&
         exchange_names_one_commodity(report) && report_revalues_between_postings(report);
}
} // namespace

changed_value_posts::changed_value_posts(post_handler_ptr handler, report_t& _report,
                                         bool _for_accounts_report,
                                         display_filter_posts* _display_filter)
    : item_handler<post_t>(std::move(handler)), report(_report),
      total_expr(report.HANDLED(revalued_total_) ? report.HANDLER(revalued_total_).expr
                                                 : report.HANDLER(display_total_).expr),
      display_total_expr(report.HANDLER(display_total_).expr), cost_expr("cost"),
      changed_values_only(report.HANDLED(revalued_only)),
      historical_prices_only(report.HANDLED(historical)), for_accounts_report(_for_accounts_report),
      suppress_for_plot(report.HANDLED(period_) &&
                        (report.HANDLED(amount_data) || report.HANDLED(total_data))),
      value_from_cost(uses_cost_baseline(_report, _for_accounts_report)),
      amount_expr(report.HANDLER(amount_).expr),
      held_value_expr("market(amount, value_date, exchange)"),
      baseline_expr("market(amount_expr, value_date, exchange)"), last_post(nullptr),
      display_filter(_display_filter) {
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

  TRACE_CTOR(changed_value_posts, "post_handler_ptr, report_t&, bool, display_filter_posts *");
}

/**
 * Evaluate @p expr for @p post as though the posting were dated @p date and,
 * when given, as though @p amount were its amount and @p total its running
 * total.
 *
 * market(..., value_date, exchange) reads the posting's value date, which
 * get_value_date() takes from xdata.value_date before xdata.date and date(),
 * so both fields are overridden.  An amount is substituted the way filters
 * substitute one, as the compound value that get_amount() returns in place
 * of post.amount; a total replaces xdata.total, which get_total() returns.
 * Every field is restored afterwards, also when the evaluation throws, so
 * the posting is left as it was.
 */
value_t calc_as_of(report_t& report, post_t& post, const date_t& date, expr_t& expr,
                   const value_t* amount = nullptr, const value_t* total = nullptr) {
  post_t::xdata_t& xdata(post.xdata());
  const date_t saved_date = xdata.date;
  const date_t saved_value_date = xdata.value_date;
  const bool saved_compound_flag = xdata.has_flags(POST_EXT_COMPOUND);
  const value_t saved_compound_value = xdata.compound_value;
  const value_t saved_total = xdata.total;
  auto restore = [&]() {
    xdata.date = saved_date;
    xdata.value_date = saved_value_date;
    xdata.compound_value = saved_compound_value;
    if (!saved_compound_flag)
      xdata.drop_flags(POST_EXT_COMPOUND);
    xdata.total = saved_total;
  };
  xdata.date = date;
  xdata.value_date = date;
  if (amount) {
    xdata.compound_value = *amount;
    xdata.add_flags(POST_EXT_COMPOUND);
  }
  if (total)
    xdata.total = *total;
  try {
    bind_scope_t bound_scope(report, post);
    value_t result = expr.calc(bound_scope);
    restore();
    return result;
  } catch (...) {
    restore();
    throw;
  }
} // namespace

/**
 * Emit final revaluation up to the report terminus date.
 *
 * After all real postings have been processed, this generates one last
 * revaluation for any price changes between the last posting and the
 * report's terminus date.  A balance report that books its gains from cost
 * basis instead books all of its unrealized gain here.
 */
void changed_value_posts::flush() {
  if (last_post && value_from_cost) {
    // The report works from cost baselines: its baseline is the running
    // total at cost (cost_baseline(), accumulated per posting), valued at
    // the terminus through the report's total expression, and one
    // revaluation at the terminus books the whole change in value, even
    // when the last posting is dated after the terminus: the report values
    // every holding at the terminus regardless of when it was acquired,
    // and the equity rows must cover that whole change.
    if (!historical_prices_only) {
      if (!for_accounts_report && !suppress_for_plot)
        output_intermediate_prices(*last_post, report.terminus.date());
      last_total =
          calc_as_of(report, *last_post, report.terminus.date(), total_expr, nullptr, &cost_total);
      output_revaluation(*last_post, report.terminus.date());
    }
    last_post = nullptr;
  } else if (last_post && books_from_basis()) {
    output_unrealized_gain(*last_post);
    last_post = nullptr;
  } else if (last_post && last_post->date() <= report.terminus.date()) {
    if (!historical_prices_only) {
      // In plot mode (-j/-J) combined with period mode (-M/--period),
      // suppress intermediate price entries: each period already reflects
      // the correct market value at its end date, and extra intra-period
      // revaluations would create unwanted extra data points for gnuplot
      // (issue #984).
      if (!for_accounts_report && !suppress_for_plot)
        output_intermediate_prices(*last_post, report.terminus.date());
      output_revaluation(*last_post, report.terminus.date());
    }
    last_post = nullptr;
  }
  item_handler<post_t>::flush();
}

/**
 * Evaluate the total expression for @p post as though the posting fell on
 * @p date, or on its own date when @p date is not valid.
 *
 * The posting's xdata date is overridden only for the evaluation.
 */
value_t changed_value_posts::total_as_of(post_t& post, const date_t& date) {
  // When interval_posts creates period-end synthetic postings, it sets
  // xdata.value_date = range_finish (the period-end date).  Because
  // get_value_date() checks xdata.value_date before falling back to date(),
  // simply setting xdata.date (as done below) is insufficient: the
  // market(display_total, value_date, exchange) expression would still use
  // the period-end date rather than the requested revaluation date.
  // Save and restore xdata.value_date so that intermediate price dates are
  // correctly used when called from output_intermediate_prices.  xdata.date
  // is restored as well, since a collapsed posting carries one of its own.
  date_t saved_date;
  date_t saved_value_date;
  const bool date_overridden = is_valid(date);
  if (date_overridden) {
    post_t::xdata_t& xdata(post.xdata());
    saved_date = xdata.date;
    saved_value_date = xdata.value_date;
    xdata.date = date;
    xdata.value_date = date;
  }

  value_t total;
  try {
    bind_scope_t bound_scope(report, post);
    total = total_expr.calc(bound_scope);
  } catch (...) {
    if (date_overridden) {
      post.xdata().date = saved_date;
      post.xdata().value_date = saved_value_date;
    }
    throw;
  }
  if (date_overridden) {
    post.xdata().date = saved_date;
    post.xdata().value_date = saved_value_date;
  }
  return total;
}

/**
 * Emit @p diff, a change in the market value of the running total, as a
 * synthetic posting dated @p date: to the <Revalued> account in a register
 * report, or to the equity gain/loss accounts in a balance report with
 * --unrealized.
 */
void changed_value_posts::output_value_change(post_t& post, const date_t& date,
                                              const value_t& diff) {
  DEBUG("filters.changed_value",
        "output_value_change(strip(diff)) = " << diff.strip_annotations(report.what_to_keep()));

  xact_t& xact = temps.create_xact();
  xact.payee = _("Commodities revalued");
  xact._date = is_valid(date) ? date : post.value_date();

  handle_value_options_t opts;
  opts.date = *xact._date;
  opts.source_post = &post;
  // NOLINTBEGIN(bugprone-branch-clone)
  if (!for_accounts_report) {
    opts.total = repriced_total;
    handle_value(diff, revalued_account, &xact, temps, handler, opts);
  } else {
    opts.mark_visited = true;
    handle_value(-diff, (diff < 0L ? losses_equity_account : gains_equity_account), &xact, temps,
                 handler, opts);
  }
  // NOLINTEND(bugprone-branch-clone)
}

/**
 * Generate a revaluation posting if the repriced total differs from last_total.
 *
 * Evaluates the total expression as of @p date and compares it with
 * last_total.  If they differ, output_value_change() emits the difference.
 */
void changed_value_posts::output_revaluation(post_t& post, const date_t& date) {
  repriced_total = total_as_of(post, date);

  DEBUG("filters.changed_value", "output_revaluation(last_total)     = " << last_total);
  DEBUG("filters.changed_value", "output_revaluation(repriced_total) = " << repriced_total);

  if (!last_total.is_null()) {
    if (value_t diff = repriced_total - last_total)
      output_value_change(post, date, diff);
  }
}

/**
 * Whether the report books its unrealized gain from cost basis.
 *
 * A balance report does, unless it uses historical prices, or its total has
 * several columns, such as the market value and cost pairs of --gain or the
 * totals of cleared and budget reports, which leave no basis to compare.
 * Those are revalued by output_revaluation() alone, as they were before.
 */
bool changed_value_posts::books_from_basis() const {
  // Under -V or -X with --unrealized, a balance report whose running total
  // is the sum of the postings keeps the revaluation between postings
  // whenever it is not one the cost-baseline machinery shapes: the report's
  // own amount expression and a single -X commodity are part of that shape,
  // and every excluded option or journal feature is (see
  // report_revalues_between_postings() and the class note's witnesses).  A
  // custom amount expression or a multi-commodity -X books the whole value
  // of the holding as a gain under the basis footing
  // (regress/unrealized-excluded-reports,
  // regress/unrealized-unconvertible-cost).
  if (report.HANDLED(market) &&
      (!amount_expr_reads_cost(report) || !exchange_names_one_commodity(report) ||
       !report_revalues_between_postings(report)))
    return false;
  return for_accounts_report && !historical_prices_only && !last_total.is_sequence() &&
         !value_from_cost;
}

/**
 * Book a balance report's unrealized gain or loss, revaluing the running
 * total at its last posting, @p post, to the report's terminus date.
 *
 * The gain is the running total's market value at the terminus less the
 * cost basis of its postings, booked as one posting: to the gains account
 * when it is a gain, and to the losses account when a loss.  Booked at each
 * date instead, it would be split between the two accounts according to
 * when unrelated transactions happened to fall.  The report values its
 * accounts at the terminus, so the total is revalued to it even from a
 * posting dated after it.
 */
void changed_value_posts::output_unrealized_gain(post_t& post) {
  const date_t terminus(report.terminus.date());
  if (!last_total.is_null())
    unrealized_gain += total_as_of(post, terminus) - last_total;

  DEBUG("filters.changed_value", "output_unrealized_gain = " << unrealized_gain);

  if (unrealized_gain)
    output_value_change(post, terminus, unrealized_gain);
  unrealized_gain = value_t();
}

namespace {
/// Whether @p value holds an amount of @p commodity.  A market valuation
/// leaves in place any amount it has no price for.
bool holds_commodity(const value_t& value, const commodity_t& commodity) {
  if (value.is_amount())
    return value.as_amount().has_commodity() &&
           value.as_amount().commodity().referent() == commodity.referent();
  if (value.is_balance()) {
    for (const balance_t::amounts_map::value_type& pair : value.as_balance().amounts)
      if (pair.first->referent() == commodity.referent())
        return true;
  }
  return false;
}
} // namespace

/**
 * Return the gain that @p post brings into a balance report's running total
 * by entering it at a market value other than its cost basis.
 *
 * Revaluing the running total measures how its market value moves, but a
 * posting whose cost basis differs from its market value moves it too,
 * with no price changing.  A lot sold above its basis takes out more market
 * value than basis, the difference now being a realized gain.  And a stock
 * split's own prices value the shares held before it at the split's new
 * price, though the split exchanges them at their basis (#1821).  The gain
 * returned, the posting's market value less its basis on its own date,
 * takes such a change back out of the unrealized gain.
 *
 * A posting's basis is its cost, as --basis reports it.  A posting that sums
 * others, such as a --period subtotal of several commodities, keeps their
 * lots but not their costs, so each of its lots is valued at its lot price.
 */
value_t changed_value_posts::basis_gain(post_t& post) {
  const value_t& value(post.xdata().visited_value);
  value_t gain;
  if (value.is_amount()) {
    bind_scope_t bound_scope(report, post);
    gain = basis_gain(post, value.as_amount(), cost_expr.calc(bound_scope));
  } else if (value.is_balance()) {
    for (const balance_t::amounts_map::value_type& pair : value.as_balance().amounts)
      if (std::optional<amount_t> price = pair.second.price())
        gain += basis_gain(post, pair.second, *price);
  }
  return gain;
}

/**
 * Return what @p amount, entering the running total at @p post, is worth
 * over its cost basis @p basis.
 *
 * An amount that does not differ from its basis brings no gain, and neither
 * does one whose commodity the report has no price for: its basis cannot be
 * compared, so it keeps the value it entered at.
 */
value_t changed_value_posts::basis_gain(post_t& post, const amount_t& amount, value_t basis) {
  // A cost may keep its full precision, but the gain is judged at display
  // precision, as revaluations are, so that no residue below it is booked.
  basis.in_place_round();
  if (basis == amount)
    return value_t();

  // Value the difference by standing it in for the posting's running total,
  // the way total_as_of() stands in another date.
  post_t::xdata_t& xdata(post.xdata());
  value_t total = value_t(amount) - basis;
  std::swap(xdata.total, total);
  value_t gain;
  try {
    bind_scope_t bound_scope(report, post);
    gain = total_expr.calc(bound_scope);
  } catch (...) {
    std::swap(xdata.total, total);
    throw;
  }
  std::swap(xdata.total, total);

  if (holds_commodity(gain, amount.commodity()))
    return value_t();
  return gain;
}

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
/**
 * The contribution of @p post to cost_total under value_from_cost.
 *
 * It is the report's amount expression with the posting's cost read in place
 * of its amount, valued on the transaction's date, or on the terminus for a
 * transaction dated after it, so that no price later than the report's
 * valuation date enters the baseline.  Valuing every posting of a
 * transaction on one date converts them all at the same rates, so a
 * transaction that balances at cost contributes zero even when a posting
 * carries its own date.  That holds because every posting seen here belongs
 * to its journal transaction, or to the copy rewrite_posts makes of the whole
 * transaction under the same date: uses_cost_baseline excludes the reports
 * whose handlers copy a posting into a transaction of its own, dated by the
 * posting.  Evaluating the amount expression gives the contribution the shape
 * of the posting's own contribution to the running total: a debit and credit
 * pair under --dc, a plain amount otherwise.
 *
 * Where the report cannot compare the posting's own contribution with its
 * cost at the terminus (incomparable_at_terminus), because it leaves the
 * holding unvalued or cannot bring the cost to the commodity in which it
 * values the holding, the posting contributes that instead, so that it adds
 * nothing to the unrealized rows.  The test is needed only when the two
 * contributions differ.  It is made one posting at a time, so a sale whose
 * cost, the proceeds, the report cannot bring to the commodity in which it
 * values the lot contributes the units sold at their value at the terminus,
 * while their purchase contributed its cost; the change in the price of those
 * units since their purchase then joins the unrealized rows
 * (regress/unrealized-unconvertible-proceeds).
 */
value_t changed_value_posts::cost_baseline(post_t& post) {
  value_t contribution;
  value_t cost;
  {
    bind_scope_t bound_scope(report, post);
    contribution = amount_expr.calc(bound_scope);
    cost = cost_expr.calc(bound_scope);
  }
  const date_t terminus = report.terminus.date();
  date_t date = post.xact ? post.xact->date() : post.date();
  if (terminus < date)
    date = terminus;
  value_t at_cost = calc_as_of(report, post, date, baseline_expr, &cost);
  if (at_cost.is_sequence() == contribution.is_sequence() && at_cost == contribution)
    return at_cost;
  return incomparable_at_terminus(post, contribution, at_cost) ? contribution : at_cost;
}

namespace {
/// @p value as a balance, or a null value when it holds no amount.
value_t as_balance(const value_t& value) {
  if (value.is_amount() && !value.as_amount().is_null())
    return balance_t(value.as_amount());
  if (value.is_balance())
    return value;
  return value_t();
}

/// The commodities of the amounts in @p value.
std::set<const commodity_t*> commodities_of(const value_t& value) {
  std::set<const commodity_t*> commodities;
  if (value.is_amount())
    commodities.insert(&value.as_amount().commodity());
  else if (value.is_balance())
    for (const auto& pair : value.as_balance().amounts)
      commodities.insert(pair.first);
  return commodities;
}
} // namespace

/**
 * Whether the report cannot compare @p contribution, a posting's contribution
 * to the running total, with @p at_cost, the posting's cost baseline, because
 * valuing each of them at the terminus leaves them in different commodities.
 *
 * That is so for a holding the report leaves as it is, one in a commodity
 * with no price (under -V also one declared with N) or an amount with no
 * commodity, when its cost is valued into another commodity.  It is so also
 * for a holding that the report values into a commodity to which it cannot
 * bring the cost: under -X, a holding in the exchange commodity bought with
 * a currency that has no price in it (regress/unrealized-unconvertible-cost),
 * and under -V, a holding whose most recent price is in another commodity at
 * the terminus than on its transaction's date
 * (regress/unrealized-valuation-commodity-change).  Booking either at its
 * cost set the holding against the amount that paid for it in an unrealized
 * row of mixed commodities.  A contribution that the amount expression has
 * valued already (under -V with --round it values each posting), or that is
 * in the valuation commodity, stays in the commodity of its valued cost and
 * is compared.  A cost of zero holds no commodity once valued; it is
 * compared with any holding that the report values, and not with one that
 * the report leaves as it is.
 *
 * The contribution is valued in the form of a balance, the form the running
 * total takes once it holds more than one commodity: market() values a lone
 * annotated amount at its lot price when it finds no market price, but not a
 * balance, so testing a lone amount would call a lot valuable that the
 * report leaves unvalued (regress/3227 under -X).  It is the contribution
 * that is tested, not the posting's amount, since under --basis the running
 * total holds the cost.  Under --dc each column that holds an amount is
 * tested against the same column of @p at_cost, and the empty column, which
 * holds zero, is passed over.
 */
bool changed_value_posts::incomparable_at_terminus(post_t& post, const value_t& contribution,
                                                   const value_t& at_cost) {
  if (contribution.is_sequence()) {
    if (!at_cost.is_sequence() || at_cost.as_sequence().size() != contribution.as_sequence().size())
      return false;
    bool tested = false;
    for (std::size_t i = 0; i < contribution.as_sequence().size(); ++i) {
      const value_t& column(contribution.as_sequence()[i]);
      if ((!column.is_amount() && !column.is_balance()) || column.is_realzero())
        continue;
      if (!incomparable_at_terminus(post, column, at_cost.as_sequence()[i]))
        return false;
      tested = true;
    }
    return tested;
  }
  value_t held = as_balance(contribution);
  if (held.is_null() || held.is_realzero())
    return false;
  const date_t terminus = report.terminus.date();
  const value_t held_value = calc_as_of(report, post, terminus, held_value_expr, &held);
  value_t baseline = as_balance(at_cost);
  if (!baseline.is_null())
    baseline = calc_as_of(report, post, terminus, held_value_expr, &baseline);
  if (commodities_of(held_value) == commodities_of(baseline))
    return false;
  if (baseline.is_null() || baseline.is_realzero())
    return held_value == held;
  return true;
}

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

    xdata.compound_value = last_total;
    xdata.add_flags(POST_EXT_COMPOUND);

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
    // One revaluation per distinct pricing date between the two postings;
    // output_revaluation() values the total as of that date.
    std::set<date_t> pricing_dates;
    for (const balance_t::amounts_map::value_type& amt_comm : display_total.as_balance().amounts)
      amt_comm.first->map_prices(
          [&pricing_dates](datetime_t when, const amount_t&) {
            DEBUG("filters.revalued", "pricing date " << when.date());
            pricing_dates.insert(when.date());
          },
          datetime_t(current), datetime_t(post.value_date()), true);

    for (const date_t& date : pricing_dates) {
      output_revaluation(post, date);
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
 * A balance report that books its gains from cost basis instead gathers its
 * unrealized gain, as the revaluation of the running total to each new date
 * plus the basis gain of each posting, and books it once, in flush().
 *
 * The posting is then forwarded downstream, and the current total is
 * captured as last_total for the next comparison.
 */
void changed_value_posts::operator()(post_t& post) {
  // The report works from cost baselines: valuing each posting at its cost
  // avoids the between-posting revaluations that leave a balance report out
  // of balance whenever a transaction does not balance at the market price
  // of its own date: two lots of one commodity bought on one day at
  // different costs (the price history keeps one price per timestamp), a
  // stock split (#1821), or a sale balanced at its lot price.  The report
  // total is then the journal's balance at cost and, when the costs are in
  // the valuation commodity, the unrealized rows are minus the --gain total.
  if (value_from_cost) {
    if (changed_values_only)
      post.xdata().add_flags(POST_EXT_DISPLAYED);
    item_handler<post_t>::operator()(post);
    cost_total += cost_baseline(post);
    last_post = &post;
    return;
  }

  if (last_post) {
    if (books_from_basis()) {
      // A day's own prices take part in both its revaluation and its basis
      // gains: a stock split values the shares held before it at the split's
      // new price, but exchanges them at their basis.  Only their sum is a
      // change in unrealized gain, which is why it is booked as a whole.
      if (last_post->value_date() != post.value_date() && !last_total.is_null())
        unrealized_gain += total_as_of(*last_post, post.value_date()) - last_total;
    } else if (last_post->xact != post.xact) {
      // Don't generate spurious revaluations when postings are from the same
      // transaction (e.g., stock splits with multiple prices on the same
      // date).  Within a transaction, postings represent a complete operation.

      // In plot mode (-j/-J) combined with period mode (-M/--period),
      // suppress intermediate price entries between periods so that each
      // period produces exactly one data point for gnuplot (issue #984).
      if (!for_accounts_report && !historical_prices_only && !suppress_for_plot)
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

  if (books_from_basis())
    unrealized_gain += basis_gain(post);
}

/*--- Subtotaling ---*/

/**
 * operator() marks each component posting compound, with its amount as the
 * value.  That mark is of this subtotal only: under --group-by-cumulative,
 * which keeps the postings' xdata between groups, --related can hand one of
 * them on to a later group, whose amount expression would otherwise read the
 * stale amount instead of evaluating the posting (with `cleared`, a bare
 * amount added to a sequence).  Restored in reverse, so a posting handed in
 * twice gets back what it came with the first time.
 */
void subtotal_posts::forget_component_posts() {
  for (auto i = saved_compounds.rbegin(); i != saved_compounds.rend(); ++i) {
    post_t::xdata_t& xdata(i->post->xdata());
    xdata.compound_value = i->value;
    if (!i->compound)
      xdata.drop_flags(POST_EXT_COMPOUND);
  }
  saved_compounds.clear();
  component_posts.clear();
}

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
 * @param payee     Optional payee, used verbatim instead of the date (by_payee_posts).
 */
void subtotal_posts::report_subtotal(const char* spec_fmt,
                                     const optional<date_interval_t>& interval,
                                     const optional<string>& payee) {
  if (component_posts.empty())
    return;

  optional<date_t> range_start = interval ? interval->start : none;
  optional<date_t> range_finish = interval ? interval->inclusive_end() : none;

  if (!range_start || !range_finish) {
    for (post_t* post : component_posts) {
      date_t date = post->date();
      date_t value_date = post->value_date();
      if (!range_start || date < *range_start)
        range_start = date;
      if (!range_finish || value_date > *range_finish)
        range_finish = value_date;
    }
  }
  forget_component_posts();

  std::ostringstream out_date;
  if (!payee) {
    if (spec_fmt) {
      out_date << format_date(*range_finish, FMT_CUSTOM, spec_fmt);
    } else {
      out_date << "- " << format_date(*range_finish);
    }
  }

  xact_t& xact = temps.create_xact();
  xact.payee = payee ? *payee : out_date.str();
  xact._date = *range_start;

  handle_value_options_t opts;
  opts.date = *range_finish;
  opts.act_date_p = false;
  opts.subtotal_row = true;
  for (values_map::value_type& pair : values) {
    opts.force_virtual = pair.second.is_virtual;
    opts.force_must_balance = pair.second.must_balance;
    handle_value(pair.second.value, pair.second.account, &xact, temps, handler, opts);
  }

  values.clear();
}

/**
 * Accumulate a posting into the subtotal.
 *
 * The posting's amount is evaluated via amount_expr and added to the
 * values_map entry for its account.  Virtual and non-virtual postings to the
 * same account are tracked separately (the key carries the posting's
 * virtual and must-balance flags) so they can be emitted with correct
 * virtual flags in report_subtotal.
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

  post_t::xdata_t& xdata(post.xdata());
  saved_compounds.push_back({&post, xdata.has_flags(POST_EXT_COMPOUND), xdata.compound_value});
  xdata.compound_value = value_t(post.amount);
  xdata.add_flags(POST_EXT_COMPOUND);

  const bool is_virtual = post.has_flags(POST_VIRTUAL);
  const bool must_balance = is_virtual && post.has_flags(POST_MUST_BALANCE);
  const values_key key(acct->fullname(), is_virtual, must_balance);

  values_map::iterator i = values.find(key);
  if (i == values.end()) {
    values.emplace(key, acct_value_t(acct, amount, is_virtual, must_balance));
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
  if (!(interval.begin() && interval.find_period(*interval.begin(), align_intervals))) {
    // Determine the beginning interval.  When --empty is active and a report
    // begin date was supplied that precedes the first posting, start from that
    // date so that empty leading periods are counted (important for --average,
    // which uses the period count as its denominator).  Otherwise fall back to
    // the earliest posting date as before.
    date_t anchor;
    if (generate_empty_posts && begin_of_report_ && !all_posts.empty() &&
        *begin_of_report_ < all_posts.front()->date())
      anchor = *begin_of_report_;
    else if (!all_posts.empty())
      anchor = all_posts.front()->date();
    else
      anchor = date_t(); // empty: nothing to do

    if (!all_posts.empty() && !interval.find_period(anchor, align_intervals))
      throw_(std::logic_error, _("Failed to find period for interval report"));
  }

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
        report_empty_period();
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

  // Generate trailing empty periods up to the report end date when --empty
  // is active and --end was specified.  This mirrors the leading-period
  // logic that uses begin_of_report_ to anchor the start.
  if (generate_empty_posts && end_of_report_) {
    ++interval;
    while (interval.start && *interval.start < *end_of_report_) {
      DEBUG("filters.interval", "Generating trailing empty post for " << *interval.start);
      report_empty_period();
      ++interval;
    }
  }

  // Tell our parent class to flush
  subtotal_posts::flush();

  // Everything seen so far is reported.  Under --group-by-cumulative
  // the splitter flushes this filter again for the next group without
  // a clear(), so start that group from the original period with no
  // postings, as clear() would, but keep temps: the postings reported
  // above must outlive the report.
  all_posts.clear();
  interval = start_interval;
}

/// Subtotal one <None> posting of 0 for the current interval (--empty).
void interval_posts::report_empty_period() {
  xact_t& null_xact = temps.create_xact();
  null_xact._date = interval.inclusive_end();

  post_t& null_post = temps.create_post(null_xact, empty_account);
  null_post.add_flags(POST_CALCULATED);
  null_post.amount = 0L;

  subtotal_posts::operator()(null_post);
  report_subtotal(interval);
}

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
  forget_component_posts();

  xact_t& xact = temps.create_xact();
  xact.payee = _("Opening Balances");
  xact._date = finish;

  const bool invert = report.HANDLED(invert);

  value_t total = 0L;
  handle_value_options_t opts;
  opts.date = finish;
  opts.act_date_p = false;
  opts.subtotal_row = true;
  for (values_map::value_type& pair : values) {
    value_t value(pair.second.value.strip_annotations(report.what_to_keep()));
    if (unround)
      value.in_place_unround();
    if (invert)
      value.in_place_negate();
    opts.force_virtual = pair.second.is_virtual;
    opts.force_must_balance = pair.second.must_balance;
    auto emit = [&](const amount_t& amt) {
      if (!amt.is_zero())
        handle_value(amt, pair.second.account, &xact, temps, handler, opts);
    };
    if (!value.is_zero()) {
      if (value.is_balance())
        value.as_balance_lval().map_sorted_amounts(emit);
      else
        emit(value.to_amount());
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
    auto balance = [&](const amount_t& amount) {
      if (amount.is_zero())
        return;
      post_t& balance_post = temps.create_post(xact, balance_account);
      balance_post.amount = -amount;
      (*handler)(balance_post);
    };
    if (total.is_balance())
      total.as_balance_lval().map_sorted_amounts(balance);
    else
      balance(total.to_amount());
  }
}

/*--- By-payee subtotaling ---*/

/// Flush each payee's subtotal_posts instance and forward results downstream.
void by_payee_posts::flush() {
  for (payee_subtotals_map::value_type& pair : payee_subtotals)
    pair.second->report_subtotal(nullptr, none, pair.first);

  item_handler<post_t>::flush();

  // The next stream (the next group under --group-by-cumulative, which no
  // clear() precedes) starts with no payees.  The flushed subtotals stay
  // alive until clear(): they own the postings just reported, which the
  // handlers downstream (calc_posts' last posting, an accounts report's
  // account totals, a Journal.query result) still point at.
  for (payee_subtotals_map::value_type& pair : payee_subtotals)
    reported_subtotals.push_back(std::move(pair.second));
  payee_subtotals.clear();
}

/// Route each posting to the subtotal_posts instance for its payee.
void by_payee_posts::operator()(post_t& post) {
  payee_subtotals_map::iterator i = payee_subtotals.find(post.payee());
  if (i == payee_subtotals.end())
    i = payee_subtotals
            .emplace(post.payee(), std::make_shared<subtotal_posts>(handler, amount_expr))
            .first;
  (*i->second)(post);
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
 * - SET_ACCOUNT_REPLACE: the expression result replaces the account entirely.
 * - SET_PAYEE: the transaction payee is replaced.
 */
post_t& transfer_details::transfer(post_t& post) {
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

    case SET_ACCOUNT:
    case SET_ACCOUNT_REPLACE: {
      string account_name = substitute.to_string();

      if (!account_name.empty() && account_name[account_name.length() - 1] != ':') {
        // SET_ACCOUNT prepends the expression result to the original account
        // hierarchy, while SET_ACCOUNT_REPLACE drops the original account
        // entirely (for --pivot-only, issue #1153).
        if (which_element == SET_ACCOUNT) {
          account_name += ':';
          account_name += temp.account->fullname();
        }
        retarget_temp_post(temp, account_name, accounts, xact.journal->master);
      }
      break;
    }

    case SET_PAYEE:
      xact.payee = substitute.to_string();
      break;
    }
  }

  return temp;
}

void transfer_details::operator()(post_t& post) {
  item_handler<post_t>::operator()(transfer(post));
}

/// Evaluate the predicate on the posting, or on its view->transfer() copy
/// when a view is set, and forward the original posting when it matches.
void filter_posts::operator()(post_t& post) {
  bind_scope_t bound_scope(context, view ? view->transfer(post) : post);
  if (pred(bound_scope))
    (*handler)(post);
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
      if (!post->has_flags(ITEM_GENERATED) || post->has_flags(POST_CALCULATED)) {
        if (xact->period.duration) {
          add_post(xact->period, *post);
        } else {
          // Default to monthly when no duration is specified (#1625).
          date_interval_t period(xact->period);
          period.duration = date_duration_t(date_duration_t::MONTHS, 1);
          add_post(period, *post);
        }
      }
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
        if (!pair.first.find_period(range_begin ? *range_begin : (origin ? *origin : date)))
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
        temp.add_flags(ITEM_GENERATED);
        temp.in_place_negate();

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
 * Add a periodic posting for forecasting, initializing its interval.
 *
 * Unlike generate_posts::add_post, this override initializes the interval
 * via find_period and advances it past all fully-elapsed historical periods
 * so that only the current and future periods are generated during flush().
 *
 * Handles three cases:
 *  - Normal: period contains CURRENT_DATE -> advance past elapsed periods
 *  - Future: period starts after CURRENT_DATE -> keep as-is for flush()
 *  - Expired: period finished before CURRENT_DATE -> silently drop
 */
void forecast_posts::add_post(const date_interval_t& period, post_t& post) {
  date_interval_t i(period);

  // Try to initialize and position the interval at CURRENT_DATE.
  if (!i.start)
    i.find_period(CURRENT_DATE());

  // If stabilization failed entirely, nothing to forecast.
  if (!i.start)
    return;

  // Drop intervals whose finish date is entirely in the past.
  if (i.finish && CURRENT_DATE() > *i.finish)
    return;

  generate_posts::add_post(i, post);

  // Advance the stored interval past all fully-elapsed periods, keeping
  // the period that contains CURRENT_DATE() so the current period's
  // forecast is included.
  date_interval_t& stored = pending_posts.back().first;
  if (!stored.next)
    stored.stabilize(CURRENT_DATE());

  // For future-start periods (start > CURRENT_DATE), the loop below
  // is a no-op, which is correct: flush() will emit the first posting
  // at start (the future from-date).
  while (stored.next && *stored.next <= CURRENT_DATE())
    ++stored;
}

/**
 * Generate all forecast postings into the future.
 *
 * Iterates through pending periodic postings, generating synthetic postings
 * in date order.  All postings sharing the same earliest start date are
 * generated as a group before evaluating the continuation predicate.  This
 * prevents a same-date posting that temporarily fails the predicate from
 * permanently removing a periodic transaction.
 *
 * The continuation predicate is evaluated after each posting is submitted
 * downstream, so that calc_posts has updated the running total.  This
 * supports both date-based predicates (e.g., 'd<[2024/09/01]') and
 * value-based predicates (e.g., 'T>={0}') that depend on the accumulated
 * total.  In posting reports, the downstream filter_posts(forecast_while) in
 * chain.cc provides additional filtering at the output level.
 *
 * A periodic posting is removed from consideration when:
 * - Its start date exceeds forecast_years beyond the last generated date, or
 * - Its group's last matching posting fails the continuation predicate, or
 * - Its interval has no more occurrences.
 */
void forecast_posts::flush() {
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
    // Find the earliest start date among all pending postings.
    pending_posts_list::iterator least = pending_posts.begin();
    for (pending_posts_list::iterator i = ++pending_posts.begin(); i != pending_posts.end(); i++) {
      assert((*i).first.start);
      assert((*least).first.start);
      if (*(*i).first.start < *(*least).first.start)
        least = i;
    }

    date_t earliest_date = *(*least).first.start;

    // Collect all pending postings that share this earliest start date,
    // so they can be generated and evaluated as a group.
    std::list<pending_posts_list::iterator> same_date;
    for (pending_posts_list::iterator i = pending_posts.begin(); i != pending_posts.end(); i++) {
      if (*(*i).first.start == earliest_date)
        same_date.push_back(i);
    }

    // Check the 5-year safety limit.  Only applies when the earliest
    // date is beyond last (CURRENT_DATE); when including the current
    // period, start may precede CURRENT_DATE, which is not a concern.
    if (earliest_date > last && static_cast<std::size_t>((earliest_date - last).days()) >
                                    static_cast<std::size_t>(365U) * forecast_years) {
      DEBUG("filters.forecast",
            "Forecast transaction exceeds " << forecast_years << " years beyond today");
      for (auto& it : same_date)
        pending_posts.erase(it);
      continue;
    }

    // Generate all postings in this same-date group and submit them to
    // the downstream pipeline.  The continuation predicate is evaluated
    // on the LAST generated posting after the entire group has been
    // processed, so that the cumulative effect of all same-date postings
    // determines termination.  This prevents a posting that temporarily
    // causes the running total to fail the predicate from permanently
    // removing periodic transactions that would otherwise continue (#1155).
    //
    // For value-based predicates (e.g. "T>={0}"), the running total is
    // only meaningful after calc_posts has processed the posting, which
    // requires submission to the downstream pipeline first.  For
    // date-based predicates (e.g. "d<[2024/09/01]"), dates are always
    // available so evaluation is straightforward.
    //
    // In posting reports, the downstream filter_posts(forecast_while) in
    // chain.cc provides additional filtering at the output level, preventing
    // any individual postings that fail the predicate from reaching the
    // output.
    std::vector<post_t*> generated;
    for (auto& it : same_date) {
      post_t& post = *(*it).second;
      xact_t& xact = temps.create_xact();
      xact.payee = _("Forecast transaction");
      xact._date = earliest_date;
      post_t& temp = temps.copy_post(post, xact);
      temp.add_flags(ITEM_GENERATED);

      DEBUG("filters.forecast", "Forecast transaction: " << temp.date() << " "
                                                         << temp.account->fullname() << " "
                                                         << temp.amount);

      // Submit the generated posting to the downstream pipeline.
      item_handler<post_t>::operator()(temp);
      generated.push_back(&temp);
    }

    // Evaluate the continuation predicate on the last generated posting
    // whose running total was actually computed by calc_posts (indicated
    // by POST_EXT_VISITED).  For register reports with account filters,
    // only postings matching the filter reach calc_posts, so evaluating
    // on a non-matching posting would see a void display_total and
    // incorrectly terminate value-based predicates (#1155).
    //
    // If no posting was visited (e.g., all filtered out, or accounts
    // report), fall back to the last posting — date-based predicates
    // still work, and value-based predicates without a running total
    // correctly terminate (no meaningful total to evaluate).
    bool group_failed = false;
    if (!generated.empty()) {
      post_t* check_post = generated.back();
      for (auto it = generated.rbegin(); it != generated.rend(); ++it) {
        if ((*it)->has_xdata() && (*it)->xdata().has_flags(POST_EXT_VISITED)) {
          check_post = *it;
          break;
        }
      }
      bind_scope_t bound_scope(context, *check_post);
      if (!pred(bound_scope)) {
        DEBUG("filters.forecast", "  group fails continuation criteria");
        group_failed = true;
        // Remove POST_EXT_VISITED from all generated postings so
        // accounts reports don't count them in their totals.
        for (post_t* p : generated) {
          if (p->has_xdata())
            p->xdata().drop_flags(POST_EXT_VISITED);
        }
      }
    }

    // If the group failed the predicate, remove all postings in the
    // group from future consideration.
    if (group_failed) {
      for (auto& it : same_date)
        pending_posts.erase(it);
      continue;
    }

    // Advance all postings in the group to their next period, removing
    // any that have exhausted their intervals.
    for (auto& it : same_date) {
      ++(*it).first;
      if (!(*it).first.start) {
        pending_posts.erase(it);
      }
    }
  }

  item_handler<post_t>::flush();
}

/*--- Tag injection ---*/

/**
 * Construct the inject_posts handler by parsing the comma-separated tag list.
 *
 * Each tag name is split by ':' to form an account path, and its account is
 * found, or created in @p accounts (the journal's report-account pool).  The
 * tags_list stores each tag name paired with its target account and a set
 * tracking which transactions have already been injected (to avoid
 * duplicates).
 */
inject_posts::inject_posts(post_handler_ptr handler, const string& tag_list, account_t* master,
                           temporaries_t& accounts)
    : item_handler<post_t>(std::move(handler)) {
  std::vector<string> tags;
  boost::split(tags, tag_list, boost::is_any_of(","));

  for (const string& tag : tags) {
    if (tag.empty())
      continue;
    account_t* account = create_temp_account_from_path(tag, accounts, master);
    account->add_flags(ACCOUNT_GENERATED);

    tags_list.push_back(tags_list_pair(tag, tag_mapping_pair(account, tag_injected_set())));
  }

  TRACE_CTOR(inject_posts, "post_handler_ptr, string, account_t *, temporaries_t&");
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
      temp.given_lot_total = std::nullopt;
      // The injected amount is the tag's value; the source posting's cost does not apply to it.
      temp.cost = std::nullopt;
      temp.given_cost = std::nullopt;
      temp.add_flags(ITEM_GENERATED);

      item_handler<post_t>::operator()(temp);
    }
  }

  item_handler<post_t>::operator()(post);
}

/*--- Payee/account rewriting ---*/

namespace {
/// The replacement given by the first rule, in @p first and then in
/// @p second, whose pattern matches @p text; empty if none matches.
string rewrite_of(const string& text, const std::list<std::pair<mask_t, string>>& first,
                  const std::list<std::pair<mask_t, string>>& second) {
  for (const auto* rules : {&first, &second})
    for (const auto& rule : *rules)
      if (rule.first.match(text))
        return rule.second;
  return string();
}
} // namespace

bool rewrite_posts::has_rules(const report_t& report) {
  return !report.payee_rewrite_mappings.empty() || !report.account_rewrite_mappings.empty() ||
         !report.session.journal->payee_rewrite_mappings.empty() ||
         !report.session.journal->account_rewrite_mappings.empty();
}

/// Forward @p post, or its copy when a rule matched its transaction.
void rewrite_posts::operator()(post_t& post) {
  xact_t& xact = *post.xact;
  xact_state_t& state = xacts[&xact];
  if (xact.posts.size() > state.seen)
    rewrite_xact(xact, state);

  auto copy = copies.find(&post);
  item_handler<post_t>::operator()(copy == copies.end() ? post : *copy->second);
}

/// Examine the postings added to @p xact since the last call; once its payee
/// or any posting's account matches a rule, copy every posting of it,
/// rewritten, into one copy.
void rewrite_posts::rewrite_xact(xact_t& xact, xact_state_t& state) {
  journal_t* journal = report.session.journal.get();
  auto new_account_of = [&](const post_t& post) {
    return rewrite_of(post.account->fullname(), report.account_rewrite_mappings,
                      journal->account_rewrite_mappings);
  };

  // The postings added since the last call: all of them the first time,
  // then those an upstream filter (--anon) appended since.
  auto first_new =
      std::prev(xact.posts.end(), static_cast<std::ptrdiff_t>(xact.posts.size() - state.seen));
  if (!state.copy) {
    string new_payee;
    if (state.seen == 0)
      new_payee =
          rewrite_of(xact.payee, report.payee_rewrite_mappings, journal->payee_rewrite_mappings);
    bool matched = !new_payee.empty();
    for (auto i = first_new; !matched && i != xact.posts.end(); ++i)
      matched = !new_account_of(**i).empty();
    if (!matched) {
      state.seen = xact.posts.size();
      return;
    }
    // One copy of the whole transaction, as anonymize_posts makes, so that
    // print shows it once with all its postings and register shows its
    // payee once.
    state.copy = &temps.copy_xact(xact);
    if (!new_payee.empty())
      state.copy->payee = new_payee;
    first_new = xact.posts.begin();
  }

  for (auto i = first_new; i != xact.posts.end(); ++i) {
    post_t& temp = temps.copy_post(**i, *state.copy);
    // Retarget the copy at once, while it is the last posting of its account.
    string new_account = new_account_of(**i);
    if (!new_account.empty())
      retarget_temp_post(temp, new_account, accounts, journal->master);
    // A copy of a journal posting stands in for it in print; a copy of one
    // ledger made up (temporary or generated) keeps that posting's flags.
    if (!(*i)->has_flags(ITEM_TEMP | ITEM_GENERATED))
      temp.add_flags(POST_ANONYMIZED);
    copies.emplace(*i, &temp);
  }
  state.seen = xact.posts.size();
}

} // namespace ledger
