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
 * @file   xact.cc
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief  Implementation of the transaction class hierarchy.
 *
 * This file contains the implementation of xact_base_t, xact_t,
 * auto_xact_t, and their supporting functions.  The most critical
 * function is xact_base_t::finalize(), which implements Ledger's
 * double-entry balance checking -- the core accounting invariant that
 * every transaction's debits must equal its credits.
 *
 * Also implemented here is auto_xact_t::extend_xact(), which applies
 * automated transaction rules to generate additional postings based
 * on predicate matching.
 */

#include <system.hh>

#include "xact.h"
#include "post.h"
#include "account.h"
#include "journal.h"
#include "context.h"
#include "format.h"
#include "pool.h"
#include "annotate.h"

namespace ledger {

/*----------------------------------------------------------------------*/
/*  Transaction Lifecycle                                               */
/*                                                                      */
/*  Constructors, destructor, and basic post management.  The           */
/*  destructor is responsible for cleaning up owned postings and        */
/*  removing them from their accounts.                                  */
/*----------------------------------------------------------------------*/

xact_base_t::xact_base_t(const xact_base_t& xact_base)
    : item_t(xact_base), journal(xact_base.journal) {
  TRACE_CTOR(xact_base_t, "copy");
}

xact_base_t::~xact_base_t() {
  TRACE_DTOR(xact_base_t);

  if (!has_flags(ITEM_TEMP)) {
    for (post_t* post : posts) {
      // If the posting is a temporary, it will be destructed when the
      // temporary is.
      assert(!post->has_flags(ITEM_TEMP));

      if (post->account)
        post->account->remove_post(post);
      checked_delete(post);
    }
  }
}

void xact_base_t::add_post(post_t* post) {
  // You can add temporary postings to transactions, but not real postings to
  // temporary transactions.
  if (!post->has_flags(ITEM_TEMP))
    assert(!has_flags(ITEM_TEMP));

  posts.push_back(post);
}

bool xact_base_t::remove_post(post_t* post) {
  auto it = std::find(posts.begin(), posts.end(), post);
  if (it == posts.end())
    return false;
  posts.erase(it);
  post->xact = nullptr;
  return true;
}

bool xact_base_t::has_xdata() {
  for (post_t* post : posts)
    if (post->has_xdata())
      return true;

  return false;
}

void xact_base_t::clear_xdata() {
  for (post_t* post : posts)
    if (!post->has_flags(ITEM_TEMP))
      post->clear_xdata();
}

/**
 * @brief Compute the absolute value of the positive (debit) side.
 *
 * Sums the cost (or amount if no cost) of every posting with a
 * positive amount.  This gives the "size" of the transaction for
 * display in error messages about imbalances, helping users understand
 * the scale of the discrepancy relative to the transaction total.
 */
value_t xact_base_t::magnitude() const {
  value_t halfbal = 0L;
  for (const post_t* post : posts) {
    if (!post->amount.is_null() && post->amount.sign() > 0) {
      if (post->cost)
        halfbal += *post->cost;
      else
        halfbal += post->amount;
    }
  }
  return halfbal;
}

/*----------------------------------------------------------------------*/
/*  Transaction Finalization                                            */
/*                                                                      */
/*  finalize() is the heart of Ledger's double-entry accounting.  It   */
/*  enforces the invariant that every transaction's debits equal its    */
/*  credits across all commodities.  The algorithm handles null-amount  */
/*  inference, multi-commodity conversions, lot matching, cost basis    */
/*  tracking, capital gains computation, and balance assertions.        */
/*----------------------------------------------------------------------*/

namespace {
/// Helper: detect account names ending with digits, `)`, `}`, or `]`,
/// which often indicate typos or auto-complete artifacts.  Used to
/// produce a more helpful error message when two null-amount postings
/// are found in the same transaction.
inline bool account_ends_with_special_char(const string& name) {
  if (name.empty())
    return false;
  string::size_type len(name.length());
  return (std::isdigit(static_cast<unsigned char>(name[len - 1])) || name[len - 1] == ')' ||
          name[len - 1] == '}' || name[len - 1] == ']');
}

/**
 * @brief Functor that creates balancing postings for null-amount inference.
 *
 * When a transaction has exactly one posting with a null amount, its
 * value is inferred as the negation of the transaction's running
 * balance.  If the balance involves multiple commodities, additional
 * postings are generated (one per commodity) beyond the first.
 *
 * The first call sets the null posting's amount; subsequent calls
 * create new generated postings on the same account.
 */
struct add_balancing_post {
  bool first;
  xact_base_t& xact;
  post_t* null_post;

  explicit add_balancing_post(xact_base_t& _xact, post_t* _null_post)
      : first(true), xact(_xact), null_post(_null_post) {
    TRACE_CTOR(add_balancing_post, "xact_base_t&, post_t *");
  }
  add_balancing_post(const add_balancing_post& other)
      : first(other.first), xact(other.xact), null_post(other.null_post) {
    TRACE_CTOR(add_balancing_post, "copy");
  }
  ~add_balancing_post() noexcept { TRACE_DTOR(add_balancing_post); }

  void set_cost_prec_if_needed(amount_t& amt) {
    commodity_t& comm = amt.commodity();
    if (comm.precision() > 0)
      return;
    // The commodity's display precision is 0 (never saw an explicit
    // amount for it).  Scan postings for an inline cost that uses
    // this commodity and recover the per-unit cost precision, then
    // check whether the auto-balanced amount actually has meaningful
    // fractional digits at that precision.
    for (auto& post : xact.posts) {
      if (post->cost && post->amount) {
        const amount_t& cost = *post->cost;
        if (&cost.commodity() == &comm.referent()) {
          auto cost_prec = cost.precision();
          auto amt_prec = post->amount.precision();
          auto price_prec = cost_prec > amt_prec ? cost_prec - amt_prec : cost_prec;
          if (price_prec > 0) {
            amount_t rounded(amt);
            rounded.in_place_roundto(price_prec);
            if (amt != rounded)
              continue; // fractional digits extend beyond price_prec
            // Check that the amount has meaningful fractional digits
            amount_t truncated(amt);
            truncated.in_place_truncate();
            if (amt != truncated)
              amt.set_cost_precision(price_prec);
            return;
          }
        }
      }
    }
  }

  void operator()(const amount_t& amount) {
    if (first) {
      null_post->amount = amount.negated();
      set_cost_prec_if_needed(null_post->amount);
      null_post->add_flags(POST_CALCULATED);
      first = false;
    } else {
      unique_ptr<post_t> p(new post_t(null_post->account, amount.negated()));
      set_cost_prec_if_needed(p->amount);
      p->copy_details(*null_post);
      p->set_flags(null_post->flags() | ITEM_GENERATED | POST_CALCULATED);
      xact.add_post(p.release());
    }
  }
};
} // namespace

/**
 * @brief Finalize a transaction: infer amounts, compute costs, enforce balance.
 *
 * This is the most important function in Ledger.  It implements the
 * double-entry accounting invariant: every transaction's debits must
 * equal its credits.  The function proceeds in several phases:
 *
 * **Phase 1 -- Scan postings and accumulate the balance.**
 * Iterate over all must-balance postings.  For each one that has an
 * amount (or cost), add it to the running balance.  Track whether any
 * posting has a null amount (meaning it should be auto-filled).  Real
 * and balanced-virtual postings ([Account]) are tracked separately.
 *
 * **Phase 2 -- Handle single-posting transactions.**
 * If only one posting exists and a default "bucket" account is
 * configured, create a second posting to that account.
 *
 * **Phase 3 -- Infer conversion prices for two-commodity transactions.**
 * When the balance contains exactly two commodities and no explicit
 * costs were given, compute the exchange rate by dividing one
 * commodity's total by the other's, and set computed costs on all
 * postings of the primary commodity.
 *
 * **Phase 4 -- Apply fixated price annotations.**
 * For postings with {=price} annotations that have not yet had costs
 * computed, derive their costs from the fixated price.
 *
 * **Phase 5 -- Lot matching (FIFO/LIFO).**
 * For unannotated sales (negative amounts), match against the
 * account's lot holdings to split the sale across specific lots.
 *
 * **Phase 6 -- Exchange amounts through the commodity pool.**
 * Process postings with explicit costs (@ or @@), converting them
 * through the commodity pool to establish price history, create
 * annotated commodities, and compute capital gains/losses.
 *
 * **Phase 7 -- Infer null-amount postings.**
 * If exactly one posting has a null amount, set it to the negation
 * of the accumulated balance.  If multiple commodities are in the
 * balance, generate additional postings (one per extra commodity).
 *
 * **Phase 8 -- Verify final balance.**
 * Check that the balance is zero.  If not, check whether per-unit
 * rounding explains the discrepancy.  Throw balance_error if not.
 *
 * **Phase 9 -- Register postings with accounts.**
 * Add each posting to its account's post list and mark it as visited.
 *
 * @return true if the transaction is valid; false if all amounts were
 *         null (indicating the transaction should be silently ignored).
 * @throws balance_error if the transaction cannot be balanced.
 */
bool xact_base_t::finalize() {
  // Phase 1: Scan all postings, accumulate balance, find null-amount posts.
  // Real postings and balanced-virtual postings ([Account]) are tracked
  // in separate balances because they must balance independently.

  value_t balance;
  value_t virtual_balance;
  post_t* null_post = nullptr;
  post_t* virtual_null_post = nullptr;

  for (post_t* post : posts) {
    if (!post->must_balance())
      continue;

    // Within the must_balance() loop, a post with POST_VIRTUAL set is
    // necessarily a balanced-virtual posting ([Account] syntax), because
    // plain virtual postings ((Account)) have POST_MUST_BALANCE clear and
    // would be skipped above.
    bool is_balanced_virtual = post->has_flags(POST_VIRTUAL);
    value_t& cur_balance = is_balanced_virtual ? virtual_balance : balance;
    post_t*& cur_null_post = is_balanced_virtual ? virtual_null_post : null_post;

    amount_t& p(post->cost ? *post->cost : post->amount);
    if (!p.is_null()) {
      DEBUG("xact.finalize", "post must balance = " << p.reduced());
      // If the amount was a cost, it very likely has the
      // "keep_precision" flag set, meaning commodity display precision
      // is ignored when displaying the amount.  We never want this set
      // for the balance, so we must clear the flag in a temporary to
      // avoid it propagating into the balance.
      add_or_set_value(cur_balance, p.keep_precision() ? p.rounded().reduced() : p.reduced());
    } else if (cur_null_post) {
      bool post_account_bad = account_ends_with_special_char(post->account->fullname());
      bool null_post_account_bad =
          account_ends_with_special_char(cur_null_post->account->fullname());

      if (post_account_bad || null_post_account_bad)
        throw_(std::logic_error,
               _f("Posting with null amount's account may be misspelled:\n  \"%1%\"") %
                   (post_account_bad ? post->account->fullname()
                                     : cur_null_post->account->fullname()));
      else
        throw_(std::logic_error, _("Only one posting with null amount allowed per transaction"));
    } else {
      cur_null_post = post;
    }
  }
  VERIFY(balance.valid());
  VERIFY(virtual_balance.valid());

#if DEBUG_ON
  DEBUG("xact.finalize", "initial balance = " << balance);
  DEBUG("xact.finalize", "balance is " << balance.label());
  if (balance.is_balance())
    DEBUG("xact.finalize", "balance commodity count = " << balance.as_balance().amounts.size());
  DEBUG("xact.finalize", "initial virtual_balance = " << virtual_balance);
  DEBUG("xact.finalize", "virtual_balance is " << virtual_balance.label());
  if (virtual_balance.is_balance())
    DEBUG("xact.finalize",
          "virtual_balance commodity count = " << virtual_balance.as_balance().amounts.size());
#endif

  // Phase 2: If there is only one posting, create a balancing posting against
  // the journal's default "bucket" account (set by the `bucket` directive).

  if (journal && journal->bucket && posts.size() == 1 && !balance.is_null()) {
    null_post = new post_t(journal->bucket, ITEM_INFERRED);
    null_post->_state = (*posts.begin())->_state;
    add_post(null_post);
  }

  // Phase 3: Two-commodity conversion price inference.
  // When the balance has exactly two commodities and no null posting,
  // compute the implied exchange rate.  For example, if the balance
  // is {$100, -80 EUR}, the per-unit cost of EUR in $ is computed as
  // $100/80 = $1.25 per EUR.  This price is assigned as a computed
  // cost on all postings of the primary commodity.
  if (!null_post && balance.is_balance() && balance.as_balance().amounts.size() == 2) {

    DEBUG("xact.finalize", "there were exactly two commodities, and no null post");

    bool saw_cost = false;
    post_t* top_post = nullptr;

    for (post_t* post : posts) {
      if (!post->amount.is_null() && post->must_balance()) {
        if (post->amount.has_annotation())
          top_post = post; // NOLINT(bugprone-branch-clone)
        else if (!top_post)
          top_post = post;
      }

      if (post->cost && !post->has_flags(POST_COST_CALCULATED)) {
        saw_cost = true;
        break;
      }
    }

    if (!saw_cost && top_post &&
        !(top_post->amount.has_annotation() && top_post->amount.annotation().price &&
          top_post->amount.annotation().has_flags(ANNOTATION_PRICE_FIXATED))) {
      const balance_t& bal(balance.as_balance());

      DEBUG("xact.finalize", "there were no costs, and a valid top_post");

      balance_t::amounts_map::const_iterator a = bal.amounts.begin();

      const amount_t* x = &(*a++).second;
      const amount_t* y = &(*a++).second;

      if (*x && *y) {
        if (x->commodity() != top_post->amount.commodity())
          std::swap(x, y);

        // Don't auto-convert when the two balance entries share the same base
        // commodity and only one side carries a lot-price annotation whose
        // cost commodity differs from the base.  Example: "Avios {1.00 bmi}"
        // vs "Avios".  The {price} annotation records cost basis for capital
        // gains tracking but does not define a currency conversion; treating
        // the two as separate currencies would silently absorb an imbalance
        // in the actual Avios quantity.
        //
        // We still allow auto-conversion when:
        //   - The commodities have different base symbols (real FX conversion).
        //   - The annotated side has only a tag or date annotation (no price).
        //   - Both sides carry lot-price annotations (e.g., a stock split
        //     between "AAPL {70 USD}" and "AAPL {70.01 USD}").
        const bool x_has_lot_price = x->has_annotation() && x->annotation().price;
        const bool y_has_lot_price = y->has_annotation() && y->annotation().price;
        const bool same_base = (&x->commodity().referent() == &y->commodity().referent());
        if (!(same_base && x_has_lot_price && !y_has_lot_price)) {
          DEBUG("xact.finalize", "primary   amount = " << *x);
          DEBUG("xact.finalize", "secondary amount = " << *y);

          commodity_t& comm(x->commodity());
          amount_t per_unit_cost = (*y / *x).abs().unrounded();

          DEBUG("xact.finalize", "per_unit_cost = " << per_unit_cost);

          for (post_t* post : posts) {
            const amount_t& amt(post->amount.reduced());

            if (post->must_balance() && amt.commodity() == comm) {
              balance -= amt;
              post->cost = per_unit_cost * amt;
              post->add_flags(POST_COST_CALCULATED);
              balance += *post->cost;

              DEBUG("xact.finalize", "set post->cost to = " << *post->cost);
            }
          }
        }
      }
    }
  }

  // Phase 4: Fixated price annotation cost derivation.
  // If the balance still has 2+ commodities and no null post, check for
  // fixated price annotations ({=price}).  These lock the per-unit cost
  // regardless of market prices.  For each posting with a fixated price
  // and no explicit cost, compute the cost from the annotation and
  // recalculate the balance.  This handles the case where multiple
  // postings have different fixated prices for the same base commodity
  // (e.g., EUR {=$1.32} and EUR {=$1.33}).
  if (!null_post && balance.is_balance() && balance.as_balance().amounts.size() >= 2) {
    bool recompute = false;
    for (post_t* post : posts) {
      if (!post->cost && !post->amount.is_null() && post->must_balance() &&
          post->amount.has_annotation() && post->amount.annotation().price &&
          post->amount.annotation().has_flags(ANNOTATION_PRICE_FIXATED)) {
        const annotation_t& ann(post->amount.annotation());
        post->cost = *ann.price;
        post->cost->in_place_unround();
        if (ann.has_flags(ANNOTATION_PRICE_NOT_PER_UNIT)) {
          if (post->amount.sign() < 0)
            post->cost->in_place_negate();
        } else {
          commodity_t& cost_commodity(post->cost->commodity());
          *post->cost *= post->amount;
          post->cost->set_commodity(cost_commodity);
        }
        post->add_flags(POST_COST_CALCULATED);
        post->add_flags(POST_COST_FIXATED);
        recompute = true;
      }
    }

    if (recompute) {
      balance = NULL_VALUE;
      for (post_t* post : posts) {
        if (!post->must_balance())
          continue;
        if (post->has_flags(POST_VIRTUAL)) // skip balanced-virtual postings
          continue;
        amount_t& p(post->cost ? *post->cost : post->amount);
        if (!p.is_null())
          add_or_set_value(balance, p.keep_precision() ? p.rounded().reduced() : p.reduced());
      }
    }
  }

  std::vector<post_t*> copy(posts.begin(), posts.end());

  if (has_date()) {
    // Phase 5: Lot matching (FIFO/LIFO) for commodity sales.
    // When a posting sells a commodity (negative unannotated amount) and
    // the journal has a lot matching policy, find the account's lot
    // holdings and split the sale across specific lots.  Each lot gets
    // its own annotated posting so cost basis is tracked correctly.
    if (journal && journal->lot_matching_policy != lot_policy_t::none) {
      for (std::size_t idx = 0; idx < copy.size(); ++idx) {
        post_t* post = copy[idx];

        // Only match unannotated negative amounts for commodities with lots
        if (post->amount.is_null() || post->amount.sign() >= 0 || post->amount.has_annotation() ||
            !post->amount.has_commodity() ||
            !post->amount.commodity().has_flags(COMMODITY_SAW_ANNOTATED))
          continue;

        const string& base_symbol = post->amount.commodity().base_symbol();

        // Collect lot holdings from account's prior postings
        // account->posts contains all posts from prior transactions (not current)
        std::map<commodity_t*, amount_t> lot_holdings;
        for (const post_t* prior : post->account->posts) {
          if (prior->amount.has_commodity() && prior->amount.has_annotation() &&
              prior->amount.commodity().base_symbol() == base_symbol) {
            auto [it, inserted] =
                lot_holdings.try_emplace(&prior->amount.commodity(), prior->amount);
            if (!inserted)
              it->second += prior->amount;
          }
        }

        // Also account for earlier postings in this same transaction that were
        // already matched (they modify the effective lot balance)
        for (std::size_t j = 0; j < idx; ++j) {
          post_t* earlier = copy[j];
          if (earlier->amount.has_commodity() && earlier->amount.has_annotation() &&
              earlier->amount.commodity().base_symbol() == base_symbol) {
            auto [it, inserted] =
                lot_holdings.try_emplace(&earlier->amount.commodity(), earlier->amount);
            if (!inserted)
              it->second += earlier->amount;
          }
        }

        // Build sorted vector of lots with positive holdings
        // For lots without explicit dates, use the earliest transaction date
        // from the account's postings for that lot as a fallback.
        struct lot_info {
          commodity_t* commodity;
          amount_t quantity; // stripped (unannotated) for comparison
          date_t date;
        };
        std::vector<lot_info> lots;
        for (auto& [comm, qty] : lot_holdings) {
          if (qty.sign() > 0 && comm->has_annotation()) {
            const annotation_t& ann = as_annotated_commodity(*comm).details;
            if (ann.date) {
              lots.push_back({comm, qty.strip_annotations(keep_details_t()), *ann.date});
            } else {
              // Fallback: find the earliest transaction date for this lot
              std::optional<date_t> earliest;
              for (const post_t* prior : post->account->posts) {
                if (&prior->amount.commodity() == comm &&
                    (prior->has_date() || (prior->xact && prior->xact->has_date()))) {
                  date_t d = prior->primary_date();
                  if (!earliest || d < *earliest)
                    earliest = d;
                }
              }
              if (earliest)
                lots.push_back({comm, qty.strip_annotations(keep_details_t()), *earliest});
            }
          }
        }

        if (lots.empty())
          continue;

        // Sort by date according to policy
        if (journal->lot_matching_policy == lot_policy_t::fifo) {
          std::sort(lots.begin(), lots.end(),
                    [](const lot_info& a, const lot_info& b) { return a.date < b.date; });
        } else {
          std::sort(lots.begin(), lots.end(),
                    [](const lot_info& a, const lot_info& b) { return a.date > b.date; });
        }

        // Consume lots in order
        const amount_t total_sale = post->amount.abs(); // positive total to sell
        const std::optional<amount_t> original_cost = post->cost;
        amount_t remaining = total_sale;
        amount_t first_consumed;
        bool first = true;

        for (const lot_info& lot : lots) {
          if (remaining.is_zero())
            break;

          amount_t consumed = (remaining <= lot.quantity) ? remaining : lot.quantity;

          // Create the annotated sale amount (NEGATIVE - it's a sale)
          amount_t sale_amt(consumed.negated());
          sale_amt.set_commodity(*lot.commodity);

          if (first) {
            // Modify the original posting
            post->amount = sale_amt;
            first_consumed = consumed;
            first = false;
          } else {
            // Create a new posting for this lot
            post_t* split = new post_t(post->account, sale_amt);
            split->set_state(post->state());
            split->add_flags(ITEM_GENERATED | POST_CALCULATED);
            if (original_cost) {
              // Split cost proportionally: this lot's share of total
              split->cost = *original_cost * (consumed / total_sale);
            }
            add_post(split);
            copy.push_back(split);
          }

          remaining -= consumed;
        }

        // Adjust the first posting's cost to its proportional share
        if (!first && original_cost && !first_consumed.is_null() && first_consumed != total_sale) {
          post->cost = *original_cost * (first_consumed / total_sale);
        }

        // If there's unmatched remainder (oversell), create an unannotated
        // posting for it so accounting remains correct
        if (!remaining.is_zero() && !first) {
          amount_t remainder_amt(remaining.negated());
          remainder_amt.set_commodity(post->amount.commodity().referent());
          post_t* remainder = new post_t(post->account, remainder_amt);
          remainder->set_state(post->state());
          remainder->add_flags(ITEM_GENERATED | POST_CALCULATED);
          if (original_cost) {
            remainder->cost = *original_cost * (remaining / total_sale);
          }
          add_post(remainder);
          copy.push_back(remainder);
        }
      }
    }

    // Phase 6: Exchange amounts through the commodity pool.
    // For every posting with an explicit or computed cost, pass it
    // through commodity_pool_t::exchange().  This creates annotated
    // commodities (with lot prices and dates), establishes price
    // history entries, and computes capital gains/losses by comparing
    // the original cost basis against the selling price.
    for (post_t* post : copy) {
      if (!post->cost)
        continue;

      if (post->amount.commodity() == post->cost->commodity())
        throw_(balance_error,
               _("A posting's cost must be of a different commodity than its amount"));

      std::optional<date_t> lot_date;
      if (post->has_flags(POST_AMOUNT_USER_DATE) && post->amount.has_annotation() &&
          post->amount.annotation().date)
        lot_date = *post->amount.annotation().date;

      cost_breakdown_t breakdown = commodity_pool_t::current_pool->exchange(
          post->amount, *post->cost, false, !post->has_flags(POST_COST_VIRTUAL),
          datetime_t(post->date(), time_duration(0, 0, 0, 0)), std::nullopt, lot_date);

      if (post->amount.has_annotation() && post->amount.annotation().price) {
        if (breakdown.basis_cost.commodity() == breakdown.final_cost.commodity()) {
          DEBUG("xact.finalize", "breakdown.basis_cost = " << breakdown.basis_cost);
          DEBUG("xact.finalize", "breakdown.final_cost = " << breakdown.final_cost);
          if (amount_t gain_loss = breakdown.basis_cost - breakdown.final_cost) {
            DEBUG("xact.finalize", "gain_loss = " << gain_loss);
            // The basis cost is reconstructed from the per-unit lot price
            // times the quantity.  When the per-unit price was rounded
            // during lot creation (e.g. $250 / 101 → $2.475248), the
            // product can differ from the original total cost by a tiny
            // rounding artifact.  Round the gain/loss to the commodity's
            // display precision to eliminate this artifact (#2975).
            if (gain_loss.has_commodity())
              gain_loss.in_place_roundto(static_cast<int>(gain_loss.commodity().precision()));
            gain_loss.in_place_round();
            DEBUG("xact.finalize", "gain_loss rounds to = " << gain_loss);
            if (post->must_balance())
              add_or_set_value(balance, gain_loss.reduced());
#if 0
            account_t * account;
            if (gain_loss.sign() > 0)
              account = journal->find_account(_("Equity:Capital Gains"));
            else
              account = journal->find_account(_("Equity:Capital Losses"));

            post_t * p = new post_t(account, gain_loss, ITEM_GENERATED);
            p->set_state(post->state());
            if (post->has_flags(POST_VIRTUAL)) {
              DEBUG("xact.finalize", "gain_loss came from a virtual post");
              p->add_flags(post->flags() & (POST_VIRTUAL | POST_MUST_BALANCE));
            }
            add_post(p);
#else
            *post->cost += gain_loss;
#endif
            DEBUG("xact.finalize", "added gain_loss, balance = " << balance);
          } else {
            DEBUG("xact.finalize", "gain_loss would have displayed as zero");
          }
        } else if (post->cost->has_annotation()) {
          DEBUG("xact.finalize", "checking if cost has price annotation");

          // Handle commodity swap over a common base currency
          // Check if price annotation is an amount that also has a cost
          const annotation_t& cost_annot = post->cost->annotation();
          if (cost_annot.price) {
            DEBUG("xact.finalize", "yes, checking if price commodities match");

            // Get the common base currency costs for both commodities
            amount_t from_cost = breakdown.basis_cost;

            // Both costs must be in the same commodity for comparison
            if (from_cost.commodity() == (*cost_annot.price).commodity()) {
              amount_t to_cost = *cost_annot.price * *post->cost;

              DEBUG("xact.finalize", "Commodity swap from_cost = " << from_cost);
              DEBUG("xact.finalize", "Commodity swap to_cost = " << to_cost);

              // Calculate gain/loss in the base commodity
              if (amount_t gain_loss = from_cost - to_cost) {
                DEBUG("xact.finalize", "Commodity swap gain_loss = " << gain_loss);
                if (gain_loss.has_commodity())
                  gain_loss.in_place_roundto(static_cast<int>(gain_loss.commodity().precision()));
                gain_loss.in_place_round();
                DEBUG("xact.finalize", "Commodity swap gain_loss rounds to = " << gain_loss);

                if (post->must_balance())
                  add_or_set_value(balance, gain_loss.reduced());

                // Modify the post->cost to reflect the adjusted value
                *post->cost = to_cost + gain_loss;

                DEBUG("xact.finalize", "added commodity swap gain_loss, balance = " << balance);
              } else {
                DEBUG("xact.finalize", "Commodity swap gain_loss would have displayed as zero");
              }
            }
          }
        }
      } else {
        post->amount =
            breakdown.amount.has_annotation()
                ? amount_t(breakdown.amount, annotation_t(breakdown.amount.annotation().price,
                                                          breakdown.amount.annotation().date,
                                                          post->amount.has_annotation()
                                                              ? post->amount.annotation().tag
                                                              : breakdown.amount.annotation().tag,
                                                          breakdown.amount.annotation().value_expr))
                : breakdown.amount;
        post->drop_flags(POST_AMOUNT_USER_ANNOTATED);
        DEBUG("xact.finalize", "added breakdown, balance = " << balance);
      }

      if (post->has_flags(POST_COST_FIXATED) && post->amount.has_annotation() &&
          post->amount.annotation().price) {
        DEBUG("xact.finalize", "fixating annotation price");
        post->amount.annotation().add_flags(ANNOTATION_PRICE_FIXATED);
      }
    }
  }

  // Phase 7: Infer null-amount postings.
  // If exactly one real posting has no amount, its value becomes the
  // negation of the accumulated balance.  If the balance involves
  // multiple commodities, additional generated postings are created
  // (one per extra commodity) on the same account.
  if (null_post != nullptr) {

    DEBUG("xact.finalize", "there was a null posting");
    add_balancing_post post_adder(*this, null_post);

    if (balance.is_balance())
      balance.as_balance_lval().map_sorted_amounts(post_adder);
    else if (balance.is_amount())
      post_adder(balance.as_amount_lval());
    else if (balance.is_long())
      post_adder(balance.to_amount());
    else if (!balance.is_null() && !balance.is_realzero())
      throw_(balance_error, _("Transaction does not balance"));

    balance = NULL_VALUE;
  }

  // Same null-amount inference, but for balanced-virtual postings
  // ([Account] syntax).  These balance independently from real postings.
  if (virtual_null_post != nullptr) {

    DEBUG("xact.finalize", "there was a null balanced-virtual posting");
    add_balancing_post post_adder(*this, virtual_null_post);

    if (virtual_balance.is_balance())
      virtual_balance.as_balance_lval().map_sorted_amounts(post_adder);
    else if (virtual_balance.is_amount())
      post_adder(virtual_balance.as_amount_lval());
    else if (virtual_balance.is_long())
      post_adder(virtual_balance.to_amount());
    else if (!virtual_balance.is_null() && !virtual_balance.is_realzero())
      throw_(balance_error, _("Transaction does not balance"));

    virtual_balance = NULL_VALUE;
  }

  DEBUG("xact.finalize", "resolved balance = " << balance);
  DEBUG("xact.finalize", "resolved virtual_balance = " << virtual_balance);

  // Phase 8: Final balance verification.
  // The balance should be zero (or null).  If it is not, check whether
  // the discrepancy can be explained by per-unit cost rounding before
  // reporting an error.
  if (!balance.is_null() && !balance.is_zero()) {
    // Check whether the imbalance is fully explained by independently rounding
    // each per-unit (@) annotated cost to the price's decimal precision (issue
    // #1125).  When a posting has 0.50 bread @ $3.99, the exact cost $1.995 is
    // kept at full precision during accumulation.  If the user supplies the
    // rounded total $9.49 instead of the exact $9.480, we accept the
    // transaction by verifying: balance + Σ(roundedCost - exactCost) == 0.
    // This only fires when there is no null posting (null posting filling
    // already cleared balance to NULL_VALUE above).
    {
      value_t rounding_adj;
      for (post_t* post : posts) {
        if (!post->must_balance() || post->has_flags(POST_VIRTUAL))
          continue;
        if (!post->cost || !post->cost->keep_precision())
          continue;
        // Only @ (per-unit) costs, not @@ (total) or internally-calculated
        if (post->has_flags(POST_COST_IN_FULL | POST_COST_CALCULATED))
          continue;
        // price_prec = cost_prec - amount_prec (cost = price * amount,
        // so cost.prec = price.prec + amount.prec after multiply())
        amount_t exact(post->cost->rounded());
        const int price_prec = exact.precision() - post->amount.precision();
        if (price_prec > 0 && exact.precision() > price_prec)
          add_or_set_value(rounding_adj, (exact.roundto(price_prec) - exact).reduced());
      }
      if (!rounding_adj.is_null()) {
        value_t test = balance;
        test += rounding_adj;
        if (test.is_null() || test.is_zero())
          goto balanced;
      }
    }

    // Under --permissive, if the imbalance was caused by a balance
    // assignment that computed the posting amount (= $target with no
    // explicit amount), revert to auto-balancing by adjusting the
    // assigned posting so the transaction balances.  This lets users
    // write `Account = $target` and have --permissive accept the
    // transaction even when the assigned amount conflicts with the
    // other postings.  Balance assignments that DO balance (issue
    // #2944) never reach this point because Phase 7 already cleared
    // the balance.
    if (journal && journal->checking_style == journal_t::CHECK_PERMISSIVE) {
      for (post_t* post : posts) {
        if (post->has_flags(POST_CALCULATED) && post->assigned_amount &&
            post->must_balance() && !post->has_flags(POST_VIRTUAL)) {
          if (balance.is_amount())
            post->amount -= balance.as_amount();
          else if (balance.is_long())
            post->amount -= balance.to_amount();
          else
            break; // multi-commodity imbalance; let it error normally
          balance = NULL_VALUE;
          goto balanced;
        }
      }
    }

    add_error_context(item_context(*this, _("While balancing transaction")));
    add_error_context(_("Unbalanced remainder is:"));
    add_error_context(value_context(balance));
    add_error_context(_("Amount to balance against:"));
    add_error_context(value_context(magnitude()));
    throw_(balance_error, _("Transaction does not balance"));
  balanced:;
  }

  if (!virtual_balance.is_null() && !virtual_balance.is_zero()) {
    add_error_context(item_context(*this, _("While balancing transaction")));
    add_error_context(_("Unbalanced remainder is:"));
    add_error_context(value_context(virtual_balance));
    add_error_context(_("Amount to balance against:"));
    add_error_context(value_context(magnitude()));
    throw_(balance_error, _("Transaction does not balance"));
  }

  // Phase 9: Register postings with their accounts.
  // Each posting is added to its account's post list so that account
  // balances are maintained incrementally.  Deferred postings (for
  // future-dated entries) are handled separately.

  if (dynamic_cast<xact_t*>(this)) {
    bool all_null = true;
    bool some_null = false;

    for (post_t* post : posts) {
      assert(post->account);

      if (!post->amount.is_null()) {
        all_null = false;
        post->amount.in_place_reduce();
      } else {
        some_null = true;
      }

      if (post->has_flags(POST_DEFERRED)) {
        if (!post->amount.is_null())
          post->account->add_deferred_post(id(), post);
      } else {
        post->account->add_post(post);
      }

      post->xdata().add_flags(POST_EXT_VISITED);
      post->account->xdata().add_flags(ACCOUNT_EXT_VISITED);
    }

    if (all_null)
      return false; // ignore this xact completely
    else if (some_null)
      throw_(balance_error, _("There cannot be null amounts after balancing a transaction"));
  }

  VERIFY(valid());

  return true;
}

/*----------------------------------------------------------------------*/
/*  Transaction Verification                                            */
/*                                                                      */
/*  verify() is a lightweight balance check used after automated        */
/*  transactions add postings.  Unlike finalize(), it does not infer   */
/*  null amounts, compute costs, or register with accounts.             */
/*----------------------------------------------------------------------*/

/**
 * @brief Re-verify that a finalized transaction still balances.
 *
 * Called by auto_xact_t::extend_xact() after adding generated postings
 * to ensure the transaction still satisfies the double-entry invariant.
 * Unlike finalize(), this method does not infer null amounts, compute
 * costs, or perform lot matching -- it simply sums all must-balance
 * postings and checks the result is zero.
 */
bool xact_base_t::verify() {
  // Scan through and compute the total balance for the xact.

  value_t balance;

  for (post_t* post : posts) {
    if (!post->must_balance())
      continue;

    amount_t& p(post->cost ? *post->cost : post->amount);
    assert(!p.is_null());

    // If the amount was a cost, it very likely has the "keep_precision" flag
    // set, meaning commodity display precision is ignored when displaying the
    // amount.  We never want this set for the balance, so we must clear the
    // flag in a temporary to avoid it propagating into the balance.
    add_or_set_value(balance, p.keep_precision() ? p.rounded().reduced() : p.reduced());
  }
  VERIFY(balance.valid());

  // Now that the post list has its final form, calculate the balance once
  // more in terms of total cost, accounting for any possible gain/loss
  // amounts.

  for (post_t* post : posts) {
    if (!post->cost)
      continue;

    if (post->amount.commodity() == post->cost->commodity())
      throw_(amount_error, _("A posting's cost must be of a different commodity than its amount"));
  }

  if (!balance.is_null() && !balance.is_zero()) {
    add_error_context(item_context(*this, _("While balancing transaction")));
    add_error_context(_("Unbalanced remainder is:"));
    add_error_context(value_context(balance));
    add_error_context(_("Amount to balance against:"));
    add_error_context(value_context(magnitude()));
    throw_(balance_error, _("Transaction does not balance"));
  }

  VERIFY(valid());

  return true;
}

/*----------------------------------------------------------------------*/
/*  xact_t Implementation                                               */
/*----------------------------------------------------------------------*/

xact_t::xact_t(const xact_t& e) : xact_base_t(e), code(e.code), payee(e.payee) {
  TRACE_CTOR(xact_t, "copy");
}

void xact_t::add_post(post_t* post) {
  post->xact = this;
  post->parent = this;
  xact_base_t::add_post(post);
}

/*----------------------------------------------------------------------*/
/*  Expression Bindings (xact_t)                                        */
/*                                                                      */
/*  These getters expose transaction-specific properties to the         */
/*  expression engine: payee, code, magnitude, and the any()/all()     */
/*  quantifier functions over postings.                                 */
/*----------------------------------------------------------------------*/

namespace {
value_t get_magnitude(xact_t& xact) {
  return xact.magnitude();
}

value_t get_code(xact_t& xact) {
  if (xact.code)
    return string_value(*xact.code);
  else
    return NULL_VALUE;
}

value_t get_payee(xact_t& xact) {
  return string_value(xact.payee);
}

template <value_t (*Func)(xact_t&)>
value_t get_wrapper(call_scope_t& scope) {
  return (*Func)(find_scope<xact_t>(scope));
}

/// Expression function: `any(expr)` -- returns true if any posting in the
/// transaction satisfies the given expression.
value_t fn_any(call_scope_t& args) {
  post_t& post(args.context<post_t>());
  expr_t::ptr_op_t expr(args.get<expr_t::ptr_op_t>(0));

  for (post_t* p : post.xact->posts) {
    bind_scope_t bound_scope(args, *p);
    if (expr->calc(bound_scope, args.locus, args.depth).to_boolean())
      return true;
  }
  return false;
}

/// Expression function: `all(expr)` -- returns true if every posting in
/// the transaction satisfies the given expression.
value_t fn_all(call_scope_t& args) {
  post_t& post(args.context<post_t>());
  expr_t::ptr_op_t expr(args.get<expr_t::ptr_op_t>(0));

  for (post_t* p : post.xact->posts) {
    bind_scope_t bound_scope(args, *p);
    if (!expr->calc(bound_scope, args.locus, args.depth).to_boolean())
      return false;
  }
  return true;
}
} // namespace

/**
 * @brief Resolve expression names for transaction-specific properties.
 *
 * Extends item_t::lookup() with transaction-level bindings:
 *   - `payee` / `p` -- the payee string
 *   - `code` -- the check number or transaction code
 *   - `magnitude` -- absolute value of the positive (debit) side
 *   - `any(expr)` -- existential quantifier over postings
 *   - `all(expr)` -- universal quantifier over postings
 *
 * Unrecognized names fall through to item_t::lookup().
 */
expr_t::ptr_op_t xact_t::lookup(const symbol_t::kind_t kind, const string& name) {
  if (kind != symbol_t::FUNCTION)
    return item_t::lookup(kind, name);

  // NOLINTBEGIN(bugprone-branch-clone)
  switch (name[0]) {
  case 'a':
    if (name == "any")
      return WRAP_FUNCTOR(&fn_any);
    else if (name == "all")
      return WRAP_FUNCTOR(&fn_all);
    break;

  case 'c':
    if (name == "code")
      return WRAP_FUNCTOR(get_wrapper<&get_code>);
    break;

  case 'm':
    if (name == "magnitude")
      return WRAP_FUNCTOR(get_wrapper<&get_magnitude>);
    break;

  case 'p':
    if (name[1] == '\0' || name == "payee")
      return WRAP_FUNCTOR(get_wrapper<&get_payee>);
    break;
  }
  // NOLINTEND(bugprone-branch-clone)

  return item_t::lookup(kind, name);
}

bool xact_t::valid() const {
  if (!_date) {
    DEBUG("ledger.validate", "xact_t: ! _date");
    return false;
  }

  for (post_t* post : posts)
    if (post->xact != this || !post->valid()) {
      DEBUG("ledger.validate", "xact_t: post not valid");
      return false;
    }

  return true;
}

/*----------------------------------------------------------------------*/
/*  Hashing                                                             */
/*----------------------------------------------------------------------*/

extern "C" unsigned char* SHA512(void* data, unsigned int data_len, unsigned char* digest);

namespace {
/// Convert a binary buffer to a lowercase hexadecimal string.
std::string bufferToHex(const unsigned char* buffer, std::size_t size) {
  std::ostringstream oss;
  oss << std::hex << std::setfill('0');
  for (std::size_t i = 0; i < size; ++i)
    oss << std::setw(2) << static_cast<int>(buffer[i]);
  return oss.str();
}
} // namespace

/**
 * @brief Compute a SHA-512 hash of this transaction for deduplication.
 *
 * Builds a canonical string representation from the nonce, dates, code,
 * payee, and all postings (sorted alphabetically for determinism), then
 * hashes it with SHA-512.  The result can be stored as a UUID tag to
 * detect duplicate imports.
 */
string xact_t::hash(const string& nonce, hash_type_t hash_type) const {
  std::ostringstream repr;

  repr << nonce;
  repr << date();
  repr << aux_date();
  if (code)
    repr << ' ' << *code;
  else
    repr << "--";
  repr << payee;

  std::vector<std::string> strings;

  posts_list all_posts(posts.begin(), posts.end());
  for (post_t* post : all_posts) {
    std::ostringstream posting;
    posting << post->account->fullname();
    if (!post->amount.is_null())
      posting << post->amount.to_fullstring();
    if (post->cost)
      posting << post->cost->to_fullstring();
    posting << post->checkin;
    posting << post->checkout;
    strings.push_back(posting.str());
  }

  std::sort(strings.begin(), strings.end());

  for (string& str : strings) {
    repr << str;
  }

  unsigned char data[128];
  string repr_str(repr.str()); // NOLINT(bugprone-unused-local-non-trivial-variable)

  SHA512(const_cast<char*>(repr_str.c_str()), repr_str.length(), data);

  return bufferToHex(data, hash_type == HASH_SHA512 ? 64 : 32 /*SHA512_DIGEST_LENGTH*/);
}

/*----------------------------------------------------------------------*/
/*  Automated Transactions                                              */
/*                                                                      */
/*  auto_xact_t::extend_xact() is the engine behind `= expr` entries. */
/*  For each posting in the target transaction that matches the         */
/*  predicate, it clones the auto_xact's template postings, resolves   */
/*  amount expressions and account name templates, applies deferred    */
/*  tags, and adds the generated postings to the transaction.           */
/*----------------------------------------------------------------------*/

namespace {
/**
 * @brief Fast predicate evaluator for simple account-matching expressions.
 *
 * Most automated transactions use simple predicates like `/Expenses:Food/`
 * which match against account names.  This function walks the predicate
 * AST directly instead of going through the full expression evaluator,
 * providing a significant performance improvement.  It handles VALUE,
 * O_MATCH, O_EQ, O_NOT, O_AND, O_OR, and O_QUERY nodes.
 *
 * If an unhandled operator is encountered, it throws calc_error, causing
 * the caller to fall back to the full evaluator and disable quick
 * matching for this auto_xact going forward.
 */
bool post_pred(const expr_t::ptr_op_t& op, post_t& post) {
  if (!op)
    return false;

  switch (op->kind) {
  case expr_t::op_t::VALUE:
    return op->as_value().to_boolean();

  case expr_t::op_t::O_MATCH:
    if (op->left()->kind == expr_t::op_t::IDENT && op->left()->as_ident() == "account" &&
        op->right()->kind == expr_t::op_t::VALUE && op->right()->as_value().is_mask())
      return op->right()->as_value().as_mask().match(post.reported_account()->fullname());
    else
      break;

  case expr_t::op_t::O_EQ:
    return post_pred(op->left(), post) == post_pred(op->right(), post);

  case expr_t::op_t::O_NOT:
    return !post_pred(op->left(), post);

  case expr_t::op_t::O_AND:
    return post_pred(op->left(), post) && post_pred(op->right(), post);

  case expr_t::op_t::O_OR:
    return post_pred(op->left(), post) || post_pred(op->right(), post);

  case expr_t::op_t::O_QUERY:
    if (post_pred(op->left(), post))
      return post_pred(op->right()->left(), post);
    else
      return post_pred(op->right()->right(), post);

  default:
    break;
  }

  throw_(calc_error, _("Unhandled operator"));
  return false;
}
} // namespace

/// If a string contains `%(` format expressions, expand them against
/// the given scope; otherwise return the string unchanged.
static string apply_format(const string& str, scope_t& scope) {
  if (contains(str, "%(")) {
    format_t str_format(str);
    std::ostringstream buf;
    buf << str_format(scope);
    return buf.str();
  } else {
    return str;
  }
}

/**
 * @brief Apply this automated transaction's rules to a regular transaction.
 *
 * This is the implementation of `= expr` automated transactions.  The
 * algorithm proceeds as follows:
 *
 * 1. **Iterate over the target transaction's postings** (a snapshot taken
 *    before any new postings are added, to avoid infinite loops).
 *
 * 2. **Skip generated postings** (to prevent auto_xacts from triggering
 *    on their own output), except for balancing postings created by
 *    finalize() which have both ITEM_GENERATED and POST_CALCULATED.
 *
 * 3. **Evaluate the predicate** against each posting.  The quick matcher
 *    (post_pred) is tried first for performance; if it cannot handle the
 *    expression, the full evaluator is used and quick matching is disabled
 *    for this auto_xact.  Results are cached by account name.
 *
 * 4. **On a match**:
 *    a. Apply any deferred notes (metadata tags) to the matched posting
 *       if they have no specific target posting.
 *    b. Evaluate check/assertion expressions if present.
 *    c. For each template posting in this auto_xact:
 *       - Compute the amount: if the template's amount has no commodity,
 *         it is treated as a multiplier (e.g., `0.10` means 10% of the
 *         matched posting's amount); otherwise it is used as-is.
 *       - Resolve `$variable` and `%(format)` expressions in the account
 *         name against the matched posting's scope.
 *       - Clone the template posting with the computed amount and account.
 *       - Inherit the parent transaction's clearing state.
 *       - Apply deferred notes targeted at this template posting.
 *       - Register the new posting with the journal and account.
 *
 * 5. **After processing all postings**, if any must-balance posting was
 *    generated, call verify() to ensure the transaction still balances.
 */
void auto_xact_t::extend_xact(xact_base_t& xact, parse_context_t& context) {
  posts_list initial_posts(xact.posts.begin(), xact.posts.end());

  if (!enabled)
    return;

  try {

    bool needs_further_verification = false;

    for (post_t* initial_post : initial_posts) {
      // Skip posts generated by auto transactions, but not balancing posts
      // generated by finalize() (which have both ITEM_GENERATED and POST_CALCULATED).
      // This ensures auto transactions can match all commodities in multi-commodity
      // balancing posts.
      if (initial_post->has_flags(ITEM_GENERATED) && !initial_post->has_flags(POST_CALCULATED))
        continue;

      bind_scope_t bound_scope(*scope_t::default_scope, *initial_post);

      // Temporarily disable use_aux_date during predicate and check
      // expression evaluation so that date() returns the primary date
      // and expressions like "aux_date != date" work correctly.  Without
      // this, --effective makes date() return aux_date, collapsing the
      // two and breaking predicates that distinguish them (fixes #2945).
      bool matches_predicate = false;
      {
        bool saved = item_t::use_aux_date;
        item_t::use_aux_date = false;
        struct restore_guard {
          bool& flag;
          bool val;
          ~restore_guard() { flag = val; }
        } guard{item_t::use_aux_date, saved};

        if (try_quick_match) {
          try {
            bool found_memoized_result = false;
            if (!memoized_results.empty()) {
              std::map<string, bool>::iterator i =
                  memoized_results.find(initial_post->account->fullname());
              if (i != memoized_results.end()) {
                found_memoized_result = true;
                matches_predicate = (*i).second;
              }
            }

            // Since the majority of people who use automated transactions simply
            // match against account names, try using a *much* faster version of
            // the predicate evaluator.
            if (!found_memoized_result) {
              matches_predicate = post_pred(predicate.get_op(), *initial_post);
              memoized_results.insert(
                  std::pair<string, bool>(initial_post->account->fullname(), matches_predicate));
            }
          } catch (...) {
            DEBUG("xact.extend.fail", "The quick matcher failed, going back to regular eval");
            try_quick_match = false;
            memoized_results.clear(); // Clear any incorrect cached results
            matches_predicate = predicate(bound_scope);
          }
        } else {
          matches_predicate = predicate(bound_scope);
        }

        if (matches_predicate) {
          if (deferred_notes) {
            for (deferred_tag_data_t& data : *deferred_notes) {
              if (data.apply_to_post == nullptr)
                initial_post->append_note(apply_format(data.tag_data, bound_scope).c_str(),
                                          bound_scope, data.overwrite_existing);
            }
          }

          if (check_exprs) {
            for (expr_t::check_expr_pair& pair : *check_exprs) {
              if (pair.second == expr_t::EXPR_GENERAL) {
                pair.first.calc(bound_scope);
              } else if (!pair.first.calc(bound_scope).to_boolean()) {
                if (pair.second == expr_t::EXPR_ASSERTION)
                  throw_(parse_error, _f("Transaction assertion failed: %1%") % pair.first);
                else
                  context.warning(_f("Transaction check failed: %1%") % pair.first);
              }
            }
          }
        }
      } // use_aux_date restored here by guard destructor

      if (matches_predicate) {
        for (post_t* post : posts) {
          amount_t post_amount;
          if (post->amount.is_null()) {
            if (!post->amount_expr)
              throw_(amount_error, _("Automated transaction's posting has no amount"));

            value_t result(post->amount_expr->calc(bound_scope));
            if (result.is_long()) {
              post_amount = result.to_amount();
            } else {
              if (!result.is_amount())
                throw_(amount_error, _("Amount expressions must result in a simple amount"));
              post_amount = result.as_amount();
            }
          } else {
            post_amount = post->amount;
          }

          amount_t amt;
          if (!post_amount.commodity())
            amt = initial_post->amount * post_amount;
          else
            amt = post_amount;

#if DEBUG_ON
          IF_DEBUG("xact.extend") {
            DEBUG("xact.extend", "Initial post on line "
                                     << initial_post->pos->beg_line << ": "
                                     << "amount " << initial_post->amount << " (precision "
                                     << initial_post->amount.precision() << ")");

            if (initial_post->amount.keep_precision())
              DEBUG("xact.extend", "  precision is kept");

            DEBUG("xact.extend", "Posting on line " << post->pos->beg_line << ": "
                                                    << "amount " << post_amount << ", amt " << amt
                                                    << " (precision " << post_amount.precision()
                                                    << " != " << amt.precision() << ")");

            if (post_amount.keep_precision())
              DEBUG("xact.extend", "  precision is kept");
            if (amt.keep_precision())
              DEBUG("xact.extend", "  amt precision is kept");
          }
#endif // DEBUG_ON

          account_t* account = post->account;
          string fullname = account->fullname();
          assert(!fullname.empty());

          const regex re("\\$([A-Za-z_]+)");
          smatch matches;
          if (regex_search(fullname, matches, re)) {
            bind_scope_t bound_scope(*context.scope, *initial_post->account);
            const string subexpr = matches[1];
            value_t result = expr_t(subexpr).calc(bound_scope);
            if (result.is_string()) {
              fullname = regex_replace(fullname, re, result.as_string());
              while (account->parent)
                account = account->parent;
              account = account->find_account(fullname);
            }
          } else if (contains(fullname, "%(")) {
            format_t account_name(fullname);
            std::ostringstream buf;
            buf << account_name(bound_scope);
            while (account->parent)
              account = account->parent;
            account = account->find_account(buf.str());
          }

          // Copy over details so that the resulting post is a mirror of
          // the automated xact's one.
          auto new_post = std::make_unique<post_t>(account, amt);
          new_post->copy_details(*post);

          // Propagate the matched posting's date overrides to the
          // generated posting so that auto-generated postings respect
          // effective dates and posting-level date overrides.
          if (!post->_date && initial_post->_date)
            new_post->_date = initial_post->_date;
          if (!post->_date_aux && initial_post->_date_aux)
            new_post->_date_aux = initial_post->_date_aux;

          if (post->cost)
            new_post->cost = post->cost;
          else if (initial_post->cost && amt.has_annotation() && amt.annotation().price) {
            // When the auto-generated amount has a price annotation (e.g., copied
            // from a posting with cost like "100 kWh @@ 72€"), derive the cost
            // from the annotation so verify() can check the balance correctly.
            new_post->cost = *amt.annotation().price;
            new_post->cost->in_place_unround();
            if (amt.annotation().has_flags(ANNOTATION_PRICE_NOT_PER_UNIT)) {
              if (amt.sign() < 0)
                new_post->cost->in_place_negate();
            } else {
              commodity_t& cost_commodity(new_post->cost->commodity());
              *new_post->cost *= amt;
              new_post->cost->set_commodity(cost_commodity);
            }
            new_post->add_flags(POST_COST_CALCULATED);
          }

          // A Cleared or Pending transaction implies all of its automatic
          // postings carry the same state. CPR 2012/10/23
          if (xact.state() == item_t::CLEARED) {
            DEBUG("xact.extend.cleared", "CLEARED");
            new_post->set_state(item_t::CLEARED);
          } else if (xact.state() == item_t::PENDING) {
            DEBUG("xact.extend.cleared", "PENDING");
            new_post->set_state(item_t::PENDING);
          }

          new_post->add_flags(ITEM_GENERATED);
          // If the target account is named "Unknown", try to resolve it using
          // the payee of the originating transaction.  This mirrors the lookup
          // in journal_t::register_account, but here we have access to
          // initial_post->xact where the payee is available.  The new post has
          // not yet been linked to a transaction at this point, so
          // register_account cannot perform the lookup on its own.
          if (account->name == _("Unknown") && initial_post->xact) {
            for (account_mapping_t& value : journal->payees_for_unknown_accounts) {
              if (value.first.match(initial_post->xact->payee)) {
                account = value.second;
                break;
              }
            }
          }
          new_post->account =
              journal->register_account(account->fullname(), new_post.get(), journal->master);

          if (deferred_notes) {
            for (deferred_tag_data_t& data : *deferred_notes) {
              if (!data.apply_to_post || data.apply_to_post == post) {
                new_post->append_note(apply_format(data.tag_data, bound_scope).c_str(), bound_scope,
                                      data.overwrite_existing);
              }
            }
          }

          extend_post(*new_post, *journal);

          post_t* raw_post = new_post.release();
          xact.add_post(raw_post);
          raw_post->account->add_post(raw_post);

          // Add flags so this post updates the account balance
          raw_post->xdata().add_flags(POST_EXT_VISITED);
          raw_post->account->xdata().add_flags(ACCOUNT_EXT_VISITED);

          if (raw_post->must_balance())
            needs_further_verification = true;
        }
      }
    }

    if (needs_further_verification)
      xact.verify();

  } catch (const std::exception&) {
    add_error_context(item_context(*this, _("While applying automated transaction")));
    add_error_context(item_context(xact, _("While extending transaction")));
    throw;
  }
}

/*----------------------------------------------------------------------*/
/*  Utility Functions                                                   */
/*----------------------------------------------------------------------*/

/**
 * @brief Serialize a transaction to a property tree for XML/JSON output.
 *
 * Writes the transaction's clearing state, dates, code, payee, note,
 * and metadata as elements and attributes in the property tree.
 */
void put_xact(property_tree::ptree& st, const xact_t& xact) {
  if (xact.state() == item_t::CLEARED)
    st.put("<xmlattr>.state", "cleared");
  else if (xact.state() == item_t::PENDING)
    st.put("<xmlattr>.state", "pending");

  if (xact.has_flags(ITEM_GENERATED))
    st.put("<xmlattr>.generated", "true");

  if (xact._date)
    put_date(st.put("date", ""), *xact._date);
  if (xact._date_aux)
    put_date(st.put("aux-date", ""), *xact._date_aux);

  if (xact.code)
    st.put("code", *xact.code);

  st.put("payee", xact.payee);

  if (xact.note)
    st.put("note", *xact.note);

  if (xact.metadata)
    put_metadata(st.put("metadata", ""), *xact.metadata);
}

} // namespace ledger
