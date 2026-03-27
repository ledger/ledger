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
 * @file   compare.cc
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief Comparison functor implementations for the `--sort` option.
 */

#include <system.hh>

#include "compare.h"
#include "op.h"
#include "scope.h"
#include "post.h"
#include "account.h"
#include "report.h"

namespace ledger {

/*--- Scope wrapper for account sort ---*/

/**
 * A thin scope wrapper that redirects 'amount' lookups to 'total'.
 *
 * When sorting accounts (e.g. balance -S t), the sort expression may
 * evaluate 'amount', which for parent accounts with no direct postings
 * returns zero.  However the balance report displays the family total
 * for these accounts, so sorting by 'amount' produces an ordering that
 * doesn't match the displayed values.
 *
 * By wrapping the account scope and intercepting only the 'amount'
 * lookup, we make the sort see the family total without affecting any
 * other expression evaluation context (format strings, select queries,
 * etc.).
 */
namespace {

class account_amount_as_total_scope_t : public child_scope_t {
public:
  explicit account_amount_as_total_scope_t(scope_t& account) : child_scope_t(account) {}

  string description() override {
    return parent ? parent->description() : _("<account sort scope>");
  }

  expr_t::ptr_op_t lookup(const symbol_t::kind_t kind, const string& name) override {
    if (kind == symbol_t::FUNCTION && name == "amount")
      return child_scope_t::lookup(kind, "total");
    return child_scope_t::lookup(kind, name);
  }
};

} // anonymous namespace

/*--- Exchange conversion for sort values ---*/

/**
 * When -X (exchange) is active, convert any amount or balance sort
 * values to the target commodity so that cross-commodity comparisons
 * sort by converted value rather than raw quantity.
 */
static void convert_sort_values_for_exchange(std::list<sort_value_t>& sort_values,
                                             report_t& report) {
  if (report.HANDLED(exchange_)) {
    string comm = report.HANDLER(exchange_).str();
    for (auto& sv : sort_values) {
      if (sv.value.is_amount() || sv.value.is_balance()) {
        sv.value = sv.value.exchange_commodities(comm, false, datetime_t());
      }
    }
  }
}

/*--- Sort value evaluation ---*/

void push_sort_value(std::list<sort_value_t>& sort_values, expr_t::ptr_op_t node, scope_t& scope) {
  // O_CONS nodes form a linked list of comma-separated sort keys.
  // Walk the chain, recursing into each left child.
  if (node->kind == expr_t::op_t::O_CONS) {
    while (node && node->kind == expr_t::op_t::O_CONS) {
      push_sort_value(sort_values, node->left(), scope);
      node = node->has_right() ? node->right() : nullptr;
    }
  } else {
    // Leaf node: check for negation (descending sort), then evaluate.
    bool inverted = false;

    if (node->kind == expr_t::op_t::O_NEG) {
      inverted = true;
      node = node->left();
    }

    sort_values.push_back(sort_value_t());
    sort_values.back().inverted = inverted;

    value_t result = expr_t(node).calc(scope);
    if (!result.is_null())
      result.in_place_simplify();
    sort_values.back().value = result;
  }
}

/*--- Template specializations for find_sort_values ---*/

template <>
void compare_items<post_t>::find_sort_values(std::list<sort_value_t>& sort_values, scope_t& scope) {
  bind_scope_t bound_scope(report, scope);
  push_sort_value(sort_values, sort_order.get_op(), bound_scope);
}

template <>
void compare_items<account_t>::find_sort_values(std::list<sort_value_t>& sort_values,
                                                scope_t& scope) {
  bind_scope_t bound_scope(report, scope);
  push_sort_value(sort_values, sort_order.get_op(), bound_scope);
}

/*--- Template specializations for operator() with sort-value caching ---*/

/**
 * Post comparison with xdata caching.  The POST_EXT_SORT_CALC flag guards
 * against re-evaluating the sort expression for a posting that has already
 * been seen -- this is important because std::stable_sort may compare the
 * same element many times.
 */
template <>
bool compare_items<post_t>::operator()(post_t* left, post_t* right) {
  assert(left);
  assert(right);

  post_t::xdata_t& lxdata(left->xdata());
  if (!lxdata.has_flags(POST_EXT_SORT_CALC)) {
    if (sort_order.get_context()) {
      bind_scope_t bound_scope(*sort_order.get_context(), *left);
      find_sort_values(lxdata.sort_values, bound_scope);
    } else {
      find_sort_values(lxdata.sort_values, *left);
    }
    convert_sort_values_for_exchange(lxdata.sort_values, report);
    lxdata.add_flags(POST_EXT_SORT_CALC);
  }

  post_t::xdata_t& rxdata(right->xdata());
  if (!rxdata.has_flags(POST_EXT_SORT_CALC)) {
    if (sort_order.get_context()) {
      bind_scope_t bound_scope(*sort_order.get_context(), *right);
      find_sort_values(rxdata.sort_values, bound_scope);
    } else {
      find_sort_values(rxdata.sort_values, *right);
    }
    convert_sort_values_for_exchange(rxdata.sort_values, report);
    rxdata.add_flags(POST_EXT_SORT_CALC);
  }

  return sort_value_is_less_than(lxdata.sort_values, rxdata.sort_values);
}

/**
 * Account comparison with xdata caching.  The ACCOUNT_EXT_SORT_CALC flag
 * prevents redundant evaluation, analogous to POST_EXT_SORT_CALC for postings.
 *
 * For parent accounts with no direct postings, 'amount' is redirected to
 * 'total' during sort evaluation so that -S t sorts by the family total
 * (which is the value the balance report actually displays).
 */
template <>
bool compare_items<account_t>::operator()(account_t* left, account_t* right) {
  assert(left);
  assert(right);

  auto eval_sort_values = [&](account_t& acct) {
    account_t::xdata_t& xdata(acct.xdata());
    if (xdata.has_flags(ACCOUNT_EXT_SORT_CALC))
      return;

    // Parent accounts with no direct postings have amount() == NULL_VALUE.
    // Redirect 'amount' -> 'total' so the sort matches the displayed value.
    bool redirect = acct.amount().is_null() && !acct.accounts.empty();

    if (redirect) {
      account_amount_as_total_scope_t wrapped(acct);
      if (sort_order.get_context()) {
        bind_scope_t bound_scope(*sort_order.get_context(), wrapped);
        find_sort_values(xdata.sort_values, bound_scope);
      } else {
        find_sort_values(xdata.sort_values, wrapped);
      }
    } else {
      if (sort_order.get_context()) {
        bind_scope_t bound_scope(*sort_order.get_context(), acct);
        find_sort_values(xdata.sort_values, bound_scope);
      } else {
        find_sort_values(xdata.sort_values, acct);
      }
    }
    convert_sort_values_for_exchange(xdata.sort_values, report);
    xdata.add_flags(ACCOUNT_EXT_SORT_CALC);
  };

  eval_sort_values(*left);
  eval_sort_values(*right);

  DEBUG("value.sort", "Comparing accounts " << left->fullname() << " <> " << right->fullname());

  return sort_value_is_less_than(left->xdata().sort_values, right->xdata().sort_values);
}

} // namespace ledger
