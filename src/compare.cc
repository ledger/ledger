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

/*--- Account sort key: the figure the row shows ---*/

namespace {

/**
 * Marks an account, for one evaluation of its sort key, as one whose
 * balance row shows its family total, so that 'amount' (get_amount in
 * account.cc) returns that total.  The mark is cleared on every exit,
 * exceptions included, so no other expression ever sees it.
 */
class sort_amount_as_total_t {
  bool& as_total;

public:
  sort_amount_as_total_t(account_t::xdata_t& xdata, const bool active)
      : as_total(xdata.sort_amount_as_total) {
    as_total = active;
  }
  ~sort_amount_as_total_t() { as_total = false; }
};

} // anonymous namespace

/*--- Exchange conversion for sort values ---*/

/**
 * When -X (exchange) is active, convert any amount or balance sort
 * values to the target commodity at @p moment -- the moment the report
 * values that row -- so that the order matches the values displayed.
 */
static void convert_sort_values_for_exchange(std::list<sort_value_t>& sort_values, report_t& report,
                                             const datetime_t& moment) {
  if (report.HANDLED(exchange_)) {
    string comm = report.HANDLER(exchange_).str();
    for (auto& sv : sort_values) {
      if (sv.value.is_amount() || sv.value.is_balance()) {
        sv.value = sv.value.exchange_commodities(comm, false, moment);
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

/*--- Sort value evaluation for one item ---*/

template <typename T>
void compare_items<T>::find_sort_values(std::list<sort_value_t>& sort_values, scope_t& scope) {
  if (scope_t* context = sort_order.get_context()) {
    bind_scope_t item_scope(*context, scope);
    bind_scope_t bound_scope(report, item_scope);
    push_sort_value(sort_values, sort_order.get_op(), bound_scope);
  } else {
    bind_scope_t bound_scope(report, scope);
    push_sort_value(sort_values, sort_order.get_op(), bound_scope);
  }
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

  auto sort_values_of = [&](post_t& post) -> std::list<sort_value_t>& {
    post_t::xdata_t& xdata(post.xdata());
    if (!xdata.has_flags(POST_EXT_SORT_CALC)) {
      find_sort_values(xdata.sort_values, post);
      convert_sort_values_for_exchange(xdata.sort_values, report, datetime_t(post.value_date()));
      xdata.add_flags(POST_EXT_SORT_CALC);
    }
    return xdata.sort_values;
  };

  std::list<sort_value_t>& left_values(sort_values_of(*left));
  return sort_value_is_less_than(left_values, sort_values_of(*right));
}

/**
 * Account comparison with xdata caching.  The ACCOUNT_EXT_SORT_CALC flag
 * prevents redundant evaluation, analogous to POST_EXT_SORT_CALC for postings.
 * While an account's key is evaluated, sort_amount_as_total_t makes 'amount'
 * the figure the balance report shows for it (see the rule beside its use).
 */
template <>
bool compare_items<account_t>::operator()(account_t* left, account_t* right) {
  assert(left);
  assert(right);

  auto eval_sort_values = [&](account_t& acct) {
    account_t::xdata_t& xdata(acct.xdata());
    if (xdata.has_flags(ACCOUNT_EXT_SORT_CALC))
      return;

    // A row shows the family total when the account has no postings of its
    // own, or in the account tree while --total is still plain 'total' (the
    // test accounts_report() makes before installing the --flat rule).
    // Otherwise it shows the account's own amount.
    const bool shows_total =
        acct.amount().is_null() ||
        (!report.HANDLED(flat) && report.HANDLER(total_).expr.base_expr == "total");
    sort_amount_as_total_t as_total(xdata, shows_total);
    find_sort_values(xdata.sort_values, acct);
    convert_sort_values_for_exchange(xdata.sort_values, report, report.terminus);
    xdata.add_flags(ACCOUNT_EXT_SORT_CALC);
  };

  eval_sort_values(*left);
  eval_sort_values(*right);

  DEBUG("value.sort", "Comparing accounts " << left->fullname() << " <> " << right->fullname());

  return sort_value_is_less_than(left->xdata().sort_values, right->xdata().sort_values);
}

} // namespace ledger
