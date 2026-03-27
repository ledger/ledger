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
    sort_values.back().value = expr_t(node).calc(scope).simplified();

    if (sort_values.back().value.is_null())
      throw_(calc_error, _("Could not determine sorting value based an expression"));
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
 */
template <>
bool compare_items<account_t>::operator()(account_t* left, account_t* right) {
  assert(left);
  assert(right);

  account_t::xdata_t& lxdata(left->xdata());
  if (!lxdata.has_flags(ACCOUNT_EXT_SORT_CALC)) {
    if (sort_order.get_context()) {
      bind_scope_t bound_scope(*sort_order.get_context(), *left);
      find_sort_values(lxdata.sort_values, bound_scope);
    } else {
      find_sort_values(lxdata.sort_values, *left);
    }
    convert_sort_values_for_exchange(lxdata.sort_values, report);
    lxdata.add_flags(ACCOUNT_EXT_SORT_CALC);
  }

  account_t::xdata_t& rxdata(right->xdata());
  if (!rxdata.has_flags(ACCOUNT_EXT_SORT_CALC)) {
    if (sort_order.get_context()) {
      bind_scope_t bound_scope(*sort_order.get_context(), *right);
      find_sort_values(rxdata.sort_values, bound_scope);
    } else {
      find_sort_values(rxdata.sort_values, *right);
    }
    convert_sort_values_for_exchange(rxdata.sort_values, report);
    rxdata.add_flags(ACCOUNT_EXT_SORT_CALC);
  }

  DEBUG("value.sort", "Comparing accounts " << left->fullname() << " <> " << right->fullname());

  return sort_value_is_less_than(lxdata.sort_values, rxdata.sort_values);
}

} // namespace ledger
