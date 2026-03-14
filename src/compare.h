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
 * @addtogroup data
 */

/**
 * @file   compare.h
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief Comparison functors for sorting postings and accounts.
 *
 * When the user supplies `--sort EXPR` on the command line, the sort
 * expression is parsed into an expression tree (expr_t) and wrapped in a
 * compare_items<T> functor.  This functor is then passed to std::stable_sort
 * to order postings or accounts.
 *
 * Sort expressions can be composite (e.g. `date, payee`): each sub-expression
 * is evaluated against both operands and the results are compared
 * lexicographically via sort_value_is_less_than (defined in value.h).  A
 * leading `-` on any sub-expression reverses its comparison direction.
 *
 * Sort values are cached in extended data (xdata) so that the sort expression
 * is evaluated at most once per item, even though the comparison functor is
 * called O(N log N) times during sorting.
 */
#pragma once

#include "expr.h"

namespace ledger {

class post_t;
class account_t;
class report_t;

/**
 * @brief Recursively evaluate a sort expression tree and push results.
 *
 * Walks the O_CONS chain that represents a comma-separated list of sort
 * keys.  Each leaf is evaluated in the given scope and appended to
 * @p sort_values.  If a leaf is wrapped in O_NEG the resulting
 * sort_value_t is marked as inverted (descending order).
 *
 * @param sort_values  Accumulator for the evaluated sort keys.
 * @param node         Root of the sort expression (or O_CONS chain).
 * @param scope        Evaluation scope (typically a bound posting or account).
 */
void push_sort_value(std::list<sort_value_t>& sort_values, expr_t::ptr_op_t node, scope_t& scope);

/**
 * @brief STL-compatible comparison functor for sorting postings or accounts.
 *
 * Given a sort expression (e.g. `date`, `-amount`, `account, total`), this
 * functor evaluates both operands, caches the results in each item's
 * extended data (xdata), and delegates to sort_value_is_less_than for the
 * actual comparison.
 *
 * Template specializations exist for post_t and account_t; each binds the
 * sort expression into the correct scope before evaluation.
 *
 * @tparam T  The item type being sorted (post_t or account_t).
 */
template <typename T>
class compare_items {
  expr_t sort_order; ///< Parsed sort expression from `--sort`.
  report_t& report;  ///< Report context for expression evaluation.

  compare_items(); ///< Deleted -- a sort expression and report are required.

public:
  compare_items(const expr_t& _sort_order, report_t& _report)
      : sort_order(_sort_order), report(_report) {
    TRACE_CTOR(compare_items, "const value_expr&, report_t&");
  }
  compare_items(const compare_items& other) : sort_order(other.sort_order), report(other.report) {
    TRACE_CTOR(compare_items, "copy");
  }
  compare_items& operator=(const compare_items&) = delete;
  ~compare_items() noexcept { TRACE_DTOR(compare_items); }

  /// Evaluate the sort expression in @p scope and append results to @p sort_values.
  void find_sort_values(std::list<sort_value_t>& sort_values, scope_t& scope);

  /// Compare two items; returns true if @p left should appear before @p right.
  bool operator()(T* left, T* right);
};

/// Evaluate a single sort-key expression node and return the result.
sort_value_t calc_sort_value(const expr_t::ptr_op_t op);

template <typename T>
bool compare_items<T>::operator()(T* left, T* right) {
  assert(left);
  assert(right);
  return sort_value_is_less_than(find_sort_values(left), find_sort_values(right));
}

template <>
bool compare_items<post_t>::operator()(post_t* left, post_t* right);
template <>
bool compare_items<account_t>::operator()(account_t* left, account_t* right);

} // namespace ledger
