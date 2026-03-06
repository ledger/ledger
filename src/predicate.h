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
 * @addtogroup expr
 */

/**
 * @file   predicate.h
 * @author John Wiegley
 * @brief  Boolean predicate wrapper around the expression engine.
 *
 * @ingroup expr
 *
 * A predicate is a boolean-valued expression used to filter journal items
 * (postings, transactions, accounts) during report generation.  When the
 * user writes `ledger bal food` or `--limit 'account =~ /food/'`, the
 * resulting expression is wrapped in a predicate_t so that every posting
 * can be tested for inclusion.
 *
 * predicate_t extends expr_t with two behaviors:
 *
 * 1. **Annotation stripping** -- Before converting the result to bool, the
 *    computed value has its commodity annotations (lot price, lot date,
 *    lot tag) stripped according to a keep_details_t policy.  This ensures
 *    that predicates comparing amounts ignore annotation details that the
 *    user did not ask to preserve.
 *
 * 2. **Default-true semantics** -- An empty (unset) predicate evaluates to
 *    `true`, meaning "include everything".  This allows callers to apply a
 *    predicate unconditionally without first checking whether one was
 *    supplied.
 */
#pragma once

#include <utility>

#include "expr.h"
#include "commodity.h"
#include "annotate.h"

namespace ledger {

/**
 * @brief Boolean predicate that wraps an expr_t for filtering journal items.
 *
 * predicate_t inherits all parsing and evaluation machinery from expr_t.
 * The key override is real_calc(), which strips commodity annotations from
 * the result and coerces it to a boolean.  An empty predicate (no
 * expression set) always returns true, so it acts as a pass-through filter.
 *
 * Typical construction paths:
 * - From a pre-built expression tree (ptr_op_t), as produced by query_t.
 * - From a string, as supplied via `--limit` or `--display` command-line
 *   options.
 *
 * @see query_t, which parses command-line arguments into predicate_t
 *      instances for each query category (limit, show, only, bold, for).
 */
class predicate_t : public expr_t {
public:
  keep_details_t what_to_keep; ///< Controls which commodity annotations survive evaluation.

  predicate_t(const keep_details_t& _what_to_keep = keep_details_t())
      : what_to_keep(_what_to_keep) {
    TRACE_CTOR(predicate_t, "");
  }
  predicate_t(const predicate_t& other) : expr_t(other), what_to_keep(other.what_to_keep) {
    TRACE_CTOR(predicate_t, "copy");
  }
  predicate_t& operator=(const predicate_t&) = default;

  /// Construct from a pre-built expression tree (e.g., from query_t parser).
  predicate_t(ptr_op_t _ptr, const keep_details_t& _what_to_keep, scope_t* _context = nullptr)
      : expr_t(std::move(_ptr), _context), what_to_keep(_what_to_keep) {
    TRACE_CTOR(predicate_t, "ptr_op_t, keep_details_t, scope_t *");
  }

  /// Construct by parsing an expression string (e.g., from `--limit`).
  predicate_t(const string& str, const keep_details_t& _what_to_keep,
              const parse_flags_t& flags = PARSE_DEFAULT)
      : expr_t(str, flags), what_to_keep(_what_to_keep) {
    TRACE_CTOR(predicate_t, "string, keep_details_t, parse_flags_t");
  }

  /// Construct by parsing an expression from an input stream.
  predicate_t(std::istream& in, const keep_details_t& _what_to_keep,
              const parse_flags_t& flags = PARSE_DEFAULT)
      : expr_t(in, flags), what_to_keep(_what_to_keep) {
    TRACE_CTOR(predicate_t, "std::istream&, keep_details_t, parse_flags_t");
  }
  ~predicate_t() override { TRACE_DTOR(predicate_t); }

  /**
   * @brief Evaluate the predicate within @p scope, returning a boolean.
   *
   * If the predicate has an expression, it is evaluated, annotations are
   * stripped per what_to_keep, and the result is coerced to bool.  If the
   * predicate is empty (no expression), returns true (include everything).
   */
  value_t real_calc(scope_t& scope) override {
    return (*this ? expr_t::real_calc(scope).strip_annotations(what_to_keep).to_boolean() : true);
  }
};

} // namespace ledger
