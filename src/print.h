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
 * @addtogroup report
 */

/**
 * @file   print.h
 * @author John Wiegley
 * @brief  Handler for the `print` command -- reconstructing transactions as
 *         human-readable text.
 *
 * @ingroup report
 *
 * The `print` command outputs transactions in a canonical text format that
 * is itself valid Ledger input.  This allows round-tripping: the output of
 * `ledger print` can be fed back to Ledger as a journal file.
 *
 * Unlike the `register` and `balance` commands which use format_t format
 * strings, the `print` command reconstructs each transaction structurally:
 * date, state, code, payee, metadata, and posting lines are assembled
 * directly, with careful attention to alignment, elision of redundant
 * amounts, and preservation of cost/price annotations.
 *
 * The handler collects unique transactions (via their postings) and
 * renders them in order during flush().  A `print_raw` mode is also
 * available, which outputs the original source text verbatim.
 */
#pragma once

#include "chain.h"
#include "predicate.h"
#include "format.h"

namespace ledger {

class xact_t;
class post_t;
class report_t;

/**
 * @brief Terminal handler that reconstructs transactions as canonical text.
 *
 * As postings flow through operator(), the handler deduplicates by
 * transaction (each xact_t is printed only once, even if multiple postings
 * match the query).  During flush(), transactions are printed in the order
 * their first matching posting was seen.
 *
 * Two modes are supported:
 *   - **Structured print** (default): Reconstructs the transaction from its
 *     parsed data structures, producing clean canonical output.
 *   - **Raw print** (print_raw = true): Outputs the original source text
 *     verbatim via print_item().
 */
class print_xacts : public item_handler<post_t> {
protected:
  using xacts_list = std::list<xact_t*>;
  using xacts_present_map = std::map<xact_t*, bool>;

  report_t& report;                ///< The report context providing options and output stream.
  xacts_present_map xacts_present; ///< Tracks which transactions have already been queued.
  xacts_list xacts;                ///< Ordered list of unique transactions to print.
  bool print_raw;   ///< If true, output original source text instead of reconstructing.
  bool first_title; ///< Tracks whether a title separator is needed.
  bool include_auto_xacts;

public:
  /// @param _report    The report context.
  /// @param _print_raw If true, use raw source output instead of structured print.
  print_xacts(report_t& _report, bool _print_raw = false, bool _include_auto_xacts = true)
      : report(_report), print_raw(_print_raw), first_title(true),
        include_auto_xacts(_include_auto_xacts) {
    TRACE_CTOR(print_xacts, "report&, bool, bool");
  }
  ~print_xacts() override { TRACE_DTOR(print_xacts); }

  /// @brief Print a blank line between report groups.
  void title(const string&) override;

  /// @brief Render all collected transactions and flush the output stream.
  void flush() override;

  /// @brief Collect the transaction that owns this posting (deduplicated).
  void operator()(post_t& post) override;

  void clear() override {
    xacts_present.clear();
    xacts.clear();

    item_handler<post_t>::clear();
  }
};

} // namespace ledger
