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
 * @file   ptree.h
 * @author John Wiegley
 *
 * @ingroup report
 *
 * @brief Property tree (XML) serialization of journal data.
 *
 * The `format_ptree` handler is the backend for Ledger's `xml` command.
 * It collects all postings that flow through the report pipeline,
 * accumulates the unique commodities and transactions they reference,
 * and on `flush()` serializes everything into a Boost.PropertyTree
 * which is then written as indented XML.
 *
 * The XML output includes three top-level sections:
 * - `<commodities>` -- all commodities referenced by the matched postings
 * - `<accounts>` -- the full account hierarchy (filtered to visited accounts)
 * - `<transactions>` -- each transaction with its visited postings
 *
 * This is the same data available through the Python bindings, but in a
 * machine-readable text format suitable for consumption by external tools.
 */
#pragma once

#include "chain.h"

namespace ledger {

class xact_t;
class account_t;
class commodity_t;
class post_t;
class report_t;

/**
 * @brief Post handler that serializes journal data to a property tree (XML).
 *
 * As postings flow through `operator()`, the handler collects the unique
 * set of commodities and transactions they reference.  When `flush()` is
 * called, it builds a Boost.PropertyTree containing the full account
 * hierarchy, commodity definitions, and transaction/posting details,
 * then writes it as indented XML to the report's output stream.
 */
class format_ptree : public item_handler<post_t> {
protected:
  report_t& report; ///< The report context (provides output stream and session)

  using commodities_map = std::map<string, commodity_t*>;
  using commodities_pair = std::pair<string, commodity_t*>;

  commodities_map commodities;        ///< Unique commodities seen in matched postings
  std::set<xact_t*> transactions_set; ///< Set for O(1) duplicate detection
  std::deque<xact_t*> transactions;   ///< Transactions in encounter order

public:
  /// Output format selector (currently only XML is supported).
  enum format_t : uint8_t { FORMAT_XML } format;

  format_ptree(report_t& _report, format_t _format = FORMAT_XML)
      : report(_report), format(_format) {
    TRACE_CTOR(format_ptree, "report&, format_t");
  }
  ~format_ptree() override { TRACE_DTOR(format_ptree); }

  /// Build the property tree from accumulated data and write XML output.
  void flush() override;
  /// Collect a posting: record its commodity and parent transaction.
  void operator()(post_t& post) override;

  /// Reset all accumulated state for reuse.
  void clear() override {
    commodities.clear();
    transactions_set.clear();
    transactions.clear();

    item_handler<post_t>::clear();
  }
};

} // namespace ledger
