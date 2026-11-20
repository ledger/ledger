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
 * @file   stats.h
 * @author John Wiegley
 *
 * @ingroup report
 */
#pragma once

#include "account.h"
#include "chain.h"

namespace ledger {

class report_t;

/**
 * @brief Accumulates statistics over the filtered posting stream for the `stats` command.
 *
 * Participates in the normal post filter chain, so `--begin`, `--end`,
 * `-p`, `--limit`, and other query filters apply to the reported
 * statistics.  An earlier free-function implementation walked the entire
 * journal unconditionally, ignoring all such filters (issue #742).
 */
class report_statistics : public item_handler<post_t> {
protected:
  report_t& report;                         ///< The report context.
  account_t::xdata_t::details_t statistics; ///< Accumulated statistics.
  const date_t today; ///< Reference date for the recency counters, fixed when the stats report
                      ///< starts (after --now).

public:
  report_statistics(report_t& _report) : report(_report), today(CURRENT_DATE()) {
    TRACE_CTOR(report_statistics, "report&");
  }
  ~report_statistics() override { TRACE_DTOR(report_statistics); }

  /// @brief Emit the accumulated statistics.
  void flush() override;
  /// @brief Incorporate a single (filtered) posting into the statistics.
  void operator()(post_t& post) override;

  void clear() override {
    statistics = account_t::xdata_t::details_t();
    item_handler<post_t>::clear();
  }
};

} // namespace ledger
