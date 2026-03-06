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
 * @file   chain.h
 * @author John Wiegley
 *
 * @ingroup report
 *
 * @brief Base handler type and pipeline construction API for the filter chain.
 *
 * This header defines the fundamental building blocks of Ledger's reporting
 * pipeline:
 *
 * - **item_handler<T>**: The base class for every filter in the chain.  Each
 *   handler holds a shared_ptr to the next handler downstream.  Posting data
 *   enters the outermost handler and flows inward toward the output handler.
 *
 * - **chain_handlers()**: The top-level entry point that constructs a complete
 *   filter pipeline from the user's command-line options.  It delegates to
 *   chain_post_handlers() (inner/downstream filters) and
 *   chain_pre_post_handlers() (outer/upstream filters), both defined in
 *   chain.cc.
 *
 * The concrete filter classes themselves are declared in filters.h; this
 * header is intentionally lightweight so that it can be included by any
 * module that needs to pass around handler pointers without pulling in the
 * full filter inventory.
 */
#pragma once

#include "utils.h"

namespace ledger {

class post_t;
class account_t;

/**
 * @brief Base class for all handlers in the filter pipeline.
 *
 * item_handler implements the Chain of Responsibility pattern: each handler
 * wraps an optional downstream handler via a shared_ptr.  The default
 * implementations of flush(), operator(), and clear() simply delegate to that
 * downstream handler, so a concrete filter only needs to override the methods
 * where it adds behavior.
 *
 * @tparam T  The item type flowing through the pipeline -- either post_t
 *            (for posting pipelines) or account_t (for account pipelines).
 *
 * @par Lifecycle
 * -# **operator()(T&)** is called once per item entering this stage.
 * -# **flush()** is called after all items have been submitted, giving
 *    accumulating filters a chance to emit their results.
 * -# **clear()** resets mutable state so the handler can be reused for a
 *    second report run (e.g., in interactive mode).
 */
template <typename T>
class item_handler : public noncopyable {
protected:
  std::shared_ptr<item_handler>
      handler; ///< Next (downstream) handler in the chain, or nullptr for the terminal handler.

public:
  item_handler() { TRACE_CTOR(item_handler, ""); }
  item_handler(std::shared_ptr<item_handler> _handler) : handler(_handler) {
    TRACE_CTOR(item_handler, "std::shared_ptr<item_handler>");
  }
  virtual ~item_handler() { TRACE_DTOR(item_handler); }

  /// Propagate a section title (e.g., from post_splitter) downstream.
  virtual void title(const string& str) {
    if (handler)
      handler->title(str);
  }

  /// Signal the end of input; accumulating handlers emit results here.
  virtual void flush() {
    if (handler)
      handler->flush();
  }

  /// Process a single item.  The default forwards it to the next handler.
  virtual void operator()(T& item) {
    if (handler) {
      check_for_signal();
      (*handler)(item);
    }
  }

  /// Reset mutable state for reuse across multiple report invocations.
  virtual void clear() {
    if (handler)
      handler->clear();
  }
};

/// Convenience alias for a shared pointer to a posting handler.
using post_handler_ptr = std::shared_ptr<item_handler<post_t>>;
/// Convenience alias for a shared pointer to an account handler.
using acct_handler_ptr = std::shared_ptr<item_handler<account_t>>;

class report_t;

/**
 * @brief Build the outer (upstream) portion of the posting filter chain.
 *
 * Wraps @p base_handler with filters that must execute before the core
 * pipeline: anonymization (--anon), the primary limit predicate (--limit),
 * and budget/forecast generation.  When budget or forecast handlers are
 * active, the limit predicate is re-applied so that only matching postings
 * contribute to budget calculations.
 *
 * @param base_handler  The handler chain built so far (from chain_post_handlers).
 * @param report        The report_t holding all parsed command-line options.
 * @return The new outermost handler in the chain.
 */
post_handler_ptr chain_pre_post_handlers(post_handler_ptr base_handler, report_t& report);

/**
 * @brief Build the inner (downstream) portion of the posting filter chain.
 *
 * Starting from the output handler, this function wraps it with filters in
 * inside-out order: inject, related, transfer_details, interval, aggregation,
 * collapse, sort, only-predicate, calc, changed_value, display filter,
 * truncation, and forecast-while.  Each filter is added conditionally based
 * on the options present in @p report.
 *
 * @param base_handler        The terminal output handler (e.g., format_posts).
 * @param report              The report_t holding all parsed command-line options.
 * @param for_accounts_report When true, several display-oriented filters
 *                            (truncation, display_filter, display predicate,
 *                            sort, collapse, subtotal/equity) are skipped
 *                            because the accounts report handles those concerns
 *                            differently.
 * @return The new outermost handler in the chain.
 */
post_handler_ptr chain_post_handlers(post_handler_ptr base_handler, report_t& report,
                                     bool for_accounts_report = false);

/**
 * @brief Construct a complete posting filter chain from command-line options.
 *
 * This is the top-level entry point called by the report commands (register,
 * balance, print, etc.) to build the full pipeline.  It first constructs the
 * inner chain via chain_post_handlers(), then wraps it with the outer chain
 * via chain_pre_post_handlers().  The order matters: since each new handler
 * wraps the previous, the last function called produces the outermost handler
 * -- postings enter chain_pre_post_handlers first, then flow into
 * chain_post_handlers, and finally reach the output handler.
 *
 * @param handler              The terminal output handler.
 * @param report               The report_t holding all parsed command-line options.
 * @param for_accounts_report  Passed through to chain_post_handlers().
 * @return The outermost handler, ready to receive postings.
 */
inline post_handler_ptr chain_handlers(post_handler_ptr handler, report_t& report,
                                       bool for_accounts_report = false) {
  handler = chain_post_handlers(handler, report, for_accounts_report);
  handler = chain_pre_post_handlers(handler, report);
  return handler;
}

} // namespace ledger
