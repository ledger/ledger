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
 * @file   emacs.h
 * @author John Wiegley
 * @brief  Emacs Lisp (s-expression) output handler for Ledger integration.
 *
 * @ingroup report
 *
 * This handler produces output as Emacs Lisp s-expressions, designed to be
 * consumed by ledger-mode in Emacs.  Rather than human-readable text, the
 * output is a nested list structure that Emacs can `read` directly into
 * Lisp objects for programmatic access to transaction and posting data.
 *
 * The output structure is:
 * @code
 * ((<xact-1-header>
 *   (<posting-1-line> <posting-2-line> ...))
 *  (<xact-2-header>
 *   (<posting-1-line> ...)))
 * @endcode
 *
 * Each transaction header includes the source file path, line number,
 * date (as an Emacs time value or custom format), code, and payee.
 * Each posting includes the line number, account name, amount, clearing
 * state, and optional cost and note.
 *
 * This mode is activated by the `--pager=emacs` or related Emacs
 * integration options in ledger-mode.
 */
#pragma once

#include "chain.h"

namespace ledger {

class xact_t;

class report_t;

/**
 * @brief Terminal handler that outputs postings as Emacs Lisp s-expressions.
 *
 * Transactions are grouped: when a new transaction is encountered, the
 * previous one's list is closed and a new one opened.  flush() closes
 * the outermost list.  Strings are escaped for safe embedding in Lisp
 * double-quoted string literals.
 */
class format_emacs_posts : public item_handler<post_t> {
  format_emacs_posts(); ///< Disabled default constructor (output stream is required).

protected:
  report_t& report;    ///< The report context (provides date format options).
  std::ostream& out;   ///< The output stream for s-expression output.
  xact_t* last_xact;  ///< Tracks the previous transaction to detect boundaries.

public:
  /// @param _report  The report context.
  /// @param _out     The output stream to write s-expressions to.
  format_emacs_posts(report_t& _report, std::ostream& _out)
      : report(_report), out(_out), last_xact(nullptr) {
    TRACE_CTOR(format_emacs_posts, "report_t&, std::ostream&");
  }
  ~format_emacs_posts() override { TRACE_DTOR(format_emacs_posts); }

  /// @brief Write the transaction header portion of the s-expression
  ///        (file path, line number, date, code, payee).
  virtual void write_xact(xact_t& xact);

  /// @brief Close the outermost list and flush the stream.
  void flush() override {
    if (last_xact)
      out << "))\n";
    out.flush();
  }

  /// @brief Format a single posting as an s-expression list element.
  void operator()(post_t& post) override;

  /// @brief Escape backslashes and double quotes for Lisp string literals.
  virtual string escape_string(string raw);
};

} // namespace ledger
