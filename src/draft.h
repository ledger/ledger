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
 * @file   draft.h
 * @author John Wiegley
 *
 * @ingroup report
 *
 * @brief Transaction drafting: the "xact" command that creates new
 *        transactions from templates and journal history.
 *
 * The draft_t class implements Ledger's "xact" (transaction drafting)
 * command.  Given a set of command-line arguments like:
 *
 *     ledger xact 2024/03/01 Grocery Store $45.00 from Checking
 *
 * it constructs a complete, balanced transaction by:
 *
 *   1. Parsing the arguments into an xact_template_t structure that
 *      captures the date, payee pattern, account patterns, amounts,
 *      and costs specified by the user.
 *   2. Searching the journal history (most recent first) for a past
 *      transaction whose payee matches the template's payee pattern.
 *   3. Filling in missing details (accounts, amounts, metadata) from
 *      the matching historical transaction.
 *   4. Inserting the new transaction into the journal (which validates
 *      it via finalize).
 *
 * The argument syntax supports natural-language prepositions:
 *   - "at <payee>"       : explicit payee
 *   - "to <account>"     : destination posting
 *   - "from <account>"   : source posting
 *   - "on <date>"        : explicit date
 *   - "code <code>"      : transaction code
 *   - "note <note>"      : transaction note
 *   - "@ <amount>"       : per-unit cost
 *   - "@@ <amount>"      : total cost
 *   - "rest"             : placeholder (ignored)
 *
 * Without prepositions, arguments are interpreted positionally: the
 * first non-date token is the payee, subsequent tokens are accounts or
 * amounts depending on whether they parse as valid amounts.
 */
#pragma once

#include "exprbase.h"
#include "value.h"

namespace ledger {

class journal_t;
class xact_t;

/**
 * @brief Parses command-line arguments into a transaction template and
 *        inserts a derived transaction into the journal.
 *
 * draft_t inherits from expr_base_t<value_t> so that it can participate
 * in the expression/command framework, though its real_calc is never
 * intended to be called (it asserts false).  The useful entry points
 * are parse_args() and insert().
 */
class draft_t : public expr_base_t<value_t> {
  using base_type = expr_base_t<value_t>;

  /**
   * @brief Internal representation of a partially-specified transaction.
   *
   * An xact_template_t captures what the user provided on the command
   * line.  Each field is optional because the user may provide only a
   * payee pattern and let the rest be filled in from journal history.
   */
  struct xact_template_t {
    optional<date_t> date;          ///< Explicit date, if provided
    optional<string> date_string;   ///< Raw date string for deferred parsing
    optional<string> code;          ///< Transaction code (e.g., check number)
    optional<string> note;          ///< Transaction note
    mask_t payee_mask;              ///< Pattern to match against historical payees

    /**
     * @brief A single posting within the transaction template.
     *
     * Each post_template_t represents one leg of the transaction.  The
     * "from" flag distinguishes source postings (whose amounts are negated)
     * from destination postings.
     */
    struct post_template_t {
      bool from;                          ///< True if this is a source ("from") posting
      optional<mask_t> account_mask;      ///< Pattern to match against account names
      optional<amount_t> amount;          ///< Explicit amount for this posting
      optional<string> cost_operator;     ///< "@" for per-unit or "@@" for total cost
      optional<amount_t> cost;            ///< Cost amount, if specified

      post_template_t() : from(false) { TRACE_CTOR(post_template_t, ""); }
      ~post_template_t() noexcept { TRACE_DTOR(post_template_t); }
    };

    std::list<post_template_t> posts; ///< The posting templates in order

    xact_template_t() { TRACE_CTOR(xact_template_t, ""); }
    xact_template_t(const xact_template_t& other)
        : date(other.date), date_string(other.date_string), code(other.code), note(other.note),
          payee_mask(other.payee_mask), posts(other.posts) {
      TRACE_CTOR(xact_template_t, "copy");
    }
    xact_template_t& operator=(const xact_template_t&) = default;
    ~xact_template_t() noexcept { TRACE_DTOR(xact_template_t); }

    /// Dump the template's contents in a human-readable format for
    /// the "template" pre-command.
    void dump(std::ostream& out) const;
  };

  optional<xact_template_t> tmpl; ///< The parsed template, set by parse_args()

public:
  /// Construct a draft, optionally parsing arguments immediately.
  draft_t(const value_t& args) : base_type() {
    if (!args.empty())
      parse_args(args);
    TRACE_CTOR(draft_t, "value_t");
  }
  ~draft_t() noexcept override { TRACE_DTOR(draft_t); }

  /// Parse command-line arguments into the internal xact_template_t.
  /// Handles dates, prepositions (at/to/from/on/code/note), amounts,
  /// costs, and positional account/payee inference.
  void parse_args(const value_t& args);

  /// Not intended to be called; asserts false.  draft_t uses insert()
  /// rather than the expression evaluation path.
  result_type real_calc(scope_t&) override {
    assert(false);
    return true;
  }

  /// Create a new transaction from the template and insert it into the
  /// journal.  Missing fields are filled from the most recent matching
  /// historical transaction.
  /// @return The newly created xact_t, owned by the journal.
  xact_t* insert(journal_t& journal);

  /// Dump the template for diagnostic display (used by the "template"
  /// pre-command).
  void dump(std::ostream& out) const override {
    if (tmpl)
      tmpl->dump(out);
  }
};

/// Command handler for "xact": draft a new transaction based on the
/// arguments and print it to the report output stream.
value_t xact_command(call_scope_t& args);

/// Command handler for "template": display the parsed transaction
/// template without creating a transaction.  Useful for debugging
/// the draft argument parser.
value_t template_command(call_scope_t& args);

} // namespace ledger
