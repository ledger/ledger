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
 * @file   output.h
 * @author John Wiegley
 * @brief  Terminal output handlers for formatted posting and account reports.
 *
 * @ingroup report
 *
 * This header declares the "terminal" handlers in Ledger's filter chain --
 * the handlers that sit at the end of the pipeline and actually produce
 * visible output.  Each handler receives postings or accounts from upstream
 * filters and renders them using format_t format strings.
 *
 * The primary handlers are:
 *   - format_posts   -- renders postings for the `register` command
 *   - format_accounts -- renders accounts for the `balance` command
 *
 * Additionally, several lightweight listing handlers exist for the
 * `accounts`, `payees`, `tags`, and `commodities` commands.  These
 * collect unique names/counts and output simple lists.
 *
 * All handlers inherit from item_handler<T>, which provides the
 * chain-of-responsibility pattern: each handler has a `handler` pointer
 * to the next downstream handler (or nullptr for terminal handlers).
 */
#pragma once

#include "chain.h"
#include "predicate.h"
#include "format.h"
#include "account.h"

namespace ledger {

class xact_t;
class post_t;
class item_t;
class report_t;

/**
 * @brief Terminal handler that formats postings for display.
 *
 * Used by the `register` command to render each posting as a formatted line.
 * The format string is split on `%/` into up to three sub-formats:
 *   1. first_line_format  -- used for the first posting of each transaction
 *   2. next_lines_format  -- used for subsequent postings in the same transaction
 *   3. between_format     -- printed between transactions (optional separator)
 *
 * The first-line format typically includes the date and payee, while the
 * next-lines format omits them so that related postings appear grouped
 * under their transaction header.
 */
class format_posts : public item_handler<post_t> {
protected:
  report_t& report;                ///< The report context providing options and output stream.
  format_t first_line_format;      ///< Format for the first posting of each transaction.
  format_t next_lines_format;      ///< Format for subsequent postings in the same transaction.
  format_t between_format;         ///< Format printed between different transactions.
  format_t prepend_format;         ///< Optional prefix prepended to every output line.
  std::size_t prepend_width;       ///< Width reserved for the prepend column.
  xact_t* last_xact;              ///< Tracks the previous transaction to detect boundaries.
  post_t* last_post;              ///< Tracks the previous posting to detect date changes.
  bool first_report_title;         ///< True until the first group title has been printed.
  string report_title;             ///< Current group title (set by upstream grouping filters).

public:
  format_posts(report_t& _report, const string& format,
               const optional<string>& _prepend_format = none, std::size_t _prepend_width = 0);
  ~format_posts() override { TRACE_DTOR(format_posts); }

  /// @brief Set the group title to be printed before the next posting.
  void title(const string& str) override { report_title = str; }

  /// @brief Flush the output stream.
  void flush() override;

  /// @brief Format and output a single posting.
  void operator()(post_t& post) override;

  void clear() override {
    last_xact = nullptr;
    last_post = nullptr;

    report_title = "";

    item_handler<post_t>::clear();
  }
};

/**
 * @brief Terminal handler that formats accounts for display.
 *
 * Used by the `balance` command.  The format string is split on `%/` into:
 *   1. account_line_format -- used for each account line
 *   2. total_line_format   -- used for the grand total line at the bottom
 *   3. separator_format    -- printed between the account list and the total
 *
 * Before rendering, flush() calls mark_accounts() to walk the account
 * hierarchy and decide which accounts should be displayed based on the
 * display predicate, the --flat flag, and whether accounts have been
 * visited by upstream filters.
 */
class format_accounts : public item_handler<account_t> {
protected:
  report_t& report;                    ///< The report context providing options and output stream.
  format_t account_line_format;        ///< Format for each account line.
  format_t total_line_format;          ///< Format for the grand total line.
  format_t separator_format;           ///< Format for the separator between accounts and total.
  format_t prepend_format;             ///< Optional prefix prepended to every output line.
  std::size_t prepend_width;           ///< Width reserved for the prepend column.
  predicate_t disp_pred;               ///< Display predicate from --display option.
  bool first_report_title;             ///< True until the first group title has been printed.
  string report_title;                 ///< Current group title.

  std::list<account_t*> posted_accounts; ///< Accounts queued for display (in posting order).

public:
  format_accounts(report_t& _report, const string& _format,
                  const optional<string>& _prepend_format = none, std::size_t _prepend_width = 0);
  ~format_accounts() override { TRACE_DTOR(format_accounts); }

  /**
   * @brief Recursively mark which accounts should be displayed.
   *
   * Walks the account hierarchy depth-first.  An account is marked for
   * display if it (or its children) have been visited, it passes the
   * display predicate, and it has a non-trivial display total.  The
   * --flat flag changes the visibility logic so that intermediate
   * parent accounts are not shown.
   *
   * @param account  The root of the subtree to mark.
   * @param flat     True if --flat mode is active.
   * @return         A pair of (visited_count, to_display_count) for the
   *                 subtree.
   */
  std::pair<std::size_t, std::size_t> mark_accounts(account_t& account, const bool flat);

  /// @brief Set the group title to be printed before the next account.
  void title(const string& str) override { report_title = str; }

  /**
   * @brief Render a single account line, recursing to parents in
   *        hierarchical (non-flat) mode.
   * @return 1 if the account was displayed, 0 otherwise.
   */
  virtual std::size_t post_account(account_t& account, const bool flat);

  /// @brief Mark accounts, render all queued accounts, and print the total.
  void flush() override;

  /// @brief Queue an account for later display during flush().
  void operator()(account_t& account) override;

  void clear() override {
    disp_pred.mark_uncompiled();
    posted_accounts.clear();

    report_title = "";

    item_handler<account_t>::clear();
  }
};

/**
 * @brief Lists unique account names with optional posting counts.
 *
 * Used by the `accounts` command.  Collects all accounts referenced by
 * postings that pass through the filter chain, plus any accounts flagged
 * as ACCOUNT_KNOWN in the journal.  When `--count` is active, each
 * account name is prefixed with the number of postings that referenced it.
 */
class report_accounts : public item_handler<post_t> {
public:
  using accounts_pair = std::map<account_t*, std::size_t>::value_type;
  using accounts_report_map = std::map<account_t*, std::size_t, account_compare>;

protected:
  report_t& report;              ///< The report context.
  accounts_report_map accounts;  ///< Map of accounts to their posting counts.

public:
  report_accounts(report_t& _report) : report(_report) { TRACE_CTOR(report_accounts, "report&"); }
  ~report_accounts() override { TRACE_DTOR(report_accounts); }

  /// @brief Output all collected account names (sorted by account_compare).
  void flush() override;
  /// @brief Record the account from this posting.
  void operator()(post_t& post) override;

  void clear() override {
    accounts.clear();
    item_handler<post_t>::clear();
  }
};

/**
 * @brief Lists unique payee names with optional posting counts.
 *
 * Used by the `payees` command.  Collects payee strings from postings
 * and outputs them alphabetically.
 */
class report_payees : public item_handler<post_t> {
protected:
  report_t& report;                        ///< The report context.
  std::map<string, std::size_t> payees;    ///< Map of payee names to posting counts.

  using payees_pair = std::map<string, std::size_t>::value_type;

public:
  report_payees(report_t& _report) : report(_report) { TRACE_CTOR(report_payees, "report&"); }
  ~report_payees() override { TRACE_DTOR(report_payees); }

  /// @brief Output all collected payee names.
  void flush() override;
  /// @brief Record the payee from this posting's transaction.
  void operator()(post_t& post) override;

  void clear() override {
    payees.clear();
    item_handler<post_t>::clear();
  }
};

/**
 * @brief Lists unique metadata tag names with optional counts.
 *
 * Used by the `tags` command.  Gathers tags from both the transaction and
 * the posting metadata.  When `--values` is active, tag values are
 * appended (e.g., "Tag: value").
 */
class report_tags : public item_handler<post_t> {
protected:
  report_t& report;                      ///< The report context.
  std::map<string, std::size_t> tags;    ///< Map of tag strings to occurrence counts.

  using tags_pair = std::map<string, std::size_t>::value_type;

public:
  report_tags(report_t& _report) : report(_report) { TRACE_CTOR(report_tags, "report&"); }
  ~report_tags() override { TRACE_DTOR(report_tags); }

  /// @brief Output all collected tag names.
  void flush() override;
  /// @brief Extract metadata tags from a single item (transaction or posting).
  virtual void gather_metadata(item_t& item);
  /// @brief Gather tags from both the transaction and the posting.
  void operator()(post_t& post) override;

  void clear() override {
    tags.clear();
    item_handler<post_t>::clear();
  }
};

/**
 * @brief Lists unique commodity symbols with optional counts.
 *
 * Used by the `commodities` command.  Collects commodities from posting
 * amounts, annotated price commodities, and cost commodities.
 */
class report_commodities : public item_handler<post_t> {
protected:
  report_t& report; ///< The report context.

  using commodities_pair = std::map<commodity_t*, std::size_t>::value_type;
  using commodities_report_map = std::map<commodity_t*, std::size_t, commodity_compare>;

  commodities_report_map commodities; ///< Map of commodities to occurrence counts.

public:
  report_commodities(report_t& _report) : report(_report) {
    TRACE_CTOR(report_commodities, "report&");
  }
  ~report_commodities() override { TRACE_DTOR(report_commodities); }

  /// @brief Output all collected commodity symbols.
  void flush() override;
  /// @brief Collect commodities from this posting's amount, price, and cost.
  void operator()(post_t& post) override;

  void clear() override {
    commodities.clear();
    item_handler<post_t>::clear();
  }
};

} // namespace ledger
