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
 * @file   journal.h
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief  Central container for all financial data
 *
 * The journal_t class is the top-level data structure that owns the
 * account tree, all parsed transactions (regular, automated, and
 * periodic), and the configuration state accumulated from directives
 * in the input files (aliases, payee mappings, commodity declarations,
 * tag assertions, etc.).
 *
 * There is exactly one journal per session.  Parsing populates it,
 * and the reporting engine reads from it.  The journal is noncopyable
 * because it owns raw pointers to accounts and transactions.
 */
#pragma once

#include "utils.h"
#include "times.h"
#include "mask.h"
#include "expr.h"
#include "types.h"

namespace ledger {

/// Lot matching policy for commodity lots (used with --lot-prices).
enum class lot_policy_t : std::uint8_t {
  none, ///< No lot matching policy; lots are not automatically consumed.
  fifo, ///< First-in, first-out: oldest lots are consumed first.
  lifo  ///< Last-in, first-out: newest lots are consumed first.
};

class xact_base_t;
class parse_context_t;
class parse_context_stack_t;

using payee_alias_mapping_t = std::pair<mask_t, string>; ///< Regex-to-payee-name alias pair.
using payee_alias_mappings_t = std::list<payee_alias_mapping_t>;
using payee_uuid_mappings_t = std::unordered_map<string, string>; ///< UUID-to-payee-name mapping.
using account_mapping_t =
    std::pair<mask_t, account_t*>; ///< Regex-to-account mapping (for payee-based account routing).
using account_mappings_t = std::list<account_mapping_t>;

using payee_rewrite_mapping_t =
    std::pair<mask_t, string>; ///< Regex-to-replacement payee rewrite rule.
using payee_rewrite_mappings_t = std::list<payee_rewrite_mapping_t>;

using account_rewrite_mapping_t =
    std::pair<mask_t, string>; ///< Regex-to-replacement account rewrite rule.
using account_rewrite_mappings_t = std::list<account_rewrite_mapping_t>;
using checksum_map_t = std::map<string, xact_t*>; ///< UUID-to-transaction map for deduplication.

/// Map from tag name to assertion/check expression pairs for metadata validation.
using tag_check_exprs_map = std::multimap<string, expr_t::check_expr_pair>;

/**
 * @brief The central container for all financial data in a Ledger session.
 *
 * journal_t owns the account tree (rooted at @c master), all regular
 * transactions (xacts), automated transactions (auto_xacts), periodic
 * transactions (period_xacts), and the accumulated configuration state
 * from journal directives: account/payee aliases, commodity declarations,
 * tag assertions, and checking-style settings.
 *
 * There is exactly one journal_t per session.  It is noncopyable because
 * it holds owning raw pointers to accounts and transactions.  The
 * destructor deletes transactions before the account tree to avoid
 * use-after-free in auto_xact_t destructors.
 */
class journal_t : public noncopyable {
public:
  /**
   * @brief Metadata about a source file that was parsed into this journal.
   *
   * Tracks the file path and its modification time so that Ledger can
   * detect changes for incremental re-reads.  When input comes from a
   * stream (stdin), from_stream is true and filename is empty.
   */
  struct fileinfo_t {
    optional<path> filename; ///< Filesystem path, or none if read from a stream.
    datetime_t modtime;      ///< Last modification time at parse time.
    bool from_stream;        ///< True if this data was read from stdin/stream.

    fileinfo_t() : from_stream(true) { TRACE_CTOR(journal_t::fileinfo_t, ""); }
    fileinfo_t(const path& _filename) : filename(_filename), from_stream(false) {
      auto ftime = std::filesystem::last_write_time(*filename);
      auto sctp = std::chrono::time_point_cast<std::chrono::system_clock::duration>(
          ftime - decltype(ftime)::clock::now() + std::chrono::system_clock::now());
      modtime = posix_time::from_time_t(std::chrono::system_clock::to_time_t(sctp));
      TRACE_CTOR(journal_t::fileinfo_t, "const path&");
    }
    fileinfo_t(const fileinfo_t& info)
        : filename(info.filename), modtime(info.modtime), from_stream(info.from_stream) {
      TRACE_CTOR(journal_t::fileinfo_t, "copy");
    }
    ~fileinfo_t() noexcept { TRACE_DTOR(journal_t::fileinfo_t); }
  };

  account_t* master;              ///< Invisible root of the account tree (depth 0, empty name).
  account_t* bucket;              ///< Default account for postings with inferred (null) amounts.
  xacts_list xacts;               ///< All regular (dated) transactions, in parse order.
  auto_xacts_list auto_xacts;     ///< Automated transactions (= expr) that extend matching xacts.
  period_xacts_list period_xacts; ///< Periodic transactions (~ period) for budgeting/forecasting.
  std::list<fileinfo_t> sources;  ///< Source files that were parsed into this journal.
  std::set<string>
      known_payees; ///< Payees declared with `payee` directive (for --strict validation).
  std::set<string> known_tags; ///< Tags declared with `tag` directive (for --strict validation).
  bool was_loaded;             ///< True after at least one successful read().
  bool check_payees;      ///< True if payee validation is enabled (payee directives were seen).
  bool day_break;         ///< True if day-break transactions should be inserted between days.
  bool recursive_aliases; ///< If true, alias expansion repeats until no more aliases match.
  bool no_aliases;        ///< If true, all alias expansion is suppressed.
  lot_policy_t lot_matching_policy =
      lot_policy_t::none; ///< How commodity lots are matched for consumption.
  payee_alias_mappings_t
      payee_alias_mappings; ///< Regex-based payee alias rules from `alias` directives.
  payee_uuid_mappings_t payee_uuid_mappings;       ///< UUID-to-payee-name mappings.
  payee_rewrite_mappings_t payee_rewrite_mappings; ///< Regex-based payee rewrite rules.
  account_mappings_t account_mappings; ///< Regex-to-account mappings for automatic categorization.
  account_rewrite_mappings_t account_rewrite_mappings; ///< Regex-based account name rewrite rules.
  accounts_map account_aliases; ///< Account alias map from `alias Name=Target` directives.
  account_mappings_t
      payees_for_unknown_accounts; ///< Payee-to-account mapping for resolving "Unknown" accounts.
  checksum_map_t checksum_map;     ///< UUID-to-transaction map for duplicate detection.
  tag_check_exprs_map tag_check_exprs; ///< Assertion/check expressions for metadata tag values.
  std::optional<expr_t> value_expr;    ///< Journal-level valuation expression.
  parse_context_t* current_context;    ///< Active parse context during read() (nullptr otherwise).

  /**
   * @brief Controls how strictly unknown accounts/payees/commodities are handled.
   *
   * - CHECK_PERMISSIVE: accept everything (default before any `account`/`payee` directives).
   * - CHECK_NORMAL: the default after initialization.
   * - CHECK_WARNING: warn on unknown entities (--strict).
   * - CHECK_ERROR: error on unknown entities (--pedantic).
   */
  enum checking_style_t : uint8_t {
    CHECK_PERMISSIVE, ///< Accept all accounts/payees/commodities without checking.
    CHECK_NORMAL,     ///< Normal mode (default).
    CHECK_WARNING,    ///< Warn on unknown entities (--strict).
    CHECK_ERROR       ///< Error on unknown entities (--pedantic).
  } checking_style;

  journal_t();
#if 0
  journal_t(const path& pathname);
  journal_t(const string& str);
#endif
  ~journal_t();

  /** @brief Reset all members to their initial state and register built-in tags. */
  void initialize();

  std::list<fileinfo_t>::iterator sources_begin() { return sources.begin(); }
  std::list<fileinfo_t>::iterator sources_end() { return sources.end(); }

  void add_account(account_t* acct);
  [[nodiscard]] bool remove_account(account_t* acct);
  [[nodiscard]] account_t* find_account(string_view name, bool auto_create = true);
  [[nodiscard]] account_t* find_account_re(const string& regexp);

  /**
   * @brief Expand account aliases, returning the resolved account or nullptr.
   *
   * Checks the account_aliases map for a full-name match first, then
   * tries matching just the first colon-separated segment.  When
   * recursive_aliases is true, expansion repeats until no more aliases
   * match.  An already_seen set prevents infinite loops.
   */
  account_t* expand_aliases(string name);

  /**
   * @brief Resolve aliases, create the account, and enforce --strict/--pedantic checking.
   *
   * This is the main entry point for registering an account name seen
   * during parsing.  Steps: (1) expand aliases, (2) find/create the
   * account in the tree, (3) map "Unknown" accounts to payee-based
   * targets, (4) check against known_accounts for --strict/--pedantic,
   * skipping template variables like $var and %(expr).
   */
  account_t* register_account(string_view name, post_t* post, account_t* master = nullptr);

  /** @brief Register a payee name as known (for --strict validation). */
  string register_payee(const string& name);

  /**
   * @brief Translate a payee name through alias mappings and validate it.
   *
   * Applies payee_alias_mappings, then checks the result against
   * known_payees when --strict or --pedantic is active.
   */
  string validate_payee(const string& name_or_alias);

  /**
   * @brief Validate a commodity against known commodities (--strict/--pedantic).
   * @param context  int(0) for directive context (auto-registers), or xact/post for error context.
   */
  void register_commodity(commodity_t& comm, std::variant<int, xact_t*, post_t*> context);

  /**
   * @brief Validate a metadata tag and evaluate any tag_check_exprs assertions.
   *
   * When --strict/--pedantic is active, warns or errors on unknown tags.
   * Then evaluates any check/assert expressions registered for this tag
   * name via the `tag` directive's value assertions.
   */
  void register_metadata(const string& key, const value_t& value,
                         std::variant<int, xact_t*, post_t*> context);

  /**
   * @brief Add a parsed transaction to the journal.
   *
   * Steps: (1) finalize (balance check), (2) extend with auto_xacts,
   * (3) validate metadata, (4) extend each posting, (5) UUID dedup --
   * if the same UUID was seen before, verify postings are equivalent,
   * apply deferred posts, then reject the duplicate.
   *
   * @return true if the transaction was added, false if rejected (bad balance or duplicate UUID).
   */
  [[nodiscard]] bool add_xact(xact_t* xact);

  /** @brief Apply all auto_xacts to the given transaction. */
  void extend_xact(xact_base_t* xact);

  [[nodiscard]] bool remove_xact(xact_t* xact);

  xacts_list::iterator xacts_begin() { return xacts.begin(); }
  xacts_list::iterator xacts_end() { return xacts.end(); }
  auto_xacts_list::iterator auto_xacts_begin() { return auto_xacts.begin(); }
  auto_xacts_list::iterator auto_xacts_end() { return auto_xacts.end(); }
  period_xacts_list::iterator period_xacts_begin() { return period_xacts.begin(); }
  period_xacts_list::iterator period_xacts_end() { return period_xacts.end(); }

  /**
   * @brief Parse a journal file (or stream) and add its contents.
   *
   * Sets up the parse context, delegates to read_textual(), records the
   * fileinfo, and clears xdata that may have been set during parsing
   * (e.g., by balance assertions).
   *
   * @return The number of transactions successfully parsed.
   */
  std::size_t read(parse_context_stack_t& context, hash_type_t hash_type,
                   bool should_clear_xdata = true);

  [[nodiscard]] bool has_xdata();
  void clear_xdata();

  [[nodiscard]] bool valid() const;

private:
  std::size_t read_textual(parse_context_stack_t& context, hash_type_t hash_type);

  /** @brief True if payee checking is enabled and the checking style requires it. */
  bool should_check_payees();
  /** @brief True if the payee name has not been declared with a `payee` directive. */
  bool payee_not_registered(const string& name);
  /** @brief Apply payee_alias_mappings to translate a payee name. */
  string translate_payee_name(const string& name);
};

} // namespace ledger
