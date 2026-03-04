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
 * @defgroup data Data representation
 */

/**
 * @file   item.h
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief  Base class for all journal items (transactions and postings).
 *
 * Every entry in a Ledger journal -- whether a full transaction (xact_t) or
 * an individual posting (post_t) -- inherits from item_t.  This base class
 * provides the shared properties that all journal items carry:
 *
 *   - **Clearing state**: UNCLEARED, CLEARED (*), or PENDING (!)
 *   - **Dates**: a primary date and an optional auxiliary/effective date
 *   - **Notes and metadata**: free-form comments plus structured key/value
 *     tags parsed from `;`-prefixed comment lines
 *   - **Source position**: file path and line range for error reporting
 *   - **Expression evaluation**: inherits from scope_t so items can serve
 *     as variable-binding scopes in the expression engine
 *
 * The companion struct position_t records where in the source file an item
 * was parsed from, enabling precise error messages and the `print` command
 * to reproduce the original text.
 */
#pragma once

#include "scope.h"

namespace ledger {

/**
 * @brief Records the source file location of a parsed journal item.
 *
 * Every item_t carries an optional position_t that records exactly where
 * the item appeared in the input file.  This information powers error
 * messages ("transaction at line 42"), the `print` command (which
 * re-reads the original bytes between beg_pos and end_pos), and the
 * `source` command.  The sequence number provides a global ordering
 * across all items parsed during a session.
 */
struct position_t {
  path pathname;                  ///< Filesystem path of the journal file.
  std::istream::pos_type beg_pos; ///< Stream offset where the item begins.
  std::size_t beg_line;           ///< Line number where the item begins (1-based).
  std::istream::pos_type end_pos; ///< Stream offset just past the item's last byte.
  std::size_t end_line;           ///< Line number where the item ends (inclusive).
  std::size_t sequence;           ///< Global parse-order sequence number.

  position_t() : beg_pos(0), beg_line(0), end_pos(0), end_line(0), sequence(0) {
    TRACE_CTOR(position_t, "");
  }
  position_t(const position_t& pos) {
    *this = pos;
    TRACE_CTOR(position_t, "copy");
  }
  ~position_t() noexcept { TRACE_DTOR(position_t); }

  position_t& operator=(const position_t& pos) {
    if (this != &pos) {
      pathname = pos.pathname;
      beg_pos = pos.beg_pos;
      beg_line = pos.beg_line;
      end_pos = pos.end_pos;
      end_line = pos.end_line;
      sequence = pos.sequence;
    }
    return *this;
  }
};

/**
 * @brief Base class for all journal items: transactions and postings.
 *
 * item_t sits at the root of the data hierarchy.  Both xact_t (a dated
 * transaction with a payee) and post_t (an individual line item debiting
 * or crediting an account) derive from item_t.  This class provides:
 *
 *   - **Flags** via supports_flags (ITEM_GENERATED, ITEM_TEMP, etc.)
 *   - **Clearing state** (`*` = CLEARED, `!` = PENDING, blank = UNCLEARED)
 *   - **Primary and auxiliary dates** (the auxiliary date is the
 *     effective/value date, written as `2024/01/01=2024/02/01`)
 *   - **Notes** (free-form text from `;` comment lines)
 *   - **Metadata tags** stored in a case-insensitive string_map, supporting
 *     both bare tags (`:tag1:tag2:`) and key/value pairs (`Key: Value`)
 *   - **Source position** for error reporting and source reproduction
 *   - **Expression scope** (inherits scope_t) so that expressions like
 *     `date > [2024-01-01]` can resolve `date` against the item
 *
 * The metadata system uses a custom case-insensitive comparator so that
 * `Payee`, `payee`, and `PAYEE` all refer to the same tag.
 */
class item_t : public flags::supports_flags<uint_least16_t>, public scope_t {
public:
#define ITEM_NORMAL 0x00 ///< No special flags; a regular user-entered item.
#define ITEM_GENERATED                                                                             \
  0x01 ///< Item was generated by Ledger (e.g., by an automated transaction), not parsed from a
       ///< journal file.
#define ITEM_TEMP                                                                                  \
  0x02 ///< Item is a temporary created during report generation; not owned by the journal.
#define ITEM_NOTE_ON_NEXT_LINE                                                                     \
  0x04 ///< Parser saw the note/comment on the line following the item, not on the same line.
#define ITEM_INFERRED                                                                              \
  0x08 ///< Item was inferred by the bucketing/balancing logic (e.g., a null-amount posting filled
       ///< in by finalize).

  /** @brief Clearing state for a journal item.
   *
   * In Ledger's journal syntax, a transaction or posting may carry a
   * clearing mark: `*` for CLEARED (reconciled with a statement),
   * `!` for PENDING (entered but not yet reconciled), or no mark for
   * UNCLEARED.  These states are used by the `--cleared`, `--pending`,
   * and `--uncleared` report filters.
   */
  enum state_t : uint8_t { UNCLEARED = 0, CLEARED, PENDING };

  /// A metadata entry: an optional value paired with a bool indicating
  /// whether the tag was parsed from a comment line (true) vs. set
  /// programmatically (false).
  using tag_data_t = std::pair<std::optional<value_t>, bool>;

  /// Case-insensitive ordered map from tag names to their values.
  /// The comparator function uses boost::algorithm::ilexicographical_compare
  /// so that "Payee" and "payee" are treated as the same key.
  using string_map = std::map<string, tag_data_t, std::function<bool(string, string)>>;

  scope_t*
      parent; ///< Enclosing scope for expression evaluation (e.g., the transaction for a posting).
  state_t _state;                ///< Current clearing state (UNCLEARED, CLEARED, or PENDING).
  optional<date_t> _date;        ///< Primary date of the item.
  optional<date_t> _date_aux;    ///< Auxiliary (effective) date, written after `=` in date syntax.
  optional<string> note;         ///< Free-form note text from `;` comment lines.
  optional<position_t> pos;      ///< Source file location for error reporting and `print` output.
  optional<string_map> metadata; ///< Structured metadata tags parsed from comment lines.
  bool defining_; ///< Re-entrancy guard for define(); prevents infinite recursion when setting tags
                  ///< triggers expression evaluation.

  item_t(flags_t _flags = ITEM_NORMAL, const optional<string>& _note = none)
      : supports_flags<uint_least16_t>(_flags), parent(nullptr), _state(UNCLEARED), note(_note),
        defining_(false) {
    TRACE_CTOR(item_t, "flags_t, const string&");
  }
  item_t(const item_t& item)
      : supports_flags<uint_least16_t>(item), scope_t(), parent(nullptr), defining_(false) {
    copy_details(item);
    TRACE_CTOR(item_t, "copy");
  }
  ~item_t() override { TRACE_DTOR(item_t); }

  /** @brief Copy all mutable fields from another item.
   *
   * Used by the copy constructor and by post_t when duplicating
   * postings for automated transactions.  Does not copy the parent
   * pointer (which is set separately by the owning transaction).
   */
  virtual void copy_details(const item_t& item) {
    set_flags(item.flags());
    set_state(item.state());

    parent = item.parent;
    _date = item._date;
    _date_aux = item._date_aux;
    note = item.note;
    pos = item.pos;
    metadata = item.metadata;
  }

  virtual bool operator==(const item_t& xact) { return this == &xact; }
  virtual bool operator!=(const item_t& xact) { return !(*this == xact); }

  scope_t* get_parent() override { return parent; }

  /**
   * @brief Return a unique identifier for this item.
   *
   * If the item carries a `UUID` metadata tag (set via `; UUID: ...`
   * in the journal), that value is returned.  Otherwise the parse-order
   * sequence number is used as a stable, session-unique identifier.
   */
  string id() const {
    if (std::optional<value_t> ref = get_tag(_("UUID"))) {
      return ref->to_string();
    } else {
      std::ostringstream buf;
      buf << seq();
      return buf.str();
    }
  }

  /// Return the global parse-order sequence number, or 0 if no position is recorded.
  std::size_t seq() const { return pos ? pos->sequence : 0L; }

  /**
   * @brief Test whether a metadata tag exists on this item.
   * @param tag   Tag name to look up (case-insensitive).
   * @param inherit  If true, also search ancestor scopes (overridden
   *                 by post_t to check the parent transaction).
   */
  virtual bool has_tag(const string& tag, bool inherit = true) const;

  /** @brief Test whether any tag matching a regex pattern exists.
   * @param tag_mask    Regex to match against tag names.
   * @param value_mask  Optional regex to additionally match the tag's value.
   * @param inherit     If true, also search ancestor scopes.
   */
  virtual bool has_tag(const mask_t& tag_mask, const std::optional<mask_t>& value_mask = {},
                       bool inherit = true) const;

  /**
   * @brief Retrieve the value of a metadata tag.
   * @param tag   Tag name to look up (case-insensitive).
   * @param inherit  If true, also search ancestor scopes.
   * @return The tag's value, or nullopt if the tag does not exist.
   */
  virtual std::optional<value_t> get_tag(const string& tag, bool inherit = true) const;

  /** @brief Retrieve the first tag value matching a regex pattern.
   * @param tag_mask    Regex to match against tag names.
   * @param value_mask  Optional regex to additionally match the tag's value.
   * @param inherit     If true, also search ancestor scopes.
   */
  virtual std::optional<value_t> get_tag(const mask_t& tag_mask,
                                         const std::optional<mask_t>& value_mask = {},
                                         bool inherit = true) const;

  /**
   * @brief Set or overwrite a metadata tag on this item.
   * @param tag                Tag name (case-insensitive key).
   * @param value              Optional value to associate with the tag.
   * @param overwrite_existing If false, an existing tag is not modified.
   * @return Iterator to the inserted or existing tag entry.
   *
   * If the metadata map does not yet exist, it is created with a
   * CaseInsensitiveKeyCompare comparator.  Null or empty-string values
   * are stored as valueless tags.
   */
  virtual string_map::iterator set_tag(const string& tag, const std::optional<value_t>& value = {},
                                       const bool overwrite_existing = true);

  /**
   * @brief Parse metadata tags from a comment line.
   *
   * Recognizes three forms of metadata in comment text:
   *   - `[date=auxdate]` -- bracketed date/auxiliary-date override
   *   - `:tag1:tag2:tag3:` -- colon-delimited bare tags (no values)
   *   - `Key: value` -- a key/value metadata pair (first token ending in `:`)
   *   - `Key:: expr` -- like `Key:` but the value is an expression that
   *     is evaluated in the current scope before being stored
   *
   * @param p                  Raw comment text (without the leading `;`).
   * @param scope              Scope for evaluating `Key::` expressions.
   * @param overwrite_existing Whether to overwrite tags that already exist.
   */
  virtual void parse_tags(const char* p, scope_t& scope, bool overwrite_existing = true);

  /**
   * @brief Append text to this item's note and parse any embedded tags.
   *
   * If the item already has a note, the new text is appended on a new
   * line.  After appending, parse_tags() is called to extract any
   * metadata from the new text.
   */
  virtual void append_note(const char* p, scope_t& scope, bool overwrite_existing = true);

  /// When true, date() returns the auxiliary date instead of the primary
  /// date.  Controlled by the `--aux-date` command-line option.
  static bool use_aux_date;

  virtual bool has_date() const { return static_cast<bool>(_date); }

  /**
   * @brief Return the effective date for this item.
   *
   * Normally returns the primary date.  When the `--aux-date` option
   * is active (use_aux_date == true), the auxiliary date is returned
   * instead, if one exists.  This allows reports to be organized by
   * effective/value date rather than entry date.
   */
  virtual date_t date() const {
    assert(_date);
    if (use_aux_date)
      if (optional<date_t> aux = aux_date())
        return *aux;
    return *_date;
  }

  /// Return the primary (entry) date, ignoring any auxiliary date.
  virtual date_t primary_date() const {
    assert(_date);
    return *_date;
  }

  /// Return the auxiliary (effective) date, if one was set.
  virtual optional<date_t> aux_date() const { return _date_aux; }

  void set_state(state_t new_state) { _state = new_state; }
  virtual state_t state() const { return _state; }

  /**
   * @brief Store an expression result as a metadata tag.
   *
   * Called by the expression engine when an assignment like
   * `define payee = "..."` is evaluated in this item's scope.  Uses
   * a re-entrancy guard (defining_) to prevent infinite recursion.
   */
  void define(const symbol_t::kind_t, const string&, const expr_t::ptr_op_t&) override;

  /**
   * @brief Resolve expression function names against this item.
   *
   * Maps names like `date`, `status`, `note`, `has_tag`, `meta`,
   * `filename`, etc. to getter functions that extract the corresponding
   * property from this item.  Also provides single-character aliases:
   *   - `d` = date
   *   - `L` = actual (not generated)
   *   - `X` = cleared
   *   - `Y` = pending
   *
   * Subclasses (xact_t, post_t) override this to add their own bindings
   * and fall through to item_t::lookup() for the base set.
   */
  expr_t::ptr_op_t lookup(const symbol_t::kind_t kind, const string& name) override;

  bool valid() const;
};

/**
 * @brief Format an item's note as a ledger-style comment string.
 *
 * Returns the note text prefixed with `;` separators suitable for
 * embedding in printed transaction output.  Short notes get `  ;`
 * on the same line; longer notes start on a new line with `\n    ;`.
 */
value_t get_comment(item_t& item);

/**
 * @brief Print the source text of an item to an output stream.
 *
 * Re-reads the original bytes between pos->beg_pos and pos->end_pos
 * from the source file and writes them with an optional line prefix.
 */
void print_item(std::ostream& out, const item_t& item, const string& prefix = "");

/**
 * @brief Build a human-readable error context string for an item.
 *
 * Produces a message like `"transaction from \"file.dat\", lines 5-8:"`
 * followed by the source text, for use in error reporting.
 */
string item_context(const item_t& item, const string& desc);

/**
 * @brief Serialize metadata tags into a property tree for XML output.
 *
 * Valueless tags are written as `<tag>name</tag>` elements; key/value
 * pairs are written as `<value key="name">...</value>` elements.
 */
void put_metadata(property_tree::ptree& pt, const item_t::string_map& metadata);

} // namespace ledger
