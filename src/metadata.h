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
 * @file   metadata.h
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief  Shared metadata-tag storage and parser for journal items and accounts.
 *
 * metadata_t is a non-virtual mixin holding the case-insensitive
 * `string_map` used by both item_t (and its derivatives xact_t, post_t)
 * and account_t.  It provides the local has_tag/get_tag/set_tag
 * operations and the `:tag:` / `Key: value` / `Key:: expr` parser.
 * Subclasses override the lookup methods to add their own inheritance
 * walk (post_t walks its parent xact, account_t walks its parent
 * account); item_t additionally extends the parser with bracketed
 * `[date]` syntax.
 */
#pragma once

#include "scope.h"

namespace ledger {

/**
 * @brief Storage and behavior for metadata tags shared by items and accounts.
 *
 * Both journal items (transactions and postings) and accounts may carry
 * a case-insensitive map of metadata tags, parsed from `;`-prefixed
 * comment lines.  metadata_t centralizes that storage and the parsing
 * logic so the two class hierarchies share one implementation.
 */
class metadata_t {
public:
  /// A metadata entry: an optional value paired with a bool indicating
  /// whether the tag was parsed from a comment line (true) vs. set
  /// programmatically (false).
  using tag_data_t = std::pair<std::optional<value_t>, bool>;

  /// Case-insensitive ordered map from tag names to their values.
  using string_map = std::map<string, tag_data_t, std::function<bool(string, string)>>;

  optional<string_map> metadata; ///< Structured metadata tags parsed from comment lines.

  metadata_t() = default;
  metadata_t(const metadata_t&) = default;
  metadata_t& operator=(const metadata_t&) = default;
  virtual ~metadata_t() = default;

  /**
   * @brief Test whether a metadata tag exists locally on this holder.
   *
   * The @p inherit parameter is unused by the base class; subclasses
   * override this to add an inheritance walk (post_t -> xact_t,
   * account_t -> parent account).
   */
  virtual bool has_tag(const string& tag, bool inherit = true) const;

  /// @copydoc has_tag(const string&, bool) const
  virtual bool has_tag(const mask_t& tag_mask, const std::optional<mask_t>& value_mask = {},
                       bool inherit = true) const;

  /**
   * @brief Retrieve the value of a metadata tag from this holder.
   *
   * Subclasses override to add inheritance.  Returns nullopt when the
   * tag is absent or its value is unset.
   */
  virtual std::optional<value_t> get_tag(const string& tag, bool inherit = true) const;

  /// @copydoc get_tag(const string&, bool) const
  virtual std::optional<value_t> get_tag(const mask_t& tag_mask,
                                         const std::optional<mask_t>& value_mask = {},
                                         bool inherit = true) const;

  /**
   * @brief Set or overwrite a metadata tag, creating the map if needed.
   *
   * The map is initialized lazily with a case-insensitive comparator.
   * Null or empty-string values are normalized to nullopt.
   */
  virtual string_map::iterator set_tag(const string& tag, const std::optional<value_t>& value = {},
                                       bool overwrite_existing = true);

  /**
   * @brief One tag found in a comment by scan_metadata_tags().
   *
   * A bare `:name:` tag has no value; a `Key:` or `Key::` setting
   * carries the trimmed rest of the line, which is an expression when
   * @c by_value is set.
   */
  struct parsed_tag_t {
    string name;
    std::optional<string> value;
    bool by_value = false;
  };

  /**
   * @brief Find the `:tag:` and `Key:`/`Key::` metadata in a comment line.
   *
   * This is the tokenizer of parse_metadata_tags(); it sets and
   * evaluates nothing.  Recognizes:
   *   - `:tag1:tag2:tag3:` -- colon-delimited bare tags.
   *   - `Key: value`       -- the first whitespace-delimited token ending
   *                           in `:` names a key whose value is the rest of
   *                           the line as a string.
   *   - `Key:: expr`       -- like `Key:` but the value is an expression,
   *                           which parse_metadata_tags() evaluates.
   *
   * Skips a leading run of `;`/whitespace so that comments written as
   * `;;Key: value` parse the same as `; Key: value` (issue #1786).
   *
   * @param p Raw comment text.
   * @return  The tags in the order they appear.
   */
  static std::vector<parsed_tag_t> scan_metadata_tags(const char* p);

  /**
   * @brief Parse `:tag:` and `Key:`/`Key::` metadata from a comment line.
   *
   * Applies the tags scan_metadata_tags() finds in @p p, in order,
   * marking each as parsed from a comment.  A `Key:: expr` value is
   * evaluated in @p outer_scope bound through @p self_scope.
   *
   * @param p                  Raw comment text.
   * @param outer_scope        Scope used to evaluate `Key:: expr` forms.
   * @param self_scope         Scope to bind through (this object as a scope_t).
   * @param overwrite_existing Whether to overwrite tags that already exist.
   */
  void parse_metadata_tags(const char* p, scope_t& outer_scope, scope_t& self_scope,
                           bool overwrite_existing);
};

/**
 * @brief Dispatch a `has_tag(...)` expression call to a metadata target.
 *
 * Used by item_t::lookup() and account_t::lookup() to translate the
 * expression-engine's argument shape into a call on the resolved
 * metadata_t.  Validates argument counts and types and throws on misuse.
 */
value_t metadata_has_tag(metadata_t& target, call_scope_t& args);

/**
 * @brief Dispatch a `tag(...)` / `meta(...)` expression call.
 *
 * Mirror of metadata_has_tag for value retrieval; returns NULL_VALUE
 * when no matching tag is found.
 */
value_t metadata_get_tag(metadata_t& target, call_scope_t& args);

/**
 * @brief Serialize metadata tags into a property tree for XML/JSON output.
 *
 * Valueless tags are written as `<tag>name</tag>` elements; key/value
 * pairs are written as `<value key="name">...</value>` elements.
 */
void put_metadata(property_tree::ptree& pt, const metadata_t::string_map& metadata);

} // namespace ledger
