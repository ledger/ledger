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
 * @addtogroup util
 */

/**
 * @file   mask.h
 * @author John Wiegley
 *
 * @ingroup util
 *
 * @brief Regular expression wrapper for account/payee/commodity matching.
 *
 * A `mask_t` wraps a compiled regular expression (Boost.Regex or its
 * ICU-backed Unicode variant) and provides case-insensitive matching
 * against strings.  It is the core mechanism behind Ledger's query
 * expressions: when the user writes `ledger reg food`, the argument
 * "food" is compiled into a mask and matched against account names.
 *
 * Key features:
 * - Always case-insensitive (Perl-compatible regex syntax).
 * - Optional diacritics folding: when `--ignore-diacritics` is active,
 *   both the pattern and the text are normalized to ASCII equivalents
 *   before matching, so "cafe" matches "caf\xc3\xa9".
 * - Glob-to-regex conversion via `assign_glob()` for `!include` file
 *   patterns (case-sensitive, since filesystems are).
 * - Unicode-aware when built with `HAVE_BOOST_REGEX_UNICODE` (ICU).
 */
#pragma once

#include "utils.h"
#if HAVE_BOOST_REGEX_UNICODE
#include "unistring.h"
#endif

namespace ledger {

/**
 * @brief Strip diacritics/accents from a UTF-8 string.
 *
 * Converts accented characters to their ASCII base equivalents,
 * e.g. "specialite" matches "sp\xc3\xa9cialit\xc3\xa9".
 * Uses a lookup table for common Latin accented characters.
 */
string fold_diacritics(string_view text);

/**
 * @brief Global flag to enable diacritics-insensitive matching.
 *
 * When set to true, mask_t will strip diacritics from both the
 * pattern and the text before matching.
 */
extern bool ignore_diacritics;

/**
 * @brief A compiled regular expression used for matching account names,
 *        payees, tags, and other journal strings.
 *
 * Patterns are always compiled with `perl | icase` flags for user-facing
 * queries.  The `assign_glob()` method compiles shell-style glob patterns
 * (used by `!include`) with case-sensitive matching instead, since file
 * paths are case-sensitive on most platforms.
 *
 * When `ignore_diacritics` is true, both the pattern and match text are
 * folded to their ASCII base equivalents via `fold_diacritics()` before
 * comparison.
 */
class mask_t {
public:
#if HAVE_BOOST_REGEX_UNICODE
  boost::u32regex expr; ///< Compiled regex (ICU Unicode-aware variant)
#else
  boost::regex expr; ///< Compiled regex (byte-oriented Boost.Regex)
#endif

  /// Construct a mask from a Perl-compatible regex pattern string.
  explicit mask_t(string_view pattern);

  mask_t() : expr() { TRACE_CTOR(mask_t, ""); }
  mask_t(const mask_t& m) : expr(m.expr) { TRACE_CTOR(mask_t, "copy"); }
  mask_t& operator=(const mask_t&) = default;
  ~mask_t() noexcept { TRACE_DTOR(mask_t); }

  /// Assign a new Perl-compatible regex pattern (case-insensitive).
  mask_t& operator=(string_view other);
  /// Assign a shell glob pattern, converting it to a regex.
  /// Unlike operator=, this compiles case-sensitively for filesystem matching.
  mask_t& assign_glob(string_view other);

  bool operator<(const mask_t& other) const { return expr < other.expr; }
  bool operator==(const mask_t& other) const { return expr == other.expr; }

  /// Test whether @p text matches this mask's regex pattern.
  /// Applies diacritics folding when `ignore_diacritics` is active.
  bool match(string_view text) const {
#if HAVE_BOOST_REGEX_UNICODE
    string match_text = ignore_diacritics ? fold_diacritics(text) : string(text);
    DEBUG("mask.match", "Matching: \""
                            << match_text << "\" =~ /" << str() << "/ = "
                            << (boost::u32regex_search(match_text, expr) ? "true" : "false"));
    return boost::u32regex_search(match_text, expr);
#else
    if (ignore_diacritics) {
      string match_text = fold_diacritics(text);
      DEBUG("mask.match",
            "Matching: \"" << match_text << "\" =~ /" << str()
                           << "/ = " << (boost::regex_search(match_text, expr) ? "true" : "false"));
      return boost::regex_search(match_text, expr);
    }
    DEBUG("mask.match", "Matching: \""
                            << text << "\" =~ /" << str() << "/ = "
                            << (boost::regex_search(text.begin(), text.end(), expr) ? "true"
                                                                                    : "false"));
    return boost::regex_search(text.begin(), text.end(), expr);
#endif
  }

  bool empty() const { return expr.empty(); } ///< True if no pattern has been assigned

  /// Return the regex pattern as a UTF-8 string.
  string str() const {
    if (!empty()) {
#if HAVE_BOOST_REGEX_UNICODE
      assert(sizeof(boost::uint32_t) == sizeof(UChar32));
      unistring ustr;
      std::basic_string<UChar32> expr_str = expr.str();
      std::copy(expr_str.begin(), expr_str.end(), std::back_inserter(ustr.utf32chars));
      return ustr.extract();
#else
      return expr.str();
#endif
    } else {
      return empty_string;
    }
  }

  bool valid() const {
    if (expr.status() != 0) {
      DEBUG("ledger.validate", "mask_t: expr.status() != 0");
      return false;
    }
    return true;
  }
};

inline std::ostream& operator<<(std::ostream& out, const mask_t& mask) {
  out << mask.str();
  return out;
}

/// Serialize a mask's pattern string into a Boost property tree node
/// (used by XML output).
inline void put_mask(property_tree::ptree& pt, const mask_t& mask) {
  pt.put_value(mask.str());
}

} // namespace ledger
