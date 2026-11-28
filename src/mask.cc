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

#include <system.hh>

#include "mask.h"

/**
 * @file   mask.cc
 * @author John Wiegley
 *
 * @ingroup util
 *
 * @brief Regular expression mask implementation and diacritics folding.
 *
 * This file implements `mask_t` construction, assignment, and glob-to-regex
 * conversion, as well as the `fold_diacritics()` function that strips
 * accents from UTF-8 text for diacritics-insensitive matching.
 */

namespace ledger {

bool ignore_diacritics = false; ///< Set by `--ignore-diacritics` command-line option

/*--- Diacritics folding ---*/

namespace {

/// Map a single Unicode codepoint to its ASCII base equivalent.
/// Covers Latin-1 Supplement (U+00C0-U+00FF) and Latin Extended-A
/// (U+0100-U+017F), the most common European diacritics.  Ligatures fold
/// to their first letter (AE -> A, OE -> O), eth to D, thorn to T, sharp s
/// and kra to s and k.  Returns the codepoint unchanged if no folding is
/// defined, which the table marks with '\0' (U+00D7, U+00F7, U+0132,
/// U+0133, U+017F).
boost::uint32_t fold_codepoint(boost::uint32_t cp) {
  static constexpr boost::uint32_t first = 0xC0;
  static constexpr char table[] =
      /* U+00C0 */ "AAAAAAACEEEEIIII"
                   /* U+00D0 */ "DNOOOOO\0OUUUUYTs"
                   /* U+00E0 */ "aaaaaaaceeeeiiii"
                   /* U+00F0 */ "dnooooo\0ouuuuyty"
                   /* U+0100 */ "AaAaAaCcCcCcCcDd"
                   /* U+0110 */ "DdEeEeEeEeEeGgGg"
                   /* U+0120 */ "GgGgHhHhIiIiIiIi"
                   /* U+0130 */ "Ii\0\0JjKkkLlLlLlL"
                   /* U+0140 */ "lLlNnNnNnnNnOoOo"
                   /* U+0150 */ "OoOoRrRrRrSsSsSs"
                   /* U+0160 */ "SsTtTtTtUuUuUuUu"
                   /* U+0170 */ "UuUuWwYyYZzZzZz\0";
  static_assert(sizeof(table) == 0x180 - first + 1, "one entry per code point");
  if (cp < first || cp >= first + sizeof(table) - 1)
    return cp;
  const char folded = table[cp - first];
  return folded != 0 ? static_cast<unsigned char>(folded) : cp;
}

} // namespace

/*--- mask_t implementation ---*/

string fold_diacritics(string_view text) {
  const char* p = text.data();
  std::size_t len = text.size();

  if (!utf8::is_valid(p, p + len))
    return string(text);

  std::vector<boost::uint32_t> utf32chars;
  utf8::unchecked::utf8to32(p, p + len, std::back_inserter(utf32chars));

  bool changed = false;
  for (auto& ch : utf32chars) {
    boost::uint32_t folded = fold_codepoint(ch);
    if (folded != ch) {
      ch = folded;
      changed = true;
    }
  }

  if (!changed)
    return string(text);

  string result;
  utf8::unchecked::utf32to8(utf32chars.begin(), utf32chars.end(), std::back_inserter(result));
  return result;
}

mask_t::mask_t(string_view pat) : expr() {
  *this = pat;
  TRACE_CTOR(mask_t, "string_view");
}

mask_t& mask_t::operator=(string_view pat) {
  string folded_pat = ignore_diacritics ? fold_diacritics(pat) : string(pat);
#if HAVE_BOOST_REGEX_UNICODE
  expr = boost::make_u32regex(folded_pat.c_str(), boost::regex::perl | boost::regex::icase);
#else
  expr.assign(folded_pat.c_str(), boost::regex::perl | boost::regex::icase);
#endif
  glob = false;
  VERIFY(valid());
  return *this;
}

/// Convert a shell-style glob pattern to a Perl-compatible regex that
/// matches whole names, and compile it.  Glob metacharacters are
/// translated:
///   `?` -> `.`  ,  `*` -> `.*`  ,  `[...]` -> `[...]`
/// Every other character matches only itself, including one escaped
/// with a backslash and a `[` that no `]` closes, so regex
/// metacharacters among them are escaped.  Unlike operator=, the
/// resulting regex is case-sensitive on every platform, so that an
/// include never picks up a differently-cased file (issue #1660).
mask_t& mask_t::assign_glob(string_view pat) {
  // A literal `]` needs no escape outside a set, and leaving it bare
  // keeps a `]` that closes `[[:digit:]]` working.
  const string_view meta = "\\^$.|?*+()[{}";
  string re_pat = "\\A";
  auto literal = [&](char c) {
    if (meta.find(c) != string_view::npos)
      re_pat += '\\';
    re_pat += c;
  };

  string_view::size_type len = pat.size();
  for (string_view::size_type i = 0; i < len; i++) {
    switch (pat[i]) {
    case '?':
      re_pat += '.';
      break;
    case '*':
      re_pat += ".*";
      break;
    case '[': {
      string_view::size_type close = pat.find(']', i + 1);
      if (close == string_view::npos) {
        literal('[');
      } else {
        re_pat += pat.substr(i, close + 1 - i);
        i = close;
      }
      break;
    }

    case '\\':
      if (i + 1 < len) {
        literal(pat[++i]);
        break;
      }
      [[fallthrough]];
    default:
      literal(pat[i]);
      break;
    }
  }
  re_pat += "\\z";
  // Filename glob matching is case-sensitive on every platform, so that
  // `include FOO.dat` never picks up foo.dat, even on a case-insensitive
  // filesystem such as default macOS APFS (issue #1660).  Unlike
  // operator=, which uses icase for account/payee query matching, a glob
  // names files and must match them exactly.  For the same reason a glob
  // is never folded by --ignore-diacritics: café.dat and cafe.dat are
  // different names.
#if HAVE_BOOST_REGEX_UNICODE
  expr = boost::make_u32regex(re_pat.c_str(), boost::regex::perl);
#else
  expr.assign(re_pat.c_str(), boost::regex::perl);
#endif
  glob = true;
  VERIFY(valid());
  return *this;
}

namespace {

/// Collect every matched substring (whole match + all capture groups) from
/// @p what, representing unmatched optional groups as empty strings so the
/// resulting vector has the same length as the pattern's mark count + 1.
///
/// @p what locates its groups in @p searched, which is either @p text itself
/// or its diacritic-folded copy; each group is returned as the substring of
/// @p text that it names.  When the two have the same content the byte
/// offsets carry over.  Otherwise both are valid UTF-8 and folding mapped
/// one code point to one, so a group's code-point offsets in @p searched
/// name the same characters in @p text.
std::vector<string> collect_matches(const boost::cmatch& what, string_view searched,
                                    string_view text) {
  const bool same = searched == text;
  auto in_text = [&](const char* p) {
    if (same)
      return text.data() + (p - searched.data());
    const char* q = text.data();
    utf8::unchecked::advance(q, utf8::unchecked::distance(searched.data(), p));
    return q;
  };

  std::vector<string> result;
  result.reserve(what.size());
  for (std::size_t i = 0; i < what.size(); ++i) {
    if (what[i].matched)
      result.emplace_back(in_text(what[i].first), in_text(what[i].second));
    else
      result.emplace_back();
  }
  return result;
}

} // namespace

optional<std::vector<string>> mask_t::match_groups(string_view text) const {
  boost::cmatch what;
  if (!folds()) {
    if (search(text, what))
      return collect_matches(what, text, text);
    return std::nullopt;
  }

  const string folded = fold_diacritics(text);
  if (search(folded, what))
    return collect_matches(what, folded, text);
  return std::nullopt;
}

} // namespace ledger
