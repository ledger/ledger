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
 * @file   unistring.h
 * @author John Wiegley
 *
 * @ingroup util
 *
 * @brief Unicode-aware string operations for column alignment.
 *
 * Ledger's report formatter needs to align columns correctly even when
 * account names, payees, or commodity symbols contain multi-byte UTF-8
 * characters or East Asian wide characters.  The `unistring` class
 * decodes a UTF-8 string into UTF-32 codepoints and provides:
 *
 * - `length()`: the number of codepoints (not bytes)
 * - `width()`: the display width in terminal columns, accounting for
 *   double-width CJK characters via `mk_wcwidth()`
 * - `extract()` / `extract_by_width()`: substring extraction by
 *   codepoint index or by display-column range
 *
 * The `justify()` free function uses `unistring` to pad or align a
 * string within a given column width, used by format expressions like
 * `%(justify(account, 40))`.
 */
#pragma once

namespace ledger {

/// Return the display width of a Unicode codepoint in terminal columns.
/// Most characters are width 1; East Asian wide characters (CJK) are
/// width 2; zero-width combining marks return 0.  This is a portable
/// replacement for the POSIX `wcwidth()` function.
int mk_wcwidth(boost::uint32_t ucs);

/**
 * @class unistring
 *
 * @brief UTF-32 decoded Unicode string with display-width awareness.
 *
 * The input is a UTF-8 encoded `std::string`, which is decoded to a
 * vector of UTF-32 codepoints on construction.  This allows O(1)
 * codepoint indexing and correct display-width calculation for
 * column alignment in reports.
 *
 * The class is intentionally simple -- it does not support mutation
 * beyond direct access to `utf32chars`.  It is used transiently during
 * report formatting, not as a general-purpose string type.
 */
class unistring {
public:
  inline static constexpr std::size_t npos = static_cast<std::size_t>(-1); ///< "Not found" sentinel

  std::vector<boost::uint32_t> utf32chars; ///< The decoded UTF-32 codepoints

  unistring() { TRACE_CTOR(unistring, ""); }
  unistring(const std::string& input) {
    const char* p = input.c_str();
    std::size_t len = input.length();

    if (!utf8::is_valid(p, p + len))
      throw_(std::runtime_error, _("Invalid UTF-8 sequence in string"));
    utf8::utf8to32(p, p + len, std::back_inserter(utf32chars));

    TRACE_CTOR(unistring, "std::string");
  }
  ~unistring() { TRACE_DTOR(unistring); }

  /// Number of Unicode codepoints (not bytes, not display columns).
  std::size_t length() const { return utf32chars.size(); }

  /// Display width in terminal columns, summing `mk_wcwidth()` for each
  /// codepoint.  East Asian wide characters contribute 2; most others 1.
  std::size_t width() const {
    std::size_t width = 0;
    for (const boost::uint32_t& ch : utf32chars) {
      width += mk_wcwidth(ch);
    }
    return width;
  }

  /// Extract a substring by codepoint index range and re-encode to UTF-8.
  /// @param begin  Starting codepoint index (0-based).
  /// @param len    Number of codepoints to extract (0 = to end).
  std::string extract(const std::string::size_type begin = 0,
                      const std::string::size_type len = 0) const {
    std::string utf8result; // NOLINT(bugprone-unused-local-non-trivial-variable)
    std::string::size_type this_len = length();

    assert(begin <= this_len);
    assert(begin + len <= this_len);

    if (this_len) {
      std::size_t end_idx =
          (len == 0) ? utf32chars.size() : std::min(begin + len, utf32chars.size());
      utf8::utf32to8(utf32chars.begin() + static_cast<std::string::difference_type>(begin),
                     utf32chars.begin() + static_cast<std::string::difference_type>(end_idx),
                     std::back_inserter(utf8result));
    }

    return utf8result;
  }

  /// Extract a substring by display-column range and re-encode to UTF-8.
  /// If a wide character straddles the begin or end boundary, its columns
  /// are replaced with '.' padding to maintain exact width alignment.
  /// @param begin  Starting display column.
  /// @param len    Number of display columns to extract.
  std::string extract_by_width(std::string::size_type begin, std::size_t len) const {
    std::string utf8result;
    std::size_t this_width = width();
    std::string::size_type this_len = length();

    assert(begin <= this_width);
    if (begin + len > this_width)
      len = this_width - begin;

    std::size_t pos = 0;
    std::size_t begin_idx = 0, end_idx = 0;
    std::size_t head = 0, tail = 0;
    for (std::size_t idx = 0; idx < this_len; ++idx) {
      std::size_t w = mk_wcwidth(utf32chars[idx]);

      if (pos < begin) {
        if (pos + w >= begin) {
          head = std::min(pos + w, begin + len) - begin;
          begin_idx = idx + 1;
        }
      } else if (pos < begin + len) {
        if (pos + w > begin + len) {
          tail = begin + len - pos;
          end_idx = idx;
        }
        if (pos + w == begin + len) {
          tail = 0;
          end_idx = idx + 1;
        }
      }
      pos += w;
    }

    utf8result += std::string(head, '.');

    if (begin_idx < end_idx)
      utf8::utf32to8(utf32chars.begin() + static_cast<std::string::difference_type>(begin_idx),
                     utf32chars.begin() + static_cast<std::string::difference_type>(end_idx),
                     std::back_inserter(utf8result));

    utf8result += std::string(tail, '.');

    return utf8result;
  }

  /// Find the first occurrence of codepoint @p _s starting at index @p _pos.
  /// @return Codepoint index, or `npos` if not found.
  std::size_t find(const boost::uint32_t _s, std::size_t _pos = 0) const {
    std::size_t idx = 0;
    for (const boost::uint32_t& ch : utf32chars) {
      if (idx >= _pos && ch == _s)
        return idx;
      idx++;
    }
    return npos;
  }

  boost::uint32_t& operator[](const std::size_t index) { return utf32chars[index]; }
  const boost::uint32_t& operator[](const std::size_t index) const { return utf32chars[index]; }
};

/// Strip ANSI CSI (Control Sequence Introducer) escape sequences from a
/// string so that invisible terminal formatting codes are not counted
/// toward the display width.  The pattern matched is:
///   ESC '[' <parameter bytes 0x30-0x3F>* <intermediate bytes 0x20-0x2F>* <final byte 0x40-0x7E>
inline std::string strip_ansi_escapes(const std::string& input) {
  std::string result;
  result.reserve(input.size());
  for (std::string::size_type i = 0; i < input.size(); ++i) {
    if (input[i] == '\033' && i + 1 < input.size() && input[i + 1] == '[') {
      i += 2; // skip ESC and '['
      while (i < input.size() && input[i] >= 0x30 && input[i] <= 0x3F)
        ++i; // parameter bytes
      while (i < input.size() && input[i] >= 0x20 && input[i] <= 0x2F)
        ++i; // intermediate bytes
      // skip final byte (0x40-0x7E)
    } else {
      result += input[i];
    }
  }
  return result;
}

/// Write @p str to @p out, padded with spaces to fill @p width display
/// columns.  Used by format expressions to align report columns.
/// @param right   If true, right-justify (pad on the left).
/// @param redden  If true, wrap the text in ANSI red escape codes
///               (used for negative amounts).
inline void justify(std::ostream& out, const std::string& str, int width, bool right = false,
                    bool redden = false) {
  if (!right) {
    if (redden)
      out << "\033[31m";
    out << str;
    if (redden)
      out << "\033[0m";
  }

  unistring temp(strip_ansi_escapes(str));

  int spacing = width - int(temp.width());
  while (spacing-- > 0)
    out << ' ';

  if (right) {
    if (redden)
      out << "\033[31m";
    out << str;
    if (redden)
      out << "\033[0m";
  }
}

} // namespace ledger
