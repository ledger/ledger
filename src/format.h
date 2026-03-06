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
 * @file   format.h
 * @author John Wiegley
 * @brief  Format string parser and evaluator for report output.
 *
 * @ingroup expr
 *
 * Ledger uses printf-inspired format strings to control how report lines are
 * rendered.  A format string such as `"%-20(account)  %12(total)\n"` is
 * parsed into a linked list of element_t nodes -- some holding literal text,
 * others holding compiled expressions.  When the format is evaluated against
 * a scope (typically bound to a posting or account), each element is rendered
 * in sequence and the results are concatenated.
 *
 * Two syntactic forms exist for expression elements:
 *   - `%(expr)`  -- evaluates the expression and applies min/max width
 *   - `%{expr}`  -- additionally wraps the result in `scrub()` and
 *     `justify()` calls, suitable for monetary amounts
 *
 * Single-letter shortcuts (e.g., `%P` for payee, `%t` for display_amount)
 * are expanded into their full expression equivalents at parse time via
 * the single_letter_mappings table in format.cc.
 *
 * The `%/` separator within a format string splits it into sub-formats:
 * the first-line format, the subsequent-lines format, and an optional
 * between-transaction format.  This is how the `register` command shows
 * the transaction header only once, followed by compact posting lines.
 */
#pragma once

#include "expr.h"
#include "unistring.h"

#include <boost/smart_ptr/scoped_ptr.hpp>

namespace ledger {

class unistring;

/**
 * @brief Exception thrown when a format string cannot be parsed.
 *
 * Raised during parse_elements() for unrecognized `%`-directives or
 * invalid field references.
 */
class format_error : public std::runtime_error {
public:
  explicit format_error(const string& why) noexcept : std::runtime_error(why) {}
  ~format_error() noexcept override {}
};

/**
 * @brief Compiles and evaluates printf-style format strings for report output.
 *
 * format_t inherits from expr_base_t<string> because, conceptually, a format
 * string *is* an expression that produces a string result.  The parse step
 * builds a linked list of element_t nodes (literal strings and compiled
 * expressions).  Evaluation walks this list, rendering each element into a
 * std::ostringstream, applying width/alignment/truncation along the way.
 *
 * Format strings support:
 *   - Literal text (passed through verbatim)
 *   - Backslash escapes (`\n`, `\t`, etc.)
 *   - `%[-][width][.maxwidth](expr)` -- expression with optional formatting
 *   - `%[-][width][.maxwidth]{expr}` -- monetary amount (auto-wrapped in
 *     scrub/justify)
 *   - `%$N` -- back-reference to the Nth expression element from a template
 *   - Single-letter shortcuts (`%d`, `%P`, `%t`, etc.)
 *
 * @see element_t for the internal linked-list node representation.
 * @see real_calc() for the evaluation loop.
 */
class format_t : public expr_base_t<string>, public noncopyable {
  using base_type = expr_base_t<string>;

  /**
   * @brief A single node in the parsed format string linked list.
   *
   * Each element is either a literal STRING or a compiled EXPR.  Elements
   * carry optional min_width and max_width constraints plus an alignment
   * flag.  The linked list is formed via the `next` pointer; ownership
   * follows the scoped_ptr chain.
   */
  struct element_t : public flags::supports_flags<>, public noncopyable {
#define ELEMENT_ALIGN_LEFT 0x01 ///< Left-justify this element within its min_width field.

    enum kind_t : uint8_t {
      STRING, ///< Literal text -- stored as a std::string in `data`.
      EXPR    ///< Compiled expression -- stored as an expr_t in `data`.
    };

    kind_t type;                          ///< Whether this element holds literal text or an expression.
    std::size_t min_width;                ///< Minimum display width (pad with spaces if shorter).
    std::size_t max_width;                ///< Maximum display width (truncate if longer; 0 means unlimited).
    std::variant<string, expr_t> data;    ///< The element payload: literal string or compiled expression.
    scoped_ptr<struct element_t> next;    ///< Next element in the linked list (nullptr for the last).

    element_t() noexcept : supports_flags<>(), type(STRING), min_width(0), max_width(0) {
      TRACE_CTOR(element_t, "");
    }
    ~element_t() noexcept { TRACE_DTOR(element_t); }

    /// @brief Copy the type, flags, widths, and data from another element
    ///        (but not the `next` pointer).
    element_t& operator=(const element_t& elem) {
      if (this != &elem) {
        supports_flags<>::operator=(elem);
        type = elem.type;
        min_width = elem.min_width;
        max_width = elem.max_width;
        data = elem.data;
      }
      return *this;
    }

    /// @brief Emit ANSI red escape and apply alignment/width to the stream.
    ///
    /// Used by the report layer to highlight negative balances in color
    /// terminals.
    friend inline void mark_red(std::ostream& out, const element_t* elem) {
      out.setf(std::ios::left);
      out.width(0);
      out << "\033[31m";

      if (elem->has_flags(ELEMENT_ALIGN_LEFT))
        out << std::left;
      else
        out << std::right;

      if (elem->min_width > 0)
        out.width(static_cast<std::streamsize>(elem->min_width));
    }

    /// @brief Write a human-readable representation of this element for
    ///        debugging.
    void dump(std::ostream& out) const;
  };

  scoped_ptr<element_t> elements; ///< Head of the parsed element linked list.

public:
  /**
   * @brief Controls how overlong strings are shortened to fit max_width.
   *
   * The user can select a truncation style via the `--truncate` option.
   * ABBREVIATE is used automatically for account names when no explicit
   * style has been chosen, producing results like "As:Ban:Chec:Register".
   */
  static enum elision_style_t : uint8_t {
    TRUNCATE_TRAILING, ///< Remove characters from the end, append "..".
    TRUNCATE_MIDDLE,   ///< Remove characters from the middle, insert "..".
    TRUNCATE_LEADING,  ///< Remove characters from the beginning, prepend "..".
    ABBREVIATE         ///< Shorten colon-separated account segments intelligently.
  } default_style;

  static bool default_style_changed; ///< True if the user explicitly set a truncation style.

private:
  /**
   * @brief Parse a format string into a linked list of element_t nodes.
   *
   * This is the core parsing routine.  It scans the format string character
   * by character, building STRING elements for literal text and EXPR elements
   * for `%`-directives.  Single-letter shortcuts are expanded into their full
   * expression equivalents using the single_letter_mappings table.
   *
   * @param fmt   The format string to parse.
   * @param tmpl  An optional "template" format whose expression elements can
   *              be referenced via `%$1`, `%$2`, etc.
   * @return      The head of the newly allocated element linked list.  The
   *              caller takes ownership.
   */
  static element_t* parse_elements(const string& fmt, const optional<format_t&>& tmpl);

public:
  format_t() : base_type() { TRACE_CTOR(format_t, ""); }

  /// @brief Construct and immediately parse a format string.
  /// @param _str     The format string (e.g., `"%-20(account)  %12(total)\n"`).
  /// @param context  Optional scope used for early compilation of expressions.
  format_t(const string& _str, scope_t* context = nullptr) : base_type(context) {
    if (!_str.empty())
      parse_format(_str);
    TRACE_CTOR(format_t, "const string&");
  }
  ~format_t() override { TRACE_DTOR(format_t); }

  /// @brief Parse (or re-parse) a format string, replacing any prior elements.
  /// @param _format  The format string to parse.
  /// @param tmpl     Optional template format for `%$N` back-references.
  void parse_format(const string& _format, const optional<format_t&>& tmpl = none) {
    elements.reset(parse_elements(_format, tmpl));
    set_text(_format);
  }

  /// @brief Reset all EXPR elements to uncompiled state so they will be
  ///        recompiled against a new scope on the next evaluation.
  void mark_uncompiled() override {
    for (element_t* elem = elements.get(); elem; elem = elem->next.get()) {
      if (elem->type == element_t::EXPR) {
        expr_t& expr(std::get<expr_t>(elem->data));
        expr.mark_uncompiled();
      }
    }
  }

  /**
   * @brief Evaluate the format string against a scope, producing a string.
   *
   * This is the main evaluation entry point (called via operator()).  It
   * walks the element linked list, renders each element (compiling and
   * evaluating EXPR elements, copying STRING elements), applies
   * min_width/max_width/alignment, and concatenates everything into the
   * final output string.
   *
   * @param scope  The scope providing variable bindings (typically a
   *               bind_scope_t wrapping a posting or account).
   * @return       The fully rendered output string.
   */
  result_type real_calc(scope_t& scope) override;

  /// @brief Dump all elements for debugging purposes.
  void dump(std::ostream& out) const override {
    for (const element_t* elem = elements.get(); elem; elem = elem->next.get())
      elem->dump(out);
  }

  /**
   * @brief Shorten a Unicode string to fit within the given display width.
   *
   * The truncation strategy is controlled by default_style.  For account
   * names the ABBREVIATE mode intelligently shortens colon-separated
   * segments (e.g., "Assets:Banking:Check:Register" becomes
   * "As:Ban:Chec:Register") to preserve the most informative rightmost
   * segment.
   *
   * @param str                    The Unicode string to truncate.
   * @param width                  The target display width.
   * @param account_abbrev_length  Minimum characters to keep per account
   *                               segment when using ABBREVIATE mode (0
   *                               disables abbreviation).
   * @return  The truncated string, guaranteed to fit within @p width
   *          display columns.
   */
  static string truncate(const unistring& str, const std::size_t width,
                         const std::size_t account_abbrev_length = 0);
};

} // namespace ledger
