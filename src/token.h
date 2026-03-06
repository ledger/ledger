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
 * @defgroup expr Value expressions
 */

/**
 * @file   token.h
 * @author John Wiegley
 *
 * @ingroup expr
 *
 * @brief Lexical token type for Ledger's expression tokenizer.
 *
 * When the user writes an expression like `amount > 100` or
 * `payee =~ /grocery/i`, the tokenizer (implemented in token.cc) breaks
 * that character stream into a sequence of typed tokens: an identifier
 * `amount`, an operator `>`, a numeric value `100`, and so on.  This
 * header defines token_t, the struct that carries each token's kind,
 * textual symbol, parsed value, and source length.
 *
 * token_t is a lightweight, non-copyable value object.  The parser
 * (parser_t) keeps a single reusable token_t as its lookahead buffer and
 * calls next() to advance through the input stream.  The kind_t enum
 * covers every token the expression grammar can produce, from literals
 * and identifiers through arithmetic, comparison, and logical operators,
 * to control-flow keywords (`if`/`else`) and the ternary `?`/`:` pair.
 *
 * @see parser_t   The Pratt parser that consumes these tokens.
 * @see op_t       The AST node type the parser builds from tokens.
 * @see expr_t     The user-facing expression wrapper.
 */
#pragma once

#include "expr.h"

namespace ledger {

/**
 * @brief A single lexical token produced by the expression tokenizer.
 *
 * token_t is a mutable, non-copyable struct reused by the parser as a
 * one-token lookahead buffer.  Each call to next() reads characters from
 * the input stream and populates @c kind, @c symbol, @c value, and
 * @c length.  The parser can push the token back (via rewind()) to let
 * the next precedence level re-examine it.
 *
 * Design note: a single token object is reused rather than allocated per
 * token because expression parsing is a hot path during report generation.
 * The assert(false) in operator= enforces this single-instance discipline.
 */
struct expr_t::token_t : public noncopyable {
  /**
   * @brief Enumeration of all token kinds in the expression grammar.
   *
   * The values are grouped by category: error/literal/identifier, grouping
   * delimiters, comparison operators, arithmetic and assignment operators,
   * logical operators, control-flow keywords, ternary operators, and
   * punctuation.  The ordering is cosmetic -- the parser dispatches on
   * these values via switch statements, not by numeric comparison.
   */
  enum kind_t : uint8_t {
    ERROR, ///< Tokenization error; triggers a parse_error exception
    VALUE, ///< Literal value: number, amount, date, string, boolean, or regex
    IDENT, ///< Identifier: alphabetic characters and underscores ([A-Za-z_]+)
    MASK,  ///< Regular expression mask: /regexp/

    LPAREN, ///< Left parenthesis `(`  -- grouping / function call
    RPAREN, ///< Right parenthesis `)` -- closes grouping / call
    LBRACE, ///< Left brace `{`  -- inline amount literal
    RBRACE, ///< Right brace `}` -- closes inline amount

    EQUAL,     ///< Equality `==`
    NEQUAL,    ///< Inequality `!=`
    LESS,      ///< Less than `<`
    LESSEQ,    ///< Less than or equal `<=`
    GREATER,   ///< Greater than `>`
    GREATEREQ, ///< Greater than or equal `>=`

    ASSIGN, ///< Assignment `=`
    MATCH,  ///< Regex match `=~`
    NMATCH, ///< Regex non-match `!~`
    MINUS,  ///< Subtraction or unary negation `-`
    PLUS,   ///< Addition `+`
    STAR,   ///< Multiplication `*`
    SLASH,  ///< Division `/` (in operator context; terminal context reads regex)
    ARROW,  ///< Lambda arrow `->` for anonymous function definitions
    KW_DIV, ///< Integer division keyword `div`

    EXCLAM, ///< Logical NOT `!` or keyword `not`
    KW_AND, ///< Logical AND: `&`, `&&`, or keyword `and`
    KW_OR,  ///< Logical OR: `|`, `||`, or keyword `or`
    KW_MOD, ///< Modulus `%`

    KW_IF,   ///< Conditional keyword `if`
    KW_ELSE, ///< Conditional keyword `else`

    QUERY, ///< Ternary condition `?`
    COLON, ///< Ternary separator `:`

    DOT,   ///< Member access `.` (e.g., `post.amount`)
    COMMA, ///< Argument separator `,`
    SEMI,  ///< Statement separator `;`

    TOK_EOF, ///< End of input stream
    UNKNOWN  ///< Uninitialized / not yet classified

  } kind; ///< The kind of this token, set by next()

  char symbol[6];    ///< Short textual representation of the token for error messages
  value_t value;     ///< Parsed value for VALUE and IDENT tokens
  std::size_t length; ///< Number of characters consumed from the input stream

  explicit token_t() : kind(UNKNOWN), length(0) { TRACE_CTOR(expr_t::token_t, ""); }
  ~token_t() noexcept { TRACE_DTOR(expr_t::token_t); }

  /// @brief Assignment is forbidden at runtime; only one token object exists.
  token_t& operator=(const token_t& other) {
    if (&other == this)
      return *this;
    assert(false); // only one token object is used at a time
    return *this;
  }

  /// @brief Reset the token to its uninitialized state between uses.
  void clear() {
    kind = UNKNOWN;
    length = 0;
    value = NULL_VALUE;
    symbol[0] = '\0';
  }

  /**
   * @brief Try to match the upcoming characters as a reserved word.
   *
   * Checks for keywords: `and`, `div`, `else`, `false`, `if`, `or`,
   * `not`, `true`.  If matched, sets kind/symbol/value accordingly.
   *
   * @param in  The input stream positioned at the start of a word.
   * @return 1 if a reserved word was consumed, 0 if an identifier was
   *         read but was not reserved (caller must rewind), -1 if the
   *         next character cannot start an identifier.
   */
  int parse_reserved_word(std::istream& in);

  /**
   * @brief Read an identifier token ([A-Za-z_]+) from the stream.
   *
   * Sets kind to IDENT and stores the identifier string in value.
   *
   * @param in  The input stream positioned at the first character of
   *            the identifier.
   */
  void parse_ident(std::istream& in);

  /**
   * @brief Read the next token from the input stream.
   *
   * This is the main tokenizer entry point.  It skips whitespace, then
   * classifies the next character(s) into a token kind.  Multi-character
   * tokens (e.g., `==`, `->`, `&&`) are handled by peeking ahead.
   *
   * Special syntactic forms handled:
   *   - `[date]` -- date interval parsed as a VALUE token
   *   - `'string'` / `"string"` -- string literals (double-quoted strings
   *     first attempt amount parsing for commodity amounts like `"$10"`)
   *   - `{amount}` -- braced amount literal (no commodity migration)
   *   - `/regexp/` -- regex mask (only in terminal context, not operator)
   *
   * When none of the above match, the tokenizer attempts amount parsing
   * and falls back to identifier parsing.
   *
   * @param in     The input stream to read from.
   * @param flags  Parse flags controlling behavior (e.g., PARSE_OP_CONTEXT
   *               determines whether `/` is division or regex delimiter).
   */
  void next(std::istream& in, const parse_flags_t& flags);

  /**
   * @brief Push this token back into the stream by seeking backwards.
   *
   * Moves the stream position back by @c length characters so the token
   * can be re-read.  Used by the parser when a precedence level decides
   * the token belongs to the caller.
   *
   * @param in  The input stream to rewind.
   */
  void rewind(std::istream& in);

  /**
   * @brief Report an unexpected token error.
   *
   * Sets the token kind to ERROR and throws a parse_error with a
   * diagnostic message that includes the token symbol or value.
   *
   * @param wanted  If non-null, the character that was expected instead.
   */
  void unexpected(const char wanted = '\0');

  /**
   * @brief Report an expected-character error.
   * @param wanted  The character that was expected (null for generic error).
   * @param c       The character that was actually found (-1 for EOF).
   */
  void expected(const char wanted, const int c);

  /**
   * @brief Report an expected-token-kind error.
   * @param wanted  The token kind that was expected.
   */
  void expected(const kind_t wanted);
};

/// @brief Stream insertion for token kind (for diagnostics and debugging).
std::ostream& operator<<(std::ostream& out, const expr_t::token_t::kind_t& kind);
/// @brief Stream insertion for a complete token (includes value for VALUE/IDENT/MASK).
std::ostream& operator<<(std::ostream& out, const expr_t::token_t& token);

} // namespace ledger
