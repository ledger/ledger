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
 * @file   parser.h
 * @author John Wiegley
 *
 * @ingroup expr
 *
 * @brief Pratt parser (operator-precedence climbing) for Ledger expressions.
 *
 * This header declares parser_t, the component that transforms a stream of
 * tokens (produced by token_t::next()) into an abstract syntax tree of op_t
 * nodes.  The parser uses the classic Pratt / precedence-climbing technique:
 * each precedence level is a separate function that calls the next-tighter
 * level to parse its operands, then loops to consume operators at its own
 * level.
 *
 * The precedence levels from lowest (loosest binding) to highest are:
 *
 * | Level               | Function                | Operators / Constructs       |
 * |---------------------|-------------------------|------------------------------|
 * | Statement sequence  | parse_value_expr        | `;`                          |
 * | Assignment          | parse_assign_expr       | `=`                          |
 * | Lambda              | parse_lambda_expr       | `->`                         |
 * | Comma (cons list)   | parse_comma_expr        | `,`                          |
 * | Ternary / if-else   | parse_querycolon_expr   | `? :`, `if`/`else`           |
 * | Logical OR          | parse_or_expr           | `or`, `\|\|`                 |
 * | Logical AND         | parse_and_expr          | `and`, `&&`                  |
 * | Comparison          | parse_logic_expr        | `==` `!=` `<` `<=` `>` `>=` `=~` `!~` |
 * | Addition            | parse_add_expr          | `+` `-`                      |
 * | Multiplication      | parse_mul_expr          | `*` `/` `div`                |
 * | Unary prefix        | parse_unary_expr        | `!` `-` (negation)           |
 * | Member access       | parse_dot_expr          | `.`                          |
 * | Function call       | parse_call_expr         | `(` args `)`                 |
 * | Primary             | parse_value_term        | literals, identifiers, `(`expr`)` |
 *
 * The parser maintains a single-token lookahead buffer.  When a precedence
 * level sees a token that does not belong to it, it pushes the token back
 * (via push_token / use_lookahead) and returns, letting the calling level
 * handle it.
 *
 * @see token_t   The tokenizer that feeds this parser.
 * @see op_t      The AST node type produced by parsing.
 * @see expr_t    The user-facing expression wrapper that owns the parser.
 */
#pragma once

#include "token.h"
#include "op.h"

namespace ledger {

/**
 * @brief Pratt parser that builds op_t AST trees from expression token streams.
 *
 * parser_t is a non-copyable, stateful parser with a one-token lookahead.
 * It is typically created transiently by expr_t::compile() or parse(), used
 * to parse a single expression string, and then destroyed.
 *
 * The public entry point is parse(), which drives the recursive descent
 * from parse_value_expr down through each precedence level and returns the
 * root of the AST.  On error, it enriches the exception with context
 * showing where in the original expression text the failure occurred.
 */
class expr_t::parser_t : public noncopyable {
  mutable token_t lookahead;  ///< Single-token lookahead buffer
  mutable bool use_lookahead; ///< True if lookahead holds an unconsumed token

  /**
   * @brief Advance to the next token, or reuse the lookahead if pushed back.
   *
   * If use_lookahead is set (meaning a previous level pushed a token back),
   * this simply clears the flag and returns the existing lookahead.  Otherwise
   * it reads the next token from the stream via token_t::next().
   *
   * @param in         The input stream being parsed.
   * @param tflags     Parse flags forwarded to the tokenizer.
   * @param expecting  If provided, assert that the token matches this kind.
   * @return Reference to the lookahead token (always the same object).
   */
  token_t& next_token(std::istream& in, const parse_flags_t& tflags,
                      const optional<token_t::kind_t>& expecting = none) const {
    if (use_lookahead)
      use_lookahead = false;
    else
      lookahead.next(in, tflags);

    if (expecting && lookahead.kind != *expecting)
      lookahead.expected(*expecting);

    return lookahead;
  }

  /**
   * @brief Push a token back so the next call to next_token() returns it again.
   *
   * This is used when a precedence level reads a token that belongs to a
   * lower-precedence (looser-binding) level.  The assert verifies that we
   * are pushing back the same token object (the lookahead buffer).
   */
  void push_token(const token_t& tok) const {
    assert(&tok == &lookahead);
    use_lookahead = true;
  }

  /// @brief Push the current lookahead back without an explicit token reference.
  void push_token() const { use_lookahead = true; }

  /*--- Precedence-level parsing functions (highest to lowest) ---*/

  /// @brief Parse a primary value: literal, identifier, or parenthesized sub-expression.
  ptr_op_t parse_value_term(std::istream& in, const parse_flags_t& flags) const;
  /// @brief Parse function-call syntax: `ident(args)`.
  ptr_op_t parse_call_expr(std::istream& in, const parse_flags_t& flags) const;
  /// @brief Parse member-access (dot) expressions: `object.member`.
  ptr_op_t parse_dot_expr(std::istream& in, const parse_flags_t& flags) const;
  /// @brief Parse unary prefix operators: `!expr` and `-expr`.
  ptr_op_t parse_unary_expr(std::istream& in, const parse_flags_t& flags) const;
  /// @brief Parse multiplicative expressions: `*`, `/`, `div`.
  ptr_op_t parse_mul_expr(std::istream& in, const parse_flags_t& flags) const;
  /// @brief Parse additive expressions: `+`, `-`.
  ptr_op_t parse_add_expr(std::istream& in, const parse_flags_t& flags) const;
  /// @brief Parse comparison/relational expressions: `==`, `!=`, `<`, `<=`, `>`, `>=`, `=~`, `!~`.
  ptr_op_t parse_logic_expr(std::istream& in, const parse_flags_t& flags) const;
  /// @brief Parse logical AND expressions: `and`, `&&`.
  ptr_op_t parse_and_expr(std::istream& in, const parse_flags_t& flags) const;
  /// @brief Parse logical OR expressions: `or`, `||`.
  ptr_op_t parse_or_expr(std::istream& in, const parse_flags_t& flags) const;
  /// @brief Parse ternary (`? :`) and postfix `if`/`else` conditional expressions.
  ptr_op_t parse_querycolon_expr(std::istream& in, const parse_flags_t& flags) const;
  /// @brief Parse comma-separated argument/cons lists.
  ptr_op_t parse_comma_expr(std::istream& in, const parse_flags_t& flags) const;
  /// @brief Parse lambda expressions: `args -> body`.
  ptr_op_t parse_lambda_expr(std::istream& in, const parse_flags_t& flags) const;
  /// @brief Parse assignment expressions: `name = value`.
  ptr_op_t parse_assign_expr(std::istream& in, const parse_flags_t& flags) const;
  /// @brief Parse semicolon-separated statement sequences (lowest precedence).
  ptr_op_t parse_value_expr(std::istream& in, const parse_flags_t& flags) const;

public:
  parser_t() : use_lookahead(false) { TRACE_CTOR(parser_t, ""); }
  ~parser_t() noexcept { TRACE_DTOR(parser_t); }

  /**
   * @brief Parse an expression from the input stream into an op_t AST.
   *
   * This is the main public entry point.  It invokes parse_value_expr (the
   * lowest-precedence level) to parse the complete expression, then cleans
   * up the lookahead state.  On failure, the original expression text is
   * included in the error context for user-friendly diagnostics.
   *
   * @param in               The input stream containing the expression text.
   * @param flags            Parse flags (e.g., PARSE_SINGLE, PARSE_PARTIAL).
   * @param original_string  If provided, the full expression text for error
   *                         context display.
   * @return Root node of the parsed AST, or nullptr if the stream is empty.
   */
  ptr_op_t parse(std::istream& in, const parse_flags_t& flags = PARSE_DEFAULT,
                 const optional<string>& original_string = boost::none);
};

} // namespace ledger
