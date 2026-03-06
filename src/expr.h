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
 * @file   expr.h
 * @author John Wiegley
 *
 * @ingroup expr
 *
 * @brief User-facing expression class and the merged-expression builder.
 *
 * expr_t is the primary interface to Ledger's expression engine.  It wraps
 * the underlying op_t AST and provides a three-phase workflow:
 *
 *   1. **Parse** -- Convert a text string (e.g., `"amount > 100"`) into an
 *      op_t AST via parser_t.
 *   2. **Compile** -- Resolve identifiers and fold constants by walking the
 *      AST against a scope_t (typically the report_t or session scope).
 *   3. **Evaluate (calc)** -- Walk the compiled AST for each posting or
 *      account to produce a value_t result.
 *
 * Expressions appear throughout Ledger: `--limit` predicates, `--display`
 * filters, format strings (`%(expr)`), automated transaction conditions,
 * and value expressions in the `=` directive.
 *
 * The @c fast_path optimization detects when an expression is a simple
 * identifier like `"amount"` and allows callers (e.g., the posting chain
 * filters) to bypass the full scope/calc machinery and read the posting
 * field directly.
 *
 * merged_expr_t extends expr_t to support incremental expression composition:
 * users can layer flags like `-O`, `-B`, `-V`, and `-A` which each prepend
 * or append transformations to a base expression term.
 */
#pragma once

#include "exprbase.h"
#include "value.h"

#include <memory>

namespace ledger {

class symbol_scope_t; // forward declaration for expr_t::accumulator_scope_

/**
 * @brief High-level expression object: owns an op_t AST and drives the
 *        parse/compile/calc pipeline.
 *
 * expr_t is the type that the rest of Ledger works with.  Report options
 * like `--limit`, `--display`, and format strings all store their logic as
 * an expr_t.  Internally it holds a root op_t (the AST) and delegates the
 * heavy lifting to op_t::compile() and op_t::calc().
 *
 * An optional @c fast_path_ field allows hot-path callers to skip the
 * scope lookup and AST evaluation for trivially simple expressions like
 * the bare identifier `amount`.
 *
 * @see op_t          The AST node that this class wraps.
 * @see expr_base_t   CRTP base providing the compile/calc/print protocol.
 * @see merged_expr_t Derived class for composable multi-flag expressions.
 */
class expr_t : public expr_base_t<value_t> {
  class parser_t;   ///< Recursive-descent parser (defined in parser.h/cc).
  using base_type = expr_base_t<value_t>;

public:
  struct token_t;          ///< Lexer token (defined in token.h).
  class op_t;              ///< AST node (defined in op.h).
  using ptr_op_t = intrusive_ptr<op_t>;          ///< Owning smart pointer to op_t.
  using const_ptr_op_t = intrusive_ptr<const op_t>; ///< Const variant.

  /// @brief Classification of check/assertion expressions from `check` and
  /// `assert` directives in journal files.
  enum check_expr_kind_t : uint8_t { EXPR_GENERAL, EXPR_ASSERTION, EXPR_CHECK };

  using check_expr_pair = std::pair<expr_t, check_expr_kind_t>;
  using check_expr_list = std::list<check_expr_pair>;

  /// @brief Fast-path identifiers for common posting field lookups.
  ///
  /// When the compiled expression is a bare identifier that maps to a
  /// well-known posting field (currently just `amount`), callers in the
  /// posting filter chain can detect this via fast_path() and read the
  /// field directly from the post_t object, avoiding the overhead of
  /// full AST evaluation through the scope chain.  This matters because
  /// `amount` is by far the most common value expression (it is the
  /// default when no `--amount` flag is given).
  enum class fast_path_t : uint8_t {
    NONE = 0,        ///< No fast path; use normal calc().
    POST_AMOUNT,     ///< Expression is "amount" (or "a"); read post_t::amount directly.
  };

protected:
  ptr_op_t ptr;                                ///< Root of the compiled AST (null if unparsed).
  fast_path_t fast_path_ = fast_path_t::NONE;  ///< Detected shortcut, if any.

public:
  expr_t();
  expr_t(const expr_t& other);
  expr_t(ptr_op_t _ptr, scope_t* _context = nullptr);

  expr_t(const string& _str, const parse_flags_t& flags = PARSE_DEFAULT);
  expr_t(std::istream& in, const parse_flags_t& flags = PARSE_DEFAULT);

  ~expr_t() override;

  expr_t& operator=(const expr_t& _expr);

  operator bool() const noexcept override;

  ptr_op_t get_op() noexcept;

  void parse(const string& str, const parse_flags_t& flags = PARSE_DEFAULT) {
    std::istringstream stream(str); // NOLINT(bugprone-unused-local-non-trivial-variable)
    return parse(stream, flags, str);
  }

  void parse(std::istream& in, const parse_flags_t& flags = PARSE_DEFAULT,
             const optional<string>& original_string = none) override;

  /// @brief Compile the parsed AST against @p scope, resolving identifiers
  /// and folding constants.  Also detects fast_path optimizations.
  void compile(scope_t& scope) override;

  /// @brief Evaluate the compiled AST, wrapping @p scope with the persistent
  /// accumulator_scope_ for variable isolation.
  value_t real_calc(scope_t& scope) override;

  /// @brief True if the compiled expression is a single VALUE constant.
  bool is_constant() const;
  value_t& constant_value();
  const value_t& constant_value() const;
  /// @brief True if the compiled expression is a single FUNCTION terminal.
  bool is_function() const;
  func_t& get_function();

  fast_path_t fast_path() const { return fast_path_; }
  void set_fast_path(fast_path_t fp) { fast_path_ = fp; }

  string context_to_str() const override;
  void print(std::ostream& out) const override;
  void dump(std::ostream& out) const override;

protected:
  // Persistent local scope for O_DEFINE variables (accumulator patterns like
  // biggest=max(amount,biggest)).  Keeps variable state isolated from the
  // report/session scope chain, preventing cross-posting pollution.
  // Subclasses (e.g. merged_expr_t) may also use this directly.
  std::unique_ptr<symbol_scope_t> accumulator_scope_;

  // Core calc implementation: evaluates ptr->calc(scope) with error context
  // handling and recursion depth protection, but WITHOUT accumulator wrapping.
  // Callers are responsible for wrapping scope with accumulator_scope_ first.
  value_t calc_with_scope(scope_t& scope);

private:
  /// @brief After compilation, inspect the root op to see if it is a bare
  /// identifier mapping to a known posting field (e.g., "amount").
  void detect_fast_path();
};

/*--- Expression / Value Interop ---*/

/// Expression AST pointers can be stored inside value_t objects (as
/// boost::any).  These helpers test for, extract, and store such embedded
/// expressions.  This is how O_LAMBDA results are passed around as
/// first-class values.

/// @brief Test whether a value_t contains an embedded expression op_t.
inline bool is_expr(const value_t& val) {
  return val.is_any() && val.as_any().type() == typeid(expr_t::ptr_op_t);
}

/// @brief Extract the embedded op_t pointer from a value_t.
expr_t::ptr_op_t as_expr(const value_t& val);
/// @brief Store an op_t pointer inside a value_t (as boost::any).
void set_expr(value_t& val, expr_t::ptr_op_t op);
/// @brief Create a value_t that wraps the given op_t pointer.
value_t expr_value(expr_t::ptr_op_t op);

/*--- Merged (Composable) Expressions ---*/

/**
 * @brief Composable expression that layers transformations onto a base term.
 *
 * merged_expr_t allows one to set an expression term (e.g., `"amount"`) and
 * a base expression (e.g., `"amount"`), then merge in later transformations:
 *
 * @code
 *    term:   "amount"
 *    base:   "amount"
 *    merge:  "amount * 10"    // from -O flag
 *    merge:  "amount + 20"    // from -B flag
 * @endcode
 *
 * When compile() is called, these are assembled into a single expression:
 *
 * @code
 *    __tmp_amount=(amount=(amount); amount=(amount * 10); amount=(amount + 20); amount); __tmp_amount
 * @endcode
 *
 * This allows users to combine flags like `-O`, `-B`, `-V`, and `-A` at any
 * time, with each flag contributing a transformation that is applied in order.
 * The @c merge_operator controls how pieces are joined (`;` for sequential
 * assignment, or an arithmetic operator for inline combination).
 *
 * @see report_t  Where merged_expr_t instances are used for amount_expr and total_expr.
 */
class merged_expr_t : public expr_t {
public:
  string term;               ///< Variable name used as the threading term (e.g., "amount").
  string base_expr;          ///< The initial expression assigned to @c term.
  string merge_operator;     ///< Operator joining merged pieces (";" or "+", "*", etc.).

  std::list<string> exprs;   ///< Accumulated transformation expressions to apply in order.

  merged_expr_t(const string& _term, const string& expr, const string& merge_op = ";");
  // Custom copy constructor: copies the configuration (term, base_expr, etc.)
  // but starts with a fresh, uncompiled expression.  This is correct for
  // push_report() which needs an independent expression context for the new
  // report rather than sharing the original's compiled state.
  merged_expr_t(const merged_expr_t& other);
  merged_expr_t& operator=(const merged_expr_t& other);
  ~merged_expr_t() override;

  void set_term(const string& _term) { term = _term; }
  void set_base_expr(const string& expr) { base_expr = expr; }
  void set_merge_operator(const string& merge_op) { merge_operator = merge_op; }

  /// @brief If @p expr is a bare identifier (no operators), adopt it as
  /// the new base_expr and clear all merged pieces.  Returns true if so.
  bool check_for_single_identifier(const string& expr);

  /// @brief Add a transformation to the front of the merge chain.
  void prepend(const string& expr) {
    if (!check_for_single_identifier(expr))
      exprs.push_front(expr);
  }
  /// @brief Add a transformation to the end of the merge chain.
  void append(const string& expr) {
    if (!check_for_single_identifier(expr))
      exprs.push_back(expr);
  }
  void remove(const string& expr) { exprs.remove(expr); }

  /// @brief Assemble the merged expression string, parse it, and compile
  /// against @p scope with accumulator isolation.
  void compile(scope_t& scope) override;

  /// @brief Evaluate the merged expression with accumulator scope wrapping.
  value_t real_calc(scope_t& scope) override;
};

class call_scope_t;

/// @brief Execute a Ledger script file (a sequence of value expressions,
/// one per line).  Used by the `script` command.
value_t script_command(call_scope_t& scope);

} // namespace ledger
