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
 * @file   exprbase.h
 * @author John Wiegley
 *
 * @ingroup expr
 *
 * @brief CRTP base template providing the common expression protocol
 *        (parse, compile, calc, print, dump) for all Ledger expression types.
 *
 * Ledger uses several domain-specific expression languages, all sharing the
 * same lifecycle: parse a text string, compile against a scope, and evaluate
 * to produce a result.  This template captures that protocol once, so that
 * derived types need only implement real_calc() and optionally override
 * parse(), compile(), print(), and dump().
 *
 * The template is parameterized on @c ResultType -- the value produced by
 * evaluation:
 *
 * | Typename    | Description                | ResultType      | Derives     |
 * |-------------|----------------------------|-----------------|-------------|
 * | expr_t      | Value expressions          | value_t         |             |
 * | predicate_t | Special form of expr_t     | bool            | expr_t      |
 * | query_t     | Report queries             | bool            | predicate_t |
 * | period_t    | Time periods and durations | date_interval_t |             |
 * | draft_t     | Partially filled xacts     | xact_t *        |             |
 * | format_t    | Format strings             | string          |             |
 *
 * Key design decisions:
 *  - **Lazy compilation**: calc() automatically compiles on first use if
 *    the expression has not been compiled yet.
 *  - **Context preservation**: compile() stores the scope pointer for
 *    later no-argument calc() calls, but is careful not to overwrite a
 *    long-lived context set via set_context() with a stack-allocated scope.
 *  - **Preview**: The preview() method shows the full pipeline (input,
 *    parsed text, AST, compiled AST, result) for diagnostic purposes.
 */
#pragma once

#include "utils.h"
#include "amount.h"

namespace ledger {

/*--- Expression Error Types ---*/

/// @brief Thrown when the parser encounters invalid syntax in an expression.
class parse_error : public std::runtime_error {
public:
  explicit parse_error(const string& why) noexcept : std::runtime_error(why) {}
  ~parse_error() noexcept override {}
};

/// @brief Thrown when the compilation pass encounters an invalid construct
/// (e.g., an invalid function definition).
class compile_error : public std::runtime_error {
public:
  explicit compile_error(const string& why) noexcept : std::runtime_error(why) {}
  ~compile_error() noexcept override {}
};

/// @brief Thrown during AST evaluation when a runtime error occurs (unknown
/// identifier, type mismatch, division by zero, etc.).
class calc_error : public std::runtime_error {
public:
  explicit calc_error(const string& why) noexcept : std::runtime_error(why) {}
  ~calc_error() noexcept override {}
};

/// @brief Thrown when expression evaluation or function calls exceed the
/// maximum recursion depth (256 for function calls, 1024 for AST depth).
class recursion_error : public std::runtime_error {
public:
  explicit recursion_error(const string& why) noexcept : std::runtime_error(why) {}
  ~recursion_error() noexcept override {}
};

/// @brief Thrown for command-line usage errors (wrong number of arguments, etc.).
class usage_error : public std::runtime_error {
public:
  explicit usage_error(const string& why) noexcept : std::runtime_error(why) {}
  ~usage_error() noexcept override {}
};

class scope_t;
class call_scope_t;

/**
 * @brief CRTP base template for all Ledger expression types.
 *
 * Provides the common lifecycle methods: parse (text to internal
 * representation), compile (resolve symbols), and calc (evaluate to produce
 * a ResultType).  Derived classes must implement real_calc() and typically
 * override parse() to build their specific internal representation.
 *
 * @tparam ResultType  The value type produced by evaluation.  For expr_t
 *         this is value_t; for format_t it is string; etc.
 */
template <typename ResultType>
class expr_base_t {
public:
  using result_type = ResultType;

  /// @brief Signature for native C++ functions callable from expressions.
  /// The call_scope_t provides access to the arguments and parent scope.
  using func_t = function<result_type(call_scope_t&)>;

protected:
  scope_t* context; ///< Long-lived scope for no-argument calc(); set by compile() or set_context().
  string str;       ///< Original text of the expression (for error messages and reparsing).
  bool compiled;    ///< True once compile() has been called successfully.

  /// @brief Subclass hook: perform the actual evaluation.
  /// Called by calc() after ensuring the expression is compiled.
  virtual result_type real_calc(scope_t& scope) = 0;

public:
  expr_base_t(const expr_base_t& other) : context(other.context), str(other.str), compiled(false) {
    TRACE_CTOR(expr_base_t, "copy");
  }
  expr_base_t(scope_t* _context = nullptr) : context(_context), compiled(false) {
    TRACE_CTOR(expr_base_t, "scope_t *");
  }
  virtual ~expr_base_t() { TRACE_DTOR(expr_base_t); }

  expr_base_t& operator=(const expr_base_t& _expr) {
    if (this != &_expr) {
      str = _expr.str;
      context = _expr.context;
      compiled = _expr.compiled;
    }
    return *this;
  }
  expr_base_t& operator=(const string& _expr) {
    parse(_expr);
    return *this;
  }

  /// @brief Boolean conversion: true if the expression has been parsed
  /// (i.e., has non-empty text).
  virtual operator bool() const noexcept { return !str.empty(); }

  /// @brief Return the original text of the expression.
  virtual string text() const noexcept { return str; }

  /// @brief Set the expression text and mark it as needing recompilation.
  void set_text(const string& txt) {
    str = txt;
    compiled = false;
  }

  /*--- Parsing ---*/

  /// @brief Parse an expression from a string (convenience wrapper).
  void parse(const string& expr_str, const parse_flags_t& flags = PARSE_DEFAULT) {
    std::istringstream stream(expr_str); // NOLINT(bugprone-unused-local-non-trivial-variable)
    return parse(stream, flags, expr_str);
  }
  /// @brief Parse an expression from a stream.  Derived classes override
  /// this to invoke their specific parser (e.g., parser_t for expr_t).
  virtual void parse(std::istream&, const parse_flags_t& = PARSE_DEFAULT,
                     const optional<string>& original_string = none) {
    set_text(original_string ? *original_string : "<stream>");
  }

  /*--- Compilation ---*/

  /// @brief Mark the expression as needing recompilation on next calc().
  virtual void mark_uncompiled() { compiled = false; }

  /// @brief Force recompilation against @p scope, even if already compiled.
  void recompile(scope_t& scope) {
    compiled = false;
    compile(scope);
  }

  /// @brief Compile the expression against @p scope.
  /// Only compiles once (idempotent).  Stores scope as context for later
  /// no-argument calc() calls, but does not overwrite a previously set
  /// context (to avoid dangling pointers to stack-allocated scopes).
  virtual void compile(scope_t& scope) {
    if (!compiled) {
      // Only set context from compile() if it hasn't been explicitly
      // set (e.g. via set_context()).  Many callers pass a
      // stack-allocated bind_scope_t as 'scope'; storing that pointer
      // would create a dangling reference once the caller returns.
      // Contexts set via set_context() point to long-lived scopes
      // (such as report_t) and must not be overwritten.
      if (!context)
        context = &scope;
      compiled = true;
    }
  }

  /*--- Evaluation ---*/

  /// @brief Functor-style evaluation (delegates to calc).
  result_type operator()(scope_t& scope) { return calc(scope); }

  /// @brief Evaluate the expression against @p scope.
  ///
  /// If the expression has not yet been compiled, it is compiled first
  /// (lazy compilation).  Then real_calc() is called to perform the
  /// actual evaluation.  This is the primary entry point for per-posting
  /// evaluation in the reporting pipeline.
  result_type calc(scope_t& scope) {
    if (!compiled) {
      // Save context before compile: the scope argument is often a
      // stack-allocated bind_scope_t that will be destroyed when the
      // caller returns.  compile() (or a derived override) may store
      // &scope into context, creating a dangling pointer.  By
      // restoring the saved context afterwards we ensure that any
      // long-lived context set via set_context() is preserved.
      scope_t* saved_context = context;

#if DEBUG_ON
      if (SHOW_DEBUG("expr.compile")) {
        DEBUG("expr.compile", "Before compilation:");
        dump(*_log_stream);
      }
#endif // DEBUG_ON

      DEBUG("expr.compile", "Compiling: " << str);
      compile(scope);

#if DEBUG_ON
      if (SHOW_DEBUG("expr.compile")) {
        DEBUG("expr.compile", "After compilation:");
        dump(*_log_stream);
      }
#endif // DEBUG_ON

      if (saved_context)
        context = saved_context;
    }

    DEBUG("expr.calc", "Calculating: " << str);
    return real_calc(scope);
  }

  /// @brief Evaluate using the stored context scope (set by compile or set_context).
  result_type calc() {
    assert(context);
    return calc(*context);
  }

  /*--- Context management ---*/

  /// @brief Get the stored scope context (may be null before compile).
  scope_t* get_context() { return context; }
  /// @brief Explicitly set the long-lived scope for no-argument calc().
  void set_context(scope_t* scope) { context = scope; }

  /*--- Output ---*/

  /// @brief Return a human-readable representation of the expression's
  /// current state, for use in error messages.
  virtual string context_to_str() const { return empty_string; }

  /// @brief Convenience: print the expression to a string.
  string print_to_str() const {
    std::ostringstream out;
    print(out);
    return out.str();
  }
  /// @brief Convenience: dump the AST to a string.
  string dump_to_str() const {
    std::ostringstream out;
    dump(out);
    return out.str();
  }
  string preview_to_str(scope_t&) const {
    std::ostringstream out;
    preview(out);
    return out.str();
  }

  /// @brief Pretty-print the expression in infix form (overridden by derived classes).
  virtual void print(std::ostream&) const {}
  /// @brief Dump the internal AST for debugging (overridden by derived classes).
  virtual void dump(std::ostream&) const {}

  /// @brief Diagnostic method: show the full pipeline from input text to
  /// computed result.  Useful with `ledger expr 'some expression'` to
  /// inspect how an expression is parsed, compiled, and evaluated.
  result_type preview(std::ostream& out, scope_t& scope) const {
    out << _("--- Input expression ---") << '\n';
    out << text() << '\n';

    out << '\n' << _("--- Text as parsed ---") << '\n';
    print(out);
    out << '\n';

    out << '\n' << _("--- Expression tree ---") << '\n';
    dump(out);

    out << '\n' << _("--- Compiled tree ---") << '\n';
    compile(scope);
    dump(out);

    out << '\n' << _("--- Result value ---") << '\n';
    return calc();
  }
};

/// @brief Stream insertion operator: prints the expression in infix form.
template <typename ResultType>
std::ostream& operator<<(std::ostream& out, const expr_base_t<ResultType>& expr) {
  expr.print(out);
  return out;
}

} // namespace ledger
