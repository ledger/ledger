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
 * @file   op.h
 * @author John Wiegley
 *
 * @ingroup expr
 *
 * @brief AST node type for Ledger's expression engine.
 *
 * Every Ledger expression -- whether it appears in a format string, a
 * `--limit` predicate, or an automated transaction condition -- is parsed
 * into a tree of op_t nodes.  Each node carries a @c kind_t tag that
 * identifies it as a constant, a terminal (function / scope), or an
 * operator, plus a polymorphic @c data payload (via std::variant) that
 * stores the node-specific value.
 *
 * The tree is processed in three phases:
 *   1. **Parse** (parser_t) -- text to AST
 *   2. **Compile** (op_t::compile) -- resolve identifiers, fold constants
 *   3. **Evaluate** (op_t::calc) -- walk the AST to produce a value_t
 *
 * Binary operators use @c left_ for the left operand and the @c ptr_op_t
 * alternative inside @c data for the right operand.  Terminals that have
 * been compiled may stash a resolved definition in @c left_ as well
 * (notably IDENT nodes).
 */
#pragma once

#include <utility>

#include "expr.h"

namespace ledger {

using namespace boost::placeholders;

/**
 * @brief Single node in the expression abstract syntax tree (AST).
 *
 * op_t is reference-counted via Boost intrusive_ptr (acquire/release) and
 * non-copyable.  Nodes are created with new_node() or the wrap_*() helpers
 * and linked into a tree by the parser.  The compile() pass resolves
 * identifiers against a scope_t chain, and calc() performs tree-walking
 * interpretation.
 *
 * @see expr_t      High-level expression wrapper that owns the root op_t.
 * @see scope_t     Symbol lookup environment used during compile and calc.
 * @see value_t     The polymorphic result type produced by calc().
 */
class expr_t::op_t : public noncopyable {
  friend class expr_t;
  friend class expr_t::parser_t;

public:
  using ptr_op_t = expr_t::ptr_op_t;

private:
  mutable short refc; ///< Intrusive reference count for boost::intrusive_ptr.
  ptr_op_t left_;     ///< Left child (operand) or compiled definition for IDENT nodes.

  /// Polymorphic payload -- the active alternative depends on @c kind:
  ///  - monostate: no payload yet (also used to detect "no right child")
  ///  - ptr_op_t:  right child for binary operators, or resolved def for IDENT
  ///  - value_t:   literal constant for VALUE nodes
  ///  - string:    identifier name for IDENT nodes
  ///  - func_t:    native C++ callable for FUNCTION terminals
  ///  - shared_ptr<scope_t>: captured lexical scope for SCOPE terminals
  std::variant<std::monostate,
               ptr_op_t,                // used by all binary operators
               value_t,                 // used by constant VALUE
               string,                  // used by constant IDENT
               expr_t::func_t,          // used by terminal FUNCTION
               std::shared_ptr<scope_t> // used by terminal SCOPE
               >
      data;

public:
  /**
   * @brief Discriminator for the kind of AST node this op_t represents.
   *
   * The enum is partitioned into ranges by sentinel values (CONSTANTS,
   * TERMINALS, UNARY_OPERATORS, BINARY_OPERATORS, OPERATORS).  These
   * sentinels are never used as actual node kinds; they serve as range
   * boundaries for assertions and switch dispatch (e.g., "kind > TERMINALS"
   * means the node is an operator).
   */
  enum kind_t : uint8_t {
    /*--- Constant Terminals ---*/
    PLUG,  ///< Internal sentinel: marks a declared-but-unassigned variable.
    VALUE, ///< Literal constant (integer, amount, string, date, etc.).
    IDENT, ///< Named identifier, resolved at compile or eval time.

    CONSTANTS, ///< Sentinel: end of constant range.

    /*--- Callable / Scope Terminals ---*/
    FUNCTION, ///< Native C++ function (std::function<value_t(call_scope_t&)>).
    SCOPE,    ///< Lexical scope capture wrapping a sub-expression.

    TERMINALS, ///< Sentinel: end of all terminal kinds.

    /*--- Unary Operators ---*/
    O_NOT, ///< Logical negation (`not expr` or `! expr`).
    O_NEG, ///< Arithmetic negation (`- expr`).

    UNARY_OPERATORS, ///< Sentinel: end of unary operator range.

    /*--- Comparison Operators ---*/
    O_EQ,  ///< Equality test (`==`).
    O_LT,  ///< Less than (`<`).
    O_LTE, ///< Less than or equal (`<=`).
    O_GT,  ///< Greater than (`>`).
    O_GTE, ///< Greater than or equal (`>=`).

    /*--- Logical Connectives ---*/
    O_AND, ///< Short-circuit logical AND (`&`).
    O_OR,  ///< Short-circuit logical OR (`|`).

    /*--- Arithmetic Operators ---*/
    O_ADD, ///< Addition (`+`).
    O_SUB, ///< Subtraction (`-`).
    O_MUL, ///< Multiplication (`*`).
    O_DIV, ///< Division (`/`).

    /*--- Ternary Conditional ---*/
    O_QUERY, ///< Ternary condition (`expr ? a : b`); right child is O_COLON.
    O_COLON, ///< Ternary branches holder; never evaluated directly.

    /*--- Structural Operators ---*/
    O_CONS, ///< Comma-separated list constructor (builds a value_t sequence).
    O_SEQ,  ///< Semicolon-separated sequence (keeps only last result).

    /*--- Definition / Invocation ---*/
    O_DEFINE, ///< Variable or function definition (`name = expr`).
    O_LOOKUP, ///< Member access / dot operator (`obj.member`).
    O_LAMBDA, ///< Lambda expression (`params -> body`).
    O_CALL,   ///< Function call (`func(args)`).
    O_MATCH,  ///< Regex match operator (`expr =~ /pattern/`).
    O_EMATCH, ///< Regex extract operator (`expr ==~ /pattern/`).

    BINARY_OPERATORS, ///< Sentinel: end of binary operator range.

    OPERATORS, ///< Sentinel: end of all operator kinds.

    UNKNOWN, ///< Default-constructed, not yet assigned a real kind.

    LAST ///< Past-the-end sentinel for range checks and assertions.
  };

  kind_t kind; ///< Discriminator tag identifying this node's role in the AST.

  explicit op_t() : refc(0), kind(UNKNOWN) { TRACE_CTOR(op_t, ""); }
  explicit op_t(const kind_t _kind) : refc(0), kind(_kind) { TRACE_CTOR(op_t, "const kind_t"); }
  ~op_t() {
    TRACE_DTOR(op_t);
    assert(refc == 0);
  }

  /*--- Value accessors (kind == VALUE) ---*/

  bool is_value() const {
    if (kind == VALUE) {
      assert(std::holds_alternative<value_t>(data));
      return true;
    }
    return false;
  }
  value_t& as_value_lval() {
    assert(is_value());
    value_t& val(std::get<value_t>(data));
    VERIFY(val.valid());
    return val;
  }
  const value_t& as_value() const { return const_cast<op_t*>(this)->as_value_lval(); }
  void set_value(const value_t& val) {
    VERIFY(val.valid());
    data = val;
  }

  /*--- Identifier accessors (kind == IDENT) ---*/

  bool is_ident() const {
    if (kind == IDENT) {
      assert(std::holds_alternative<string>(data));
      return true;
    }
    return false;
  }
  string& as_ident_lval() {
    assert(is_ident());
    return std::get<string>(data);
  }
  const string& as_ident() const { return const_cast<op_t*>(this)->as_ident_lval(); }
  void set_ident(const string& val) { data = val; }

  /*--- Function accessors (kind == FUNCTION) ---*/

  bool is_function() const { return kind == FUNCTION; }
  expr_t::func_t& as_function_lval() {
    assert(is_function());
    return std::get<expr_t::func_t>(data);
  }
  const expr_t::func_t& as_function() const { return const_cast<op_t*>(this)->as_function_lval(); }
  void set_function(const expr_t::func_t& val) { data = val; }

  /*--- Scope accessors (kind == SCOPE) ---*/

  bool is_scope() const { return kind == SCOPE; }
  bool is_scope_unset() const { return data.index() == 0; }
  std::shared_ptr<scope_t> as_scope_lval() {
    assert(is_scope());
    return std::get<std::shared_ptr<scope_t>>(data);
  }
  const std::shared_ptr<scope_t> as_scope() const {
    return const_cast<op_t*>(this)->as_scope_lval();
  }
  void set_scope(const std::shared_ptr<scope_t>& val) { data = val; }

  /*--- Tree structure accessors (left child, right child) ---*/

  // These three functions must use 'kind == IDENT' rather than
  // 'is_ident()', because they are called before the `data' member gets
  // set, which is_ident() tests.

  /// @brief Access the left child of this operator node.
  /// For IDENT nodes, left() holds the compile-time resolved definition.
  /// For operators, left() is the left (or sole) operand.
  ptr_op_t& left() {
    assert(kind > TERMINALS || kind == IDENT || is_scope());
    return left_;
  }
  const ptr_op_t& left() const {
    assert(kind > TERMINALS || kind == IDENT || is_scope());
    return left_;
  }
  void set_left(const ptr_op_t& expr) {
    assert(kind > TERMINALS || kind == IDENT || is_scope());
    left_ = expr;
  }

  /// @brief Access the ptr_op_t stored in the data variant (the right child).
  ptr_op_t& as_op_lval() {
    assert(kind > TERMINALS || is_ident());
    return std::get<ptr_op_t>(data);
  }
  const ptr_op_t& as_op() const { return const_cast<op_t*>(this)->as_op_lval(); }

  /// @brief Access the right child of a binary operator node.
  /// The right operand is stored in the data variant as a ptr_op_t.
  ptr_op_t& right() {
    assert(kind > TERMINALS);
    return as_op_lval();
  }
  const ptr_op_t& right() const {
    assert(kind > TERMINALS);
    return as_op();
  }
  void set_right(const ptr_op_t& expr) {
    assert(kind > TERMINALS);
    data = expr;
  }
  /// @brief Test whether this node has a right child set.
  /// Returns false for terminal nodes; for operators, checks that the
  /// data variant holds a non-null ptr_op_t.
  bool has_right() const {
    if (kind < TERMINALS)
      return false;
    return data.index() != 0 && as_op();
  }

  /*--- Intrusive reference counting ---*/

private:
  void acquire() const {
    DEBUG("op.memory", "Acquiring " << this << ", refc now " << refc + 1);
    assert(refc >= 0);
    refc++;
  }
  void release() const {
    DEBUG("op.memory", "Releasing " << this << ", refc now " << refc - 1);
    assert(refc > 0);
    if (--refc == 0)
      checked_delete(this);
  }

  friend void intrusive_ptr_add_ref(const op_t* op);
  friend void intrusive_ptr_release(const op_t* op);

  /// @brief Create a shallow copy of this node, optionally overriding children.
  /// Terminal payloads (VALUE, IDENT, FUNCTION, SCOPE) are copied by value.
  /// If @p _left or @p _right are null, the originals are preserved.
  ptr_op_t copy(const ptr_op_t& _left = nullptr, const ptr_op_t& _right = nullptr) const {
    ptr_op_t node(new_node(kind, _left, _right));
    if (kind < TERMINALS)
      node->data = data;
    return node;
  }

  /*--- Factory and core operations ---*/

public:
  /// @brief Allocate a new op_t node with the given kind and optional children.
  static ptr_op_t new_node(kind_t _kind, const ptr_op_t& _left = nullptr,
                           const ptr_op_t& _right = nullptr);

  /// @brief Compile this AST node against the given scope.
  ///
  /// Compilation resolves identifiers (IDENT) to their definitions in
  /// @p scope, folds constant sub-expressions, and processes O_DEFINE
  /// and O_LAMBDA nodes.  The result is a (possibly new) op_t tree that
  /// is ready for evaluation.
  ///
  /// @param scope       The symbol scope for identifier resolution.
  /// @param depth       Current recursion depth (for debug indentation).
  /// @param param_scope Optional scope holding function parameter bindings
  ///                    (used when compiling inside an O_LAMBDA body).
  /// @return A compiled op_t tree (may be `this` if no changes were needed).
  ptr_op_t compile(scope_t& scope, const int depth = 0, scope_t* param_scope = nullptr);

  /// @brief Evaluate this AST node to produce a value_t result.
  ///
  /// Walks the tree recursively, dispatching on kind_t.  Arithmetic and
  /// comparison operators evaluate both children and combine the results;
  /// IDENT nodes look up their definition; FUNCTION nodes invoke their
  /// callable.  Short-circuit evaluation applies to O_AND, O_OR, and
  /// O_QUERY.
  ///
  /// @param scope  The runtime scope providing variable/function bindings.
  /// @param locus  If non-null and an exception occurs, set to the failing
  ///               node for error-context reporting.
  /// @param depth  Current recursion depth (capped at 1024).
  /// @return The computed value.
  value_t calc(scope_t& scope, ptr_op_t* locus = nullptr, const int depth = 0);

  /// @brief Invoke this node as a callable with the given arguments.
  ///
  /// Dispatches to the underlying FUNCTION or O_LAMBDA, or resolves the
  /// node to a callable definition first.
  value_t call(const value_t& args, scope_t& scope, ptr_op_t* locus = nullptr, const int depth = 0);

  /*--- Printing and diagnostic output ---*/

  /// @brief Context for the print() method, used to highlight a specific
  /// sub-expression in error messages.  When op_to_find is matched during
  /// printing, start_pos/end_pos are recorded so the caller can underline
  /// the offending sub-expression with carets.
  struct context_t {
    ptr_op_t expr_op;                  ///< Root of the expression being printed.
    ptr_op_t op_to_find;               ///< Node to highlight (may be null).
    std::ostream::pos_type* start_pos; ///< Output position where op_to_find starts.
    std::ostream::pos_type* end_pos;   ///< Output position where op_to_find ends.
    bool relaxed;                      ///< Use relaxed (human-friendly) value formatting.

    context_t() : start_pos(nullptr), end_pos(nullptr), relaxed(false) {}

    context_t(const ptr_op_t& _expr_op, const ptr_op_t& _op_to_find,
              std::ostream::pos_type* const _start_pos = nullptr,
              std::ostream::pos_type* const _end_pos = nullptr, const bool _relaxed = true)
        : expr_op(_expr_op), op_to_find(_op_to_find), start_pos(_start_pos), end_pos(_end_pos),
          relaxed(_relaxed) {}
  };

  /// @brief Pretty-print this expression tree as an infix expression.
  /// @return true if op_to_find was located during printing.
  bool print(std::ostream& out, const context_t& context = context_t()) const;

  /// @brief Dump the AST in a hierarchical debug format (one node per line).
  void dump(std::ostream& out, const int depth = 0) const;

  /*--- Convenience factory helpers ---*/

  /// @brief Wrap a literal value_t as a VALUE node.
  static ptr_op_t wrap_value(const value_t& val);
  /// @brief Wrap a native C++ callable as a FUNCTION node.
  static ptr_op_t wrap_functor(const expr_t::func_t& fobj);
  /// @brief Wrap a scope pointer as a SCOPE node.
  static ptr_op_t wrap_scope(std::shared_ptr<scope_t> sobj);

  /*--- Private calc helpers ---*/

private:
  /// @brief Evaluate an O_CALL node: resolve the callee and invoke it.
  value_t calc_call(scope_t& scope, ptr_op_t* locus, const int depth);
  /// @brief Evaluate an O_CONS node: build a value_t sequence from a comma list.
  value_t calc_cons(scope_t& scope, ptr_op_t* locus, const int depth);
  /// @brief Evaluate an O_SEQ node: execute statements in order, keep last result.
  value_t calc_seq(scope_t& scope, ptr_op_t* locus, const int depth);
};

inline expr_t::ptr_op_t expr_t::op_t::new_node(kind_t _kind, const ptr_op_t& _left,
                                               const ptr_op_t& _right) {
  ptr_op_t node(new op_t(_kind));
  if (_left)
    node->set_left(_left);
  if (_right)
    node->set_right(_right);
  return node;
}

inline expr_t::ptr_op_t expr_t::op_t::wrap_value(const value_t& val) {
  ptr_op_t temp(new op_t(op_t::VALUE));
  temp->set_value(val);
  return temp;
}

inline expr_t::ptr_op_t expr_t::op_t::wrap_functor(const expr_t::func_t& fobj) {
  ptr_op_t temp(new op_t(op_t::FUNCTION));
  temp->set_function(fobj);
  return temp;
}

/// Helper macro: bind a member function to `this` and wrap it as a FUNCTION op_t.
/// Usage: `MAKE_FUNCTOR(my_class_t::my_method)` inside a member of my_class_t.
#define MAKE_FUNCTOR(...) expr_t::op_t::wrap_functor(bind(&__VA_ARGS__, this, _1))

/// Helper macro: wrap an existing std::function or callable as a FUNCTION op_t.
#define WRAP_FUNCTOR(x) expr_t::op_t::wrap_functor(x)

/// @brief Format an expression tree as a string for error context display.
///
/// If @p locus is provided, the sub-expression at that node will be
/// underlined with caret characters in the output.
string op_context(const expr_t::ptr_op_t& op, const expr_t::ptr_op_t& locus = nullptr);

/// @brief Flatten an O_CONS chain into a value_t sequence.
///
/// O_CONS nodes form a right-leaning linked list representing
/// comma-separated argument lists.  This function walks the chain and
/// collects each element into a single value_t sequence, which is
/// needed to pass arguments to function calls.
value_t split_cons_expr(const expr_t::ptr_op_t& op);

} // namespace ledger
