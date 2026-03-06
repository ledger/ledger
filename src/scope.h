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
 * @file   scope.h
 * @author John Wiegley
 * @brief  Scope hierarchy for expression variable and function resolution.
 *
 * @ingroup expr
 *
 * The expression engine evaluates identifiers like `amount`, `total`,
 * `account`, and `payee` by looking them up through a chain of scope
 * objects.  Each scope in the chain represents a different level of the
 * ledger data model:
 *
 * @verbatim
 *   post_t  -->  xact_t  -->  item_t  -->  session_t  -->  global_scope_t
 *   (posting)   (transaction) (common)     (journal/options) (built-in functions)
 * @endverbatim
 *
 * When the expression engine encounters an IDENT node (e.g., `amount`),
 * it calls scope_t::lookup() on the innermost scope.  If that scope does
 * not recognize the name, it delegates to its parent via child_scope_t,
 * walking up the chain until the symbol is found or the chain is exhausted.
 *
 * This design allows each data-model type (post_t, xact_t, account_t) to
 * expose its own fields as expression variables without the expression
 * engine needing to know the concrete types.  The virtual lookup() method
 * is the key extension point: each scope subclass overrides it to provide
 * its own symbols.
 *
 * Additional scope types provide specialized behaviors:
 * - **bind_scope_t**: Joins two scopes (parent + grandchild), looking up
 *   in the grandchild first, then the parent.  Used when evaluating
 *   expressions in a context that combines two scopes (e.g., a posting
 *   within a format string).
 * - **lexical_scope_t**: Like bind_scope_t but definitions only go to
 *   the grandchild, preventing local variables from leaking upward.
 * - **symbol_scope_t**: Maintains a local symbol table for user-defined
 *   variables and functions.
 * - **call_scope_t**: Provides function call arguments and lazy evaluation.
 * - **context_scope_t**: Carries type context for coercion hints.
 * - **value_scope_t**: Wraps a single value_t for simple lookups.
 */
#pragma once

#include <utility>

#include "op.h"

namespace ledger {

/**
 * @brief An entry in a symbol table: a named, typed definition.
 *
 * Symbols can represent functions, options, commands, directives, or
 * format strings.  They are stored in symbol_scope_t's sorted map and
 * compared by (kind, name) for lookup.
 */
struct symbol_t {
  enum kind_t : uint8_t {
    UNKNOWN,    ///< Uninitialized sentinel.
    FUNCTION,   ///< A callable function (most expression identifiers).
    OPTION,     ///< A command-line option handler.
    PRECOMMAND, ///< A pre-command (executed before journal parsing).
    COMMAND,    ///< A report command (balance, register, print, etc.).
    DIRECTIVE,  ///< A journal directive handler.
    FORMAT      ///< A named format string.
  };

  kind_t kind;                 ///< The category of this symbol.
  string name;                 ///< The symbol name (e.g., "amount", "balance").
  expr_t::ptr_op_t definition; ///< The expression tree implementing this symbol.

  symbol_t() : kind(UNKNOWN), name(""), definition(nullptr) { TRACE_CTOR(symbol_t, ""); }
  symbol_t(kind_t _kind, string _name, expr_t::ptr_op_t _definition = nullptr)
      : kind(_kind), name(std::move(_name)), definition(std::move(_definition)) {
    TRACE_CTOR(symbol_t, "symbol_t::kind_t, string");
  }
  symbol_t(const symbol_t& sym) : kind(sym.kind), name(sym.name), definition(sym.definition) {
    TRACE_CTOR(symbol_t, "copy");
  }
  ~symbol_t() noexcept { TRACE_DTOR(symbol_t); }

  bool operator<(const symbol_t& sym) const { return kind < sym.kind || name < sym.name; }
  bool operator==(const symbol_t& sym) const { return kind == sym.kind || name == sym.name; }
};

class empty_scope_t;

/**
 * @brief Abstract base class for all scopes in the expression evaluation chain.
 *
 * Every scope must implement lookup() to resolve symbol names into expression
 * operations.  The default define() is a no-op (most scopes delegate to a
 * parent or a symbol_scope_t).
 *
 * The two static members provide global access points:
 * - default_scope: the session-level scope used when no explicit scope is given.
 * - empty_scope: a scope that resolves nothing (used as a safe fallback).
 */
class scope_t {
public:
  static scope_t* default_scope;     ///< The global default scope (typically the active session).
  static empty_scope_t* empty_scope; ///< A no-op scope that resolves no symbols.

  explicit scope_t() { TRACE_CTOR(scope_t, ""); }
  virtual ~scope_t() { TRACE_DTOR(scope_t); }

  /// @brief Return a human-readable description of this scope (for debugging).
  virtual string description() = 0;

  /// @brief Define a symbol in this scope.  Default implementation is a no-op.
  virtual void define(const symbol_t::kind_t, const string&, const expr_t::ptr_op_t&) {}

  /**
   * @brief Resolve a symbol name to an expression operation.
   *
   * This is the core extension point.  Each concrete scope (post_t, xact_t,
   * session_t, etc.) overrides this to provide its own variables and
   * functions.  If the symbol is not found, the scope should either
   * delegate to a parent or return nullptr.
   *
   * @param kind The category of symbol to look up (FUNCTION, OPTION, etc.).
   * @param name The symbol name (e.g., "amount", "payee", "total").
   * @return The expression operation for the symbol, or nullptr if not found.
   */
  virtual expr_t::ptr_op_t lookup(const symbol_t::kind_t kind, const string& name) = 0;

  /// @brief Return the parent scope, or nullptr if this is a root scope.
  virtual scope_t* get_parent() { return nullptr; }

  /// @brief Return the expected value type for expression evaluation in this context.
  virtual value_t::type_t type_context() const { return value_t::VOID; }
  /// @brief Whether the type_context is a hard requirement (true) or a hint (false).
  virtual bool type_required() const { return false; }
};

/**
 * @brief A scope that resolves nothing -- used as a safe null-object fallback.
 */
class empty_scope_t : public scope_t {
public:
  empty_scope_t() { TRACE_CTOR(empty_scope_t, ""); }
  ~empty_scope_t() noexcept override { TRACE_DTOR(empty_scope_t); }

  string description() override { return _("<empty>"); }
  expr_t::ptr_op_t lookup(const symbol_t::kind_t, const string&) override { return nullptr; }
};

/**
 * @brief A scope that delegates both define() and lookup() to a parent scope.
 *
 * This is the base class for most scope types in the hierarchy.  It
 * implements the chain-of-responsibility pattern: if a lookup is not
 * handled by a derived class, it automatically propagates to the parent.
 *
 * Most data-model types (post_t, xact_t, account_t) inherit from
 * child_scope_t (via item_t) and override lookup() to provide their
 * own fields while delegating unknown names to the parent.
 */
class child_scope_t : public noncopyable, public scope_t {
public:
  scope_t* parent; ///< The parent scope to delegate to, or nullptr if detached.

  explicit child_scope_t() : parent(nullptr) { TRACE_CTOR(child_scope_t, ""); }
  explicit child_scope_t(scope_t& _parent) : parent(&_parent) {
    TRACE_CTOR(child_scope_t, "scope_t&");
  }
  ~child_scope_t() override { TRACE_DTOR(child_scope_t); }

  scope_t* get_parent() override { return parent; }

  /// @brief Delegate symbol definition to the parent scope.
  void define(const symbol_t::kind_t kind, const string& name,
              const expr_t::ptr_op_t& def) override {
    if (parent)
      parent->define(kind, name, def);
  }

  /// @brief Delegate symbol lookup to the parent scope.
  expr_t::ptr_op_t lookup(const symbol_t::kind_t kind, const string& name) override {
    if (parent)
      return parent->lookup(kind, name);
    return nullptr;
  }
};

/**
 * @brief Joins two scopes: lookups check the grandchild first, then the parent.
 *
 * bind_scope_t is used when evaluating an expression that needs access to
 * two independent scope chains.  For example, when formatting a posting
 * in a report, the format expression needs both the posting's fields
 * (grandchild) and the report-level options (parent).
 *
 * Lookups try the grandchild scope first; if not found, they fall through
 * to the parent via child_scope_t::lookup().  Definitions propagate to
 * both scopes.
 */
class bind_scope_t : public child_scope_t {
  bind_scope_t();

public:
  scope_t& grandchild; ///< The inner scope, searched first during lookup.

  explicit bind_scope_t(scope_t& _parent, scope_t& _grandchild)
      : child_scope_t(_parent), grandchild(_grandchild) {
    DEBUG("scope.symbols", "Binding scope " << &_parent << " with " << &_grandchild);
    TRACE_CTOR(bind_scope_t, "scope_t&, scope_t&");
  }
  ~bind_scope_t() override { TRACE_DTOR(bind_scope_t); }

  string description() override { return grandchild.description(); }

  /// @brief Propagate definitions to both parent and grandchild scopes.
  void define(const symbol_t::kind_t kind, const string& name,
              const expr_t::ptr_op_t& def) override {
    parent->define(kind, name, def);
    grandchild.define(kind, name, def);
  }

  /// @brief Look up in grandchild first, then fall through to parent.
  expr_t::ptr_op_t lookup(const symbol_t::kind_t kind, const string& name) override {
    if (expr_t::ptr_op_t def = grandchild.lookup(kind, name))
      return def;
    return child_scope_t::lookup(kind, name);
  }
};

/**
 * @brief Like bind_scope_t, but definitions only go to the grandchild (local scope).
 *
 * Used for SCOPE node compilation so that local variable definitions
 * (O_DEFINE) don't leak into enclosing scopes.  Lookups still check
 * grandchild first, then parent (inherited from bind_scope_t).
 */
class lexical_scope_t : public bind_scope_t {
  lexical_scope_t();

public:
  explicit lexical_scope_t(scope_t& _parent, scope_t& _grandchild)
      : bind_scope_t(_parent, _grandchild) {
    TRACE_CTOR(lexical_scope_t, "scope_t&, scope_t&");
  }
  ~lexical_scope_t() override { TRACE_DTOR(lexical_scope_t); }

  /// @brief Define only in the grandchild scope (prevents leaking to parent).
  void define(const symbol_t::kind_t kind, const string& name,
              const expr_t::ptr_op_t& def) override {
    grandchild.define(kind, name, def);
  }
};

/**
 * @brief Walk the scope chain to find a scope of a specific type.
 *
 * This template function performs a depth-first search through the scope
 * hierarchy using dynamic_cast.  For bind_scope_t nodes (which have two
 * children), the search order depends on @p prefer_direct_parents:
 * - false (default): check grandchild first, then parent
 * - true: check parent first, then grandchild
 *
 * @tparam T The concrete scope type to search for (e.g., report_t, session_t).
 * @param ptr The starting scope.
 * @param prefer_direct_parents If true, prioritize parent over grandchild at bind nodes.
 * @return A pointer to the found scope, or nullptr if not found.
 */
template <typename T>
T* search_scope(scope_t* ptr, bool prefer_direct_parents = false) {
  DEBUG("scope.search", "Searching scope " << ptr->description());

  if (T* sought = dynamic_cast<T*>(ptr))
    return sought;

  if (bind_scope_t* scope = dynamic_cast<bind_scope_t*>(ptr)) {
    if (T* sought = search_scope<T>(prefer_direct_parents ? scope->parent : &scope->grandchild))
      return sought;
    return search_scope<T>(prefer_direct_parents ? &scope->grandchild : scope->parent);
  } else if (scope_t* parent = ptr->get_parent()) {
    return search_scope<T>(parent);
  }
  return nullptr;
}

/**
 * @brief Find a scope of type T in the chain, throwing if not found.
 * @param skip_this If true (default), start searching from scope.parent.
 */
template <typename T>
inline T& find_scope(child_scope_t& scope, bool skip_this = true,
                     bool prefer_direct_parents = false) {
  if (T* sought = search_scope<T>(skip_this ? scope.parent : &scope, prefer_direct_parents))
    return *sought;

  throw_(std::runtime_error, _("Could not find scope"));
  return reinterpret_cast<T&>(scope); // never executed
}

/// @brief Overload of find_scope for a bare scope_t reference.
template <typename T>
inline T& find_scope(scope_t& scope, bool prefer_direct_parents = false) {
  if (T* sought = search_scope<T>(&scope, prefer_direct_parents))
    return *sought;

  throw_(std::runtime_error, _("Could not find scope"));
  return reinterpret_cast<T&>(scope); // never executed
}

/**
 * @brief A scope with a local symbol table for user-defined variables and functions.
 *
 * symbol_scope_t maintains an optional sorted map of symbols.  When define()
 * is called, the symbol is stored locally.  When lookup() is called, the
 * local table is checked first; if the symbol is not found, the lookup
 * delegates to the parent via child_scope_t.
 *
 * This scope type is used by session_t and report_t to store option
 * handlers, commands, and user-defined functions.
 */
class symbol_scope_t : public child_scope_t {
  using symbol_map = std::map<symbol_t, expr_t::ptr_op_t>;

  optional<symbol_map> symbols; ///< Lazily initialized local symbol table.

public:
  explicit symbol_scope_t() : child_scope_t() { TRACE_CTOR(symbol_scope_t, ""); }
  explicit symbol_scope_t(scope_t& _parent) : child_scope_t(_parent) {
    TRACE_CTOR(symbol_scope_t, "scope_t&");
  }
  ~symbol_scope_t() override { TRACE_DTOR(symbol_scope_t); }

  string description() override {
    if (parent)
      return parent->description();
    return _("<scope>");
  }

  /// @brief Define a symbol in the local table (replaces existing definition if present).
  void define(const symbol_t::kind_t kind, const string& name,
              const expr_t::ptr_op_t& def) override;

  /// @brief Look up in local table first, then delegate to parent.
  expr_t::ptr_op_t lookup(const symbol_t::kind_t kind, const string& name) override;
};

/**
 * @brief A scope that carries type-context hints for expression evaluation.
 *
 * When the expression engine needs to coerce a result to a specific type
 * (e.g., a format expects a string, or a comparison expects an amount),
 * a context_scope_t wraps the parent and advertises the desired type.
 */
class context_scope_t : public child_scope_t {
  value_t::type_t value_type_context; ///< The expected result type.
  bool required;                      ///< Whether the type must match exactly.

public:
  explicit context_scope_t(scope_t& _parent, value_t::type_t _type_context = value_t::VOID,
                           const bool _required = true)
      : child_scope_t(_parent), value_type_context(_type_context), required(_required) {
    TRACE_CTOR(context_scope_t, "scope_t&, value_t::type_t, bool");
  }
  ~context_scope_t() override { TRACE_DTOR(context_scope_t); }

  string description() override { return parent->description(); }

  value_t::type_t type_context() const override { return value_type_context; }
  bool type_required() const override { return required; }
};

/**
 * @brief Scope for function/method calls, providing arguments and lazy evaluation.
 *
 * When the expression engine encounters a function call (O_CALL), it creates
 * a call_scope_t that holds the function arguments.  Arguments may be
 * unevaluated expression trees (stored as value_t::ANY holding a ptr_op_t);
 * resolve() lazily evaluates them on first access, coercing to the expected
 * type.
 *
 * The context<T>() method provides access to a specific scope type in the
 * chain, caching the result in @c ptr for efficient repeated access.  This
 * is how built-in functions reach the report_t or session_t they need.
 */
class call_scope_t : public context_scope_t {
public:
  value_t args;      ///< The function arguments (may contain lazy expr_t::ptr_op_t values).
  mutable void* ptr; ///< Cached pointer to a context scope found via context<T>().

  /**
   * @brief Lazily evaluate argument at @p index, coercing to @p context type.
   * @param index Zero-based argument index.
   * @param context The expected value type for coercion.
   * @param required If true, throw if the result does not match the context type.
   * @return Reference to the (now resolved) argument value.
   */
  value_t& resolve(const std::size_t index, value_t::type_t context = value_t::VOID,
                   const bool required = false);

public:
  expr_t::ptr_op_t* locus; ///< Points to the call-site op node (for error reporting).
  const int depth;         ///< Expression evaluation depth (for recursion tracking).

  explicit call_scope_t(scope_t& _parent, expr_t::ptr_op_t* _locus = nullptr, const int _depth = 0)
      : context_scope_t(_parent, _parent.type_context(), _parent.type_required()), ptr(nullptr),
        locus(_locus), depth(_depth) {
    TRACE_CTOR(call_scope_t, "scope_t&, expr_t::ptr_op_t *, const int");
  }
  ~call_scope_t() override { TRACE_DTOR(call_scope_t); }

  void set_args(const value_t& _args) { args = _args; }
  value_t& value() {
    // Make sure that all of the arguments have been resolved.
    for (std::size_t index = 0; index < args.size(); index++)
      resolve(index);
    return args;
  }

  value_t& operator[](const std::size_t index) { return resolve(index); }
#if 0
  const value_t& operator[](const std::size_t index) const {
    return args[index];
  }
#endif

  bool has(std::size_t index) { return index < args.size() && !(*this)[index].is_null(); }
  template <typename T>
  bool has(std::size_t index);
  template <typename T>
  T get(std::size_t index, bool convert = true);

  /**
   * @brief Find and cache a scope of type T in the scope chain.
   *
   * On first call, walks the scope chain via find_scope<T>.  The result
   * is cached in @c ptr so subsequent calls are O(1).  This is the
   * primary way built-in functions access report_t or session_t.
   */
  template <typename T>
  T& context() {
    if (ptr == nullptr)
      ptr = &find_scope<T>(*this);
    assert(ptr != nullptr);
    return *static_cast<T*>(ptr);
  }

  void push_front(const value_t& val) { args.push_front(val); }
  void push_back(const value_t& val) { args.push_back(val); }
  void pop_back() { args.pop_back(); }

  using iterator = value_t::sequence_t::iterator;

  value_t::sequence_t::iterator begin() { return args.begin(); }
  value_t::sequence_t::iterator end() { return args.end(); }

  using const_iterator = value_t::sequence_t::const_iterator;

  value_t::sequence_t::const_iterator begin() const { return args.begin(); }
  value_t::sequence_t::const_iterator end() const { return args.end(); }

  std::size_t size() const { return args.size(); }
  bool empty() const { return args.size() == 0; }
};

/**
 * @brief Check whether argument @p index exists and has a non-null value of type T.
 *
 * Resolves the argument with the appropriate type context, but does not
 * require it (the argument may be missing or null).
 */
template <typename T>
inline bool call_scope_t::has(std::size_t index) {
  if (index < args.size()) {
    if constexpr (std::is_same_v<T, bool>)
      resolve(index, value_t::BOOLEAN, false);
    else if constexpr (std::is_same_v<T, int> || std::is_same_v<T, long>)
      resolve(index, value_t::INTEGER, false);
    else if constexpr (std::is_same_v<T, amount_t>)
      resolve(index, value_t::AMOUNT, false);
    else if constexpr (std::is_same_v<T, balance_t>)
      resolve(index, value_t::BALANCE, false);
    else if constexpr (std::is_same_v<T, string>)
      resolve(index, value_t::STRING, false);
    else if constexpr (std::is_same_v<T, date_t>)
      resolve(index, value_t::DATE, false);
    else if constexpr (std::is_same_v<T, datetime_t>)
      resolve(index, value_t::DATETIME, false);
    else if constexpr (std::is_same_v<T, scope_t*>)
      resolve(index, value_t::SCOPE, false);
    else if constexpr (std::is_same_v<T, expr_t::ptr_op_t>)
      resolve(index, value_t::ANY, false);
    return !args[index].is_null();
  }
  return false;
}

/**
 * @brief Retrieve argument @p index as type T, optionally converting.
 *
 * When @p convert is true (default), uses to_*() conversion methods which
 * attempt coercion.  When false, uses as_*() accessors which require an
 * exact type match.
 */
template <typename T>
inline T call_scope_t::get(std::size_t index, bool convert) {
  if constexpr (std::is_same_v<T, bool>) {
    if (convert)
      return resolve(index, value_t::BOOLEAN, false).to_boolean();
    return resolve(index, value_t::BOOLEAN).as_boolean();
  } else if constexpr (std::is_same_v<T, int>) {
    return resolve(index, value_t::INTEGER, false).to_int();
  } else if constexpr (std::is_same_v<T, long>) {
    if (convert)
      return resolve(index, value_t::INTEGER, false).to_long();
    return resolve(index, value_t::INTEGER).as_long();
  } else if constexpr (std::is_same_v<T, amount_t>) {
    if (convert)
      return resolve(index, value_t::AMOUNT, false).to_amount();
    return resolve(index, value_t::AMOUNT).as_amount();
  } else if constexpr (std::is_same_v<T, balance_t>) {
    if (convert)
      return resolve(index, value_t::BALANCE, false).to_balance();
    return resolve(index, value_t::BALANCE).as_balance();
  } else if constexpr (std::is_same_v<T, string>) {
    if (convert)
      return resolve(index, value_t::STRING, false).to_string();
    return resolve(index, value_t::STRING).as_string();
  } else if constexpr (std::is_same_v<T, mask_t>) {
    if (convert)
      return resolve(index, value_t::MASK, false).to_mask();
    return resolve(index, value_t::MASK).as_mask();
  } else if constexpr (std::is_same_v<T, date_t>) {
    if (convert)
      return resolve(index, value_t::DATE, false).to_date();
    return resolve(index, value_t::DATE).as_date();
  } else if constexpr (std::is_same_v<T, datetime_t>) {
    if (convert)
      return resolve(index, value_t::DATETIME, false).to_datetime();
    return resolve(index, value_t::DATETIME).as_datetime();
  } else if constexpr (std::is_same_v<T, scope_t*>) {
    return resolve(index, value_t::SCOPE).as_scope();
  } else if constexpr (std::is_same_v<T, expr_t::ptr_op_t>) {
    return args[index].as_any<expr_t::ptr_op_t>();
  }
}

/// @brief Concatenate all call arguments into a single space-separated string.
inline string join_args(call_scope_t& args) {
  std::ostringstream buf;
  bool first = true;

  for (std::size_t i = 0; i < args.size(); i++) {
    if (first)
      first = false;
    else
      buf << ' ';
    buf << args[i];
  }

  return buf.str();
}

/**
 * @brief A minimal scope that provides a single "value" identifier.
 *
 * Used when an expression needs to operate on a specific value_t
 * without a full data-model context.  Looking up "value" returns the
 * stored value; all other lookups delegate to the parent.
 */
class value_scope_t : public child_scope_t {
  value_t value; ///< The value exposed as the "value" identifier.

  value_t get_value(call_scope_t&) { return value; }

public:
  value_scope_t(scope_t& _parent, const value_t& _value) : child_scope_t(_parent), value(_value) {
    TRACE_CTOR(value_scope_t, "scope_t&, value_t");
  }
  ~value_scope_t() noexcept override { TRACE_DTOR(value_scope_t); }

  string description() override { return parent->description(); }

  expr_t::ptr_op_t lookup(const symbol_t::kind_t kind, const string& name) override {
    if (kind != symbol_t::FUNCTION)
      return nullptr;

    if (name == "value")
      return MAKE_FUNCTOR(value_scope_t::get_value);

    return child_scope_t::lookup(kind, name);
  }
};

} // namespace ledger
