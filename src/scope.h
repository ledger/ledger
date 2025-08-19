/*
 * Copyright (c) 2003-2023, John Wiegley.  All rights reserved.
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
 *
 * @ingroup expr
 */
#pragma once

#include "op.h"

namespace ledger {

struct symbol_t
{
  enum kind_t {
    UNKNOWN,
    FUNCTION,
    OPTION,
    PRECOMMAND,
    COMMAND,
    DIRECTIVE,
    FORMAT
  };

  kind_t           kind;
  string           name;
  expr_t::ptr_op_t definition;

  symbol_t() : kind(UNKNOWN), name(""), definition(NULL) {
    TRACE_CTOR(symbol_t, "");
  }
  symbol_t(kind_t _kind, string _name, expr_t::ptr_op_t _definition = NULL)
    : kind(_kind), name(_name), definition(_definition) {
    TRACE_CTOR(symbol_t, "symbol_t::kind_t, string");
  }
  symbol_t(const symbol_t& sym)
    : kind(sym.kind), name(sym.name), definition(sym.definition) {
    TRACE_CTOR(symbol_t, "copy");
  }
  ~symbol_t() throw() {
    TRACE_DTOR(symbol_t);
  }

  bool operator<(const symbol_t& sym) const {
    return kind < sym.kind || name < sym.name;
  }
  bool operator==(const symbol_t& sym) const {
    return kind == sym.kind || name == sym.name;
  }
};

class empty_scope_t;

class scope_t
{
public:
  static scope_t *       default_scope;
  static empty_scope_t * empty_scope;

  explicit scope_t() {
    TRACE_CTOR(scope_t, "");
  }
  virtual ~scope_t() {
    TRACE_DTOR(scope_t);
  }

  virtual string description() = 0;

  virtual void define(const symbol_t::kind_t, const string&,
                      expr_t::ptr_op_t) {}
  virtual expr_t::ptr_op_t lookup(const symbol_t::kind_t kind,
                                  const string& name) = 0;

  virtual value_t::type_t type_context() const {
    return value_t::VOID;
  }
  virtual bool type_required() const {
    return false;
  }
};

class empty_scope_t : public scope_t
{
public:
  empty_scope_t() {
    TRACE_CTOR(empty_scope_t, "");
  }
  ~empty_scope_t() throw() {
    TRACE_DTOR(empty_scope_t);
  }

  virtual string description() {
    return _("<empty>");
  }
  virtual expr_t::ptr_op_t lookup(const symbol_t::kind_t, const string&) {
    return NULL;
  }
};

class child_scope_t : public noncopyable, public scope_t
{
public:
  scope_t * parent;

  explicit child_scope_t() : parent(NULL) {
    TRACE_CTOR(child_scope_t, "");
  }
  explicit child_scope_t(scope_t& _parent) : parent(&_parent) {
    TRACE_CTOR(child_scope_t, "scope_t&");
  }
  virtual ~child_scope_t() {
    TRACE_DTOR(child_scope_t);
  }

  virtual void define(const symbol_t::kind_t kind,
                      const string& name, expr_t::ptr_op_t def) {
    if (parent)
      parent->define(kind, name, def);
  }

  virtual expr_t::ptr_op_t lookup(const symbol_t::kind_t kind,
                                  const string& name) {
    if (parent)
      return parent->lookup(kind, name);
    return NULL;
  }
};

class bind_scope_t : public child_scope_t
{
  bind_scope_t();

public:
  scope_t& grandchild;

  explicit bind_scope_t(scope_t& _parent,
                        scope_t& _grandchild)
    : child_scope_t(_parent), grandchild(_grandchild) {
    DEBUG("scope.symbols",
          "Binding scope " << &_parent << " with " << &_grandchild);
    TRACE_CTOR(bind_scope_t, "scope_t&, scope_t&");
  }
  virtual ~bind_scope_t() {
    TRACE_DTOR(bind_scope_t);
  }

  virtual string description() {
    return grandchild.description();
  }

  virtual void define(const symbol_t::kind_t kind, const string& name,
                      expr_t::ptr_op_t def) {
    parent->define(kind, name, def);
    grandchild.define(kind, name, def);
  }

  virtual expr_t::ptr_op_t lookup(const symbol_t::kind_t kind,
                                  const string& name) {
    if (expr_t::ptr_op_t def = grandchild.lookup(kind, name))
      return def;
    return child_scope_t::lookup(kind, name);
  }
};

template <typename T>
T * search_scope(scope_t * ptr, bool prefer_direct_parents = false)
{
  DEBUG("scope.search", "Searching scope " << ptr->description());

  if (T * sought = dynamic_cast<T *>(ptr))
    return sought;

  if (bind_scope_t * scope = dynamic_cast<bind_scope_t *>(ptr)) {
    if (T * sought = search_scope<T>(prefer_direct_parents ?
                                     scope->parent : &scope->grandchild))
      return sought;
    return search_scope<T>(prefer_direct_parents ?
                           &scope->grandchild : scope->parent);
  }
  else if (child_scope_t * child_scope = dynamic_cast<child_scope_t *>(ptr)) {
    return search_scope<T>(child_scope->parent);
  }
  return NULL;
}

template <typename T>
inline T& find_scope(child_scope_t& scope, bool skip_this = true,
                     bool prefer_direct_parents = false)
{
  if (T * sought = search_scope<T>(skip_this ? scope.parent : &scope,
                                   prefer_direct_parents))
    return *sought;

  throw_(std::runtime_error, _("Could not find scope"));
  return reinterpret_cast<T&>(scope); // never executed
}

template <typename T>
inline T& find_scope(scope_t& scope, bool prefer_direct_parents = false)
{
  if (T * sought = search_scope<T>(&scope, prefer_direct_parents))
    return *sought;

  throw_(std::runtime_error, _("Could not find scope"));
  return reinterpret_cast<T&>(scope); // never executed
}

class symbol_scope_t : public child_scope_t
{
  typedef std::map<symbol_t, expr_t::ptr_op_t> symbol_map;

  optional<symbol_map> symbols;

public:
  explicit symbol_scope_t() : child_scope_t() {
    TRACE_CTOR(symbol_scope_t, "");
  }
  explicit symbol_scope_t(scope_t& _parent) : child_scope_t(_parent) {
    TRACE_CTOR(symbol_scope_t, "scope_t&");
  }
  virtual ~symbol_scope_t() {
    TRACE_DTOR(symbol_scope_t);
  }

  virtual string description() {
    if (parent)
      return parent->description();
    else
      assert(false);
    return empty_string;
  }

  virtual void define(const symbol_t::kind_t kind, const string& name,
                      expr_t::ptr_op_t def);

  virtual expr_t::ptr_op_t lookup(const symbol_t::kind_t kind,
                                  const string& name);
};

class context_scope_t : public child_scope_t
{
  value_t::type_t value_type_context;
  bool            required;

public:
  explicit context_scope_t(scope_t&        _parent,
                           value_t::type_t _type_context = value_t::VOID,
                           const bool      _required     = true)
    : child_scope_t(_parent), value_type_context(_type_context),
      required(_required) {
    TRACE_CTOR(context_scope_t, "scope_t&, value_t::type_t, bool");
  }
  virtual ~context_scope_t() {
    TRACE_DTOR(context_scope_t);
  }

  virtual string description() {
    return parent->description();
  }

  virtual value_t::type_t type_context() const {
    return value_type_context;
  }
  virtual bool type_required() const {
    return required;
  }
};

class call_scope_t : public context_scope_t
{
public:
  value_t        args;
  mutable void * ptr;

  value_t& resolve(const std::size_t index,
                   value_t::type_t   context  = value_t::VOID,
                   const bool        required = false);

public:
  expr_t::ptr_op_t * locus;
  const int          depth;

  explicit call_scope_t(scope_t&           _parent,
                        expr_t::ptr_op_t * _locus = NULL,
                        const int          _depth = 0)
    : context_scope_t(_parent, _parent.type_context(),
                      _parent.type_required()),
      ptr(NULL), locus(_locus), depth(_depth) {
    TRACE_CTOR(call_scope_t, "scope_t&, expr_t::ptr_op_t *, const int");
  }
  virtual ~call_scope_t() {
    TRACE_DTOR(call_scope_t);
  }

  void set_args(const value_t& _args) {
    args = _args;
  }
  value_t& value() {
    // Make sure that all of the arguments have been resolved.
    for (std::size_t index = 0; index < args.size(); index++)
      resolve(index);
    return args;
  }

  value_t& operator[](const std::size_t index) {
    return resolve(index);
  }
#if 0
  const value_t& operator[](const std::size_t index) const {
    return args[index];
  }
#endif

  bool has(std::size_t index) {
    return index < args.size() && ! (*this)[index].is_null();
  }
  template <typename T>
  bool has(std::size_t index);
  template <typename T>
  T get(std::size_t index, bool convert = true);

  template <typename T>
  T& context() {
    if (ptr == NULL)
      ptr = &find_scope<T>(*this);
    assert(ptr != NULL);
    return *static_cast<T *>(ptr);
  }

  void push_front(const value_t& val) {
    args.push_front(val);
  }
  void push_back(const value_t& val) {
    args.push_back(val);
  }
  void pop_back() {
    args.pop_back();
  }

  typedef value_t::sequence_t::iterator iterator;

  value_t::sequence_t::iterator begin() {
    return args.begin();
  }
  value_t::sequence_t::iterator end() {
    return args.end();
  }

  typedef value_t::sequence_t::const_iterator const_iterator;

  value_t::sequence_t::const_iterator begin() const {
    return args.begin();
  }
  value_t::sequence_t::const_iterator end() const {
    return args.end();
  }

  std::size_t size() const {
    return args.size();
  }
  bool empty() const {
    return args.size() == 0;
  }
};

template <>
inline bool call_scope_t::has<bool>(std::size_t index) {
  if (index < args.size()) {
    resolve(index, value_t::BOOLEAN, false);
    return ! args[index].is_null();
  }
  return false;
}
template <>
inline bool call_scope_t::has<int>(std::size_t index) {
  if (index < args.size()) {
    resolve(index, value_t::INTEGER, false);
    return ! args[index].is_null();
  }
  return false;
}
template <>
inline bool call_scope_t::has<long>(std::size_t index) {
  if (index < args.size()) {
    resolve(index, value_t::INTEGER, false);
    return ! args[index].is_null();
  }
  return false;
}
template <>
inline bool call_scope_t::has<amount_t>(std::size_t index) {
  if (index < args.size()) {
    resolve(index, value_t::AMOUNT, false);
    return ! args[index].is_null();
  }
  return false;
}
template <>
inline bool call_scope_t::has<balance_t>(std::size_t index) {
  if (index < args.size()) {
    resolve(index, value_t::BALANCE, false);
    return ! args[index].is_null();
  }
  return false;
}
template <>
inline bool call_scope_t::has<string>(std::size_t index) {
  if (index < args.size()) {
    resolve(index, value_t::STRING, false);
    return ! args[index].is_null();
  }
  return false;
}
template <>
inline bool call_scope_t::has<date_t>(std::size_t index) {
  if (index < args.size()) {
    resolve(index, value_t::DATE, false);
    return ! args[index].is_null();
  }
  return false;
}
template <>
inline bool call_scope_t::has<datetime_t>(std::size_t index) {
  if (index < args.size()) {
    resolve(index, value_t::DATETIME, false);
    return ! args[index].is_null();
  }
  return false;
}
template <>
inline bool call_scope_t::has<scope_t *>(std::size_t index) {
  if (index < args.size()) {
    resolve(index, value_t::SCOPE, false);
    return ! args[index].is_null();
  }
  return false;
}
template <>
inline bool call_scope_t::has<expr_t::ptr_op_t>(std::size_t index) {
  if (index < args.size()) {
    resolve(index, value_t::ANY, false);
    return ! args[index].is_null();
  }
  return false;
}

template <>
inline bool call_scope_t::get<bool>(std::size_t index, bool convert) {
  if (convert)
    return resolve(index, value_t::BOOLEAN, false).to_boolean();
  else
    return resolve(index, value_t::BOOLEAN).as_boolean();
}
template <>
inline int call_scope_t::get<int>(std::size_t index, bool) {
  return resolve(index, value_t::INTEGER, false).to_int();
}
template <>
inline long call_scope_t::get<long>(std::size_t index, bool convert) {
  if (convert)
    return resolve(index, value_t::INTEGER, false).to_long();
  else
    return resolve(index, value_t::INTEGER).as_long();
}
template <>
inline amount_t call_scope_t::get<amount_t>(std::size_t index, bool convert) {
  if (convert)
    return resolve(index, value_t::AMOUNT, false).to_amount();
  else
    return resolve(index, value_t::AMOUNT).as_amount();
}
template <>
inline balance_t call_scope_t::get<balance_t>(std::size_t index, bool convert) {
  if (convert)
    return resolve(index, value_t::BALANCE, false).to_balance();
  else
    return resolve(index, value_t::BALANCE).as_balance();
}
template <>
inline string call_scope_t::get<string>(std::size_t index, bool convert) {
  if (convert)
    return resolve(index, value_t::STRING, false).to_string();
  else
    return resolve(index, value_t::STRING).as_string();
}
template <>
inline mask_t call_scope_t::get<mask_t>(std::size_t index, bool convert) {
  if (convert)
    return resolve(index, value_t::MASK, false).to_mask();
  else
    return resolve(index, value_t::MASK).as_mask();
}
template <>
inline date_t call_scope_t::get<date_t>(std::size_t index, bool convert) {
  if (convert)
    return resolve(index, value_t::DATE, false).to_date();
  else
    return resolve(index, value_t::DATE).as_date();
}
template <>
inline datetime_t call_scope_t::get<datetime_t>(std::size_t index, bool convert) {
  if (convert)
    return resolve(index, value_t::DATETIME, false).to_datetime();
  else
    return resolve(index, value_t::DATETIME).as_datetime();
}

#if 0
template <>
inline value_t::sequence_t&
call_scope_t::get<value_t::sequence_t&>(std::size_t index, bool) {
  return resolve(index, value_t::SEQUENCE).as_sequence_lval();
}
template <>
inline const value_t::sequence_t&
call_scope_t::get<const value_t::sequence_t&>(std::size_t index, bool) {
  return resolve(index, value_t::SEQUENCE).as_sequence();
}
#endif

template <>
inline scope_t * call_scope_t::get<scope_t *>(std::size_t index, bool) {
  return resolve(index, value_t::SCOPE).as_scope();
}
template <>
inline expr_t::ptr_op_t
call_scope_t::get<expr_t::ptr_op_t>(std::size_t index, bool) {
  return args[index].as_any<expr_t::ptr_op_t>();
}

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

class value_scope_t : public child_scope_t
{
  value_t value;

  value_t get_value(call_scope_t&) {
    return value;
  }

public:
  value_scope_t(scope_t& _parent, const value_t& _value)
    : child_scope_t(_parent), value(_value) {
    TRACE_CTOR(value_scope_t, "scope_t&, value_t");
  }
  ~value_scope_t() throw() {
    TRACE_DTOR(value_scope_t);
  }

  virtual string description() {
    return parent->description();
  }

  virtual expr_t::ptr_op_t lookup(const symbol_t::kind_t kind,
                                  const string& name)
  {
    if (kind != symbol_t::FUNCTION)
      return NULL;

    if (name == "value")
      return MAKE_FUNCTOR(value_scope_t::get_value);

    return child_scope_t::lookup(kind, name);
  }
};

} // namespace ledger
