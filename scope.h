/*
 * Copyright (c) 2003-2008, John Wiegley.  All rights reserved.
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

#ifndef _SCOPE_H
#define _SCOPE_H

#include "expr.h"
#include "op.h"

namespace ledger {

class scope_t : public noncopyable
{
  scope_t();

protected:
  enum type_t {
    CHILD_SCOPE,
    SYMBOL_SCOPE,
    CALL_SCOPE,
    CONTEXT_SCOPE
  } type_;

public:
  explicit scope_t(type_t _type) : type_(_type) {
    TRACE_CTOR(scope_t, "type_t");
  }
  virtual ~scope_t() {
    TRACE_DTOR(scope_t);
  }

  const type_t type() const {
    return type_;
  }

  void    define(const string& name, const value_t& val) {
    define(name, expr_t::op_t::wrap_value(val));
  }
  void    define(const string& name, const function_t& func) {
    define(name, expr_t::op_t::wrap_functor(func));
  }

  value_t resolve(const string& name) {
    expr_t::ptr_op_t definition = lookup(name);
    if (definition)
      return definition->calc(*this);
    else
      return NULL_VALUE;
  }

  virtual void define(const string& name, expr_t::ptr_op_t def) = 0;
  virtual expr_t::ptr_op_t lookup(const string& name) = 0;

protected:
  virtual optional<scope_t&> find_scope(const type_t _type,
					bool skip_this = false) = 0;
  virtual optional<scope_t&> find_first_scope(const type_t _type1,
					      const type_t _type2,
					      bool skip_this = false) = 0;
  template <typename T>
  T& find_scope(bool skip_this = false) {
    assert(false);
  }
  template <typename T>
  optional<T&> maybe_find_scope(bool skip_this = false) {
    assert(false);
  }

  friend class child_scope_t;
  friend class expr_t::op_t;
};

class child_scope_t : public scope_t
{
  scope_t * parent;

  child_scope_t();

public:
  explicit child_scope_t(type_t _type = CHILD_SCOPE)
    : scope_t(_type), parent(NULL) {
    TRACE_CTOR(child_scope_t, "type_t");
  }
  explicit child_scope_t(scope_t& _parent, type_t _type = CHILD_SCOPE)
    : scope_t(_type), parent(&_parent) {
    TRACE_CTOR(child_scope_t, "scope_t&, type_t");
  }
  virtual ~child_scope_t() {
    TRACE_DTOR(child_scope_t);
  }

  virtual void define(const string& name, expr_t::ptr_op_t def) {
    if (parent)
      parent->define(name, def);
  }
  virtual expr_t::ptr_op_t lookup(const string& name) {
    if (parent)
      return parent->lookup(name);
    return expr_t::ptr_op_t();
  }

protected:
  virtual optional<scope_t&> find_scope(type_t _type,
					bool skip_this = false) {
    for (scope_t * ptr = (skip_this ? parent : this); ptr; ) {
      if (ptr->type() == _type)
	return *ptr;

      ptr = polymorphic_downcast<child_scope_t *>(ptr)->parent;
    }
    return none;
  }

  virtual optional<scope_t&> find_first_scope(const type_t _type1,
					      const type_t _type2,
					      bool skip_this = false) {
    for (scope_t * ptr = (skip_this ? parent : this); ptr; ) {
      if (ptr->type() == _type1 || ptr->type() == _type2)
	return *ptr;

      ptr = polymorphic_downcast<child_scope_t *>(ptr)->parent;
    }
    return none;
  }
};

class symbol_scope_t : public child_scope_t
{
  typedef std::map<const string, expr_t::ptr_op_t> symbol_map;

  symbol_map symbols;

public:
  explicit symbol_scope_t()
    : child_scope_t(SYMBOL_SCOPE) {
    TRACE_CTOR(symbol_scope_t, "");
  }
  explicit symbol_scope_t(scope_t& _parent)
    : child_scope_t(_parent, SYMBOL_SCOPE) {
    TRACE_CTOR(symbol_scope_t, "scope_t&");
  }
  virtual ~symbol_scope_t() {
    TRACE_DTOR(symbol_scope_t);
  }

  void define(const string& name, const value_t& val) {
    scope_t::define(name, val);
  }
  void define(const string& name, const function_t& func) {
    scope_t::define(name, func);
  }

  virtual void define(const string& name, expr_t::ptr_op_t def);
  virtual expr_t::ptr_op_t lookup(const string& name);
};

class call_scope_t : public child_scope_t
{
  value_t args;

  call_scope_t();

public:
  explicit call_scope_t(scope_t& _parent)
    : child_scope_t(_parent, CALL_SCOPE) {
    TRACE_CTOR(call_scope_t, "scope_t&");
  }
  virtual ~call_scope_t() {
    TRACE_DTOR(call_scope_t);
  }

  void set_args(const value_t& _args) {
    args = _args;
  }
  value_t& value() {
    return args;
  }

  value_t& operator[](const unsigned int index) {
    // jww (2008-07-21): exception here if it's out of bounds
    return args[index];
  }
  const value_t& operator[](const unsigned int index) const {
    // jww (2008-07-21): exception here if it's out of bounds
    return args[index];
  }

  void push_back(const value_t& val) {
    args.push_back(val);
  }
  void pop_back() {
    args.pop_back();
  }

  const std::size_t size() const {
    return args.size();
  }
};

template <typename T>
class var_t : public noncopyable
{
  T * value;

  var_t();

public:
  // jww (2008-07-21): Give a good exception here if we can't find "name"
  var_t(scope_t& scope, const string& name)
    : value(scope.resolve(name).template as_pointer<T>()) {
    TRACE_CTOR(var_t, "scope_t&, const string&");
  }
  var_t(call_scope_t& scope, const unsigned int idx)
    : value(scope[idx].template as_pointer<T>()) {
    TRACE_CTOR(var_t, "call_scope_t&, const unsigned int");
  }
  ~var_t() throw() {
    TRACE_DTOR(var_t);
  }

  T& operator *() { return *value; }
  T * operator->() { return value; }
};

#if 0
class context_scope_t : public child_scope_t
{
public:
  value_t     current_element;
  std::size_t element_index;
  std::size_t sequence_size;

  explicit context_scope_t(scope_t&	        _parent,
			   const value_t&     _element        = NULL_VALUE,
			   const std::size_t  _element_index  = 0,
			   const std::size_t  _sequence_size  = 0)
    : child_scope_t(_parent, CONTEXT_SCOPE), current_element(_element),
      element_index(_element_index), sequence_size(_sequence_size)
  {
    TRACE_CTOR(expr::context_scope_t, "scope_t&, const value_t&, ...");
  }
  virtual ~context_scope_t() {
    TRACE_DTOR(expr::context_scope_t);
  }

  const std::size_t index() const {
    return element_index;
  }
  const std::size_t size() const {
    return sequence_size;
  }

  value_t& value() {
    return current_element;
  }
};

struct context_t
{
  const entry_t * entry() {
    return NULL;
  }
  const transaction_t * xact() {
    return NULL;
  }
  const account_t * account() {
    return NULL;
  }
};

struct entry_context_t : public context_t
{
  const entry_t * entry_;

  const entry_t * entry() {
    return entry_;
  }
};

struct xact_context_t : public context_t
{
  const transaction_t * xact_;

  const entry_t * entry() {
    return xact_->entry;
  }
  const transaction_t * xact() {
    return xact_;
  }
  const account_t * account() {
    return xact_->account;
  }
};

struct account_context_t : public context_t
{
  const account_t * account_;

  const account_t * account() {
    return account_;
  }
};
#endif

template<>
inline symbol_scope_t&
scope_t::find_scope<symbol_scope_t>(bool skip_this) {
  optional<scope_t&> scope = find_scope(SYMBOL_SCOPE, skip_this);
  assert(scope);
  return downcast<symbol_scope_t>(*scope);
}

template<>
inline call_scope_t&
scope_t::find_scope<call_scope_t>(bool skip_this) {
  optional<scope_t&> scope = find_scope(CALL_SCOPE, skip_this);
  assert(scope);
  return downcast<call_scope_t>(*scope);
}

#if 0
template<>
inline context_scope_t&
scope_t::find_scope<context_scope_t>(bool skip_this) {
  optional<scope_t&> scope = find_scope(CONTEXT_SCOPE, skip_this);
  assert(scope);
  return downcast<context_scope_t>(*scope);
}
#endif

#define FIND_SCOPE(scope_type, scope_ref) \
  downcast<scope_t>(scope_ref).find_scope<scope_type>()

#define CALL_SCOPE(scope_ref) \
  FIND_SCOPE(call_scope_t, scope_ref)
#define SYMBOL_SCOPE(scope_ref) \
  FIND_SCOPE(symbol_scope_t, scope_ref)
#if 0
#define CONTEXT_SCOPE(scope_ref) \
  FIND_SCOPE(context_scope_t, scope_ref)
#endif

} // namespace ledger

#endif // _SCOPE_H

