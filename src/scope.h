/*
 * Copyright (c) 2003-2009, John Wiegley.  All rights reserved.
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
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _SCOPE_H
#define _SCOPE_H

#include "expr.h"
#include "op.h"

namespace ledger {

/**
 * @brief Brief
 *
 * Long.
 */
class scope_t
{
public:
  explicit scope_t() {
    TRACE_CTOR(scope_t, "");
  }
  virtual ~scope_t() {
    TRACE_DTOR(scope_t);
  }

  virtual void define(const string&, expr_t::ptr_op_t) {}
  virtual expr_t::ptr_op_t lookup(const string& name) = 0;
};

/**
 * @brief Brief
 *
 * Long.
 */
class child_scope_t : public noncopyable, public scope_t
{
public:
  scope_t * parent;

  explicit child_scope_t() : parent(NULL) {
    TRACE_CTOR(child_scope_t, "");
  }
  explicit child_scope_t(scope_t& _parent)
    : parent(&_parent) {
    TRACE_CTOR(child_scope_t, "scope_t&");
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
    return NULL;
  }
};

/**
 * @brief Brief
 *
 * Long.
 */
class symbol_scope_t : public child_scope_t
{
  typedef std::map<const string, expr_t::ptr_op_t> symbol_map;

  symbol_map symbols;

public:
  explicit symbol_scope_t() {
    TRACE_CTOR(symbol_scope_t, "");
  }
  explicit symbol_scope_t(scope_t& _parent) : child_scope_t(_parent) {
    TRACE_CTOR(symbol_scope_t, "scope_t&");
  }
  virtual ~symbol_scope_t() {
    TRACE_DTOR(symbol_scope_t);
  }

  virtual void define(const string& name, expr_t::ptr_op_t def);

  virtual expr_t::ptr_op_t lookup(const string& name);
};

/**
 * @brief Brief
 *
 * Long.
 */
class call_scope_t : public child_scope_t
{
  value_t args;

  call_scope_t();

public:
  explicit call_scope_t(scope_t& _parent) : child_scope_t(_parent) {
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

  value_t& operator[](const std::size_t index) {
    return args[index];
  }
  const value_t& operator[](const std::size_t index) const {
    return args[index];
  }

  void push_back(const value_t& val) {
    args.push_back(val);
  }
  void pop_back() {
    args.pop_back();
  }

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

/**
 * @brief Brief
 *
 * Long.
 */
class bind_scope_t : public child_scope_t
{
  bind_scope_t();

public:
  scope_t& grandchild;

  explicit bind_scope_t(scope_t& _parent,
			scope_t& _grandchild)
    : child_scope_t(_parent), grandchild(_grandchild) {
    TRACE_CTOR(bind_scope_t, "scope_t&, scope_t&");
  }
  virtual ~bind_scope_t() {
    TRACE_DTOR(bind_scope_t);
  }

  virtual void define(const string& name, expr_t::ptr_op_t def) {
    parent->define(name, def);
    grandchild.define(name, def);
  }

  virtual expr_t::ptr_op_t lookup(const string& name) {
    if (expr_t::ptr_op_t def = grandchild.lookup(name))
      return def;
    return child_scope_t::lookup(name);
  }
};

/**
 * @brief Brief
 *
 * Long.
 */
template <typename T>
T * search_scope(scope_t * ptr)
{
  if (T * sought = dynamic_cast<T *>(ptr))
    return sought;

  if (bind_scope_t * scope = dynamic_cast<bind_scope_t *>(ptr)) {
    if (T * sought = search_scope<T>(&scope->grandchild))
      return sought;
    return search_scope<T>(scope->parent);
  }
  else if (child_scope_t * scope = dynamic_cast<child_scope_t *>(ptr)) {
    return search_scope<T>(scope->parent);
  }
  return NULL;
}

template <typename T>
inline T& find_scope(child_scope_t& scope, bool skip_this = true)
{
  if (T * sought = search_scope<T>(skip_this ? scope.parent : &scope))
    return *sought;

  throw_(std::runtime_error, "Could not find scope");
  return reinterpret_cast<T&>(scope); // never executed
}

} // namespace ledger

#endif // _SCOPE_H
