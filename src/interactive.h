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
 * @file   interactive.h
 * @author John Wiegley
 *
 * @ingroup expr
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _INTERACTIVE_H
#define _INTERACTIVE_H

#include "scope.h"

namespace ledger {

/**
 * @brief Brief
 *
 * Long.
 */
class interactive_t : public noncopyable
{
  call_scope_t& args;
  string        spec;

public:
  explicit interactive_t(call_scope_t& _args, const string& _spec = "")
    : args(_args), spec(_spec)  {
    TRACE_CTOR(interactive_t, "call_scope_t&, const string&");
    verify_arguments();
  }
  virtual ~interactive_t() {
    TRACE_DTOR(interactive_t);
  }

  void verify_arguments() const;

  bool has(std::size_t index) const {
    if (index < args.size() && ! args[index].is_null())
      return true;
    return false;
  }

  const value_t& value_at(std::size_t index) const {
    assert(has(index));
    return args[index];
  }

  template <typename T>
  T get(std::size_t index) const;
};

template <>
inline bool interactive_t::get<bool>(std::size_t index) const {
  return value_at(index).to_boolean();
}
template <>
inline long interactive_t::get<long>(std::size_t index) const {
  return value_at(index).to_long();
}
template <>
inline string interactive_t::get<string>(std::size_t index) const {
  return value_at(index).to_string();
}
template <>
inline mask_t interactive_t::get<mask_t>(std::size_t index) const {
  return value_at(index).to_mask();
}
template <>
inline date_t interactive_t::get<date_t>(std::size_t index) const {
  return value_at(index).to_date();
}
template <>
inline datetime_t interactive_t::get<datetime_t>(std::size_t index) const {
  return value_at(index).to_datetime();
}

template <typename T>
class in_context_t : public interactive_t
{
  T& context;

public:
  explicit in_context_t(call_scope_t& args, const string& spec)
    : interactive_t(args, spec), context(find_scope<T>(args)) {
    TRACE_CTOR(in_context_t, "call_scope_t&, const string&");
  }
  virtual ~in_context_t() {
    TRACE_DTOR(in_context_t);
  }

  T& operator *() {
    return context;
  }
  T * operator->() {
    return &context;
  }
};

string join_args(call_scope_t& args);

} // namespace ledger

#endif // _INTERACTIVE_H

