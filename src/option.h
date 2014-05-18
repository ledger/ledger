/*
 * Copyright (c) 2003-2014, John Wiegley.  All rights reserved.
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
 * @file   option.h
 * @author John Wiegley
 *
 * @ingroup expr
 */
#ifndef _OPTION_H
#define _OPTION_H

#include "scope.h"

namespace ledger {

class call_scope_t;

template <typename T>
class option_t
{
protected:
  const char *      name;
  string::size_type name_len;
  const char        ch;
  bool              handled;
  optional<string>  source;

  option_t& operator=(const option_t&);

public:
  T *    parent;
  string value;
  bool   wants_arg;

  option_t(const char * _name, const char _ch = '\0')
    : name(_name), name_len(std::strlen(name)), ch(_ch),
      handled(false), parent(NULL), value(),
      wants_arg(name_len > 0 ? name[name_len - 1] == '_' : false) {
    DEBUG("option.names", "Option: " << name);
    TRACE_CTOR(option_t, "const char *, const char");
  }
  option_t(const option_t& other)
    : name(other.name),
      name_len(other.name_len),
      ch(other.ch),
      handled(other.handled),
      parent(NULL),
      value(other.value),
      wants_arg(other.wants_arg)
  {
    TRACE_CTOR(option_t, "copy");
  }

  virtual ~option_t() {
    TRACE_DTOR(option_t);
  }

  void report(std::ostream& out) const {
    if (handled && source) {
      out.width(24);
      out << std::right << desc();
      if (wants_arg) {
        out << " = ";
        out.width(42);
        out << std::left << value;
      } else {
        out.width(45);
        out << ' ';
      }
      out << std::left << *source << std::endl;
    }
  }

  string desc() const {
    std::ostringstream out;
    out << "--";
    for (const char * p = name; *p; p++) {
      if (*p == '_') {
        if (*(p + 1))
          out << '-';
      } else {
        out << *p;
      }
    }
    if (ch)
      out << " (-" << ch << ")";
    return out.str();
  }

  operator bool() const {
    return handled;
  }

  string str() const {
    assert(handled);
    if (value.empty())
      throw_(std::runtime_error, _f("No argument provided for %1%") % desc());
    return value;
  }

  void on(const char * whence) {
    on(string(whence));
  }
  void on(const optional<string>& whence) {
    handler_thunk(whence);

    handled = true;
    source  = whence;
  }

  void on(const char * whence, const string& str) {
    on(string(whence), str);
  }
  void on(const optional<string>& whence, const string& str) {
    string before = value;

    handler_thunk(whence, str);

    if (value == before)
      value = str;

    handled = true;
    source  = whence;
  }

  void off() {
    handled = false;
    value   = "";
    source  = none;
  }

  virtual void handler_thunk(const optional<string>&) {}
  virtual void handler_thunk(const optional<string>&, const string&) {}

  value_t handler(call_scope_t& args) {
    if (wants_arg) {
      if (args.size() < 2)
        throw_(std::runtime_error, _f("No argument provided for %1%") % desc());
      else if (args.size() > 2)
        throw_(std::runtime_error, _f("To many arguments provided for %1%") % desc());
      else if (! args[0].is_string())
        throw_(std::runtime_error, _f("Context argument for %1% not a string") % desc());
      on(args.get<string>(0), args.get<string>(1));
    }
    else if (args.size() < 1) {
      throw_(std::runtime_error, _f("No argument provided for %1%") % desc());
    }
    else if (! args[0].is_string()) {
      throw_(std::runtime_error, _f("Context argument for %1% not a string") % desc());
    }
    else {
      on(args.get<string>(0));
    }
    return true;
  }

  virtual value_t operator()(call_scope_t& args) {
    if (! args.empty()) {
      args.push_front(string_value("?expr"));
      return handler(args);
    }
    else if (wants_arg) {
      return string_value(value);
    }
    else {
      return handled;
    }
  }
};

#define BEGIN(type, name)                               \
  struct name ## option_t : public option_t<type>

#define CTOR(type, name)                                \
  name ## option_t() : option_t<type>(#name)
#define CTOR_(type, name, base)                         \
  name ## option_t() : option_t<type>(#name), base
#define DECL1(type, name, vartype, var, value)          \
  vartype var ;                                         \
  name ## option_t() : option_t<type>(#name), var value

#define DO()      virtual void handler_thunk(const optional<string>& whence)
#define DO_(var)  virtual void handler_thunk(const optional<string>& whence, \
                                             const string& var)

#define END(name) name ## handler

#define COPY_OPT(name, other) name ## handler(other.name ## handler)

#define MAKE_OPT_HANDLER(type, x)                                       \
  expr_t::op_t::wrap_functor(bind(&option_t<type>::handler, x, _1))

#define MAKE_OPT_FUNCTOR(type, x)                                       \
  expr_t::op_t::wrap_functor(bind(&option_t<type>::operator(), x, _1))

inline bool is_eq(const char * p, const char * n) {
  // Test whether p matches n, substituting - in p for _ in n.
  for (; *p && *n; p++, n++) {
    if (! (*p == '-' && *n == '_') && *p != *n)
      return false;
  }
  // Ignore any trailing underscore
  return *p == *n || (! *p && *n == '_' && ! *(n + 1));
}

#define OPT(name)                                                       \
  if (is_eq(p, #name))                                                  \
    return ((name ## handler).parent = this, &(name ## handler))

#define OPT_ALT(name, alt)                                              \
  if (is_eq(p, #name) || is_eq(p, #alt))                                \
    return ((name ## handler).parent = this, &(name ## handler))

#define OPT_(name)                                                      \
  if (! *(p + 1) ||                                                     \
      ((name ## handler).wants_arg &&                                   \
       *(p + 1) == '_' && ! *(p + 2)) ||                                \
      is_eq(p, #name))                                                  \
    return ((name ## handler).parent = this, &(name ## handler))

#define OPT_CH(name)                                                    \
  if (! *(p + 1) ||                                                     \
      ((name ## handler).wants_arg &&                                   \
       *(p + 1) == '_' && ! *(p + 2)))                                  \
    return ((name ## handler).parent = this, &(name ## handler))

#define HANDLER(name) name ## handler
#define HANDLED(name) HANDLER(name)

#define OPTION(type, name)                              \
  BEGIN(type, name)                                     \
  {                                                     \
    CTOR(type, name) {}                                 \
  }                                                     \
  END(name)

#define OPTION_(type, name, body)                       \
  BEGIN(type, name)                                     \
  {                                                     \
    CTOR(type, name) {}                                 \
    body                                                \
  }                                                     \
  END(name)

#define OPTION__(type, name, body)                      \
  BEGIN(type, name)                                     \
  {                                                     \
    body                                                \
  }                                                     \
  END(name)

#define OTHER(name)                             \
  parent->HANDLER(name).parent = parent;        \
  parent->HANDLER(name)

bool process_option(const string& whence, const string& name, scope_t& scope,
                    const char * arg, const string& varname);

void process_environment(const char ** envp, const string& tag,
                         scope_t& scope);

strings_list process_arguments(strings_list args, scope_t& scope);

DECLARE_EXCEPTION(option_error, std::runtime_error);

} // namespace ledger

#endif // _OPTION_H
