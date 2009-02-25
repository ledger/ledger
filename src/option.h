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
 * @file   option.h
 * @author John Wiegley
 *
 * @ingroup expr
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _OPTION_H
#define _OPTION_H

#include "interactive.h"

namespace ledger {

template <typename T>
class option_t
{
protected:
  const char * name;
  std::size_t  name_len;
  const char   ch;
  bool	       handled;

  option_t& operator=(const option_t&);

public:
  T *          parent;
  value_t      value;
  bool	       wants_arg;

  option_t(const char * _name, const char _ch = '\0')
    : name(_name), name_len(std::strlen(name)), ch(_ch),
      handled(false), parent(NULL), value(),
      wants_arg(name[name_len - 1] == '_') {
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

  virtual void help(std::ostream& out) {
    out << "No help for " << desc() << "\n";
  }

  operator bool() const {
    return handled;
  }

  string& str() {
    assert(handled);
    if (! value)
      throw_(std::runtime_error, "No argument provided for " << desc());
    return value.as_string_lval();
  }

  void on_only() {
    handled = true;
  }
  void on(const string& str) {
    on_with(string_value(str));
  }
  virtual void on_with(const value_t& val) {
    handled = true;
    value   = val;
  }

  void off() {
    handled = false;
    value   = value_t();
  }

  virtual void handler_thunk(call_scope_t&) {}

  virtual void handler(call_scope_t& args) {
    if (wants_arg) {
      if (args.empty())
	throw_(std::runtime_error, "No argument provided for " << desc());
      on_with(args[0]);
    } else {
      on_only();
    }

    handler_thunk(args);
  }

  virtual value_t handler_wrapper(call_scope_t& args) {
    handler(args);
    return true;
  }

  virtual value_t operator()(call_scope_t& args) {
    if (! args.empty()) {
      return handler_wrapper(args);
    }
    else if (wants_arg) {
      if (handled)
	return value;
      else
	return NULL_VALUE;
    }
    else {
      return handled;
    }
  }
};

#define BEGIN(type, name)				\
  struct name ## _option_t : public option_t<type>

#define CTOR(type, name)				\
  name ## _option_t() : option_t<type>(#name)
#define DECL1(type, name, vartype, var, value)		\
  vartype var ;						\
  name ## _option_t() : option_t<type>(#name), var(value)
  
#define HELP(var) virtual void help(std::ostream& var)
#define DO()      virtual void handler_thunk(call_scope_t&)
#define DO_(var)  virtual void handler_thunk(call_scope_t& var)

#define END(name) name ## _handler

#define COPY_OPT(name, other) name ## _handler(other.name ## _handler)

#define MAKE_OPT_HANDLER(type, x)					\
  expr_t::op_t::wrap_functor(bind(&option_t<type>::handler_wrapper, x, _1))

#define MAKE_OPT_FUNCTOR(type, x)					\
  expr_t::op_t::wrap_functor(bind(&option_t<type>::operator(), x, _1))

inline bool is_eq(const char * p, const char * n) {
  // Test whether p matches n, substituting - in p for _ in n.
  for (; *p && *n; p++, n++) {
    if (! (*p == '-' && *n == '_' ) && *p != *n)
      return false;
  }
  // Ignore any trailing underscore
  return *p == *n || (! *p && *n == '_' && ! *(n + 1));
}

#define OPT(name)							\
  if (is_eq(p, #name))							\
    return ((name ## _handler).parent = this, &(name ## _handler))

#define OPT_ALT(name, alt)						\
  if (is_eq(p, #name) || is_eq(p, #alt))				\
    return ((name ## _handler).parent = this, &(name ## _handler))

#define OPT_(name)							\
  if (! *(p + 1) ||							\
      ((name ## _handler).wants_arg &&					\
       *(p + 1) == '_' && ! *(p + 2)) ||				\
      is_eq(p, #name))							\
    return ((name ## _handler).parent = this, &(name ## _handler))

#define OPT_CH(name)							\
  if (! *(p + 1) ||							\
      ((name ## _handler).wants_arg &&					\
       *(p + 1) == '_' && ! *(p + 2)))					\
    return ((name ## _handler).parent = this, &(name ## _handler))

#define HANDLER(name) name ## _handler
#define HANDLED(name) HANDLER(name)

#define OPTION(type, name)				\
  BEGIN(type, name)					\
  {							\
    CTOR(type, name) {}					\
  }							\
  END(name)

#define OPTION_(type, name, body)			\
  BEGIN(type, name)					\
  {							\
    CTOR(type, name) {}					\
    body						\
  }							\
  END(name)

#define OPTION__(type, name, body)			\
  BEGIN(type, name)					\
  {							\
    body						\
  }							\
  END(name)

#define OPT_PREFIX "opt_"
#define OPT_PREFIX_LEN 4

#define WANT_OPT()					\
  (std::strncmp(p, OPT_PREFIX, OPT_PREFIX_LEN) == 0)

#define PRECMD_PREFIX "precmd_"
#define PRECMD_PREFIX_LEN 7

#define WANT_PRECMD()					\
  (std::strncmp(p, PRECMD_PREFIX, PRECMD_PREFIX_LEN) == 0)

#define CMD_PREFIX "cmd_"
#define CMD_PREFIX_LEN 4

#define WANT_CMD()					\
  (std::strncmp(p, CMD_PREFIX, CMD_PREFIX_LEN) == 0)

#define DIR_PREFIX "dir_"
#define DIR_PREFIX_LEN 4

#define WANT_DIR()					\
  (std::strncmp(p, DIR_PREFIX, DIR_PREFIX_LEN) == 0)

void process_option(const string& name, scope_t& scope,
		    const char * arg, const string& varname);

void process_environment(const char ** envp, const string& tag,
			 scope_t& scope);

strings_list process_arguments(strings_list args, scope_t& scope);

DECLARE_EXCEPTION(option_error, std::runtime_error);

} // namespace ledger

#endif // _OPTION_H
