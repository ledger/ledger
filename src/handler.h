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
 * @addtogroup report
 */

/**
 * @file   handler.h
 * @author John Wiegley
 *
 * @ingroup report
 *
 * @brief Defines a scheme for more easily handling commands and options via
 * value expressions.
 *
 * The OPTION and OPTION_ macros are used to define an option handler within a
 * scope_t derived class.  The _ variant can specify a body in order to
 * provide HELP(out) and DO() or DO_(args) methods.
 *
 * The other macros are used for looking up and referring to handlers,
 * commands and functions.  Typically they occur in the object's lookup
 * method, for example:
 *
 * expr_t::ptr_op_t some_class_t::lookup(const string& name)
 * {
 *   const char * p = name.c_str();
 *   switch (*p) {
 *   case 'a':
 *     METHOD(some_class_t, my_method); // looks up a method by name
 *     break;
 * 
 *   case 'c':
 *     if (WANT_CMD()) { p += CMD_PREFIX_LEN;
 *       switch (*p) {
 *       case 'a':
 *         COMMAND(args); // looks up global func named "args_command"
 *         break;
 *       }
 *     }
 *     break;
 * 
 *   case 'o':
 *     if (WANT_OPT()) { p += OPT_PREFIX_LEN;
 *       switch (*p) {
 *       case 'f':
 *         OPT(foo);		// look for a handler named "foo"
 *         else OPT(foo2_);     // same, but foo2 wants an argument
 *         else OPT_(foo3);     // match on "foo3" or 'f'
 *         else OPT_CH(foo4_);  // match only on 'f'
 *         break;
 *       }
 *     }
 *     break;
 *   }
 * 
 *   return expr_t::ptr_op_t();
 * }
 */
#ifndef _HANDLER_H
#define _HANDLER_H

#include "scope.h"

namespace ledger {

template <typename T>
class handler_t
{
  const char * name;
  std::size_t  name_len;
  const char   ch;
  bool	       handled;

public:
  T *          parent;
  value_t      value;
  bool	       wants_arg;

  handler_t(const char * _name, const char _ch = '\0')
    : name(_name), name_len(std::strlen(name)), ch(_ch),
      handled(false), parent(NULL), value(),
      wants_arg(name[name_len - 1] == '_') {
    TRACE_CTOR(handler_t, "const char *, const char");
  }
  handler_t(const handler_t& other)
    : name(other.name),
      name_len(other.name_len),
      ch(other.ch),
      handled(other.handled),
      parent(NULL),
      value(other.value)
  {
    TRACE_CTOR(handler_t, "copy");
  }

  virtual ~handler_t() {
    TRACE_DTOR(handler_t);
  }

  string desc() const {
    std::ostringstream out;
    if (ch)
      out << "--" << name << " (-" << ch << ")";
    else
      out << "--" << name;
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
    return value.as_string_lval();
  }

  void on() {
    handled = true;
  }
  void on(const char * str) {
    handled = true;
    value   = string_value(str);
  }
  void on(const string& str) {
    handled = true;
    value   = string_value(str);
  }
  void on(const value_t& val) {
    handled = true;
    value   = val;
  }

  void off() {
    handled = false;
    value   = value_t();
  }

  virtual void handler(call_scope_t& args) {
    if (wants_arg)
      value = args[0];
  }

  virtual value_t operator()(call_scope_t& args) {
    handled = true;
    handler(args);
    return true;
  }
};

#define BEGIN(type, name)				\
  struct name ## _handler_t : public handler_t<type>

#define CTOR(type, name)				\
  name ## _handler_t() : handler_t<type>(#name)
#define DECL1(type, name, vartype, var, value)		\
  vartype var ;						\
  name ## _handler_t() : handler_t<type>(#name), var(value)
  
#define HELP(var) virtual void help(std::ostream& var)
#define DO()      virtual void handler(call_scope_t&)
#define DO_(var)  virtual void handler(call_scope_t& var)

#define END(name) name ## _handler

#define COPY_OPT(name, other) name ## _handler(other.name ## _handler)

#define CALL_FUNCTOR(x) \
  expr_t::op_t::wrap_functor(bind(&x ## _t::operator(), &x, _1))

inline bool optcmp(const char * p, const char * n) {
  // Test whether p matches n, substituting - in p for _ in n.
  for (; *p && *n; p++, n++) {
    if (! (*p == '-' && *n == '_' ) && *p != *n)
      return false;
  }
  return *p == *n;
}

#define OPT(name)					\
  if (optcmp(p, #name))					\
    return ((name ## _handler).parent = this,		\
	    CALL_FUNCTOR(name ## _handler))

#define OPT_(name)					\
  if (! *(p + 1) ||					\
      ((name ## _handler).wants_arg &&			\
       *(p + 1) == '_' && ! *(p + 2)) ||		\
      optcmp(p, #name))					\
    return ((name ## _handler).parent = this,		\
	    CALL_FUNCTOR(name ## _handler))

#define OPT_CH(name)					\
  if (! *(p + 1) ||					\
      ((name ## _handler).wants_arg &&			\
       *(p + 1) == '_' && ! *(p + 2)))			\
    return ((name ## _handler).parent = this,		\
	    CALL_FUNCTOR(name ## _handler))

#define FUNCTION(name)					\
  if (std::strcmp(p, #name) == 0)			\
    return WRAP_FUNCTOR(fn_ ## name)

#define METHOD(type, name)				\
  if (std::strcmp(p, #name) == 0)			\
    return MAKE_FUNCTOR(type::fn_ ## name)

#define M_COMMAND(type, name)				\
  if (std::strcmp(p, #name) == 0)			\
    return MAKE_FUNCTOR(type::name ## _command)

#define COMMAND(name)					\
  if (std::strcmp(p, #name) == 0)			\
    return WRAP_FUNCTOR(name ## _command)

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

} // namespace ledger

#endif // _HANDLER_H
