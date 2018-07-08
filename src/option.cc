/*
 * Copyright (c) 2003-2018, John Wiegley.  All rights reserved.
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

#include <system.hh>

#include "option.h"

namespace ledger {

namespace {
  typedef std::pair<expr_t::ptr_op_t, bool> op_bool_tuple;

  op_bool_tuple find_option(scope_t& scope, const string& name)
  {
    char buf[128];
    char * p = buf;
    foreach (char ch, name) {
      if (ch == '-')
        *p++ = '_';
      else
        *p++ = ch;
    }
    *p++ = '_';
    *p = '\0';

    if (expr_t::ptr_op_t op = scope.lookup(symbol_t::OPTION, buf))
      return op_bool_tuple(op, true);

    *--p = '\0';

    return op_bool_tuple(scope.lookup(symbol_t::OPTION, buf), false);
  }

  op_bool_tuple find_option(scope_t& scope, const char letter)
  {
    char buf[4];
    buf[0] = letter;
    buf[1] = '_';
    buf[2] = '\0';

    if (expr_t::ptr_op_t op = scope.lookup(symbol_t::OPTION, buf))
      return op_bool_tuple(op, true);

    buf[1] = '\0';

    return op_bool_tuple(scope.lookup(symbol_t::OPTION, buf), false);
  }

  void process_option(const string& whence, const expr_t::func_t& opt,
                      scope_t& scope, const char * arg, const string& name)
  {
    try {
      call_scope_t args(scope);

      args.push_back(string_value(whence));
      if (arg)
        args.push_back(string_value(arg));

      opt(args);
    }
    catch (const std::exception&) {
      if (name[0] == '-')
        add_error_context(_f("While parsing option '%1%'") % name);
      else
        add_error_context(_f("While parsing environent variable '%1%'") % name);
      throw;
    }
  }
}

bool process_option(const string& whence, const string& name, scope_t& scope,
                    const char * arg, const string& varname)
{
  op_bool_tuple opt(find_option(scope, name));
  if (opt.first) {
    process_option(whence, opt.first->as_function(), scope, arg, varname);
    return true;
  }
  return false;
}

void process_environment(const char ** envp, const string& tag,
                         scope_t& scope)
{
  const char *      tag_p   = tag.c_str();
  string::size_type tag_len = tag.length();

  assert(tag_p);
  assert(tag_len > 0);

  for (const char ** p = envp; *p; p++) {
    if (std::strlen(*p) >= tag_len && std::strncmp(*p, tag_p, tag_len) == 0) {
      char   buf[8192];
      char * r = buf;
      const char * q;
      for (q = *p + tag_len;
           *q && *q != '=' && r - buf < 8191;
           q++)
        if (*q == '_')
          *r++ = '-';
        else
          *r++ = static_cast<char>(std::tolower(*q));
      *r = '\0';

      if (*q == '=') {
        try {
          string value = string(*p, static_cast<std::string::size_type>(q - *p));
          if (! value.empty())
            process_option(string("$") + buf, string(buf), scope, q + 1, value);
        }
        catch (const std::exception&) {
          add_error_context(_f("While parsing environment variable option '%1%':")
                            % *p);
          throw;
        }
      }
    }
  }
}

namespace {
  struct op_bool_char_tuple {
    expr_t::ptr_op_t op;
    bool truth;
    char ch;

    op_bool_char_tuple(expr_t::ptr_op_t _op, bool _truth, char _ch)
      : op(_op), truth(_truth), ch(_ch) {}
  };
}

strings_list process_arguments(strings_list args, scope_t& scope)
{
  bool anywhere = true;

  strings_list remaining;

  for (strings_list::iterator i = args.begin();
       i != args.end();
       i++) {
    DEBUG("option.args", "Examining argument '" << *i << "'");

    if (! anywhere || (*i)[0] != '-') {
      DEBUG("option.args", "  adding to list of real args");
      remaining.push_back(*i);
      continue;
    }

    // --long-option or -s
    if ((*i)[1] == '-') {
      if ((*i)[2] == '\0') {
        DEBUG("option.args", "  it's a --, ending options processing");
        anywhere = false;
        continue;
      }

      DEBUG("option.args", "  it's an option string");

      string       opt_name;
      const char * name  = (*i).c_str() + 2;
      const char * value = NULL;

      if (const char * p = std::strchr(name, '=')) {
        opt_name = string(name, static_cast<std::string::size_type>(p - name));
        value = ++p;
        DEBUG("option.args", "  read option value from option: " << value);
      } else {
        opt_name = name;
      }

      op_bool_tuple opt(find_option(scope, opt_name));
      if (! opt.first)
        throw_(option_error, _f("Illegal option --%1%") % name);

      if (opt.second && ! value && ++i != args.end() && value == NULL) {
        value = (*i).c_str();
        DEBUG("option.args", "  read option value from arg: " << value);
        if (value == NULL)
          throw_(option_error, _f("Missing option argument for --%1%") % name);
      }
      process_option(string("--") + name,
                     opt.first->as_function(), scope, value,
                     string("--") + name);
    }
    else if ((*i)[1] == '\0') {
      throw_(option_error, _f("illegal option -%1%") % (*i)[0]);
    }
    else {
      DEBUG("option.args", "  single-char option");

      std::list<op_bool_char_tuple> option_queue;

      std::string::size_type x = 1;
      for (char c = (*i)[x]; c != '\0'; x++, c = (*i)[x]) {
        op_bool_tuple opt(find_option(scope, c));
        if (! opt.first)
          throw_(option_error, _f("Illegal option -%1%") % c);

        option_queue.push_back(op_bool_char_tuple(opt.first, opt.second, c));
      }

      foreach (op_bool_char_tuple& o, option_queue) {
        const char * value = NULL;
        if (o.truth && ++i != args.end()) {
          value = (*i).c_str();
          DEBUG("option.args", "  read option value from arg: " << value);
          if (value == NULL)
            throw_(option_error,
                   _f("Missing option argument for -%1%") % o.ch);
        }
        process_option(string("-") + o.ch, o.op->as_function(), scope, value,
                       string("-") + o.ch);
      }
    }
  }

  return remaining;
}

} // namespace ledger
