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

#include "expr.h"
#include "parser.h"
#include "scope.h"

namespace ledger {

expr_t::expr_t() : base_type()
{
  TRACE_CTOR(expr_t, "");
}

expr_t::expr_t(const expr_t& other) : base_type(other), ptr(other.ptr)
{
  TRACE_CTOR(expr_t, "copy");
}
expr_t::expr_t(ptr_op_t _ptr, scope_t * _context)
  : base_type(_context), ptr(_ptr)
{
  TRACE_CTOR(expr_t, "const ptr_op_t&, scope_t *");
}

expr_t::expr_t(const string& _str, const parse_flags_t& flags)
  : base_type()
{
  if (! _str.empty())
    parse(_str, flags);
  TRACE_CTOR(expr_t, "string, parse_flags_t");
}

expr_t::expr_t(std::istream& in, const parse_flags_t& flags)
  : base_type()
{
  parse(in, flags);
  TRACE_CTOR(expr_t, "std::istream&, parse_flags_t");
}

expr_t::~expr_t() {
  TRACE_DTOR(expr_t);
}

expr_t& expr_t::operator=(const expr_t& _expr)
{
  if (this != &_expr) {
    base_type::operator=(_expr);
    ptr = _expr.ptr;
  }
  return *this;
}

expr_t::operator bool() const throw()
{
  return ptr.get() != NULL;
}

expr_t::ptr_op_t expr_t::get_op() throw()
{
  return ptr;
}

void expr_t::parse(std::istream& in, const parse_flags_t& flags,
                   const optional<string>& original_string)
{
  parser_t parser;
  istream_pos_type start_pos = in.tellg();
  ptr = parser.parse(in, flags, original_string);
  istream_pos_type end_pos = in.tellg();

  if (original_string) {
    set_text(*original_string);
  }
  else if (end_pos > start_pos) {
    in.clear();
    in.seekg(start_pos, std::ios::beg);
    scoped_array<char> buf
      (new char[static_cast<std::size_t>(end_pos - start_pos) + 1]);
    int len = static_cast<int>(end_pos) - static_cast<int>(start_pos);
    in.read(buf.get(), len);
    buf[len] = '\0';
    set_text(buf.get());
  }
  else {
    set_text("<stream>");
  }
}

void expr_t::compile(scope_t& scope)
{
  if (! compiled && ptr) {
    ptr = ptr->compile(scope);
    base_type::compile(scope);
  }
}

value_t expr_t::real_calc(scope_t& scope)
{
  if (ptr) {
    ptr_op_t locus;
    try {
      return ptr->calc(scope, &locus);
    }
    catch (const std::exception&) {
      if (locus) {
        string current_context = error_context();

        add_error_context(_("While evaluating value expression:"));
        add_error_context(op_context(ptr, locus));

        if (SHOW_INFO()) {
          add_error_context(_("The value expression tree was:"));
          std::ostringstream buf;
          ptr->dump(buf, 0);

          std::istringstream in(buf.str());
          std::ostringstream out;
          char linebuf[1024];
          bool first = true;
          while (in.good() && ! in.eof()) {
            in.getline(linebuf, 1023);
            std::streamsize len = in.gcount();
            if (len > 0) {
              if (first)
                first = false;
              else
                out << '\n';
              out << "  " << linebuf;
            }
          }
          add_error_context(out.str());
        }

        if (! current_context.empty())
          add_error_context(current_context);
      }
      throw;
    }
  }
  return NULL_VALUE;
}

bool expr_t::is_constant() const
{
  assert(compiled);
  return ptr && ptr->is_value();
}

bool expr_t::is_function() const
{
  assert(compiled);
  return ptr && ptr->is_function();
}

value_t& expr_t::constant_value()
{
  assert(is_constant());
  return ptr->as_value_lval();
}

const value_t& expr_t::constant_value() const
{
  assert(is_constant());
  return ptr->as_value();
}

expr_t::func_t& expr_t::get_function()
{
  assert(is_function());
  return ptr->as_function_lval();
}

string expr_t::context_to_str() const
{
  return ptr ? op_context(ptr) : _("<empty expression>");
}

void expr_t::print(std::ostream& out) const
{
  if (ptr)
    ptr->print(out);
}

void expr_t::dump(std::ostream& out) const
{
  if (ptr) ptr->dump(out, 0);
}

bool merged_expr_t::check_for_single_identifier(const string& expr)
{
  bool single_identifier = true;
  for (const char * p = expr.c_str(); *p; ++p)
    if (! std::isalnum(*p) || *p == '_') {
      single_identifier = false;
      break;
    }

  if (single_identifier) {
    set_base_expr(expr);
    exprs.clear();
    return true;
  } else {
    return false;
  }
}

void merged_expr_t::compile(scope_t& scope)
{
  if (exprs.empty()) {
    parse(base_expr);
  } else {
    std::ostringstream buf;

    buf << "__tmp_" << term << "=(" << term << "=(" << base_expr << ")";
    foreach (const string& expr, exprs) {
      if (merge_operator == ";")
        buf << merge_operator << term << "=" << expr;
      else
        buf << merge_operator << "(" << expr << ")";
    }
    buf << ";" << term << ");__tmp_" << term;

    DEBUG("expr.merged.compile", "Compiled expr: " << buf.str());
    parse(buf.str());
  }

  expr_t::compile(scope);
}

expr_t::ptr_op_t as_expr(const value_t& val)
{
  VERIFY(val.is_any());
  return val.as_any<expr_t::ptr_op_t>();
}

void set_expr(value_t& val, expr_t::ptr_op_t op)
{
  val.set_any(op);
}

value_t expr_value(expr_t::ptr_op_t op)
{
  value_t temp;
  temp.set_any(op);
  return temp;
}

value_t source_command(call_scope_t& args)
{
  std::istream * in = NULL;
  scoped_ptr<ifstream> stream;
  string pathname;

  if (args.has(0)) {
    pathname = args.get<string>(0);
    stream.reset(new ifstream(path(pathname)));
    in = stream.get();
  } else {
    pathname = "<stdin>";
    in = &std::cin;
  }

  symbol_scope_t   file_locals(args);
  std::size_t      linenum = 0;
  char             buf[4096];
  istream_pos_type pos;

  while (in->good() && ! in->eof()) {
    pos = in->tellg();
    in->getline(buf, 4095);
    linenum++;

    char * p = skip_ws(buf);
    if (*p && *p != ';') {
      try {
        expr_t(p).calc(file_locals);
      }
      catch (const std::exception&) {
        add_error_context(_f("While parsing value expression on line %1%:")
                          % linenum);
        add_error_context(source_context(pathname, pos, in->tellg(), "> "));
      }
    }
  }

  return true;
}

} // namespace ledger
