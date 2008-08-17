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

#include "expr.h"
#include "parser.h"
#include "op.h"

namespace ledger {

std::auto_ptr<expr_t::parser_t> expr_t::parser;

expr_t::expr_t() : compiled(false)
{
  TRACE_CTOR(expr_t, "");
}

expr_t::expr_t(const expr_t& other)
  : ptr(other.ptr), str(other.str), compiled(other.compiled)
{
  TRACE_CTOR(expr_t, "copy");
}

expr_t::expr_t(const string& _str, const unsigned int flags)
  : str(_str), compiled(false)
{
  TRACE_CTOR(expr_t, "const string&");

  if (! _str.empty())
    ptr = parser->parse(str, flags);
}

expr_t::expr_t(std::istream& in, const unsigned int flags)
  : compiled(false)
{
  TRACE_CTOR(expr_t, "std::istream&");

  ptr = parser->parse(in, flags);
}

expr_t::expr_t(const ptr_op_t& _ptr, const string& _str)
  : ptr(_ptr), str(_str), compiled(false)
{
  TRACE_CTOR(expr_t, "const ptr_op_t&, const string&");
}

expr_t::~expr_t() throw()
{
  TRACE_DTOR(expr_t);
}

expr_t& expr_t::operator=(const expr_t& _expr)
{
  if (this != &_expr) {
    str	     = _expr.str;
    ptr	     = _expr.ptr;
    compiled = _expr.compiled;
  }
  return *this;
}

void expr_t::parse(const string& _str, const unsigned int flags)
{
  if (! parser.get())
    throw_(parse_error, "Value expression parser not initialized");

  str	   = _str;
  ptr	   = parser->parse(str, flags);
  compiled = false;
}

void expr_t::parse(std::istream& in, const unsigned int flags)
{
  if (! parser.get())
    throw_(parse_error, "Value expression parser not initialized");

  str	   = "<stream>";
  ptr	   = parser->parse(in, flags);
  compiled = false;
}

void expr_t::compile(scope_t& scope)
{
  if (ptr.get() && ! compiled) {
    ptr	     = ptr->compile(scope);
    compiled = true;
  }
}

value_t expr_t::calc(scope_t& scope)
{
  if (ptr.get()) {
    if (! compiled)
      compile(scope);
    return ptr->calc(scope);
  }
  return NULL_VALUE;
}

bool expr_t::is_constant() const
{
  assert(compiled);
  return ptr.get() && ptr->is_value();
}

bool expr_t::is_function() const
{
  assert(compiled);
  return ptr.get() && ptr->is_function();
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

function_t& expr_t::get_function()
{
  assert(is_function());
  return ptr->as_function_lval();
}

value_t expr_t::eval(const string& _expr, scope_t& scope)
{
  return expr_t(_expr).calc(scope);
}

void expr_t::print(std::ostream& out) const
{
  if (ptr) {
    op_t::print_context_t context;
    ptr->print(out, context);
  }
}

void expr_t::dump(std::ostream& out) const
{
  if (ptr) ptr->dump(out, 0);
}

void expr_t::read(const char *& data)
{
  if (ptr) ptr->read(data);
}

void expr_t::write(std::ostream& out) const
{
  if (ptr) ptr->write(out);
}

void expr_t::initialize()
{
  parser.reset(new expr_t::parser_t);
}

void expr_t::shutdown()
{
  parser.reset();
}

std::ostream& operator<<(std::ostream& out, const expr_t& expr) {
  expr.print(out);
  return out;
}

} // namespace ledger
