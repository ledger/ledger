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

#include "expr.h"
#include "parser.h"
#include "op.h"

namespace ledger {

expr_t::expr_t() : context(NULL), compiled(false)
{
  TRACE_CTOR(expr_t, "");
}

expr_t::expr_t(const expr_t& other)
  : ptr(other.ptr), context(other.context), str(other.str), compiled(false)
{
  TRACE_CTOR(expr_t, "copy");
}

expr_t::expr_t(const string& _str, const uint_least8_t flags)
  : context(NULL), str(_str), compiled(false)
{
  TRACE_CTOR(expr_t, "const string&");
  if (! _str.empty())
    parse(str, flags);
}

expr_t::expr_t(std::istream& in, const uint_least8_t flags)
  : context(NULL), compiled(false)
{
  TRACE_CTOR(expr_t, "std::istream&");
  parse(in, flags);
}

expr_t::expr_t(const ptr_op_t& _ptr, scope_t * _context, const string& _str)
  : ptr(_ptr), context(_context), str(_str), compiled(false)
{
  TRACE_CTOR(expr_t, "const ptr_op_t&, scope_t *, const string&");
}

expr_t::~expr_t() throw()
{
  TRACE_DTOR(expr_t);
}

expr_t::ptr_op_t expr_t::get_op() throw()
{
  return ptr;
}

expr_t& expr_t::operator=(const expr_t& _expr)
{
  if (this != &_expr) {
    str	     = _expr.str;
    ptr	     = _expr.ptr;
    context  = _expr.context;
    compiled = _expr.compiled;
  }
  return *this;
}

void expr_t::parse(const string& _str, const uint32_t flags)
{
  parser_t parser;
  str	   = _str;
  ptr	   = parser.parse(str, flags);
  context  = NULL;
  compiled = false;
}

void expr_t::parse(std::istream& in, const uint32_t flags,
		   const string * original_string)
{
  parser_t parser;
  str	   = "<stream>";
  ptr	   = parser.parse(in, flags, original_string);
  context  = NULL;
  compiled = false;
}

void expr_t::recompile(scope_t& scope)
{
  if (ptr.get()) {
    ptr	     = ptr->compile(scope);
    context  = &scope;
    compiled = true;
  }
}

void expr_t::compile(scope_t& scope)
{
  if (! compiled)
    recompile(scope);
}

value_t expr_t::calc(scope_t& scope)
{
  if (ptr.get()) {
    if (! compiled) {
      if (SHOW_DEBUG("expr.compile")) {
	DEBUG("expr.compile", "Before compilation:");
	dump(*_log_stream);
      }

      compile(scope);

      if (SHOW_DEBUG("expr.compile")) {
	DEBUG("expr.compile", "After compilation:");
	dump(*_log_stream);
      }
    }

    ptr_op_t locus;
    try {
      return ptr->calc(scope, &locus);
    }
    catch (const std::exception& err) {
      if (locus) {
	add_error_context(_("While evaluating value expression:"));
	add_error_context(op_context(ptr, locus));
      }
      throw;
    }
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
  if (ptr)
    ptr->print(out);
}

void expr_t::dump(std::ostream& out) const
{
  if (ptr) ptr->dump(out, 0);
}

std::ostream& operator<<(std::ostream& out, const expr_t& expr) {
  expr.print(out);
  return out;
}

string expr_context(const expr_t& expr)
{
  return expr ? op_context(expr.ptr) : _("<empty expression>");
}

} // namespace ledger
