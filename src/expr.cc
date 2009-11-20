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

#include <system.hh>

#include "expr.h"
#include "parser.h"

namespace ledger {

void expr_t::parse(std::istream& in, const parse_flags_t& flags,
		   const optional<string>& original_string)
{
  base_type::parse(in, flags, original_string);

  parser_t parser;
  ptr = parser.parse(in, flags, original_string);
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
    catch (const std::exception& err) {
      if (locus) {
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

} // namespace ledger
