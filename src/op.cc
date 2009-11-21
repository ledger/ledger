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

#include "op.h"
#include "scope.h"
#include "commodity.h"
#include "pool.h"

namespace ledger {

expr_t::ptr_op_t expr_t::op_t::compile(scope_t& scope, const int depth)
{
  if (is_ident()) {
    DEBUG("expr.compile", "lookup: " << as_ident());

    if (ptr_op_t def = scope.lookup(symbol_t::FUNCTION, as_ident())) {
      // Identifier references are first looked up at the point of
      // definition, and then at the point of every use if they could
      // not be found there.
      if (SHOW_DEBUG("expr.compile")) {
	DEBUG("expr.compile", "Found definition:");
	def->dump(*_log_stream, 0);
      }
      return copy(def);
    }
    else if (left()) {
      return copy();
    }
    return this;
  }

  if (kind < TERMINALS)
    return this;

  if (kind == O_DEFINE) {
    switch (left()->kind) {
    case IDENT:
      scope.define(symbol_t::FUNCTION, left()->as_ident(), right());
      break;
    case O_CALL:
      if (left()->left()->is_ident())
	scope.define(symbol_t::FUNCTION, left()->left()->as_ident(), this);
      else
	throw_(compile_error, _("Invalid function definition"));
      break;
    default:
      throw_(compile_error, _("Invalid function definition"));
    }
    return wrap_value(value_t());
  }

  ptr_op_t lhs(left()->compile(scope, depth));
  ptr_op_t rhs(kind > UNARY_OPERATORS && has_right() ?
	       (kind == O_LOOKUP ? right() :
		right()->compile(scope, depth)) : NULL);

  if (lhs == left() && (! rhs || rhs == right()))
    return this;

  ptr_op_t intermediate(copy(lhs, rhs));

  // Reduce constants immediately if possible
  if ((! lhs || lhs->is_value()) && (! rhs || rhs->is_value()))
    return wrap_value(intermediate->calc(scope, NULL, depth));

  return intermediate;
}

value_t expr_t::op_t::calc(scope_t& scope, ptr_op_t * locus, const int depth)
{
#if defined(DEBUG_ON)
  bool skip_debug = false;
#endif
  try {

  value_t result;

  switch (kind) {
  case VALUE:
    result = as_value();
    break;

  case IDENT: {
    if (! left())
      throw_(calc_error, _("Unknown identifier '%1'") << as_ident());

    // Evaluating an identifier is the same as calling its definition
    // directly, so we create an empty call_scope_t to reflect the scope for
    // this implicit call.
    call_scope_t call_args(scope);
    result = left()->compile(call_args, depth + 1)
                   ->calc(call_args, locus, depth + 1);
    break;
  }

  case FUNCTION: {
    // Evaluating a FUNCTION is the same as calling it directly; this happens
    // when certain functions-that-look-like-variables (such as "amount") are
    // resolved.
    call_scope_t call_args(scope);
    result = as_function()(call_args);
#if defined(DEBUG_ON)
    skip_debug = true;
#endif
    break;
  }

  case O_DEFINE: {
    call_scope_t&  call_args(downcast<call_scope_t>(scope));
    std::size_t	   args_count = call_args.size();
    std::size_t	   args_index = 0;

    assert(left()->kind == O_CALL);

    for (ptr_op_t sym = left()->right();
	 sym;
	 sym = sym->has_right() ? sym->right() : NULL) {
      ptr_op_t varname = sym;
      if (sym->kind == O_CONS)
	varname = sym->left();

      if (! varname->is_ident())
	throw_(calc_error, _("Invalid function definition"));
      else if (args_index == args_count)
	scope.define(symbol_t::FUNCTION, varname->as_ident(),
		     wrap_value(false));
      else
	scope.define(symbol_t::FUNCTION, varname->as_ident(),
		     wrap_value(call_args[args_index++]));
    }

    if (args_index < args_count)
      throw_(calc_error,
	     _("Too many arguments in function call (saw %1)") << args_count);

    result = right()->calc(scope, locus, depth + 1);
    break;
  }

  case O_LOOKUP:
    if (value_t obj = left()->calc(scope, locus, depth + 1)) {
      if (obj.is_scope()) {
	if (obj.as_scope() == NULL) {
	  throw_(calc_error, _("Left operand of . operator is NULL"));
	} else {
	  scope_t& objscope(*obj.as_scope());
	  if (ptr_op_t member =
	      objscope.lookup(symbol_t::FUNCTION, right()->as_ident())) {
	    result = member->calc(objscope, NULL, depth + 1);
	    break;
	  }
	}
      }
    }
    if (right()->kind != IDENT)
      throw_(calc_error,
	     _("Right operand of . operator must be an identifier"));
    else
      throw_(calc_error,
	     _("Failed to lookup member '%1'") << right()->as_ident());
    break;

  case O_CALL: {
    call_scope_t call_args(scope);

    if (has_right())
      call_args.set_args(right()->calc(scope, locus, depth + 1));

    ptr_op_t func = left();
    const string& name(func->as_ident());

    func = func->left();
    if (! func)
      throw_(calc_error, _("Calling unknown function '%1'") << name);

    if (func->is_function())
      result = func->as_function()(call_args);
    else
      result = func->calc(call_args, locus, depth + 1);
    break;
  }

  case O_MATCH:
    result = (right()->calc(scope, locus, depth + 1).as_mask()
	      .match(left()->calc(scope, locus, depth + 1).to_string()));
    break;

  case O_EQ:
    result = (left()->calc(scope, locus, depth + 1) ==
	      right()->calc(scope, locus, depth + 1));
    break;
  case O_LT:
    result = (left()->calc(scope, locus, depth + 1) <
	      right()->calc(scope, locus, depth + 1));
    break;
  case O_LTE:
    result = (left()->calc(scope, locus, depth + 1) <=
	      right()->calc(scope, locus, depth + 1));
    break;
  case O_GT:
    result = (left()->calc(scope, locus, depth + 1) >
	      right()->calc(scope, locus, depth + 1));
    break;
  case O_GTE:
    result = (left()->calc(scope, locus, depth + 1) >=
	      right()->calc(scope, locus, depth + 1));
    break;

  case O_ADD:
    result = (left()->calc(scope, locus, depth + 1) +
	      right()->calc(scope, locus, depth + 1));
    break;
  case O_SUB:
    result = (left()->calc(scope, locus, depth + 1) -
	      right()->calc(scope, locus, depth + 1));
    break;
  case O_MUL:
    result = (left()->calc(scope, locus, depth + 1) *
	      right()->calc(scope, locus, depth + 1));
    break;
  case O_DIV:
    result = (left()->calc(scope, locus, depth + 1) /
	      right()->calc(scope, locus, depth + 1));
    break;

  case O_NEG:
    result = left()->calc(scope, locus, depth + 1).negated();
    break;

  case O_NOT:
    result = ! left()->calc(scope, locus, depth + 1);
    break;

  case O_AND:
    if (left()->calc(scope, locus, depth + 1))
      result = right()->calc(scope, locus, depth + 1);
    else
      result = false;
    break;

  case O_OR:
    if (value_t temp = left()->calc(scope, locus, depth + 1))
      result = temp;
    else
      result = right()->calc(scope, locus, depth + 1);
    break;

  case O_QUERY:
    assert(right());
    assert(right()->kind == O_COLON);

    if (value_t temp = left()->calc(scope, locus, depth + 1))
      result = right()->left()->calc(scope, locus, depth + 1);
    else
      result = right()->right()->calc(scope, locus, depth + 1);
    break;

  case O_COLON:
    assert(! "We should never calculate an O_COLON operator");
    break;

  case O_CONS:
    result = left()->calc(scope, locus, depth + 1);
    DEBUG("op.cons", "car = " << result);

    if (has_right()) {
      value_t temp;
      temp.push_back(result);

      ptr_op_t next = right();
      while (next) {
	ptr_op_t value_op;
	if (next->kind == O_CONS) {
	  value_op = next->left();
	  next     = next->right();
	} else {
	  value_op = next;
	  next     = NULL;
	}
	temp.push_back(value_op->calc(scope, locus, depth + 1));
	DEBUG("op.cons", "temp now = " << temp);
      }
      result = temp;
    }
    break;

  case O_SEQ: {
    symbol_scope_t seq_scope(scope);

    // An O_SEQ is very similar to an O_CONS except that only the last result
    // value in the series is kept.  O_CONS builds up a list.
    //
    // Another feature of O_SEQ is that it pushes a new symbol scope onto the
    // stack.
    result = left()->calc(seq_scope, locus, depth + 1);

    if (has_right()) {
      ptr_op_t next = right();
      while (next) {
	ptr_op_t value_op;
	if (next->kind == O_SEQ) {
	  value_op = next->left();
	  next     = next->right();
	} else {
	  value_op = next;
	  next     = NULL;
	}
	result = value_op->calc(seq_scope, locus, depth + 1);
      }
    }
    break;
  }

  case LAST:
  default:
    assert(false);
    break;
  }

#if defined(DEBUG_ON)
  if (! skip_debug && SHOW_DEBUG("expr.calc")) {
    for (int i = 0; i < depth; i++)
      ledger::_log_buffer << '.';
    ledger::_log_buffer << op_context(this) <<  " => ";
    result.dump(ledger::_log_buffer, true);
    DEBUG("expr.calc", "");
  }
#endif

  return result;

  }
  catch (const std::exception& err) { 
    if (locus && ! *locus)
      *locus = this;
    throw;
  }
}

namespace {
  bool print_cons(std::ostream& out, const expr_t::const_ptr_op_t op,
		  const expr_t::op_t::context_t& context)
  {
    bool found = false;

    assert(op->left());
    if (op->left()->print(out, context))
      found = true;

    if (op->has_right()) {
      out << ", ";
      if (op->right()->kind == expr_t::op_t::O_CONS)
	found = print_cons(out, op->right(), context);
      else if (op->right()->print(out, context))
	found = true;
    }
    return found;
  }

  bool print_seq(std::ostream& out, const expr_t::const_ptr_op_t op,
		 const expr_t::op_t::context_t& context)
  {
    bool found = false;

    assert(op->left());
    if (op->left()->print(out, context))
      found = true;

    if (op->has_right()) {
      out << "; ";

      if (op->right()->kind == expr_t::op_t::O_CONS)
	found = print_cons(out, op->right(), context);
      else if (op->right()->print(out, context))
	found = true;
    }

    return found;
  }
}

bool expr_t::op_t::print(std::ostream& out, const context_t& context) const
{
  bool found = false;

  if (context.start_pos && this == context.op_to_find) {
    *context.start_pos = out.tellp();
    *context.start_pos -= 1;
    found = true;
  }

  string symbol;

  switch (kind) {
  case VALUE:
    as_value().dump(out, context.relaxed);
    break;

  case IDENT:
    out << as_ident();
    break;

  case FUNCTION:
    out << "<FUNCTION>";
    break;

  case O_NOT:
    out << "!(";
    if (left() && left()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_NEG:
    out << "-(";
    if (left() && left()->print(out, context))
      found = true;
    out << ")";
    break;

  case O_ADD:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " + ";
    if (has_right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_SUB:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " - ";
    if (has_right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_MUL:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " * ";
    if (has_right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_DIV:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " / ";
    if (has_right() && right()->print(out, context))
      found = true;
    out << ")";
    break;

  case O_EQ:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " == ";
    if (has_right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_LT:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " < ";
    if (has_right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_LTE:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " <= ";
    if (has_right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_GT:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " > ";
    if (has_right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_GTE:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " >= ";
    if (has_right() && right()->print(out, context))
      found = true;
    out << ")";
    break;

  case O_AND:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " & ";
    if (has_right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_OR:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " | ";
    if (has_right() && right()->print(out, context))
      found = true;
    out << ")";
    break;

  case O_QUERY:
    if (left() && left()->print(out, context))
      found = true;
    out << " ? ";
    if (has_right() && right()->print(out, context))
      found = true;
    break;

  case O_COLON:
    if (left() && left()->print(out, context))
      found = true;
    out << " : ";
    if (has_right() && right()->print(out, context))
      found = true;
    break;

  case O_CONS:
    found = print_cons(out, this, context);
    break;

  case O_SEQ:
    out << "(";
    found = print_seq(out, this, context);
    out << ")";
    break;

  case O_DEFINE:
    if (left() && left()->print(out, context))
      found = true;
    out << " := ";
    if (has_right() && right()->print(out, context))
      found = true;
    break;

  case O_LOOKUP:
    if (left() && left()->print(out, context))
      found = true;
    out << ".";
    if (has_right() && right()->print(out, context))
      found = true;
    break;

  case O_CALL:
    if (left() && left()->print(out, context))
      found = true;
    if (has_right()) {
      if (right()->kind == O_SEQ) {
	if (right()->print(out, context))
	  found = true;
      } else {
	out << "(";
	if (has_right() && right()->print(out, context))
	  found = true;
	out << ")";
      }
    } else {
      out << "()";
    }
    break;

  case O_MATCH:
    if (left() && left()->print(out, context))
      found = true;
    out << " =~ ";
    if (has_right() && right()->print(out, context))
      found = true;
    break;

  case LAST:
  default:
    assert(false);
    break;
  }

  if (! symbol.empty()) {
    if (commodity_pool_t::current_pool->find(symbol))
      out << '@';
    out << symbol;
  }

  if (context.end_pos && this == context.op_to_find) {
    *context.end_pos = out.tellp();
    *context.end_pos -= 1;
  }

  return found;
}

void expr_t::op_t::dump(std::ostream& out, const int depth) const
{
  out.setf(std::ios::left);
  out.width((sizeof(void *) * 2) + 2);
  out << this;

  for (int i = 0; i < depth; i++)
    out << " ";

  switch (kind) {
  case VALUE:
    out << "VALUE: ";
    as_value().dump(out);
    break;

  case IDENT:
    out << "IDENT: " << as_ident();
    break;

  case FUNCTION:
    out << "FUNCTION";
    break;

  case O_DEFINE: out << "O_DEFINE"; break;
  case O_LOOKUP: out << "O_LOOKUP"; break;
  case O_CALL:	 out << "O_CALL"; break;
  case O_MATCH:	 out << "O_MATCH"; break;

  case O_NOT:	 out << "O_NOT"; break;
  case O_NEG:	 out << "O_NEG"; break;

  case O_ADD:	 out << "O_ADD"; break;
  case O_SUB:	 out << "O_SUB"; break;
  case O_MUL:	 out << "O_MUL"; break;
  case O_DIV:	 out << "O_DIV"; break;

  case O_EQ:	 out << "O_EQ"; break;
  case O_LT:	 out << "O_LT"; break;
  case O_LTE:	 out << "O_LTE"; break;
  case O_GT:	 out << "O_GT"; break;
  case O_GTE:	 out << "O_GTE"; break;

  case O_AND:	 out << "O_AND"; break;
  case O_OR:	 out << "O_OR"; break;

  case O_QUERY:  out << "O_QUERY"; break;
  case O_COLON:	 out << "O_COLON"; break;

  case O_CONS:	 out << "O_CONS"; break;
  case O_SEQ:	 out << "O_SEQ"; break;

  case LAST:
  default:
    assert(false);
    break;
  }

  out << " (" << refc << ')' << std::endl;

  // An identifier is a special non-terminal, in that its left() can
  // hold the compiled definition of the identifier.
  if (kind > TERMINALS || is_ident()) {
    if (left()) {
      left()->dump(out, depth + 1);
      if (kind > UNARY_OPERATORS && has_right())
	right()->dump(out, depth + 1);
    }
    else if (kind > UNARY_OPERATORS) {
      assert(! has_right());
    }
  }
}

string op_context(const expr_t::ptr_op_t op,
		  const expr_t::ptr_op_t locus)
{
  ostream_pos_type start_pos, end_pos;
  expr_t::op_t::context_t context(op, locus, &start_pos, &end_pos);
  std::ostringstream buf;
  buf << "  ";
  if (op->print(buf, context)) {
    buf << "\n";
    for (int i = 0; i <= end_pos; i++) {
      if (i > start_pos)
	buf << "^";
      else
	buf << " ";
    }
  }
  return buf.str();
}

} // namespace ledger
