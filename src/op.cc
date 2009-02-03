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

#include "op.h"
#include "scope.h"

namespace ledger {

expr_t::ptr_op_t expr_t::op_t::compile(scope_t& scope)
{
  if (kind == IDENT) {
    DEBUG("expr.compile", "Looking up identifier '" << as_ident() << "'");

    if (ptr_op_t def = scope.lookup(as_ident())) {
      // Identifier references are first looked up at the point of
      // definition, and then at the point of every use if they could
      // not be found there.
      if (SHOW_DEBUG("expr.compile")) {
	DEBUG("expr.compile", "Found definition:");
	def->dump(*_log_stream, 0);
      }
      return copy(def);
    }
    return this;
  }

  if (kind < TERMINALS)
    return this;

  ptr_op_t lhs(left()->compile(scope));
  ptr_op_t rhs(has_right() ? (kind == O_LOOKUP ?
			      right() : right()->compile(scope)) :
	       ptr_op_t());

  if (lhs == left() && (! rhs || rhs == right()))
    return this;

  ptr_op_t intermediate(copy(lhs, rhs));

  // Reduce constants immediately if possible
  if (lhs->is_value() && (! rhs || rhs->is_value()))
    return wrap_value(intermediate->calc(scope));

  return intermediate;
}

namespace {
  expr_t::ptr_op_t context_op_ptr;
}

value_t expr_t::op_t::calc(scope_t& scope)
{
  try {
    context_op_ptr = ptr_op_t();
    return opcalc(scope);
  }
  catch (const std::exception& err) {
    if (context_op_ptr) {
      add_error_context("While evaluating value expression:");
      add_error_context(op_context(this, context_op_ptr));
    }
    throw;
  }
}

value_t expr_t::op_t::opcalc(scope_t& scope)
{
  try {
  switch (kind) {
  case VALUE:
    return as_value();

  case IDENT:
    if (! left())
      throw_(calc_error, "Unknown identifier '" << as_ident() << "'");
    return left()->opcalc(scope);

  case FUNCTION: {
    // Evaluating a FUNCTION is the same as calling it directly; this happens
    // when certain functions-that-look-like-variables (such as "amount") are
    // resolved.
    call_scope_t call_args(scope);
    return as_function()(call_args);
  }

  case O_LOOKUP:
    if (left()->kind == IDENT &&
	left()->left() && left()->left()->kind == FUNCTION) {
      call_scope_t call_args(scope);
      if (value_t obj = left()->left()->as_function()(call_args)) {
	if (obj.is_pointer()) {
	  scope_t& objscope(obj.as_ref_lval<scope_t>());
	  if (ptr_op_t member = objscope.lookup(right()->as_ident()))
	    return member->calc(objscope);
	}
      }
    }
    if (right()->kind != IDENT) {
      throw_(calc_error,
	     "Right operand of . operator must be an identifier");
    } else {
      throw_(calc_error,
	     "Failed to lookup member '" << right()->as_ident() << "'");
    }
    break;

  case O_CALL: {
    call_scope_t call_args(scope);

    if (has_right())
      call_args.set_args(right()->opcalc(scope));

    ptr_op_t func = left();
    const string& name(func->as_ident());

    func = func->left();
    if (! func || func->kind != FUNCTION)
      throw_(calc_error, "Calling non-function '" << name << "'");

    return func->as_function()(call_args);
  }

  case O_MATCH:
#if 0
    if (! right()->is_value() || ! right()->as_value().is_mask())
      throw_(calc_error, "Right-hand argument to match operator must be a regex");
#endif
    return right()->opcalc(scope).as_mask().match(left()->opcalc(scope).to_string());

  case INDEX: {
    const call_scope_t& args(downcast<const call_scope_t>(scope));

    if (as_index() < args.size())
      return args[as_index()];
    else
      throw_(calc_error, "Reference to non-existing argument " << as_index());
    break;
  }

  case O_EQ:
    return left()->opcalc(scope) == right()->opcalc(scope);
  case O_LT:
    return left()->opcalc(scope) <  right()->opcalc(scope);
  case O_LTE:
    return left()->opcalc(scope) <= right()->opcalc(scope);
  case O_GT:
    return left()->opcalc(scope) >  right()->opcalc(scope);
  case O_GTE:
    return left()->opcalc(scope) >= right()->opcalc(scope);

  case O_ADD:
    return left()->opcalc(scope) + right()->opcalc(scope);
  case O_SUB:
    return left()->opcalc(scope) - right()->opcalc(scope);
  case O_MUL:
    return left()->opcalc(scope) * right()->opcalc(scope);
  case O_DIV:
    return left()->opcalc(scope) / right()->opcalc(scope);

  case O_NEG:
    return left()->opcalc(scope).negate();

  case O_NOT:
    return ! left()->opcalc(scope);

  case O_AND:
    return ! left()->opcalc(scope) ? value_t(false) : right()->opcalc(scope);

  case O_OR:
    if (value_t temp = left()->opcalc(scope))
      return temp;
    else
      return right()->opcalc(scope);

  case O_COMMA: {
    value_t result(left()->opcalc(scope));

    ptr_op_t next = right();
    while (next) {
      ptr_op_t value_op;
      if (next->kind == O_COMMA) {
	value_op = next->left();
	next     = next->right();
      } else {
	value_op = next;
	next     = NULL;
      }

      result.push_back(value_op->opcalc(scope));
    }
    return result;
  }

  case LAST:
  default:
    assert(false);
    break;
  }

  return NULL_VALUE;
  }
  catch (const std::exception& err) {
    if (! context_op_ptr)
      context_op_ptr = this;
    throw;
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
    as_value().print(out, context.relaxed);
    break;

  case IDENT:
    out << as_ident();
    break;

  case FUNCTION:
    out << "<FUNCTION>";
    break;

  case INDEX:
    out << '@' << as_index();
    break;

  case O_NOT:
    out << "!";
    if (left() && left()->print(out, context))
      found = true;
    break;
  case O_NEG:
    out << "-";
    if (left() && left()->print(out, context))
      found = true;
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
    out << " // ";
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

  case O_COMMA:
    if (left() && left()->print(out, context))
      found = true;
    out << ", ";
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
    out << "(";
    if (has_right() && right()->print(out, context))
      found = true;
    out << ")";
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
    if (amount_t::current_pool->find(symbol))
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
  out.width(10);
  out << this;

  for (int i = 0; i < depth; i++)
    out << " ";

  switch (kind) {
  case VALUE:
    out << "VALUE: " << as_value();
    break;

  case IDENT:
    out << "IDENT: " << as_ident();
    break;

  case INDEX:
    out << "INDEX: " << as_index();
    break;

  case FUNCTION:
    out << "FUNCTION";
    break;

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

  case O_COMMA:	 out << "O_COMMA"; break;

  case LAST:
  default:
    assert(false);
    break;
  }

  out << " (" << refc << ')' << std::endl;

  // An identifier is a special non-terminal, in that its left() can
  // hold the compiled definition of the identifier.
  if (kind > TERMINALS || kind == IDENT) {
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

string op_context(const expr_t::ptr_op_t op, const expr_t::ptr_op_t goal)
{
  ostream_pos_type start_pos, end_pos;
  expr_t::op_t::context_t context(op, goal, &start_pos, &end_pos);
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
