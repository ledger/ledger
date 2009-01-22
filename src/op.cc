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
#include "binary.h"

namespace ledger {

expr_t::ptr_op_t expr_t::op_t::compile(scope_t& scope)
{
  switch (kind) {
  case IDENT:
    if (ptr_op_t def = scope.lookup(as_ident())) {
      // Definitions are compiled at the point of definition, not the
      // point of use.
      return copy(def);
    }
    return this;

  default:
    break;
  }

  if (kind < TERMINALS)
    return this;

  ptr_op_t lhs(left()->compile(scope));
  ptr_op_t rhs(kind > UNARY_OPERATORS ? right()->compile(scope) : ptr_op_t());

  if (lhs == left() && (! rhs || rhs == right()))
    return this;

  ptr_op_t intermediate(copy(lhs, rhs));

  if (lhs->is_value() && (! rhs || rhs->is_value()))
    return wrap_value(intermediate->calc(scope));

  return intermediate;
}

value_t expr_t::op_t::calc(scope_t& scope)
{
  switch (kind) {
  case VALUE:
    return as_value();

  case IDENT:
    if (! left())
      throw_(calc_error, "Unknown identifier '" << as_ident() << "'");
    return left()->calc(scope);

  case FUNCTION: {
    // Evaluating a FUNCTION is the same as calling it directly; this happens
    // when certain functions-that-look-like-variables (such as "amount") are
    // resolved.
    call_scope_t call_args(scope);
    return as_function()(call_args);
  }

  case O_CALL: {
    call_scope_t call_args(scope);

    if (right())
      call_args.set_args(right()->calc(scope));

    ptr_op_t func = left();

    assert(func->kind == IDENT);
    func = func->left();

    if (! func || func->kind != FUNCTION)
      throw_(calc_error, "Calling non-function");

    return func->as_function()(call_args);
  }

  case O_MATCH:
    assert(right()->is_mask());
    return right()->as_mask().match(left()->calc(scope).to_string());

  case INDEX: {
    const call_scope_t& args(downcast<const call_scope_t>(scope));

    if (as_index() < args.size())
      return args[as_index()];
    else
      throw_(calc_error, "Reference to non-existing argument " << as_index());
    break;
  }

  case O_EQ:
    return left()->calc(scope) == right()->calc(scope);
  case O_LT:
    return left()->calc(scope) <  right()->calc(scope);
  case O_LTE:
    return left()->calc(scope) <= right()->calc(scope);
  case O_GT:
    return left()->calc(scope) >  right()->calc(scope);
  case O_GTE:
    return left()->calc(scope) >= right()->calc(scope);

  case O_ADD:
    return left()->calc(scope) + right()->calc(scope);
  case O_SUB:
    return left()->calc(scope) - right()->calc(scope);
  case O_MUL:
    return left()->calc(scope) * right()->calc(scope);
  case O_DIV:
    return left()->calc(scope) / right()->calc(scope);

  case O_NEG:
    return left()->calc(scope).negate();

  case O_NOT:
    return ! left()->calc(scope);

  case O_AND:
    return ! left()->calc(scope) ? value_t(false) : right()->calc(scope);

  case O_OR:
    if (value_t temp = left()->calc(scope))
      return temp;
    else
      return right()->calc(scope);

  case O_COMMA: {
    value_t result(left()->calc(scope));

    ptr_op_t next = right();
    while (next) {
      ptr_op_t value_op;
      if (next->kind == O_COMMA /* || next->kind == O_UNION */) {
	value_op = next->left();
	next     = next->right();
      } else {
	value_op = next;
	next     = NULL;
      }

      result.push_back(value_op->calc(scope));
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

bool expr_t::op_t::print(std::ostream& out, print_context_t& context) const
{
  bool found = false;

  if (context.start_pos && this == context.op_to_find) {
    *context.start_pos = out.tellp();
    *context.start_pos--;
    found = true;
  }

  string symbol;

  switch (kind) {
  case VALUE: {
    as_value().print(out, context.relaxed);
    break;
  }

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
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_SUB:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " - ";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_MUL:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " * ";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_DIV:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " / ";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;

  case O_EQ:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " == ";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_LT:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " < ";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_LTE:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " <= ";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_GT:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " > ";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_GTE:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " >= ";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;

  case O_AND:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " & ";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_OR:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " | ";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;

  case O_COMMA:
    if (left() && left()->print(out, context))
      found = true;
    out << ", ";
    if (right() && right()->print(out, context))
      found = true;
    break;

  case O_CALL:
    if (left() && left()->print(out, context))
      found = true;
    out << "(";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;

  case O_MATCH:
    out << '/';
    if (left() && left()->print(out, context))
      found = true;
    out << "/ =~ ";
    if (right() && right()->print(out, context))
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

  if (context.end_pos && this == context.op_to_find)
    *context.end_pos = static_cast<unsigned long>(out.tellp()) - 1;

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
    out << "VALUE - " << as_value();
    break;

  case IDENT:
    out << "IDENT - " << as_ident();
    break;

  case INDEX:
    out << "INDEX - " << as_index();
    break;

  case FUNCTION:
    out << "FUNCTION";
    break;

  case O_CALL:	out << "O_CALL"; break;
  case O_MATCH:	out << "O_MATCH"; break;

  case O_NOT:	out << "O_NOT"; break;
  case O_NEG:	out << "O_NEG"; break;

  case O_ADD:	out << "O_ADD"; break;
  case O_SUB:	out << "O_SUB"; break;
  case O_MUL:	out << "O_MUL"; break;
  case O_DIV:	out << "O_DIV"; break;

  case O_EQ:	out << "O_EQ"; break;
  case O_LT:	out << "O_LT"; break;
  case O_LTE:	out << "O_LTE"; break;
  case O_GT:	out << "O_GT"; break;
  case O_GTE:	out << "O_GTE"; break;

  case O_AND:	out << "O_AND"; break;
  case O_OR:	out << "O_OR"; break;

  case O_COMMA:	out << "O_COMMA"; break;

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
      if (kind > UNARY_OPERATORS && right())
	right()->dump(out, depth + 1);
    }
    else if (kind > UNARY_OPERATORS) {
      assert(! right());
    }
  }
}

void expr_t::op_t::read(const char *& data)
{
  kind = binary::read_long<kind_t>(data);

  if (kind > TERMINALS) {
    set_left(new expr_t::op_t());
    left()->read(data);

    if (kind > UNARY_OPERATORS && binary::read_bool(data)) {
      set_right(new expr_t::op_t());
      right()->read(data);
    }
  }

  switch (kind) {
  case VALUE: {
    value_t temp;
    temp.read(data);
    set_value(temp);
    break;
  }
  case IDENT: {
    string temp;
    binary::read_string(data, temp);
    set_ident(temp);
    break;
  }
  case MASK: {
    mask_t temp;
    temp.read(data);
    set_mask(temp);
    break;
  }
  case INDEX: {
    long temp;
    binary::read_long(data, temp);
    set_index(temp);
    break;
  }

  default:
    assert(false);
    break;
  }
}

void expr_t::op_t::write(std::ostream& out) const
{
  binary::write_long<kind_t>(out, kind);

  if (kind > TERMINALS) {
    left()->write(out);

    if (kind > UNARY_OPERATORS) {
      if (right()) {
	binary::write_bool(out, true);
	right()->write(out);
      } else {
	binary::write_bool(out, false);
      }
    }
  } else {
    switch (kind) {
    case VALUE:
      as_value().write(out);
      break;
    case IDENT:
      binary::write_string(out, as_ident());
      break;
    case MASK:
      as_mask().write(out);
      break;
    case INDEX:
      binary::write_long(out, as_index());
      break;

    case FUNCTION:
    default:
      assert(false);
      break;
    }
  }
}

} // namespace ledger
