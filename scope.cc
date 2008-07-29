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

#include "scope.h"

namespace ledger {

void symbol_scope_t::define(const string& name, expr_t::ptr_op_t def)
{
  DEBUG("ledger.xpath.syms", "Defining '" << name << "' = " << def);

  std::pair<symbol_map::iterator, bool> result
    = symbols.insert(symbol_map::value_type(name, def));
  if (! result.second) {
    symbol_map::iterator i = symbols.find(name);
    assert(i != symbols.end());
    symbols.erase(i);

    std::pair<symbol_map::iterator, bool> result2
      = symbols.insert(symbol_map::value_type(name, def));
    if (! result2.second)
      throw_(compile_error,
	     "Redefinition of '" << name << "' in same scope");
  }
}

value_t get_amount(scope_t& scope)
{
  assert("I can't get the amount!");
  return NULL_VALUE;
}

expr_t::ptr_op_t symbol_scope_t::lookup(const string& name)
{
  switch (name[0]) {
  case 'a':
    if (name[1] == '\0' || name == "amount")
      return WRAP_FUNCTOR(bind(get_amount, _1));
    break;
  }

  symbol_map::const_iterator i = symbols.find(name);
  if (i != symbols.end())
    return (*i).second;

  return child_scope_t::lookup(name);
}

#if 0
namespace {
  int count_leaves(expr_t::ptr_op_t expr)
  {
    int count = 0;
    if (expr->kind != expr_t::op_t::O_COMMA) {
      count = 1;
    } else {
      count += count_leaves(expr->left());
      count += count_leaves(expr->right());
    }
    return count;
  }

  expr_t::ptr_op_t reduce_leaves(expr_t::ptr_op_t expr,
				 expr_t::ptr_op_t context)
  {
    if (! expr)
      return NULL;

    expr_t::ptr_op_t temp;

    if (expr->kind != expr_t::op_t::O_COMMA) {
      if (expr->kind < expr_t::op_t::TERMINALS) {
	temp.reset(expr);
      } else {
	temp.reset(new op_t(expr_t::op_t::VALUE));
	temp->set_value(NULL_VALUE);
	expr->compute(temp->as_value_lval(), context);
      }
    } else {
      temp.reset(new op_t(expr_t::op_t::O_COMMA));
      temp->set_left(reduce_leaves(expr->left(), context));
      temp->set_right(reduce_leaves(expr->right(), context));
    }
    return temp.release();
  }

  expr_t::ptr_op_t find_leaf(expr_t::ptr_op_t context, int goal, long& found)
  {
    if (! context)
      return NULL;

    if (context->kind != expr_t::op_t::O_COMMA) {
      if (goal == found++)
	return context;
    } else {
      expr_t::ptr_op_t expr = find_leaf(context->left(), goal, found);
      if (expr)
	return expr;
      expr = find_leaf(context->right(), goal, found);
      if (expr)
	return expr;
    }
    return NULL;
  }
}
#endif

} // namespace ledger
