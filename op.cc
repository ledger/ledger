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

#include "op.h"
#include "scope.h"
#include "binary.h"

namespace ledger {

#if 0
void expr_t::op_t::compute(value_t&	    result,
			   const details_t& details,
			   ptr_op_t         context) const
{
  try {
  switch (kind) {
  case INDEX:
    throw new compute_error("Cannot directly compute an argument index");

  case VALUE:
    result = as_value();
    break;

  case F_NOW:
    result = terminus;
    break;

  case AMOUNT:
    if (details.xact) {
      if (xact_has_xdata(*details.xact) &&
	  xact_xdata_(*details.xact).dflags & XACT_COMPOUND)
	result = xact_xdata_(*details.xact).value;
      else
	result = details.xact->amount;
    }
    else if (details.account && account_has_xdata(*details.account)) {
      result = account_xdata(*details.account).value;
    }
    else {
      result = 0L;
    }
    break;

  case PRICE:
    if (details.xact) {
      bool set = false;
      if (xact_has_xdata(*details.xact)) {
	xact_xdata_t& xdata(xact_xdata_(*details.xact));
	if (xdata.dflags & XACT_COMPOUND) {
	  result = xdata.value.value();
	  set = true;
	}
      }
      if (! set) {
	optional<amount_t> value = details.xact->amount.value();
	if (value)
	  result = *value;
	else
	  result = 0L;
      }
    }
    else if (details.account && account_has_xdata(*details.account)) {
      result = account_xdata(*details.account).value.value();
    }
    else {
      result = 0L;
    }
    break;

  case COST:
    if (details.xact) {
      bool set = false;
      if (xact_has_xdata(*details.xact)) {
	xact_xdata_t& xdata(xact_xdata_(*details.xact));
	if (xdata.dflags & XACT_COMPOUND) {
	  result = xdata.value.cost();
	  set = true;
	}
      }

      if (! set) {
	if (details.xact->cost)
	  result = *details.xact->cost;
	else
	  result = details.xact->amount;
      }
    }
    else if (details.account && account_has_xdata(*details.account)) {
      result = account_xdata(*details.account).value.cost();
    }
    else {
      result = 0L;
    }
    break;

  case TOTAL:
    if (details.xact && xact_has_xdata(*details.xact))
      result = xact_xdata_(*details.xact).total;
    else if (details.account && account_has_xdata(*details.account))
      result = account_xdata(*details.account).total;
    else
      result = 0L;
    break;
  case PRICE_TOTAL:
    if (details.xact && xact_has_xdata(*details.xact))
      result = xact_xdata_(*details.xact).total.value();
    else if (details.account && account_has_xdata(*details.account))
      result = account_xdata(*details.account).total.value();
    else
      result = 0L;
    break;
  case COST_TOTAL:
    if (details.xact && xact_has_xdata(*details.xact))
      result = xact_xdata_(*details.xact).total.cost();
    else if (details.account && account_has_xdata(*details.account))
      result = account_xdata(*details.account).total.cost();
    else
      result = 0L;
    break;

  case VALUE_EXPR:
    if (value_expr::amount_expr.get())
      value_expr::amount_expr->compute(result, details, context);
    else
      result = 0L;
    break;
  case TOTAL_EXPR:
    if (value_expr::total_expr.get())
      value_expr::total_expr->compute(result, details, context);
    else
      result = 0L;
    break;

  case DATE:
    if (details.xact && xact_has_xdata(*details.xact) &&
	is_valid(xact_xdata_(*details.xact).date))
      result = xact_xdata_(*details.xact).date;
    else if (details.xact)
      result = details.xact->date();
    else if (details.entry)
      result = details.entry->date();
    else
      result = terminus;
    break;

  case ACT_DATE:
    if (details.xact && xact_has_xdata(*details.xact) &&
	is_valid(xact_xdata_(*details.xact).date))
      result = xact_xdata_(*details.xact).date;
    else if (details.xact)
      result = details.xact->actual_date();
    else if (details.entry)
      result = details.entry->actual_date();
    else
      result = terminus;
    break;

  case EFF_DATE:
    if (details.xact && xact_has_xdata(*details.xact) &&
	is_valid(xact_xdata_(*details.xact).date))
      result = xact_xdata_(*details.xact).date;
    else if (details.xact)
      result = details.xact->effective_date();
    else if (details.entry)
      result = details.entry->effective_date();
    else
      result = terminus;
    break;

  case CLEARED:
    if (details.xact)
      result = details.xact->state == xact_t::CLEARED;
    else
      result = false;
    break;
  case PENDING:
    if (details.xact)
      result = details.xact->state == xact_t::PENDING;
    else
      result = false;
    break;

  case REAL:
    if (details.xact)
      result = ! (details.xact->has_flags(XACT_VIRTUAL));
    else
      result = true;
    break;

  case ACTUAL:
    if (details.xact)
      result = ! (details.xact->has_flags(XACT_AUTO));
    else
      result = true;
    break;

  case INDEX:
    if (details.xact && xact_has_xdata(*details.xact))
      result = long(xact_xdata_(*details.xact).index + 1);
    else if (details.account && account_has_xdata(*details.account))
      result = long(account_xdata(*details.account).count);
    else
      result = 0L;
    break;

  case COUNT:
    if (details.xact && xact_has_xdata(*details.xact))
      result = long(xact_xdata_(*details.xact).index + 1);
    else if (details.account && account_has_xdata(*details.account))
      result = long(account_xdata(*details.account).total_count);
    else
      result = 0L;
    break;

  case DEPTH:
    if (details.account)
      result = long(details.account->depth);
    else
      result = 0L;
    break;

  case F_PRICE: {
    long arg_index = 0;
    ptr_op_t expr = find_leaf(context, 0, arg_index);
    expr->compute(result, details, context);
    result = result.value();
    break;
  }

  case F_DATE: {
    long arg_index = 0;
    ptr_op_t expr = find_leaf(context, 0, arg_index);
    expr->compute(result, details, context);
    result = result.as_datetime();
    break;
  }

  case F_DATECMP: {
    long arg_index = 0;
    ptr_op_t expr = find_leaf(context, 0, arg_index);
    expr->compute(result, details, context);
    result = result.as_datetime();
    if (! result)
      break;

    arg_index = 0;
    expr = find_leaf(context, 1, arg_index);
    value_t moment;
    expr->compute(moment, details, context);
    if (moment.is_type(value_t::DATETIME)) {
      result.cast(value_t::INTEGER);
      moment.cast(value_t::INTEGER);
      result -= moment;
    } else {
      throw new compute_error("Invalid date passed to datecmp(value,date)",
			      new valexpr_context(expr));
    }
    break;
  }

  case F_YEAR:
  case F_MONTH:
  case F_DAY: {
    long arg_index = 0;
    ptr_op_t expr = find_leaf(context, 0, arg_index);
    expr->compute(result, details, context);

    if (! result.is_type(value_t::DATETIME))
      throw new compute_error("Invalid date passed to year|month|day(date)",
			      new valexpr_context(expr));

    const datetime_t& moment(result.as_datetime());
    switch (kind) {
    case F_YEAR:
      result = (long)moment.date().year();
      break;
    case F_MONTH:
      result = (long)moment.date().month();
      break;
    case F_DAY:
      result = (long)moment.date().day();
      break;
    default:
      break;
    }
    break;
  }

  case F_ARITH_MEAN: {
    long arg_index = 0;
    ptr_op_t expr = find_leaf(context, 0, arg_index);
    if (details.xact && xact_has_xdata(*details.xact)) {
      expr->compute(result, details, context);
      result /= amount_t(long(xact_xdata_(*details.xact).index + 1));
    }
    else if (details.account && account_has_xdata(*details.account) &&
	     account_xdata(*details.account).total_count) {
      expr->compute(result, details, context);
      result /= amount_t(long(account_xdata(*details.account).total_count));
    }
    else {
      result = 0L;
    }
    break;
  }

  case F_PARENT:
    if (details.account && details.account->parent)
      left()->compute(result, details_t(*details.account->parent), context);
    break;

  case F_ABS: {
    long arg_index = 0;
    ptr_op_t expr = find_leaf(context, 0, arg_index);
    expr->compute(result, details, context);
    result.abs();
    break;
  }

  case F_ROUND: {
    long arg_index = 0;
    ptr_op_t expr = find_leaf(context, 0, arg_index);
    expr->compute(result, details, context);
    result.round();
    break;
  }

  case F_COMMODITY: {
    long arg_index = 0;
    ptr_op_t expr = find_leaf(context, 0, arg_index);
    expr->compute(result, details, context);
    if (! result.is_type(value_t::AMOUNT))
      throw new compute_error("Argument to commodity() must be a commoditized amount",
			      new valexpr_context(expr));
    amount_t temp("1");
    temp.set_commodity(result.as_amount().commodity());
    result = temp;
    break;
  }

  case F_SET_COMMODITY: {
    long arg_index = 0;
    ptr_op_t expr = find_leaf(context, 0, arg_index);
    value_t temp;
    expr->compute(temp, details, context);

    arg_index = 0;
    expr = find_leaf(context, 1, arg_index);
    expr->compute(result, details, context);
    if (! result.is_type(value_t::AMOUNT))
      throw new compute_error
	("Second argument to set_commodity() must be a commoditized amount",
	 new valexpr_context(expr));
    amount_t one("1");
    one.set_commodity(result.as_amount().commodity());
    result = one;

    result *= temp;
    break;
  }

  case F_QUANTITY: {
    long arg_index = 0;
    ptr_op_t expr = find_leaf(context, 0, arg_index);
    expr->compute(result, details, context);

    const balance_t * bal = NULL;
    switch (result.type()) {
    case value_t::BALANCE_PAIR:
      bal = &(result.as_balance_pair().quantity());
      // fall through...

    case value_t::BALANCE:
      if (! bal)
	bal = &result.as_balance();

      if (bal->amounts.size() < 2) {
	result.cast(value_t::AMOUNT);
      } else {
	value_t temp;
	for (balance_t::amounts_map::const_iterator i = bal->amounts.begin();
	     i != bal->amounts.end();
	     i++) {
	  amount_t x = (*i).second;
	  x.clear_commodity();
	  temp += x;
	}
	result = temp;
	assert(temp.is_type(value_t::AMOUNT));
      }
      // fall through...

    case value_t::AMOUNT:
      result.as_amount_lval().clear_commodity();
      break;

    default:
      break;
    }
    break;
  }

  case F_CODE_MASK:
    if (details.entry && details.entry->code)
      result = as_mask().match(*details.entry->code);
    else
      result = false;
    break;

  case F_PAYEE_MASK:
    if (details.entry)
      result = as_mask().match(details.entry->payee);
    else
      result = false;
    break;

  case F_NOTE_MASK:
    if (details.xact && details.xact->note)
      result = as_mask().match(*details.xact->note);
    else
      result = false;
    break;

  case F_ACCOUNT_MASK:
    if (details.account)
      result = as_mask().match(details.account->fullname());
    else
      result = false;
    break;

  case F_SHORT_ACCOUNT_MASK:
    if (details.account)
      result = as_mask().match(details.account->name);
    else
      result = false;
    break;

  case F_COMMODITY_MASK:
    if (details.xact)
      result = as_mask().match(details.xact->amount.commodity().base_symbol());
    else
      result = false;
    break;

  case O_ARG: {
    long arg_index = 0;
    assert(left()->kind == INDEX);
    ptr_op_t expr = find_leaf(context, left()->as_long(), arg_index);
    if (expr)
      expr->compute(result, details, context);
    else
      result = 0L;
    break;
  }

  case O_COMMA:
    if (! left())
      throw new compute_error("Comma operator missing left operand",
			      new valexpr_context(const_cast<op_t *>(this)));
    if (! right())
      throw new compute_error("Comma operator missing right operand",
			      new valexpr_context(const_cast<op_t *>(this)));
    left()->compute(result, details, context);
    right()->compute(result, details, context);
    break;

  case O_DEF:
    result = 0L;
    break;

  case O_REF: {
    assert(left());
    if (right()) {
      value_expr args(reduce_leaves(right(), details, context));
      left()->compute(result, details, args.get());
    } else {
      left()->compute(result, details, context);
    }
    break;
  }

  case F_VALUE: {
    long arg_index = 0;
    ptr_op_t expr = find_leaf(context, 0, arg_index);
    expr->compute(result, details, context);

    arg_index = 0;
    expr = find_leaf(context, 1, arg_index);
    value_t moment;
    expr->compute(moment, details, context);
    if (! moment.is_type(value_t::DATETIME))
      throw new compute_error("Invalid date passed to P(value,date)",
			      new valexpr_context(expr));

    result = result.value(moment.as_datetime());
    break;
  }

  case O_NOT:
    left()->compute(result, details, context);
    if (result.strip_annotations())
      result = false;
    else
      result = true;
    break;

  case O_QUES: {
    assert(left());
    assert(right());
    assert(right()->kind == O_COL);
    left()->compute(result, details, context);
    if (result.strip_annotations())
      right()->left()->compute(result, details, context);
    else
      right()->right()->compute(result, details, context);
    break;
  }

  case O_AND:
    assert(left());
    assert(right());
    left()->compute(result, details, context);
    result = result.strip_annotations();
    if (result)
      right()->compute(result, details, context);
    break;

  case O_OR:
    assert(left());
    assert(right());
    left()->compute(result, details, context);
    if (! result.strip_annotations())
      right()->compute(result, details, context);
    break;

  case O_NEQ:
  case O_EQ:
  case O_LT:
  case O_LTE:
  case O_GT:
  case O_GTE: {
    assert(left());
    assert(right());
    value_t temp;
    left()->compute(temp, details, context);
    right()->compute(result, details, context);
    switch (kind) {
    case O_NEQ: result = temp != result; break;
    case O_EQ:  result = temp == result; break;
    case O_LT:  result = temp <  result; break;
    case O_LTE: result = temp <= result; break;
    case O_GT:  result = temp >  result; break;
    case O_GTE: result = temp >= result; break;
    default: assert(false); break;
    }
    break;
  }

  case O_NEG:
    assert(left());
    left()->compute(result, details, context);
    result.negate();
    break;

  case O_ADD:
  case O_SUB:
  case O_MUL:
  case O_DIV: {
    assert(left());
    assert(right());
    value_t temp;
    right()->compute(temp, details, context);
    left()->compute(result, details, context);
    switch (kind) {
    case O_ADD: result += temp; break;
    case O_SUB: result -= temp; break;
    case O_MUL: result *= temp; break;
    case O_DIV: result /= temp; break;
    default: assert(false); break;
    }
    break;
  }

  case O_PERC: {
    assert(left());
    result = "100.0%";
    value_t temp;
    left()->compute(temp, details, context);
    result *= temp;
    break;
  }

  case LAST:
  default:
    assert(false);
    break;
  }
  }
  catch (error * err) {
    if (err->context.empty() ||
	! dynamic_cast<valexpr_context *>(err->context.back()))
      err->context.push_back(new valexpr_context(const_cast<op_t *>(this)));
    throw err;
  }
}
#endif

expr_t::ptr_op_t expr_t::op_t::compile(scope_t& scope)
{
  switch (kind) {
  case IDENT:
    if (ptr_op_t def = scope.lookup(as_ident())) {
#if 1
      return def;
#else
      // Aren't definitions compiled when they go in?  Would
      // recompiling here really add any benefit?
      return def->compile(scope);
#endif
    }
    return this;

  default:
    break;
  }

  if (kind < TERMINALS)
    return this;

  ptr_op_t lhs(left()->compile(scope));
  ptr_op_t rhs(right() ? right()->compile(scope) : ptr_op_t());

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
    if (ptr_op_t reference = compile(scope)) {
      if (reference != this)
	return reference->calc(scope);
    }
    throw_(calc_error, "Unknown identifier '" << as_ident() << "'");

  case FUNCTION:
    // This should never be evaluated directly; it only appears as the
    // left node of an O_CALL operator.
    assert(false);
    break;

  case O_CALL: {
    call_scope_t call_args(scope);

    if (right())
      call_args.set_args(right()->calc(scope));

    ptr_op_t func = left();
    string   name;

    if (func->kind == IDENT) {
      name = func->as_ident();
      ptr_op_t def = func->compile(scope);
      if (def == func)
	throw_(calc_error,
	       "Calling unknown function '" << name << "'");
      func = def;
    }

    if (func->kind != FUNCTION)
      throw_(calc_error, "Calling non-function");

    return func->as_function()(call_args);
  }

  case INDEX: {
    call_scope_t args(scope);

    if (as_index() >= 0 && as_index() < args.size())
      return args[as_index()];
    else
      throw_(calc_error, "Reference to non-existing argument " << as_index());
    break;
  }

  case O_NEQ:
    return left()->calc(scope) != right()->calc(scope);
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
    assert(! right());
    return left()->calc(scope).negate();

  case O_NOT:
    assert(! right());
    return ! left()->calc(scope);

  case O_AND:
    return left()->calc(scope) && right()->calc(scope);
  case O_OR:
    return left()->calc(scope) || right()->calc(scope);

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
    *context.start_pos = (long)out.tellp() - 1;
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

  case O_NEQ:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " != ";
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
    *context.end_pos = (long)out.tellp() - 1;

  return found;
}

void expr_t::op_t::dump(std::ostream& out, const int depth) const
{
  out.setf(std::ios::left);
  out.width(10);
  out << this << " ";

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

  case O_NOT:	out << "O_NOT"; break;
  case O_NEG:	out << "O_NEG"; break;

  case O_ADD:	out << "O_ADD"; break;
  case O_SUB:	out << "O_SUB"; break;
  case O_MUL:	out << "O_MUL"; break;
  case O_DIV:	out << "O_DIV"; break;

  case O_NEQ:	out << "O_NEQ"; break;
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

  if (kind > TERMINALS) {
    if (left()) {
      left()->dump(out, depth + 1);
      if (right())
	right()->dump(out, depth + 1);
    } else {
      assert(! right());
    }
  }
}

void expr_t::op_t::read(std::ostream& in)
{
}

void expr_t::op_t::read(const char *& data)
{
#if 0
  if (! read_bool(data))
    return expr_t::ptr_op_t();

  expr_t::op_t::kind_t kind;
  read_number(data, kind);

  expr_t::ptr_op_t expr = new expr_t::op_t(kind);

  if (kind > expr_t::op_t::TERMINALS)
    expr->set_left(read_value_expr(data));

  switch (expr->kind) {
  case expr_t::op_t::INDEX: {
    long temp;
    read_long(data, temp);
    expr->set_index(temp);
    break;
  }
  case expr_t::op_t::VALUE: {
    value_t temp;
    read_value(data, temp);
    expr->set_value(temp);
    break;
  }

  case expr_t::op_t::MASK:
    if (read_bool(data))
      read_mask(data, expr->as_mask_lval());
    break;

  default:
    if (kind > expr_t::op_t::TERMINALS)
      expr->set_right(read_value_expr(data));
    break;
  }

  return expr;
#endif
}

void expr_t::op_t::write(std::ostream& out) const
{
#if 0
  if (! expr) {
    write_bool(out, false);
    return;
  }

  write_bool(out, true);
  write_number(out, expr->kind);

  if (expr->kind > expr_t::op_t::TERMINALS)
    write_value_expr(out, expr->left());

  switch (expr->kind) {
  case expr_t::op_t::INDEX:
    write_long(out, expr->as_index());
    break;
  case expr_t::op_t::IDENT:
    write_long(out, expr->as_ident());
    break;
  case expr_t::op_t::VALUE:
    write_value(out, expr->as_value());
    break;

  case expr_t::op_t::MASK:
    if (expr->as_mask()) {
      write_bool(out, true);
      write_mask(out, expr->as_mask());
    } else {
      write_bool(out, false);
    }
    break;

  default:
    if (expr->kind > expr_t::op_t::TERMINALS)
      write_value_expr(out, expr->right());
    break;
  }
#endif
}

#if 0
class op_predicate : public noncopyable
{
  ptr_op_t op;

  op_predicate();

public:
  explicit op_predicate(ptr_op_t _op) : op(_op) {
    TRACE_CTOR(op_predicate, "ptr_op_t");
  }
  ~op_predicate() throw() {
    TRACE_DTOR(op_predicate);
  }
  bool operator()(scope_t& scope) {
    return op->calc(scope).to_boolean();
  }
};

#endif

} // namespace ledger
