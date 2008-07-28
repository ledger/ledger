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

#include "valexpr.h"
#include "parsexp.h"
#include "walk.h"
#include "utils.h"

namespace ledger {

namespace expr {

std::auto_ptr<symbol_scope_t> global_scope;
datetime_t terminus;

details_t::details_t(const transaction_t& _xact)
  : entry(_xact.entry), xact(&_xact), account(xact_account(_xact))
{
  TRACE_CTOR(details_t, "const transaction_t&");
}

bool compute_amount(ptr_op_t expr, amount_t& amt,
		    const transaction_t * xact, ptr_op_t context)
{
  value_t result;
  try {
    expr->compute(result, xact ? details_t(*xact) : details_t(), context);

    // Most of the time when computing the amount of a transaction this cast
    // will do nothing at all.
    assert(result.valid());
    result.in_place_cast(value_t::AMOUNT);
    amt = result.as_amount();
    assert(amt.valid());
  }
  catch (error * err) {
    if (err->context.empty() ||
	! dynamic_cast<valexpr_context *>(err->context.back()))
      err->context.push_back(new valexpr_context(expr));
    error_context * last = err->context.back();
    if (valexpr_context * ctxt = dynamic_cast<valexpr_context *>(last)) {
      ctxt->expr = expr;
      ctxt->desc = "While computing amount expression:";
    }
    throw err;
  }
  return true;
}

void scope_t::define(const string& name, const value_t& val) {
  define(name, op_t::wrap_value(val));
}

value_t scope_t::resolve(const string& name) {
  ptr_op_t definition = lookup(name);
  if (definition)
    return definition->calc(*this);
  else
    return NULL_VALUE;
}

void symbol_scope_t::define(const string& name, ptr_op_t def)
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

namespace {
  int count_leaves(ptr_op_t expr)
  {
    int count = 0;
    if (expr->kind != op_t::O_COMMA) {
      count = 1;
    } else {
      count += count_leaves(expr->left());
      count += count_leaves(expr->right());
    }
    return count;
  }

  ptr_op_t reduce_leaves(ptr_op_t expr, const details_t& details,
			       ptr_op_t context)
  {
    if (! expr)
      return NULL;

    value_expr temp;

    if (expr->kind != op_t::O_COMMA) {
      if (expr->kind < op_t::TERMINALS) {
	temp.reset(expr);
      } else {
	temp.reset(new op_t(op_t::VALUE));
	temp->set_value(NULL_VALUE);
	expr->compute(temp->as_value_lval(), details, context);
      }
    } else {
      temp.reset(new op_t(op_t::O_COMMA));
      temp->set_left(reduce_leaves(expr->left(), details, context));
      temp->set_right(reduce_leaves(expr->right(), details, context));
    }
    return temp.release();
  }

  ptr_op_t find_leaf(ptr_op_t context, int goal, long& found)
  {
    if (! context)
      return NULL;

    if (context->kind != op_t::O_COMMA) {
      if (goal == found++)
	return context;
    } else {
      ptr_op_t expr = find_leaf(context->left(), goal, found);
      if (expr)
	return expr;
      expr = find_leaf(context->right(), goal, found);
      if (expr)
	return expr;
    }
    return NULL;
  }
}

value_t get_amount(scope_t& scope)
{
  assert("I can't get the amount!");
}

ptr_op_t symbol_scope_t::lookup(const string& name)
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


void op_t::compute(value_t& result, const details_t& details,
		   ptr_op_t context) const
{
  try {
  switch (kind) {
  case ARG_INDEX:
    throw new compute_error("Cannot directly compute an arg_index");

  case VALUE:
    result = as_value();
    break;

  case F_NOW:
    result = terminus;
    break;

  case AMOUNT:
    if (details.xact) {
      if (transaction_has_xdata(*details.xact) &&
	  transaction_xdata_(*details.xact).dflags & TRANSACTION_COMPOUND)
	result = transaction_xdata_(*details.xact).value;
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
      if (transaction_has_xdata(*details.xact)) {
	transaction_xdata_t& xdata(transaction_xdata_(*details.xact));
	if (xdata.dflags & TRANSACTION_COMPOUND) {
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
      if (transaction_has_xdata(*details.xact)) {
	transaction_xdata_t& xdata(transaction_xdata_(*details.xact));
	if (xdata.dflags & TRANSACTION_COMPOUND) {
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
    if (details.xact && transaction_has_xdata(*details.xact))
      result = transaction_xdata_(*details.xact).total;
    else if (details.account && account_has_xdata(*details.account))
      result = account_xdata(*details.account).total;
    else
      result = 0L;
    break;
  case PRICE_TOTAL:
    if (details.xact && transaction_has_xdata(*details.xact))
      result = transaction_xdata_(*details.xact).total.value();
    else if (details.account && account_has_xdata(*details.account))
      result = account_xdata(*details.account).total.value();
    else
      result = 0L;
    break;
  case COST_TOTAL:
    if (details.xact && transaction_has_xdata(*details.xact))
      result = transaction_xdata_(*details.xact).total.cost();
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
    if (details.xact && transaction_has_xdata(*details.xact) &&
	is_valid(transaction_xdata_(*details.xact).date))
      result = transaction_xdata_(*details.xact).date;
    else if (details.xact)
      result = details.xact->date();
    else if (details.entry)
      result = details.entry->date();
    else
      result = terminus;
    break;

  case ACT_DATE:
    if (details.xact && transaction_has_xdata(*details.xact) &&
	is_valid(transaction_xdata_(*details.xact).date))
      result = transaction_xdata_(*details.xact).date;
    else if (details.xact)
      result = details.xact->actual_date();
    else if (details.entry)
      result = details.entry->actual_date();
    else
      result = terminus;
    break;

  case EFF_DATE:
    if (details.xact && transaction_has_xdata(*details.xact) &&
	is_valid(transaction_xdata_(*details.xact).date))
      result = transaction_xdata_(*details.xact).date;
    else if (details.xact)
      result = details.xact->effective_date();
    else if (details.entry)
      result = details.entry->effective_date();
    else
      result = terminus;
    break;

  case CLEARED:
    if (details.xact)
      result = details.xact->state == transaction_t::CLEARED;
    else
      result = false;
    break;
  case PENDING:
    if (details.xact)
      result = details.xact->state == transaction_t::PENDING;
    else
      result = false;
    break;

  case REAL:
    if (details.xact)
      result = ! (details.xact->has_flags(TRANSACTION_VIRTUAL));
    else
      result = true;
    break;

  case ACTUAL:
    if (details.xact)
      result = ! (details.xact->has_flags(TRANSACTION_AUTO));
    else
      result = true;
    break;

  case INDEX:
    if (details.xact && transaction_has_xdata(*details.xact))
      result = long(transaction_xdata_(*details.xact).index + 1);
    else if (details.account && account_has_xdata(*details.account))
      result = long(account_xdata(*details.account).count);
    else
      result = 0L;
    break;

  case COUNT:
    if (details.xact && transaction_has_xdata(*details.xact))
      result = long(transaction_xdata_(*details.xact).index + 1);
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
    if (details.xact && transaction_has_xdata(*details.xact)) {
      expr->compute(result, details, context);
      result /= amount_t(long(transaction_xdata_(*details.xact).index + 1));
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
    assert(left()->kind == ARG_INDEX);
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

void valexpr_context::describe(std::ostream& out) const throw()
{
  if (! expr) {
    out << "valexpr_context expr not set!" << std::endl;
    return;
  }

  if (! desc.empty())
    out << desc << std::endl;

  out << "  ";
#if 0
  unsigned long start = (long)out.tellp() - 1;
  unsigned long begin;
  unsigned long end;
  bool found = print_value_expr(out, expr, true, error_node, &begin, &end);
  out << std::endl;
  if (found) {
    out << "  ";
    for (unsigned int i = 0; i < end - start; i++) {
      if (i >= begin - start)
	out << "^";
      else
	out << " ";
    }
    out << std::endl;
  }
#endif
}

ptr_op_t op_t::compile(scope_t& scope)
{
  switch (kind) {
#if 0
  case VAR_NAME:
  case FUNC_NAME:
    if (ptr_op_t def = scope.lookup(as_string())) {
#if 1
      return def;
#else
      // Aren't definitions compiled when they go in?  Would
      // recompiling here really add any benefit?
      return def->compile(scope);
#endif
    }
    return this;
#endif

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


value_t op_t::calc(scope_t& scope)
{
#if 0
  bool find_all_nodes = false;
#endif

  switch (kind) {
  case VALUE:
    return as_value();

#if 0
  case VAR_NAME:
  case FUNC_NAME:
    if (ptr_op_t reference = compile(scope)) {
      return reference->calc(scope);
    } else {
      throw_(calc_error, "No " << (kind == VAR_NAME ? "variable" : "function")
	     << " named '" << as_string() << "'");
    }
    break;
#endif

  case FUNCTION:
    // This should never be evaluated directly; it only appears as the
    // left node of an O_CALL operator.
    assert(false);
    break;

#if 0
  case O_CALL: {
    call_scope_t call_args(scope);

    if (right())
      call_args.set_args(right()->calc(scope));

    ptr_op_t func = left();
    string   name;

    if (func->kind == FUNC_NAME) {
      name = func->as_string();
      func = func->compile(scope);
    }

    if (func->kind != FUNCTION)
      throw_(calc_error,
	     name.empty() ? string("Attempt to call non-function") :
	     (string("Attempt to call unknown function '") + name + "'"));

    return func->as_function()(call_args);
  }
#endif

  case ARG_INDEX: {
    call_scope_t& args(CALL_SCOPE(scope));

    if (as_long() >= 0 && as_long() < args.size())
      return args[as_long()];
    else
      throw_(calc_error, "Reference to non-existing argument");
    break;
  }

#if 0
  case O_FIND:
  case O_RFIND:
    return select_nodes(scope, left()->calc(scope), right(), kind == O_RFIND);

  case O_PRED: {
    value_t values = left()->calc(scope);

    if (! values.is_null()) {
      op_predicate pred(right());

      if (! values.is_sequence()) {
	context_scope_t value_scope(scope, values, 0, 1);
	if (pred(value_scope))
	  return values;
	return NULL_VALUE;
      } else {
	std::size_t index = 0;
	std::size_t size  = values.as_sequence().size();

	value_t result;

	foreach (const value_t& value, values.as_sequence()) {
	  context_scope_t value_scope(scope, value, index, size);
	  if (pred(value_scope))
	    result.push_back(value);
	  index++;
	}
	return result;
      }
    }
    break;
  }

  case NODE_ID:
    switch (as_name()) {
    case document_t::CURRENT:
      return current_value(scope);

    case document_t::PARENT:
      if (optional<parent_node_t&> parent = current_xml_node(scope).parent())
	return &*parent;
      else
	throw_(std::logic_error, "Attempt to access parent of root node");
      break;

    case document_t::ROOT:
      return &current_xml_node(scope).document();

    case document_t::ALL:
      find_all_nodes = true;
      break;

    default:
      break;			// pass down to the NODE_NAME case
    }
    // fall through...

  case NODE_NAME: {
    node_t& current_node(current_xml_node(scope));

    if (current_node.is_parent_node()) {
      const bool have_name_id = kind == NODE_ID;

      parent_node_t& parent(current_node.as_parent_node());

      value_t result;
      foreach (node_t * child, parent) {
	if (find_all_nodes ||
	    (  have_name_id && as_name()   == child->name_id()) ||
	    (! have_name_id && as_string() == child->name()))
	  result.push_back(child);
      }
      return result;
    }
    break;
  }

  case ATTR_ID:
  case ATTR_NAME:
    if (optional<value_t&> value =
	kind == ATTR_ID ? current_xml_node(scope).get_attr(as_name()) :
			  current_xml_node(scope).get_attr(as_string()))
      return *value;

    break;
#endif

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

#if 0
  case O_UNION:
#endif
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

} // namespace expr

std::auto_ptr<value_expr>     value_expr::amount_expr;
std::auto_ptr<value_expr>     value_expr::total_expr;
std::auto_ptr<expr::parser_t> value_expr::parser;

void value_expr::initialize()
{
  parser.reset(new expr::parser_t);
}

void value_expr::shutdown()
{
  amount_expr.reset();
  total_expr.reset();
  parser.reset();
}

value_expr::value_expr(const string& _expr_str) : expr_str(_expr_str)
{
  TRACE_CTOR(value_expr, "const string&");

  if (! _expr_str.empty())
    ptr = parser->parse(expr_str).ptr;
}

} // namespace ledger
