#include "repitem.h"

namespace ledger {

repitem_t::~repitem_t()
{
  TRACE_DTOR("repitem_t");

  if (istemp) {
    switch (kind) {
    case TRANSACTION:
      delete xact;
      break;
    case ENTRY:
      delete entry;
      break;
    case ACCOUNT:
      delete account_ptr;
      break;
    case JOURNAL:
      assert(0);
      break;
    }
  }
  if (contents) delete contents;
  if (children) delete children;
  if (next) delete next;
}

void repitem_t::add_total(value_t& val)
{
  add_value(val);

  for (repitem_t * ptr = children; ptr; ptr = ptr->next)
    ptr->add_total(val);
}

void add_transaction_to(const transaction_t& xact, value_t& value)
{
#if 0
  if (transaction_has_xdata(xact) &&
      transaction_xdata_(xact).dflags & TRANSACTION_COMPOUND) {
    value += transaction_xdata_(xact).value;
  }
  else if (xact.cost || ! value.realzero()) {
    value.add(xact.amount, xact.cost);
  }
  else {
    value = xact.amount;
  }
#endif
}

void repitem_t::add_value(value_t& val)
{
  switch (kind) {
  case TRANSACTION:
    add_transaction_to(*xact, val);
    break;

  case ENTRY:
  case ACCOUNT:
    for (repitem_t * ptr = contents; ptr; ptr = ptr->next)
      ptr->add_total(val);
    break;
  }
}

void repitem_t::add_sort_value(value_t& val)
{
  assert(0);
}

datetime_t repitem_t::date() const
{
  if (reported_date)
    return reported_date;

  switch (kind) {
  case TRANSACTION: return xact->date();
  case ENTRY: return entry->date();

  case ACCOUNT:
    assert(0);
    return datetime_t();
  }
}

datetime_t repitem_t::effective_date() const
{
  if (reported_date)
    return reported_date;

  switch (kind) {
  case TRANSACTION:  return xact->effective_date();
  case ENTRY: return entry->effective_date();

  case ACCOUNT:
    assert(0);
    return datetime_t();
  }
}

datetime_t repitem_t::actual_date() const
{
  if (reported_date)
    return reported_date;

  switch (kind) {
  case TRANSACTION: return xact->actual_date();
  case ENTRY: return entry->actual_date();

  case ACCOUNT:
    assert(0);
    return datetime_t();
  }
}

account_t * repitem_t::account() const
{
  if (reported_account != NULL)
    return reported_account;

  switch (kind) {
  case TRANSACTION:
    return xact->account;
  case ENTRY:
    return NULL;
  case ACCOUNT:
    return account_ptr;
  }
}

bool repitem_t::valid() const
{
  assert(0);
  return false;
}

repitem_t * repitem_t::wrap(transaction_t * txact, repitem_t * owner)
{
  if (txact->data != NULL) {
    repitem_t * temp = static_cast<repitem_t *>(txact->data);
    txact->data = NULL;
    return temp;
  }

  repitem_t * temp = new repitem_t(TRANSACTION, owner);
  temp->xact = txact;
  return temp;
}

repitem_t * repitem_t::wrap(entry_t * tentry, repitem_t * owner, bool deep)
{
  if (tentry->data != NULL) {
    repitem_t * temp = static_cast<repitem_t *>(tentry->data);
    tentry->data = NULL;
    return temp;
  }

  repitem_t * temp = new repitem_t(ENTRY, owner);
  temp->entry = tentry;

  if (deep) {
    for (transactions_list::iterator i = tentry->transactions.begin();
	 i != tentry->transactions.end();
	 i++)
      temp->add_content(wrap(*i, temp));
  }

  return temp;
}

repitem_t * repitem_t::wrap(account_t * taccount, repitem_t * owner, bool deep)
{
  repitem_t * temp = new repitem_t(ACCOUNT, owner);
  temp->account_ptr = taccount;
  assert(! deep);
  return temp;
}

repitem_t * repitem_t::wrap(journal_t * tjournal, repitem_t * owner, bool deep)
{
  if (tjournal->data != NULL) {
    repitem_t * temp = static_cast<repitem_t *>(tjournal->data);
    tjournal->data = NULL;
    return temp;
  }

  repitem_t * temp = new repitem_t(JOURNAL, owner);
  temp->journal = tjournal;

  if (deep) {
    for (entries_list::iterator i = tjournal->entries.begin();
	 i != tjournal->entries.end();
	 i++)
      temp->add_child(wrap(*i, temp, true));
  }

  return temp;
}

repitem_t * repitem_t::add_content(repitem_t * item)
{
  repitem_t * start = item;

  if (contents == NULL) {
    assert(last_content == NULL);
    contents = item;
    item->prev = NULL;
  } else {
    assert(last_content != NULL);
    last_content->next = item;
    item->prev = last_content;
  }

  item->parent = this;
  while (item->next) {
    repitem_t * next_item = item->next;
    next_item->prev   = item;
    next_item->parent = this;
    item = next_item;
  }

  last_content = item;

  return start;
}

repitem_t * repitem_t::add_child(repitem_t * item)
{
  repitem_t * start = item;

  if (children == NULL) {
    assert(last_child == NULL);
    children = item;
    item->prev = NULL;
  } else {
    assert(last_child != NULL);
    last_child->next = item;
    item->prev = last_child;
  }

  item->parent = this;
  while (item->next) {
    repitem_t * next_item = item->next;
    next_item->prev   = item;
    next_item->parent = this;
    item = next_item;
  }

  last_child = item;

  return start;
}

repitem_t * repitem_t::fake_transaction(account_t * taccount)
{
  repitem_t * temp = new repitem_t(TRANSACTION);
  temp->xact = new transaction_t(taccount);
  temp->istemp = true;
  return temp;
}

repitem_t * repitem_t::fake_entry(const datetime_t& edate,
				  const datetime_t& rdate,
				  const std::string& payee)
{
  repitem_t * temp = new repitem_t(ENTRY);
  temp->entry = new entry_t;
  temp->entry->_date_eff = edate;
  temp->entry->_date = rdate;
  temp->entry->payee = payee;
  temp->istemp = true;
  return temp;
}

void repitem_t::populate_account(account_t& acct, repitem_t * item)
{
  repitem_t * acct_item;
  if (acct.parent == NULL)
    acct_item = this;
  else if (acct.data == NULL)
    acct.data = acct_item = repitem_t::wrap(&acct);
  else
    acct_item = (repitem_t *) acct.data;

  if (item->kind == ACCOUNT)
    acct_item->add_child(item);
  else
    acct_item->add_content(item);

  if (acct.parent && acct.parent->data == NULL)
    populate_account(*acct.parent, acct_item);
}

void repitem_t::populate_accounts(entries_list& entries,
				  const valexpr_t& filter)
{
  for (entries_list::iterator i = entries.begin();
       i != entries.end();
       i++)
    for (transactions_list::iterator j = (*i)->transactions.begin();
	 j != (*i)->transactions.end();
	 j++)
      // jww (2006-09-10): Make a scope based on **j
      if (filter.calc().get_boolean())
	populate_account(*(*j)->account, repitem_t::wrap(*j));
}

void repitem_t::populate_accounts(entries_list& entries)
{
  for (entries_list::iterator i = entries.begin();
       i != entries.end();
       i++)
    for (transactions_list::iterator j = (*i)->transactions.begin();
	 j != (*i)->transactions.end();
	 j++)
      populate_account(*(*j)->account, repitem_t::wrap(*j));
}

void repitem_t::print_tree(std::ostream& out, int depth)
{
  for (int i = 0; i < depth; i++)
    out << "  ";

  switch (kind) {
  case TRANSACTION: out << "TRANSACTION " << xact; break;
  case ENTRY: out << "ENTRY " << entry; break;
  case ACCOUNT: out << "ACCOUNT " << account_ptr; break;
  }
  out << std::endl;

  if (contents) {
    for (int i = 0; i < depth; i++)
      out << "  ";
    out << "Contents:" << std::endl;

    for (repitem_t * ptr = contents; ptr; ptr = ptr->next)
      ptr->print_tree(out, depth + 1);
  }

  if (children) {
    for (int i = 0; i < depth; i++)
      out << "  ";
    out << "Children:" << std::endl;

    for (repitem_t * ptr = children; ptr; ptr = ptr->next)
      ptr->print_tree(out, depth + 1);
  }
}

#if 0
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
	  result = xdata.value.price();
	  set = true;
	}
      }
      if (! set)
	result = details.xact->amount.price();
    }
    else if (details.account && account_has_xdata(*details.account)) {
      result = account_xdata(*details.account).value.price();
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
      result = transaction_xdata_(*details.xact).total.price();
    else if (details.account && account_has_xdata(*details.account))
      result = account_xdata(*details.account).total.price();
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
    if (amount_expr.get())
      amount_expr->compute(result, args);
    else
      result = 0L;
    break;
  case TOTAL_EXPR:
    if (total_expr.get())
      total_expr->compute(result, args);
    else
      result = 0L;
    break;

  case DATE:
    if (details.xact && transaction_has_xdata(*details.xact) &&
	transaction_xdata_(*details.xact).date)
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
	transaction_xdata_(*details.xact).date)
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
	transaction_xdata_(*details.xact).date)
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
      result = ! (details.xact->flags & TRANSACTION_VIRTUAL);
    else
      result = true;
    break;

  case ACTUAL:
    if (details.xact)
      result = ! (details.xact->flags & TRANSACTION_AUTO);
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
    int arg_index = 0;
    value_expr_t * expr = find_leaf(args, 0, arg_index);
    expr->compute(result, args);
    result = result.price();
    break;
  }

  case F_DATE: {
    int arg_index = 0;
    value_expr_t * expr = find_leaf(args, 0, arg_index);
    expr->compute(result, args);
    result = result.date();
    break;
  }

  case F_DATECMP: {
    int arg_index = 0;
    value_expr_t * expr = find_leaf(args, 0, arg_index);
    expr->compute(result, args);
    result = result.date();
    if (! result)
      break;

    arg_index = 0;
    expr = find_leaf(args, 1, arg_index);
    value_t moment;
    expr->compute(moment, args);
    if (moment.type == value_t::DATETIME) {
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
    int arg_index = 0;
    value_expr_t * expr = find_leaf(args, 0, arg_index);
    expr->compute(result, args);

    if (result.type != value_t::DATETIME)
      throw new compute_error("Invalid date passed to year|month|day(date)",
			      new valexpr_context(expr));

    datetime_t& moment(*((datetime_t *)result.data));
    switch (kind) {
    case F_YEAR:
      result = (long)moment.year();
      break;
    case F_MONTH:
      result = (long)moment.month();
      break;
    case F_DAY:
      result = (long)moment.day();
      break;
    }
    break;
  }

  case F_ARITH_MEAN: {
    int arg_index = 0;
    value_expr_t * expr = find_leaf(args, 0, arg_index);
    if (details.xact && transaction_has_xdata(*details.xact)) {
      expr->compute(result, args);
      result /= amount_t(long(transaction_xdata_(*details.xact).index + 1));
    }
    else if (details.account && account_has_xdata(*details.account) &&
	     account_xdata(*details.account).total_count) {
      expr->compute(result, args);
      result /= amount_t(long(account_xdata(*details.account).total_count));
    }
    else {
      result = 0L;
    }
    break;
  }

  case F_PARENT:
    if (details.account && details.account->parent)
      left->compute(result, details_t(*details.account->parent), args);
    break;

  case F_ABS: {
    int arg_index = 0;
    value_expr_t * expr = find_leaf(args, 0, arg_index);
    expr->compute(result, args);
    result.abs();
    break;
  }

  case F_ROUND: {
    int arg_index = 0;
    value_expr_t * expr = find_leaf(args, 0, arg_index);
    expr->compute(result, args);
    result.round();
    break;
  }

  case F_COMMODITY: {
    int arg_index = 0;
    value_expr_t * expr = find_leaf(args, 0, arg_index);
    expr->compute(result, args);
    if (result.type != value_t::AMOUNT)
      throw new compute_error("Argument to commodity() must be a commoditized amount",
			      new valexpr_context(expr));
    amount_t temp("1");
    temp.set_commodity(((amount_t *) result.data)->commodity());
    result = temp;
    break;
  }

  case F_SET_COMMODITY: {
    int arg_index = 0;
    value_expr_t * expr = find_leaf(args, 0, arg_index);
    value_t temp;
    expr->compute(temp, args);

    arg_index = 0;
    expr = find_leaf(args, 1, arg_index);
    expr->compute(result, args);
    if (result.type != value_t::AMOUNT)
      throw new compute_error
	("Second argument to set_commodity() must be a commoditized amount",
	 new valexpr_context(expr));
    amount_t one("1");
    one.set_commodity(((amount_t *) result.data)->commodity());
    result = one;

    result *= temp;
    break;
  }

  case F_QUANTITY: {
    int arg_index = 0;
    value_expr_t * expr = find_leaf(args, 0, arg_index);
    expr->compute(result, args);

    balance_t * bal = NULL;
    switch (result.type) {
    case value_t::BALANCE_PAIR:
      bal = &((balance_pair_t *) result.data)->quantity;
      // fall through...

    case value_t::BALANCE:
      if (! bal)
	bal = (balance_t *) result.data;

      if (bal->amounts.size() < 2) {
	result.cast(value_t::AMOUNT);
      } else {
	value_t temp;
	for (amounts_map::const_iterator i = bal->amounts.begin();
	     i != bal->amounts.end();
	     i++) {
	  amount_t x = (*i).second;
	  x.clear_commodity();
	  temp += x;
	}
	result = temp;
	assert(temp.type == value_t::AMOUNT);
      }
      // fall through...

    case value_t::AMOUNT:
      ((amount_t *) result.data)->clear_commodity();
      break;

    default:
      break;
    }
    break;
  }

  case F_CODE_MASK:
    assert(mask);
    if (details.entry)
      result = mask->match(details.entry->code);
    else
      result = false;
    break;

  case F_PAYEE_MASK:
    assert(mask);
    if (details.entry)
      result = mask->match(details.entry->payee);
    else
      result = false;
    break;

  case F_NOTE_MASK:
    assert(mask);
    if (details.xact)
      result = mask->match(details.xact->note);
    else
      result = false;
    break;

  case F_ACCOUNT_MASK:
    assert(mask);
    if (details.account)
      result = mask->match(details.account->fullname());
    else
      result = false;
    break;

  case F_SHORT_ACCOUNT_MASK:
    assert(mask);
    if (details.account)
      result = mask->match(details.account->name);
    else
      result = false;
    break;

  case F_COMMODITY_MASK:
    assert(mask);
    if (details.xact)
      result = mask->match(details.xact->amount.commodity().base_symbol());
    else
      result = false;
    break;

  case F_VALUE: {
    int arg_index = 0;
    value_expr_t * expr = find_leaf(args, 0, arg_index);
    expr->compute(result, args);

    arg_index = 0;
    expr = find_leaf(args, 1, arg_index);
    value_t moment;
    expr->compute(moment, args);
    if (moment.type != value_t::DATETIME)
      throw new compute_error("Invalid date passed to P(value,date)",
			      new valexpr_context(expr));

    result = result.value(*((datetime_t *)moment.data));
    break;
  }
#endif

#if 0
    case element_t::AMOUNT:
    case element_t::TOTAL:
    case element_t::VALUE_EXPR: {
      value_expr calc;
      switch (elem->type) {
      case element_t::AMOUNT:     calc = amount_expr; break;
      case element_t::TOTAL:      calc = total_expr; break;
      case element_t::VALUE_EXPR: calc = elem->val_expr; break;
      default:
	assert(0);
	break;
      }
      if (! calc)
	break;

      value_t     value;
      balance_t * bal = NULL;

      calc->compute(value, details);

      if (! amount_t::keep_price ||
	  ! amount_t::keep_date ||
	  ! amount_t::keep_tag) {
	switch (value.type) {
	case value_t::AMOUNT:
	case value_t::BALANCE:
	case value_t::BALANCE_PAIR:
	  value = value.strip_annotations();
	  break;
	default:
	  break;
	}
      }

      bool highlighted = false;

      switch (value.type) {
      case value_t::BOOLEAN:
	out << (*((bool *) value.data) ? "true" : "false");
	break;

      case value_t::INTEGER:
	if (ansi_codes && elem->flags & ELEMENT_HIGHLIGHT) {
	  if (ansi_invert) {
	    if (*((long *) value.data) > 0) {
	      mark_red(out, elem);
	      highlighted = true;
	    }
	  } else {
	    if (*((long *) value.data) < 0) {
	      mark_red(out, elem);
	      highlighted = true;
	    }
	  }
	}
	out << *((long *) value.data);
	break;

      case value_t::DATETIME:
	out << *((datetime_t *) value.data);
	break;

      case value_t::AMOUNT:
	if (ansi_codes && elem->flags & ELEMENT_HIGHLIGHT) {
	  if (ansi_invert) {
	    if (*((amount_t *) value.data) > 0) {
	      mark_red(out, elem);
	      highlighted = true;
	    }
	  } else {
	    if (*((amount_t *) value.data) < 0) {
	      mark_red(out, elem);
	      highlighted = true;
	    }
	  }
	}
	out << *((amount_t *) value.data);
	break;

      case value_t::BALANCE:
	bal = (balance_t *) value.data;
	// fall through...

      case value_t::BALANCE_PAIR:
	if (! bal)
	  bal = &((balance_pair_t *) value.data)->quantity;

	if (ansi_codes && elem->flags & ELEMENT_HIGHLIGHT) {
	  if (ansi_invert) {
	    if (*bal > 0) {
	      mark_red(out, elem);
	      highlighted = true;
	    }
	  } else {
	    if (*bal < 0) {
	      mark_red(out, elem);
	      highlighted = true;
	    }
	  }
	}
	bal->write(out, elem->min_width,
		   (elem->max_width > 0 ?
		    elem->max_width : elem->min_width));

	ignore_max_width = true;
	break;
      default:
	assert(0);
	break;
      }

      if (highlighted)
	mark_plain(out);
      break;
    }

    case element_t::OPT_AMOUNT:
      if (details.xact) {
	std::string disp;
	bool use_disp = false;

	if (details.xact->cost && details.xact->amount) {
	  std::ostringstream stream;
	  if (! details.xact->amount_expr.expr.empty())
	    stream << details.xact->amount_expr.expr;
	  else
	    stream << details.xact->amount.strip_annotations();

	  if (! details.xact->cost_expr.empty())
	    stream << details.xact->cost_expr;
	  else
	    stream << " @ " << amount_t(*details.xact->cost /
					details.xact->amount).unround();
	  disp = stream.str();
	  use_disp = true;
	}
	else if (details.entry) {
	  unsigned int    xacts_count = 0;
	  transaction_t * first = NULL;
	  transaction_t * last  = NULL;

	  for (transactions_list::const_iterator i
		 = details.entry->transactions.begin();
	       i != details.entry->transactions.end();
	       i++)
	    if (transaction_has_xdata(**i) &&
		transaction_xdata_(**i).dflags & TRANSACTION_TO_DISPLAY) {
	      xacts_count++;
	      if (! first)
		first = *i;
	      last = *i;
	    }

	  use_disp = (xacts_count == 2 && details.xact == last &&
		      first->amount == - last->amount);
	}

	if (! use_disp) {
	  if (! details.xact->amount_expr.expr.empty())
	    out << details.xact->amount_expr.expr;
	  else
	    out << details.xact->amount.strip_annotations();
	} else {
	  out << disp;
	}
      }
      break;

    case element_t::SOURCE:
      if (details.entry && details.entry->journal) {
	int idx = details.entry->src_idx;
	for (strings_list::iterator i = details.entry->journal->sources.begin();
	     i != details.entry->journal->sources.end();
	     i++)
	  if (! idx--) {
	    out << *i;
	    break;
	  }
      }
      break;

    case element_t::ENTRY_BEG_POS:
      if (details.entry)
	out << (unsigned long)details.entry->beg_pos;
      break;

    case element_t::ENTRY_BEG_LINE:
      if (details.entry)
	out << details.entry->beg_line;
      break;

    case element_t::ENTRY_END_POS:
      if (details.entry)
	out << (unsigned long)details.entry->end_pos;
      break;

    case element_t::ENTRY_END_LINE:
      if (details.entry)
	out << details.entry->end_line;
      break;

    case element_t::XACT_BEG_POS:
      if (details.xact)
	out << (unsigned long)details.xact->beg_pos;
      break;

    case element_t::XACT_BEG_LINE:
      if (details.xact)
	out << details.xact->beg_line;
      break;

    case element_t::XACT_END_POS:
      if (details.xact)
	out << (unsigned long)details.xact->end_pos;
      break;

    case element_t::XACT_END_LINE:
      if (details.xact)
	out << details.xact->end_line;
      break;

    case element_t::DATE_STRING: {
      datetime_t date;
      if (details.xact)
	date = details.xact->date();
      else if (details.entry)
	date = details.entry->date();

      char buf[256];
      std::strftime(buf, 255, elem->chars.c_str(), date.localtime());
      out << (elem->max_width == 0 ? buf : truncate(buf, elem->max_width));
      break;
    }

    case element_t::COMPLETE_DATE_STRING: {
      datetime_t actual_date;
      datetime_t effective_date;
      if (details.xact) {
	actual_date    = details.xact->actual_date();
	effective_date = details.xact->effective_date();
      }
      else if (details.entry) {
	actual_date    = details.entry->actual_date();
	effective_date = details.entry->effective_date();
      }

      char abuf[256];
      std::strftime(abuf, 255, elem->chars.c_str(), actual_date.localtime());

      if (effective_date && effective_date != actual_date) {
	char buf[512];
	char ebuf[256];
	std::strftime(ebuf, 255, elem->chars.c_str(),
		      effective_date.localtime());

	std::strcpy(buf, abuf);
	std::strcat(buf, "=");
	std::strcat(buf, ebuf);

	out << (elem->max_width == 0 ? buf : truncate(buf, elem->max_width));
      } else {
	out << (elem->max_width == 0 ? abuf : truncate(abuf, elem->max_width));
      }
      break;
    }

    case element_t::CLEARED:
      if (details.xact) {
	switch (details.xact->state) {
	case transaction_t::CLEARED:
	  out << "* ";
	  break;
	case transaction_t::PENDING:
	  out << "! ";
	  break;
	}
      }
      break;

    case element_t::ENTRY_CLEARED:
      if (details.entry) {
	transaction_t::state_t state;
	if (details.entry->get_state(&state))
	  switch (state) {
	  case transaction_t::CLEARED:
	    out << "* ";
	    break;
	  case transaction_t::PENDING:
	    out << "! ";
	    break;
	  }
      }
      break;

    case element_t::CODE: {
      std::string temp;
      if (details.entry && ! details.entry->code.empty()) {
	temp += "(";
	temp += details.entry->code;
	temp += ") ";
      }
      out << temp;
      break;
    }

    case element_t::PAYEE:
      if (details.entry)
	out << (elem->max_width == 0 ?
		details.entry->payee : truncate(details.entry->payee,
						elem->max_width));
      break;

    case element_t::OPT_NOTE:
      if (details.xact && ! details.xact->note.empty())
	out << "  ; ";
      // fall through...

    case element_t::NOTE:
      if (details.xact)
	out << (elem->max_width == 0 ?
		details.xact->note : truncate(details.xact->note,
					      elem->max_width));
      break;

    case element_t::OPT_ACCOUNT:
      if (details.entry && details.xact) {
	transaction_t::state_t state;
	if (! details.entry->get_state(&state))
	  switch (details.xact->state) {
	  case transaction_t::CLEARED:
	    name = "* ";
	    break;
	  case transaction_t::PENDING:
	    name = "! ";
	    break;
	  }
      }
      // fall through...

    case element_t::ACCOUNT_NAME:
    case element_t::ACCOUNT_FULLNAME:
      if (details.account) {
	name += (elem->type == element_t::ACCOUNT_FULLNAME ?
		 details.account->fullname() :
		 partial_account_name(*details.account));

	if (details.xact && details.xact->flags & TRANSACTION_VIRTUAL) {
	  if (elem->max_width > 2)
	    name = truncate(name, elem->max_width - 2, true);

	  if (details.xact->flags & TRANSACTION_BALANCE)
	    name = "[" + name + "]";
	  else
	    name = "(" + name + ")";
	}
	else if (elem->max_width > 0)
	  name = truncate(name, elem->max_width, true);

	out << name;
      } else {
	out << " ";
      }
      break;

    case element_t::SPACER:
      out << " ";
      break;

    case element_t::DEPTH_SPACER:
      for (const account_t * acct = details.account;
	   acct;
	   acct = acct->parent)
	if (account_has_xdata(*acct) &&
	    account_xdata_(*acct).dflags & ACCOUNT_DISPLAYED) {
	  if (elem->min_width > 0 || elem->max_width > 0)
	    out.width(elem->min_width > elem->max_width ?
		      elem->min_width : elem->max_width);
	  out << " ";
	}
      break;
#endif

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>
#if 0
#include <boost/python/suite/indexing/list_indexing_suite.hpp>
#endif

using namespace boost::python;
using namespace ledger;

value_t py_repitem_total(repitem_t * item) {
  value_t temp;
  item->add_total(temp);
  return temp;
}

value_t py_repitem_value(repitem_t * item) {
  value_t temp;
  item->add_value(temp);
  return temp;
}

value_t py_repitem_sort_value(repitem_t * item) {
  value_t temp;
  item->add_sort_value(temp);
  return temp;
}

void export_repitem()
{
  class_< repitem_t > ("ReportItem")
#if 0
    .def(self == self)
    .def(self != self)
#endif

    .add_property("total", &py_repitem_total)
    .add_property("value", &py_repitem_value)
    .add_property("sort_value", &py_repitem_sort_value)

    .add_property("date", &repitem_t::date)
    .add_property("effective_date", &repitem_t::effective_date)
    .add_property("actual_date", &repitem_t::actual_date)

    .add_property("account",
		  make_function(&repitem_t::account,
				return_value_policy<reference_existing_object>()))

    .def("add_content", &repitem_t::add_content,
	 return_internal_reference<1, with_custodian_and_ward<1, 2> >())

    .def("add_child", &repitem_t::add_child,
	 return_internal_reference<1, with_custodian_and_ward<1, 2> >())

    .def("valid", &repitem_t::valid)
    ;

#if 0
  class_< transform_queue_list > ("TransformQueueList")
    .def(list_indexing_suite<transform_queue_list>())
    ;
#endif
}

#endif // USE_BOOST_PYTHON

#if 0
void init_valexpr_t()
{
  global_scope.reset(new scope_t());
  scope_t * globals = global_scope.get();

  valexpr_t::node_t * node;

  // Basic terms
  node = new valexpr_t::node_t(valexpr_t::node_t::F_NOW);
  globals->define("m", node);
  globals->define("now", node);
  globals->define("today", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::AMOUNT);
  globals->define("a", node);
  globals->define("amount", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::PRICE);
  globals->define("i", node);
  globals->define("price", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::COST);
  globals->define("b", node);
  globals->define("cost", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::DATE);
  globals->define("d", node);
  globals->define("date", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::ACT_DATE);
  globals->define("act_date", node);
  globals->define("actual_date", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::EFF_DATE);
  globals->define("eff_date", node);
  globals->define("effective_date", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::CLEARED);
  globals->define("X", node);
  globals->define("cleared", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::PENDING);
  globals->define("Y", node);
  globals->define("pending", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::REAL);
  globals->define("R", node);
  globals->define("real", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::ACTUAL);
  globals->define("L", node);
  globals->define("actual", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::INDEX);
  globals->define("n", node);
  globals->define("index", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::COUNT);
  globals->define("N", node);
  globals->define("count", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::DEPTH);
  globals->define("l", node);
  globals->define("depth", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::TOTAL);
  globals->define("O", node);
  globals->define("total", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::PRICE_TOTAL);
  globals->define("I", node);
  globals->define("total_price", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::COST_TOTAL);
  globals->define("B", node);
  globals->define("total_cost", node);

  // Relating to format_t
  globals->define("t", new valexpr_t::node_t(valexpr_t::node_t::VALEXPR_T));
  globals->define("T", new valexpr_t::node_t(valexpr_t::node_t::TOTAL_EXPR));

  // Functions
  node = new valexpr_t::node_t(valexpr_t::node_t::O_DEF);
  node->set_left(new valexpr_t::node_t(valexpr_t::node_t::ARG_INDEX));
  node->left->arg_index = 1;
  node->set_right(new valexpr_t::node_t(valexpr_t::node_t::F_ABS));
  globals->define("U", node);
  globals->define("abs", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::O_DEF);
  node->set_left(new valexpr_t::node_t(valexpr_t::node_t::ARG_INDEX));
  node->left->arg_index = 1;
  node->set_right(new valexpr_t::node_t(valexpr_t::node_t::F_ROUND));
  globals->define("round", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::O_DEF);
  node->set_left(new valexpr_t::node_t(valexpr_t::node_t::ARG_INDEX));
  node->left->arg_index = 1;
  node->set_right(new valexpr_t::node_t(valexpr_t::node_t::F_QUANTITY));
  globals->define("S", node);
  globals->define("quant", node);
  globals->define("quantity", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::O_DEF);
  node->set_left(new valexpr_t::node_t(valexpr_t::node_t::ARG_INDEX));
  node->left->arg_index = 1;
  node->set_right(new valexpr_t::node_t(valexpr_t::node_t::F_COMMODITY));
  globals->define("comm", node);
  globals->define("commodity", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::O_DEF);
  node->set_left(new valexpr_t::node_t(valexpr_t::node_t::ARG_INDEX));
  node->left->arg_index = 2;
  node->set_right(new valexpr_t::node_t(valexpr_t::node_t::F_SET_COMMODITY));
  globals->define("setcomm", node);
  globals->define("set_commodity", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::O_DEF);
  node->set_left(new valexpr_t::node_t(valexpr_t::node_t::ARG_INDEX));
  node->left->arg_index = 1;
  node->set_right(new valexpr_t::node_t(valexpr_t::node_t::F_ARITH_MEAN));
  globals->define("A", node);
  globals->define("avg", node);
  globals->define("mean", node);
  globals->define("average", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::O_DEF);
  node->set_left(new valexpr_t::node_t(valexpr_t::node_t::ARG_INDEX));
  node->left->arg_index = 2;
  node->set_right(new valexpr_t::node_t(valexpr_t::node_t::F_VALUE));
  globals->define("P", node);

  parse_value_definition("@value=@P(@t,@m)", globals);
  parse_value_definition("@total_value=@P(@T,@m)", globals);
  parse_value_definition("@valueof(x)=@P(@x,@m)", globals);
  parse_value_definition("@datedvalueof(x,y)=@P(@x,@y)", globals);

  node = new valexpr_t::node_t(valexpr_t::node_t::O_DEF);
  node->set_left(new valexpr_t::node_t(valexpr_t::node_t::ARG_INDEX));
  node->left->arg_index = 1;
  node->set_right(new valexpr_t::node_t(valexpr_t::node_t::F_PRICE));
  globals->define("priceof", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::O_DEF);
  node->set_left(new valexpr_t::node_t(valexpr_t::node_t::ARG_INDEX));
  node->left->arg_index = 1;
  node->set_right(new valexpr_t::node_t(valexpr_t::node_t::F_DATE));
  globals->define("dateof", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::O_DEF);
  node->set_left(new valexpr_t::node_t(valexpr_t::node_t::ARG_INDEX));
  node->left->arg_index = 2;
  node->set_right(new valexpr_t::node_t(valexpr_t::node_t::F_DATECMP));
  globals->define("datecmp", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::O_DEF);
  node->set_left(new valexpr_t::node_t(valexpr_t::node_t::ARG_INDEX));
  node->left->arg_index = 1;
  node->set_right(new valexpr_t::node_t(valexpr_t::node_t::F_YEAR));
  globals->define("yearof", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::O_DEF);
  node->set_left(new valexpr_t::node_t(valexpr_t::node_t::ARG_INDEX));
  node->left->arg_index = 1;
  node->set_right(new valexpr_t::node_t(valexpr_t::node_t::F_MONTH));
  globals->define("monthof", node);

  node = new valexpr_t::node_t(valexpr_t::node_t::O_DEF);
  node->set_left(new valexpr_t::node_t(valexpr_t::node_t::ARG_INDEX));
  node->left->arg_index = 1;
  node->set_right(new valexpr_t::node_t(valexpr_t::node_t::F_DAY));
  globals->define("dayof", node);

  parse_value_definition("@year=@yearof(@d)", globals);
  parse_value_definition("@month=@monthof(@d)", globals);
  parse_value_definition("@day=@dayof(@d)", globals);

  // Macros
  node = parse_valexpr_t("@P(@a,@d)");
  globals->define("v", node);
  globals->define("market", node);

  node = parse_valexpr_t("@P(@O,@d)");
  globals->define("V", node);
  globals->define("total_market", node);

  node = parse_valexpr_t("@v-@b");
  globals->define("g", node);
  globals->define("gain", node);

  node = parse_valexpr_t("@V-@B");
  globals->define("G", node);
  globals->define("total_gain", node);

  parse_value_definition("@min(x,y)=@x<@y?@x:@y", globals);
  parse_value_definition("@max(x,y)=@x>@y?@x:@y", globals);
}
#endif
