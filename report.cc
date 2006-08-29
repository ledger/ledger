#include "report.h"

namespace ledger {

report_t::report_t()
{
  ledger::amount_expr = "@a";
  ledger::total_expr  = "@O";

  predicate	      = "";
  secondary_predicate = "";
  display_predicate   = "";
  descend_expr	      = "";

  budget_flags   = BUDGET_NO_BUDGET;

  head_entries = 0;
  tail_entries = 0;

  show_collapsed     = false;
  show_subtotal      = false;
  show_totals        = false;
  show_related       = false;
  show_all_related   = false;
  show_inverted      = false;
  show_empty	     = false;
  days_of_the_week   = false;
  by_payee           = false;
  comm_as_payee      = false;
  code_as_payee      = false;
  show_revalued      = false;
  show_revalued_only = false;
  keep_price         = false;
  keep_date          = false;
  keep_tag           = false;
  entry_sort         = false;
  sort_all           = false;
}

void
report_t::regexps_to_predicate(const std::string& command,
			       std::list<std::string>::const_iterator begin,
			       std::list<std::string>::const_iterator end,
			       const bool account_regexp,
			       const bool add_account_short_masks,
			       const bool logical_and)
{
  std::string regexps[2];

  assert(begin != end);

  // Treat the remaining command-line arguments as regular
  // expressions, used for refining report results.

  for (std::list<std::string>::const_iterator i = begin;
       i != end;
       i++)
    if ((*i)[0] == '-') {
      if (! regexps[1].empty())
	regexps[1] += "|";
      regexps[1] += (*i).substr(1);
    }
    else if ((*i)[0] == '+') {
      if (! regexps[0].empty())
	regexps[0] += "|";
      regexps[0] += (*i).substr(1);
    }
    else {
      if (! regexps[0].empty())
	regexps[0] += "|";
      regexps[0] += *i;
    }

  for (int i = 0; i < 2; i++) {
    if (regexps[i].empty())
      continue;

    if (! predicate.empty())
      predicate += logical_and ? "&" : "|";

    int add_predicate = 0;	// 1 adds /.../, 2 adds ///.../
    if (i == 1) {
      predicate += "!";
    }
    else if (add_account_short_masks) {
      if (regexps[i].find(':') != std::string::npos ||
	  regexps[i].find('.') != std::string::npos ||
	  regexps[i].find('*') != std::string::npos ||
	  regexps[i].find('+') != std::string::npos ||
	  regexps[i].find('[') != std::string::npos ||
	  regexps[i].find('(') != std::string::npos) {
	show_subtotal = true;
	add_predicate = 1;
      } else {
	add_predicate = 2;
      }
    }
    else {
      add_predicate = 1;
    }

    if (i != 1 && command == "b" && account_regexp) {
      if (! show_related && ! show_all_related) {
	if (! display_predicate.empty())
	  display_predicate += "&";
	if (! show_empty)
	  display_predicate += "T&";

	if (add_predicate == 2)
	  display_predicate += "//";
	display_predicate += "/(?:";
	display_predicate += regexps[i];
	display_predicate += ")/";
      }
      else if (! show_empty) {
	if (! display_predicate.empty())
	  display_predicate += "&";
	display_predicate += "T";
      }
    }

    if (! account_regexp)
      predicate += "/";
    predicate += "/(?:";
    predicate += regexps[i];
    predicate += ")/";
  }
}

void report_t::process_options(const std::string&     command,
			       strings_list::iterator arg,
			       strings_list::iterator args_end)
{
  // Configure some other options depending on report type

  if (command == "p" || command == "e" || command == "w") {
    show_related     =
    show_all_related = true;
  }
  else if (command == "E") {
    show_subtotal = true;
  }
  else if (show_related) {
    if (command == "r") {
      show_inverted = true;
    } else {
      show_subtotal    = true;
      show_all_related = true;
    }
  }

  if (command != "b" && command != "r")
    amount_t::keep_base = true;

  // Process remaining command-line arguments

  if (command != "e") {
    // Treat the remaining command-line arguments as regular
    // expressions, used for refining report results.

    std::list<std::string>::iterator i = arg;
    for (; i != args_end; i++)
      if (*i == "--")
	break;

    if (i != arg)
      regexps_to_predicate(command, arg, i, true,
			   (command == "b" && ! show_subtotal &&
			    display_predicate.empty()));
    if (i != args_end && ++i != args_end)
      regexps_to_predicate(command, i, args_end);
  }

  // Setup the default value for the display predicate

  if (display_predicate.empty()) {
    if (command == "b") {
      if (! show_empty)
	display_predicate = "T";
      if (! show_subtotal) {
	if (! display_predicate.empty())
	  display_predicate += "&";
	display_predicate += "l<=1";
      }
    }
    else if (command == "E") {
      display_predicate = "t";
    }
    else if (command == "r" && ! show_empty) {
      display_predicate = "a";
    }
  }

  DEBUG_PRINT("ledger.config.predicates", "Predicate: " << predicate);
  DEBUG_PRINT("ledger.config.predicates", "Display P: " << display_predicate);

  // Setup the values of %t and %T, used in format strings

  if (! amount_expr.empty())
    ledger::amount_expr = amount_expr;
  if (! total_expr.empty())
    ledger::total_expr  = total_expr;

  // Now setup the various formatting strings

  if (! date_output_format.empty())
    date_t::output_format = date_output_format;

  amount_t::keep_price = keep_price;
  amount_t::keep_date  = keep_date;
  amount_t::keep_tag   = keep_tag;

  if (! report_period.empty() && ! sort_all)
    entry_sort = true;
}

item_handler<transaction_t> *
report_t::chain_xact_handlers(const std::string& command,
			      item_handler<transaction_t> * base_formatter,
			      journal_t * journal,
			      account_t * master,
			      std::list<item_handler<transaction_t> *>& ptrs)
{
  bool remember_components = false;

  item_handler<transaction_t> * formatter = NULL;

  ptrs.push_back(formatter = base_formatter);

  // format_transactions write each transaction received to the
  // output stream.
  if (! (command == "b" || command == "E")) {
    // truncate_entries cuts off a certain number of _entries_ from
    // being displayed.  It does not affect calculation.
    if (head_entries || tail_entries)
      ptrs.push_back(formatter =
		     new truncate_entries(formatter,
					  head_entries, tail_entries));

    // filter_transactions will only pass through transactions
    // matching the `display_predicate'.
    if (! display_predicate.empty())
      ptrs.push_back(formatter =
		     new filter_transactions(formatter,
					     display_predicate));

    // calc_transactions computes the running total.  When this
    // appears will determine, for example, whether filtered
    // transactions are included or excluded from the running total.
    ptrs.push_back(formatter = new calc_transactions(formatter));

    // component_transactions looks for reported transaction that
    // match the given `descend_expr', and then reports the
    // transactions which made up the total for that reported
    // transaction.
    if (! descend_expr.empty()) {
      std::list<std::string> descend_exprs;

      std::string::size_type beg = 0;
      for (std::string::size_type pos = descend_expr.find(';');
	   pos != std::string::npos;
	   beg = pos + 1, pos = descend_expr.find(';', beg))
	descend_exprs.push_back(std::string(descend_expr, beg, pos - beg));
      descend_exprs.push_back(std::string(descend_expr, beg));

      for (std::list<std::string>::reverse_iterator i =
	     descend_exprs.rbegin();
	   i != descend_exprs.rend();
	   i++)
	ptrs.push_back(formatter =
		       new component_transactions(formatter, *i));

      remember_components = true;
    }

    // reconcile_transactions will pass through only those
    // transactions which can be reconciled to a given balance
    // (calculated against the transactions which it receives).
    if (! reconcile_balance.empty()) {
      datetime_t cutoff = datetime_t::now;
      if (! reconcile_date.empty())
	cutoff = reconcile_date;
      ptrs.push_back(formatter =
		     new reconcile_transactions
		     (formatter, value_t(reconcile_balance), cutoff));
    }

    // filter_transactions will only pass through transactions
    // matching the `secondary_predicate'.
    if (! secondary_predicate.empty())
      ptrs.push_back(formatter =
		     new filter_transactions(formatter,
					     secondary_predicate));

    // sort_transactions will sort all the transactions it sees, based
    // on the `sort_order' value expression.
    if (! sort_string.empty()) {
      if (entry_sort)
	ptrs.push_back(formatter =
		       new sort_entries(formatter, sort_string));
      else
	ptrs.push_back(formatter =
		       new sort_transactions(formatter, sort_string));
    }

    // changed_value_transactions adds virtual transactions to the
    // list to account for changes in market value of commodities,
    // which otherwise would affect the running total unpredictably.
    if (show_revalued)
      ptrs.push_back(formatter =
		     new changed_value_transactions(formatter,
						    show_revalued_only));

    // collapse_transactions causes entries with multiple transactions
    // to appear as entries with a subtotaled transaction for each
    // commodity used.
    if (show_collapsed)
      ptrs.push_back(formatter = new collapse_transactions(formatter));

    // subtotal_transactions combines all the transactions it receives
    // into one subtotal entry, which has one transaction for each
    // commodity in each account.
    //
    // period_transactions is like subtotal_transactions, but it
    // subtotals according to time periods rather than totalling
    // everything.
    //
    // dow_transactions is like period_transactions, except that it
    // reports all the transactions that fall on each subsequent day
    // of the week.
    if (show_subtotal)
      ptrs.push_back(formatter =
		     new subtotal_transactions(formatter, remember_components));

    if (days_of_the_week)
      ptrs.push_back(formatter =
		     new dow_transactions(formatter, remember_components));
    else if (by_payee)
      ptrs.push_back(formatter =
		     new by_payee_transactions(formatter, remember_components));

    // interval_transactions groups transactions together based on a
    // time period, such as weekly or monthly.
    if (! report_period.empty()) {
      ptrs.push_back(formatter =
		     new interval_transactions(formatter, report_period,
					       remember_components));
      ptrs.push_back(formatter = new sort_transactions(formatter, "d"));
    }
  }

  // invert_transactions inverts the value of the transactions it
  // receives.
  if (show_inverted)
    ptrs.push_back(formatter = new invert_transactions(formatter));

  // related_transactions will pass along all transactions related
  // to the transaction received.  If `show_all_related' is true,
  // then all the entry's transactions are passed; meaning that if
  // one transaction of an entry is to be printed, all the
  // transaction for that entry will be printed.
  if (show_related)
    ptrs.push_back(formatter =
		   new related_transactions(formatter,
					    show_all_related));

  // This filter_transactions will only pass through transactions
  // matching the `predicate'.
  if (! predicate.empty())
    ptrs.push_back(formatter = new filter_transactions(formatter, predicate));

  // budget_transactions takes a set of transactions from a data
  // file and uses them to generate "budget transactions" which
  // balance against the reported transactions.
  //
  // forecast_transactions is a lot like budget_transactions, except
  // that it adds entries only for the future, and does not balance
  // them against anything but the future balance.

  if (budget_flags) {
    budget_transactions * handler
      = new budget_transactions(formatter, budget_flags);
    handler->add_period_entries(journal->period_entries);
    ptrs.push_back(formatter = handler);

    // Apply this before the budget handler, so that only matching
    // transactions are calculated toward the budget.  The use of
    // filter_transactions above will further clean the results so
    // that no automated transactions that don't match the filter get
    // reported.
    if (! predicate.empty())
      ptrs.push_back(formatter = new filter_transactions(formatter, predicate));
  }
  else if (! forecast_limit.empty()) {
    forecast_transactions * handler
      = new forecast_transactions(formatter, forecast_limit);
    handler->add_period_entries(journal->period_entries);
    ptrs.push_back(formatter = handler);

    // See above, under budget_transactions.
    if (! predicate.empty())
      ptrs.push_back(formatter = new filter_transactions(formatter, predicate));
  }

  if (comm_as_payee)
    ptrs.push_back(formatter = new set_comm_as_payee(formatter));
  else if (code_as_payee)
    ptrs.push_back(formatter = new set_code_as_payee(formatter));

  return formatter;
}

repitem_t::~repitem_t()
{
  if (istemp) {
    switch (kind) {
    case XACT:
      delete xact;
      break;
    case ENTRY:
      delete entry;
      break;
    case ACCOUNT:
      delete account_ptr;
      break;
    }
  }
  if (contents) delete contents;
  if (children) delete children;
  if (next) delete next;
}

void repitem_t::add_total(value_t& val) const
{
  add_value(val);

  for (repitem_t * ptr = children; ptr; ptr = ptr->next)
    ptr->add_total(val);
}

void repitem_t::add_value(value_t& val) const
{
  switch (kind) {
  case XACT:
    add_transaction_to(*xact, val);
    break;

  case ENTRY:
  case ACCOUNT:
    for (repitem_t * ptr = contents; ptr; ptr = ptr->next)
      ptr->add_total(val);
    break;
  }
}

void repitem_t::add_sort_value(value_t& val) const
{
  assert(0);
}

datetime_t repitem_t::date() const
{
  if (reported_date)
    return reported_date;

  switch (kind) {
  case XACT: return xact->date();
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
  case XACT:  return xact->effective_date();
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
  case XACT: return xact->actual_date();
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
  case XACT:
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

repitem_t * repitem_t::wrap_item(transaction_t * txact)
{
  repitem_t * temp = new repitem_t;
  temp->xact = txact;
  temp->kind = XACT;
  return temp;
}

repitem_t * repitem_t::wrap_item(entry_t * tentry)
{
  repitem_t * temp = new repitem_t;
  temp->entry = tentry;
  temp->kind = ENTRY;
  return temp;
}

repitem_t * repitem_t::wrap_item(account_t * taccount)
{
  repitem_t * temp = new repitem_t;
  temp->account_ptr = taccount;
  temp->kind = ACCOUNT;
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
  repitem_t * temp = new repitem_t;
  temp->xact = new transaction_t(taccount);
  temp->kind = XACT;
  temp->istemp = true;
  return temp;
}

repitem_t * repitem_t::fake_entry(const datetime_t& date, const std::string& payee)
{
  repitem_t * temp = new repitem_t;
  temp->entry = new entry_t;
  temp->entry->_date = date;
  temp->entry->payee = payee;
  temp->kind = ENTRY;
  temp->istemp = true;
  return temp;
}

void repitem_t::populate_entries(entries_list& entries,
				 const value_expr_t * filter)
{
  item_predicate<transaction_t> predicate(filter);

  for (entries_list::iterator i = entries.begin();
       i != entries.end();
       i++) {
    repitem_t * entry = NULL;
    for (transactions_list::iterator j = (*i)->transactions.begin();
	 j != (*i)->transactions.end();
	 j++) {
      if (predicate(**j)) {
	if (entry == NULL)
	  entry = repitem_t::wrap_item(*i);
	entry->add_content(repitem_t::wrap_item(*j));
      }
    }
    if (entry != NULL)
      add_content(entry);
  }
}

void repitem_t::populate_entries(entries_list& entries)
{
  for (entries_list::iterator i = entries.begin();
       i != entries.end();
       i++) {
    repitem_t * entry = repitem_t::wrap_item(*i);
    for (transactions_list::iterator j = (*i)->transactions.begin();
	 j != (*i)->transactions.end();
	 j++)
      entry->add_content(repitem_t::wrap_item(*j));
    add_content(entry);
  }
}

void repitem_t::populate_account(account_t& acct, repitem_t * item)
{
  repitem_t * acct_item;
  if (acct.parent == NULL)
    acct_item = this;
  else if (acct.data == NULL)
    acct.data = acct_item = repitem_t::wrap_item(&acct);
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
				  const value_expr_t * filter)
{
  item_predicate<transaction_t> predicate(filter);
  for (entries_list::iterator i = entries.begin();
       i != entries.end();
       i++)
    for (transactions_list::iterator j = (*i)->transactions.begin();
	 j != (*i)->transactions.end();
	 j++)
      if (predicate(**j))
	populate_account(*(*j)->account, repitem_t::wrap_item(*j));
}

void repitem_t::populate_accounts(entries_list& entries)
{
  for (entries_list::iterator i = entries.begin();
       i != entries.end();
       i++)
    for (transactions_list::iterator j = (*i)->transactions.begin();
	 j != (*i)->transactions.end();
	 j++)
      populate_account(*(*j)->account, repitem_t::wrap_item(*j));
}

void repitem_t::print_tree(std::ostream& out, int depth)
{
  for (int i = 0; i < depth; i++)
    out << "  ";

  switch (kind) {
  case XACT: out << "XACT " << xact; break;
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

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>
#include <boost/python/exception_translator.hpp>

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

void export_report()
{
  class_< repitem_t > ("ReportItem")
    //.def(self == self)
    //.def(self != self)

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
}

#endif // USE_BOOST_PYTHON
