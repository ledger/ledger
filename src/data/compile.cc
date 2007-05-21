/*
 * Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
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

#include "compile.h"
#include "parser.h"

namespace ledger {
namespace xml {

void compile_node(node_t& node, xpath_t::scope_t& scope)
{
  switch (node.name_id()) {
  case JOURNAL_NODE:
    downcast<journal_node_t>(node).compile(scope);
    break;
  case ENTRY_NODE:
    downcast<entry_node_t>(node).compile(scope);
    break;
  case TRANSACTION_NODE:
    downcast<transaction_node_t>(node).compile(scope);
    break;

  default:
    break;
  }

  node.compiled = true;

  if (node.is_parent_node())
    foreach (node_t * child, node.as_parent_node())
      compile_node(*child, scope);
}

void journal_node_t::compile(xpath_t::scope_t& scope)
{
  if (! journal.get())
    journal.reset(new journal_t);
}

void entry_node_t::compile(xpath_t::scope_t& scope)
{
  parent_node_t& parent_node(*parent());

  assert(parent_node.name_id() == JOURNAL_NODE);
  assert(parent_node.is_compiled());

  journal_t * journal = downcast<journal_node_t>(parent_node).journal.get();

  if (! entry.get()) {
    entry.reset(new entry_t);
#if 0
    journal->add_entry(entry.get());
#endif
  }
  entry->journal = journal;

  foreach (attr_pair& attr, *attributes) {
    if (attr.first == DATE_ATTR && attr.second.is_string())
      entry->_date = parse_datetime(attr.second.as_string().c_str());
    else if (attr.first == EFF_DATE_ATTR && attr.second.is_string())
      entry->_date_eff = parse_datetime(attr.second.as_string().c_str());
    else if (attr.first == CODE_ATTR)
      entry->code = attr.second.as_string();
  }
}

void transaction_node_t::parse_amount_expr(xpath_t::scope_t& scope,
					   const char * amount_expr)
{
  value_t * amount;

  std::istringstream in(amount_expr);

  PUSH_CONTEXT();

  // jww (2006-09-15): Make sure it doesn't gobble up the upcoming @ symbol

  unsigned long beg = (long)in.tellg();

  amount_t temp;
  temp.parse(in, AMOUNT_PARSE_NO_REDUCE);

  char c;
  if (! in.eof() && (c = peek_next_nonws(in)) != '@' &&
      c != ';' && ! in.eof()) {
    in.seekg(beg, std::ios::beg);

    xpath_t xpath(in, (XPATH_PARSE_NO_REDUCE | XPATH_PARSE_RELAXED |
		       XPATH_PARSE_PARTIAL));

    xpath_t::context_scope_t node_scope(scope, this);
    amount = &set_attr(AMOUNT_ATTR, xpath.calc(node_scope));

    //unsigned long end = (long)in.tellg();
  } else {
    amount = &set_attr(AMOUNT_ATTR, temp);
  }

  // jww (2007-04-30): This should be a string context, or perhaps a
  // file context
  POP_CONTEXT(context("While parsing transaction amount"));

  // Parse the optional cost (@ PER-UNIT-COST, @@ TOTAL-COST)

  unsigned int linenum = -1;

  if (in.good() && ! in.eof()) {
    char c = peek_next_nonws(in);
    if (c == '@') {
      DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
	    "Found a price indicator");
      bool per_unit = true;
      in.get(c);
      if (in.peek() == '@') {
	in.get(c);
	per_unit = false;
	DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
	      "And it's for a total price");
      }

      if (in.good() && ! in.eof()) {
	amount_t temp;

	PUSH_CONTEXT();

	//unsigned long beg = (long)in.tellg();

	temp.parse(in);

	if (temp.sign() < 0)
	  throw_(parse_error, "A transaction's cost may not be negative");

	//unsigned long end = (long)in.tellg();

	POP_CONTEXT(context("While parsing transaction cost"));

	amount_t per_unit_cost(temp);
	amount_t& base_amount(amount->as_amount_lval());
	if (per_unit)
	  temp *= base_amount.number();
	else
	  per_unit_cost /= base_amount.number();

	value_t& cost = set_attr(COST_ATTR, temp);

	if (base_amount.commodity() && ! base_amount.commodity().annotated) {
	  assert(transaction);
	  assert(transaction->entry);
	  base_amount.annotate_commodity
	    (annotation_t(per_unit_cost, transaction->entry->actual_date(),
			  transaction->entry->code));
	}

	DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
	      "Total cost is " << cost);
	DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
	      "Per-unit cost is " << per_unit_cost);
	DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
	      "Annotated amount is " << base_amount);
	DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
	      "Bare amount is " << base_amount.number());
      }
    }
  }

  amount->in_place_reduce();

  DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
	"Reduced amount is " << *amount);
}

void transaction_node_t::compile(xpath_t::scope_t& scope)
{
  parent_node_t& parent_node(*parent());

  assert(parent_node.name_id() == ENTRY_NODE);
  assert(parent_node.is_compiled());

  entry_t * entry = downcast<entry_node_t>(parent_node).entry.get();

  if (! transaction.get()) {
    transaction.reset(new transaction_t);
#if 0
    entry->add_transaction(transaction.get());
#endif
  }
  transaction->entry = entry;

  foreach (node_t * child, *this) {
    switch (child->name_id()) {
    case AMOUNT_EXPR_NODE:
      parse_amount_expr(scope, child->as_terminal_node().text());
      break;

    case ACCOUNT_PATH_NODE: {
      assert(entry);

      journal_t * journal = entry->journal;
      assert(journal);

      transaction->account =
	journal->find_account(child->as_terminal_node().text());

      // jww (2007-05-18): Need to set an attribute that refers to the
      // unique id of the account
      break;
    }

    default:
      break;
    }
  }
}

} // namespace xml
} // namespace ledger
