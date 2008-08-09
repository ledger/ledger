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

#if defined(__GNUG__) && __GNUG__ < 3
#define _XOPEN_SOURCE
#endif

#include "textual.h"
#include "expr.h"
#include "parser.h"
#include "session.h"
#include "option.h"
#include "acconf.h"

#define TIMELOG_SUPPORT 1

namespace ledger {

#define MAX_LINE 1024

static path         pathname;
static unsigned int linenum;
static unsigned int src_idx;
static accounts_map account_aliases;

static std::list<std::pair<path, int> > include_stack;

#ifdef TIMELOG_SUPPORT
struct time_entry_t
{
  datetime_t  checkin;
  account_t * account;
  string      desc;

  time_entry_t() : account(NULL) {
    TRACE_CTOR(time_entry_t, "");
  }
  time_entry_t(const datetime_t& _checkin,
	       account_t *	 _account = NULL,
	       const string&     _desc	  = "")
    : checkin(_checkin), account(_account), desc(_desc) {
    TRACE_CTOR(time_entry_t, "const datetime_t&, account_t *, const string&");
  }
  time_entry_t(const time_entry_t& entry)
    : checkin(entry.checkin), account(entry.account),
      desc(entry.desc) {
    TRACE_CTOR(time_entry_t, "copy");
  }
  ~time_entry_t() throw() {
    TRACE_DTOR(time_entry_t);
  }
};
#endif

namespace {
  optional<expr_t> parse_amount_expr(std::istream&  in,
				     amount_t&      amount,
				     xact_t *	    xact,
				     unsigned short flags = 0)
  {
    expr_t expr(in, flags | EXPR_PARSE_PARTIAL);

    DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
	  "Parsed an amount expression");

#ifdef DEBUG_ENABLED
    DEBUG_IF("ledger.textual.parse") {
      if (_debug_stream) {
	ledger::dump_value_expr(*_debug_stream, expr);
	*_debug_stream << std::endl;
      }
    }
#endif

    if (expr) {
      amount = expr.calc(*xact).as_amount();
      DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
	    "The transaction amount is " << amount);
      return expr;
    }
    return none;
  }
}

xact_t * parse_xact(char * line, account_t * account, entry_t * entry = NULL)
{
  std::istringstream in(line);

  string err_desc;
  try {

  // The account will be determined later...
  std::auto_ptr<xact_t> xact(new xact_t(NULL));
  if (entry)
    xact->entry = entry;

  // Parse the state flag

  char p = peek_next_nonws(in);
  switch (p) {
  case '*':
    xact->state = xact_t::CLEARED;
    in.get(p);
    p = peek_next_nonws(in);
    DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
		"Parsed the CLEARED flag");
    break;
  case '!':
    xact->state = xact_t::PENDING;
    in.get(p);
    p = peek_next_nonws(in);
    DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
		"Parsed the PENDING flag");
    break;
  }

  // Parse the account name

  unsigned long account_beg = static_cast<unsigned long>(in.tellg());
  unsigned long account_end = account_beg;
  while (! in.eof()) {
    in.get(p);
    if (in.eof() || (std::isspace(p) &&
		     (p == '\t' || in.peek() == EOF ||
		      std::isspace(in.peek()))))
      break;
    account_end++;
  }

  if (account_beg == account_end)
    throw parse_error("No account was specified");

  char * b = &line[account_beg];
  char * e = &line[account_end];
  if ((*b == '[' && *(e - 1) == ']') ||
      (*b == '(' && *(e - 1) == ')')) {
    xact->add_flags(XACT_VIRTUAL);
    DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
		"Parsed a virtual account name");
    if (*b == '[') {
      xact->add_flags(XACT_BALANCE);
      DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
		  "Parsed a balanced virtual account name");
    }
    b++; e--;
  }

  string name(b, e - b);
  DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
	      "Parsed account name " << name);
  if (account_aliases.size() > 0) {
    accounts_map::const_iterator i = account_aliases.find(name);
    if (i != account_aliases.end())
      xact->account = (*i).second;
  }
  if (! xact->account)
    xact->account = account->find_account(name);

  // Parse the optional amount

  bool saw_amount = false;

  if (in.good() && ! in.eof()) {
    p = peek_next_nonws(in);
    if (in.eof())
      goto finished;
    if (p == ';')
      goto parse_note;
    if (p == '=' && entry)
      goto parse_assign;

    try {
      unsigned long beg = static_cast<unsigned long>(in.tellg());

      xact->amount_expr =
	parse_amount_expr(in, xact->amount, xact.get(),
			  EXPR_PARSE_NO_REDUCE | EXPR_PARSE_NO_ASSIGN);
      saw_amount = true;

      if (! xact->amount.is_null()) {
	xact->amount.reduce();
	DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
	      "Reduced amount is " << xact->amount);
      }

      // We don't need to store the actual expression that resulted in the
      // amount if it's constant
      if (xact->amount_expr) {
	if (xact->amount_expr->is_constant())
	  xact->amount_expr = expr_t();

	unsigned long end = static_cast<unsigned long>(in.tellg());
	xact->amount_expr->set_text(string(line, beg, end - beg));
      }
    }
    catch (const std::exception& err) {
      add_error_context("While parsing transaction amount:\n");
      throw err;
    }
  }

  // Parse the optional cost (@ PER-UNIT-COST, @@ TOTAL-COST)

  if (in.good() && ! in.eof()) {
    p = peek_next_nonws(in);
    if (p == '@') {
      if (! saw_amount)
	throw parse_error
	  ("Transaction cannot have a cost expression with an amount");
	
      DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
		  "Found a price indicator");
      bool per_unit = true;
      in.get(p);
      if (in.peek() == '@') {
	in.get(p);
	per_unit = false;
	DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
		    "And it's for a total price");
      }

      if (in.good() && ! in.eof()) {
	xact->cost = amount_t();

	try {
	  unsigned long beg = static_cast<unsigned long>(in.tellg());

	  xact->cost_expr =
	    parse_amount_expr(in, *xact->cost, xact.get(),
			      EXPR_PARSE_NO_MIGRATE |
			      EXPR_PARSE_NO_ASSIGN);

	  if (xact->cost_expr) {
	    unsigned long end = static_cast<unsigned long>(in.tellg());
	    if (per_unit)
	      xact->cost_expr->set_text(string("@") +
					string(line, beg, end - beg));
	    else
	      xact->cost_expr->set_text(string("@@") +
					string(line, beg, end - beg));
	  }
	}
	catch (const std::exception& err) {
	  add_error_context("While parsing transaction cost:\n");
	  throw err;
	}

	if (xact->cost->sign() < 0)
	  throw parse_error("A transaction's cost may not be negative");

	amount_t per_unit_cost(*xact->cost);
	if (per_unit)
	  *xact->cost *= xact->amount;
	else
	  per_unit_cost /= xact->amount;

	if (xact->amount.commodity() &&
	    ! xact->amount.commodity().annotated) {
	  if (xact->entry)
	    xact->amount.annotate(annotation_t(per_unit_cost,
					       xact->entry->actual_date(),
					       xact->entry->code));
	  else
	    xact->amount.annotate(annotation_t(per_unit_cost));
	}

	DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
		    "Total cost is " << *xact->cost);
	DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
		    "Per-unit cost is " << per_unit_cost);
	DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
		    "Annotated amount is " << xact->amount);
      }
    }
  }

 parse_assign:
  if (entry != NULL) {
    // Parse the optional assigned (= AMOUNT)

    if (in.good() && ! in.eof()) {
      p = peek_next_nonws(in);
      if (p == '=') {
	in.get(p);
	DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
	      "Found a balance assignment indicator");
	if (in.good() && ! in.eof()) {
	  amount_t amt;

	  try {
	    unsigned long beg = static_cast<unsigned long>(in.tellg());

	    optional<expr_t> total_expr =
	      parse_amount_expr(in, amt, xact.get(), EXPR_PARSE_NO_MIGRATE);

	    if (amt.is_null())
	      throw parse_error
		("An assigned balance must evaluate to a constant value");

	    DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
		  "XACT assign: parsed amt = " << amt);

	    if (total_expr) {
	      unsigned long end = static_cast<unsigned long>(in.tellg());
	      total_expr->set_text(string("=") +
				   string(line, beg, end - beg));
	    }

	    // jww (2008-08-02): Save total_expr somewhere!

	    account_t::xdata_t& xdata(xact->account->xdata());

	    DEBUG("ledger.xact.assign", "account balance = " << xdata.value);
	    DEBUG("ledger.xact.assign", "xact amount = " << amt);

	    amount_t diff;
	    if (xdata.value.is_amount()) {
	      diff = amt - xdata.value.as_amount();
	    }
	    else if (xdata.value.is_balance()) {
	      optional<amount_t> comm_bal =
		xdata.value.as_balance().commodity_amount(amt.commodity());
	      diff = amt - (comm_bal ? *comm_bal : amount_t(0L));
	    }
	    else if (xdata.value.is_balance_pair()) {
	      optional<amount_t> comm_bal =
		xdata.value.as_balance_pair().commodity_amount(amt.commodity());
	      diff = amt - (comm_bal ? *comm_bal : amount_t(0L));
	    }
	    else {
	      diff = amt;
	    }

	    DEBUG("ledger.xact.assign", "diff = " << diff);
	    DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
		  "XACT assign: diff = " << diff);

	    if (! diff.is_realzero()) {
	      if (! xact->amount.is_null()) {
		xact_t * temp =
		  new xact_t(xact->account, diff,
			     XACT_GENERATED | XACT_CALCULATED);
		entry->add_xact(temp);

		DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
		      "Created balancing transaction");
	      } else {
		xact->amount = diff;
		DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
		      "Overwrite null transaction");
	      }
	      xdata.value = amt;
	    }
	  }
	  catch (const std::exception& err) {
	    add_error_context("While parsing assigned balance:\n");
	    throw err;
	  }
	}
      }
    }
  }

  // Parse the optional note

 parse_note:
  if (in.good() && ! in.eof()) {
    p = peek_next_nonws(in);
    if (p == ';') {
      in.get(p);
      p = peek_next_nonws(in);
      xact->note = &line[static_cast<unsigned long>(in.tellg())];
      DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
		  "Parsed a note '" << *xact->note << "'");

      if (char * b = std::strchr(xact->note->c_str(), '['))
	if (char * e = std::strchr(xact->note->c_str(), ']')) {
	  char buf[256];
	  std::strncpy(buf, b + 1, e - b - 1);
	  buf[e - b - 1] = '\0';

	  DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
		"Parsed a transaction date " << buf);

	  if (char * p = std::strchr(buf, '=')) {
	    *p++ = '\0';
	    xact->_date_eff = parse_date(p);
	  }
	  if (buf[0])
	    xact->_date = parse_date(buf);
	}
    }
  }

 finished:
  return xact.release();

  }
  catch (const std::exception& err) {
    add_error_context("While parsing transaction:\n");
    add_error_context(line_context(line, static_cast<unsigned long>(in.tellg()) - 1));
    throw err;
  }
}

bool parse_xacts(std::istream& in,
		 account_t *   account,
		 entry_base_t& entry,
		 const string& kind,
		 unsigned long beg_pos)
{
  TRACE_START(entry_xacts, 1, "Time spent parsing transactions:");

  static char line[MAX_LINE + 1];
  bool	      added = false;

  while (! in.eof() && (in.peek() == ' ' || in.peek() == '\t')) {
    in.getline(line, MAX_LINE);
    if (in.eof())
      break;

    int len = std::strlen(line);
    if (line[len - 1] == '\r')
      line[--len] = '\0';

    beg_pos += len + 1;
    linenum++;

    if (line[0] == ' ' || line[0] == '\t') {
      char * p = skip_ws(line);
      if (! *p)
	break;
    }
    if (xact_t * xact = parse_xact(line, account)) {
      entry.add_xact(xact);
      added = true;
    }
  }

  TRACE_STOP(entry_xacts, 1);

  return added;
}

entry_t * parse_entry(std::istream& in, char * line, account_t * master,
		      textual_parser_t& parser, unsigned long& pos)
{
  TRACE_START(entry_text, 1, "Time spent preparing entry text:");

  std::auto_ptr<entry_t> curr(new entry_t);

  // Parse the date

  char * next = next_element(line);

  if (char * p = std::strchr(line, '=')) {
    *p++ = '\0';
    curr->_date_eff = parse_date(p);
  }
  curr->_date = parse_date(line);

  // Parse the optional cleared flag: *

  xact_t::state_t state = xact_t::UNCLEARED;
  if (next) {
    switch (*next) {
    case '*':
      state = xact_t::CLEARED;
      next = skip_ws(++next);
      break;
    case '!':
      state = xact_t::PENDING;
      next = skip_ws(++next);
      break;
    }
  }

  // Parse the optional code: (TEXT)

  if (next && *next == '(') {
    if (char * p = std::strchr(next++, ')')) {
      *p++ = '\0';
      curr->code = next;
      next = skip_ws(p);
    }
  }

  // Parse the description text

  curr->payee = next ? next : "<Unspecified payee>";

  TRACE_STOP(entry_text, 1);

  // Parse all of the xacts associated with this entry

  TRACE_START(entry_details, 1, "Time spent parsing entry details:");

  unsigned long end_pos;
  unsigned long beg_line = linenum;

  while (! in.eof() && (in.peek() == ' ' || in.peek() == '\t')) {
    unsigned long beg_pos = static_cast<unsigned long>(in.tellg());

    line[0] = '\0';
    in.getline(line, MAX_LINE);
    if (in.eof() && line[0] == '\0')
      break;

    int len = std::strlen(line);
    if (line[len - 1] == '\r')
      line[--len] = '\0';

    end_pos = beg_pos + len + 1;
    linenum++;

    if (line[0] == ' ' || line[0] == '\t') {
      char * p = skip_ws(line);
      if (! *p)
	break;
    }

    if (xact_t * xact = parse_xact(line, master, curr.get())) {
      if (state != xact_t::UNCLEARED &&
	  xact->state == xact_t::UNCLEARED)
	xact->state = state;

      xact->beg_pos  = beg_pos;
      xact->beg_line = beg_line;
      xact->end_pos  = end_pos;
      xact->end_line = linenum;
      pos = end_pos;

      curr->add_xact(xact);
    }

    if (in.eof())
      break;
  }

  TRACE_STOP(entry_details, 1);

  return curr.release();
}

static inline void parse_symbol(char *& p, string& symbol)
{
  if (*p == '"') {
    char * q = std::strchr(p + 1, '"');
    if (! q)
      throw parse_error("Quoted commodity symbol lacks closing quote");
    symbol = string(p + 1, 0, q - p - 1);
    p = q + 2;
  } else {
    char * q = next_element(p);
    symbol = p;
    if (q)
      p = q;
    else
      p += symbol.length();
  }
  if (symbol.empty())
    throw parse_error("Failed to parse commodity");
}

bool textual_parser_t::test(std::istream& in) const
{
  char buf[5];

  in.read(buf, 5);
  if (std::strncmp(buf, "<?xml", 5) == 0) {
#if defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)
    throw parse_error("Ledger file contains XML data, but format was not recognized");
#else
    throw parse_error("Ledger file contains XML data, but no XML support present");
#endif
  }

  in.clear();
  in.seekg(0, std::ios::beg);
  assert(in.good());
  return true;
}

static void clock_out_from_timelog(std::list<time_entry_t>& time_entries,
				   const datetime_t&	    when,
				   account_t *		    account,
				   const char *		    desc,
				   journal_t&		    journal)
{
  time_entry_t event;

  if (time_entries.size() == 1) {
    event = time_entries.back();
    time_entries.clear();
  }
  else if (time_entries.empty()) {
    throw parse_error("Timelog check-out event without a check-in");
  }
  else if (! account) {
    throw parse_error
      ("When multiple check-ins are active, checking out requires an account");
  }
  else {
    bool found = false;

    for (std::list<time_entry_t>::iterator i = time_entries.begin();
	 i != time_entries.end();
	 i++)
      if (account == (*i).account) {
	event = *i;
	found = true;
	time_entries.erase(i);
	break;
      }

    if (! found)
      throw parse_error
	("Timelog check-out event does not match any current check-ins");
  }

  if (desc && event.desc.empty()) {
    event.desc = desc;
    desc = NULL;
  }

  std::auto_ptr<entry_t> curr(new entry_t);
  curr->_date = when.date();
  curr->code  = desc ? desc : "";
  curr->payee = event.desc;

  if (when < event.checkin)
    throw parse_error
      ("Timelog check-out date less than corresponding check-in");

  char buf[32];
  std::sprintf(buf, "%lds", long((when - event.checkin).seconds()));
  amount_t amt;
  amt.parse(buf);
  assert(amt.valid());

  xact_t * xact
    = new xact_t(event.account, amt, XACT_VIRTUAL);
  xact->state = xact_t::CLEARED;
  curr->add_xact(xact);

  if (! journal.add_entry(curr.get()))
    throw parse_error("Failed to record 'out' timelog entry");
  else
    curr.release();
}

unsigned int textual_parser_t::parse(std::istream& in,
				     session_t&    session,
				     journal_t&	   journal,
				     account_t *   master,
				     const path *  original_file)
{
  TRACE_START(parsing_total, 1, "Total time spent parsing text:");

  static bool  added_auto_entry_hook = false;
  static char  line[MAX_LINE + 1];
  unsigned int count  = 0;
  unsigned int errors = 0;

  std::list<account_t *>  account_stack;
  auto_entry_finalizer_t  auto_entry_finalizer(&journal);
  std::list<time_entry_t> time_entries;

  if (! master)
    master = journal.master;

  account_stack.push_front(master);

  pathname = journal.sources.back();
  src_idx  = journal.sources.size() - 1;
  linenum  = 1;

  INFO("Parsing file '" << pathname.string() << "'");

  unsigned long beg_pos = static_cast<unsigned long>(in.tellg());
  unsigned long end_pos;
  unsigned long beg_line = linenum;

  while (in.good() && ! in.eof()) {
    try {
      in.getline(line, MAX_LINE);
      if (in.eof())
	break;

      int len = std::strlen(line);
      if (line[len - 1] == '\r')
	line[--len] = '\0';

      end_pos = beg_pos + len + 1;
      linenum++;

      switch (line[0]) {
      case '\0':
	break;

      case ' ':
      case '\t': {
	char * p = skip_ws(line);
	if (*p)
	  throw parse_error("Line begins with whitespace");
	break;
      }

#ifdef TIMELOG_SUPPORT
      case 'i':
      case 'I': {
	string date(line, 2, 19);

	char * p = skip_ws(line + 22);
	char * n = next_element(p, true);

	time_entry_t event(parse_datetime(date),
			   account_stack.front()->find_account(p), n ? n : "");

	if (! time_entries.empty())
	  foreach (time_entry_t& time_entry, time_entries)
	    if (event.account == time_entry.account)
	      throw parse_error("Cannot double check-in to the same account");

	time_entries.push_back(event);
	break;
      }

      case 'o':
      case 'O':
	if (time_entries.empty()) {
	  throw parse_error("Timelog check-out event without a check-in");
	} else {
	  string date(line, 2, 19);

	  char * p = skip_ws(line + 22);
	  char * n = next_element(p, true);

	  clock_out_from_timelog
	    (time_entries, parse_datetime(date),
	     p ? account_stack.front()->find_account(p) : NULL, n, journal);
	  count++;
	}
	break;
#endif // TIMELOG_SUPPORT

      case 'D':	{		// a default commodity for "entry"
	amount_t amt(skip_ws(line + 1));
	assert(amt.valid());
	amount_t::current_pool->default_commodity = &amt.commodity();
	break;
      }

      case 'A':		        // a default account for unbalanced xacts
	journal.basket =
	  account_stack.front()->find_account(skip_ws(line + 1));
	break;

      case 'C':			// a set of conversions
	if (char * p = std::strchr(line + 1, '=')) {
	  *p++ = '\0';
	  // jww (2008-04-22): NYI!
#if 0
	  parse_conversion(line + 1, p);
#endif
	}
	break;

      case 'P': {		// a pricing entry
	char * date_field_ptr = skip_ws(line + 1);
	char * time_field_ptr = next_element(date_field_ptr);
	if (! time_field_ptr) break;
	string date_field = date_field_ptr;

	char *     symbol_and_price;
	datetime_t datetime;

	if (std::isdigit(time_field_ptr[0])) {
	  symbol_and_price = next_element(time_field_ptr);
	  if (! symbol_and_price) break;
	  datetime = parse_datetime(date_field + " " + time_field_ptr);
	} else {
	  symbol_and_price = time_field_ptr;
	  datetime = parse_datetime(date_field);
	}

	string symbol;
	parse_symbol(symbol_and_price, symbol);
	amount_t price(symbol_and_price);
	assert(price.valid());

	if (commodity_t * commodity =
	    amount_t::current_pool->find_or_create(symbol))
	  commodity->add_price(datetime, price);
	break;
      }

      case 'N': {			// don't download prices
	char * p = skip_ws(line + 1);
	string symbol;
	parse_symbol(p, symbol);

	if (commodity_t * commodity =
	    amount_t::current_pool->find_or_create(symbol))
	  commodity->add_flags(COMMODITY_STYLE_NOMARKET);
	break;
      }

      case 'Y':                   // set the current year
	current_year = std::atoi(skip_ws(line + 1));
	break;

#ifdef TIMELOG_SUPPORT
      case 'h':
      case 'b':
#endif
      case '*':                   // comment line
      case ';':                   // comment line
	break;

      case '-': {                 // option setting
	char * p = next_element(line);
	if (! p) {
	  p = std::strchr(line, '=');
	  if (p)
	    *p++ = '\0';
	}
	process_option(line + 2, session, p);
	break;
      }

      case '=': {		// automated entry
	if (! added_auto_entry_hook) {
	  journal.add_entry_finalizer(&auto_entry_finalizer);
	  added_auto_entry_hook = true;
	}

	auto_entry_t * ae = new auto_entry_t(skip_ws(line + 1));
	if (parse_xacts(in, account_stack.front(), *ae,
			       "automated", end_pos)) {
	  journal.auto_entries.push_back(ae);
	  ae->src_idx  = src_idx;
	  ae->beg_pos  = beg_pos;
	  ae->beg_line = beg_line;
	  ae->end_pos  = end_pos;
	  ae->end_line = linenum;
	}
	break;
      }

      case '~': {		// period entry
	period_entry_t * pe = new period_entry_t(skip_ws(line + 1));
	if (! pe->period)
	  throw_(parse_error, "Parsing time period '" << line << "'");

	if (parse_xacts(in, account_stack.front(), *pe,
			       "period", end_pos)) {
	  if (pe->finalize()) {
	    extend_entry_base(&journal, *pe, true);
	    journal.period_entries.push_back(pe);
	    pe->src_idx	 = src_idx;
	    pe->beg_pos	 = beg_pos;
	    pe->beg_line = beg_line;
	    pe->end_pos	 = end_pos;
	    pe->end_line = linenum;
	  } else {
	    throw new parse_error("Period entry failed to balance");
	  }
	}
	break;
      }

      case '@':
      case '!': {                 // directive
	char * p = next_element(line);
	string word(line + 1);
	if (word == "include") {
	  push_variable<path>	       save_pathname(pathname);
	  push_variable<unsigned int>  save_src_idx(src_idx);
	  push_variable<unsigned long> save_beg_pos(beg_pos);
	  push_variable<unsigned long> save_end_pos(end_pos);
	  push_variable<unsigned int>  save_linenum(linenum);

	  pathname = p;
#if 0
	  if (pathname[0] != '/' && pathname[0] != '\\' && pathname[0] != '~') {
	    string::size_type pos = save_pathname.prev.rfind('/');
	    if (pos == string::npos)
	      pos = save_pathname.prev.rfind('\\');
	    if (pos != string::npos)
	      pathname = string(save_pathname.prev, 0, pos + 1) + pathname;
	  }
	  pathname = resolve_path(pathname);

	  DEBUG("ledger.textual.include", "line " << linenum << ": " <<
		      "Including path '" << pathname << "'");

	  include_stack.push_back(std::pair<path, int>
				  (journal.sources.back(), linenum - 1));
	  count += parse_journal_file(pathname, config, journal,
				      account_stack.front());
	  include_stack.pop_back();
#endif
	}
	else if (word == "account") {
	  account_t * acct;
	  acct = account_stack.front()->find_account(p);
	  account_stack.push_front(acct);
	}
	else if (word == "end") {
	  account_stack.pop_front();
	}
	else if (word == "alias") {
	  char * b = p;
	  if (char * e = std::strchr(b, '=')) {
	    char * z = e - 1;
	    while (std::isspace(*z))
	      *z-- = '\0';
	    *e++ = '\0';
	    e = skip_ws(e);

	    // Once we have an alias name (b) and the target account
	    // name (e), add a reference to the account in the
	    // `account_aliases' map, which is used by the xact
	    // parser to resolve alias references.
	    account_t * acct = account_stack.front()->find_account(e);
	    std::pair<accounts_map::iterator, bool> result
	      = account_aliases.insert(accounts_map::value_type(b, acct));
	    assert(result.second);
	  }
	}
	else if (word == "def") {
	  expr_t def(p);
	  def.compile(session);	// causes definitions to be established
	}
	break;
      }

      default: {
	unsigned long pos = beg_pos;
	TRACE_START(entries, 1, "Time spent handling entries:");
	if (entry_t * entry =
	    parse_entry(in, line, account_stack.front(), *this, pos)) {
	  if (journal.add_entry(entry)) {
	    entry->src_idx  = src_idx;
	    entry->beg_pos  = beg_pos;
	    entry->beg_line = beg_line;
	    entry->end_pos  = pos;
	    entry->end_line = linenum;
	    count++;
	  } else {
	    checked_delete(entry);
	    throw parse_error("Entry does not balance");
	  }
	} else {
	  throw parse_error("Failed to parse entry");
	}
	end_pos = pos;
      TRACE_STOP(entries, 1);
	break;
      }
      }
    }
    catch (const std::exception& err) {
      for (std::list<std::pair<path, int> >::reverse_iterator i =
	     include_stack.rbegin();
	   i != include_stack.rend();
	   i++) {
	add_error_context("In file included from ");
#if 0
	add_error_context(include_context((*i).first, (*i).second));
#endif
      }
      add_error_context(file_context(pathname, linenum - 1));

      std::cout.flush();
      std::cerr << "Error: " << error_context() << err.what()
		<< std::endl;
      errors++;
    }
    beg_pos = end_pos;
  }

  if (! time_entries.empty()) {
    std::list<account_t *> accounts;

    foreach (time_entry_t& time_entry, time_entries)
      accounts.push_back(time_entry.account);

    foreach (account_t * account, accounts)
      clock_out_from_timelog(time_entries, current_time, account, NULL,
			     journal);

    assert(time_entries.empty());
  }

  if (added_auto_entry_hook)
    journal.remove_entry_finalizer(&auto_entry_finalizer);

  if (errors > 0)
    throw static_cast<int>(errors);

  TRACE_STOP(parsing_total, 1);

  return count;
}

void write_textual_journal(journal_t&	    journal,
			   const path&	    pathname,
			   xact_handler_ptr formatter,
			   const string&    write_hdr_format,
			   std::ostream&    out)
{
  unsigned long index = 0;
  path		found;

  if (pathname.empty()) {
    if (! journal.sources.empty())
      found = *journal.sources.begin();
  } else {
#ifdef HAVE_REALPATH
    char buf1[PATH_MAX];
    char buf2[PATH_MAX];

    ::realpath(pathname.string().c_str(), buf1);

    foreach (const path& path, journal.sources) {
      ::realpath(path.string().c_str(), buf2);
      if (std::strcmp(buf1, buf2) == 0) {
	found = path;
	break;
      }
      index++;
    }
#else
    foreach (const path& path, journal.sources) {
      if (pathname == path) {
	found = path;
	break;
      }
      index++;
    }
#endif
  }

  if (found.empty())
    throw_(std::runtime_error,
	   "Journal does not refer to file '" << pathname << "'");

  entries_list::iterator	el = journal.entries.begin();
  auto_entries_list::iterator	al = journal.auto_entries.begin();
  period_entries_list::iterator pl = journal.period_entries.begin();

  unsigned long pos = 0;

  format_t hdr_fmt(write_hdr_format);
  boost::filesystem::ifstream in(found);

  while (! in.eof()) {
    entry_base_t * base = NULL;
    if (el != journal.entries.end() && pos == (*el)->beg_pos) {
      hdr_fmt.format(out, **el);
      base = *el++;
    }
    else if (al != journal.auto_entries.end() && pos == (*al)->beg_pos) {
      out << "= " << (*al)->predicate.predicate.text() << '\n';
      base = *al++;
    }
    else if (pl != journal.period_entries.end() && pos == (*pl)->beg_pos) {
      out << "~ " << (*pl)->period_string << '\n';
      base = *pl++;
    }

    char c;
    if (base) {
      foreach (xact_t * xact, base->xacts) {
	if (! xact->has_flags(XACT_AUTO)) {
	  xact->xdata().add_flags(XACT_EXT_TO_DISPLAY);
	  (*formatter)(*xact);
	}
      }
      formatter->flush();

      while (pos < base->end_pos) {
	in.get(c);
	pos = static_cast<unsigned long>(in.tellg()); // pos++;
      }
    } else {
      in.get(c);
      pos = static_cast<unsigned long>(in.tellg()); // pos++;
      out.put(c);
    }
  }
}

} // namespace ledger
