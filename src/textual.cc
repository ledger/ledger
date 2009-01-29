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

#if defined(__GNUG__) && __GNUG__ < 3
#define _XOPEN_SOURCE
#endif

#include "textual.h"
#if defined(TIMELOG_SUPPORT)
#include "timelog.h"
#endif
#include "expr.h"
#include "parser.h"
#include "session.h"
#include "option.h"
#include "acconf.h"

namespace ledger {

bool textual_parser_t::test(std::istream& in) const
{
  char   buf[12];
  char * p;

  in.read(buf, 11);
  if (utf8::is_bom(buf))
    p = &buf[3];
  else
    p = buf;

  if (std::strncmp(p, "<?xml", 5) == 0)
    throw_(parse_error,
	   "Ledger file contains XML data, but format was not recognized");

  in.clear();
  in.seekg(0, std::ios::beg);
  assert(in.good());
  return true;
}

std::size_t textual_parser_t::parse(std::istream& in,
				    session_t&    session,
				    journal_t&	  journal,
				    account_t *   master,
				    const path *  original_file)
{
  TRACE_START(parsing_total, 1, "Total time spent parsing text:");

  std::list<account_t *> account_stack;
#if defined(TIMELOG_SUPPORT)
  time_log_t		 timelog(journal);
#endif

  instance_t parsing_instance(account_stack,
#if defined(TIMELOG_SUPPORT)
			      timelog,
#endif
			      in, session, journal, master,
			      original_file);
  parsing_instance.parse();

  TRACE_STOP(parsing_total, 1);

  if (parsing_instance.errors > 0)
    throw static_cast<int>(parsing_instance.errors);

  return parsing_instance.count;
}

namespace {
  optional<expr_t> parse_amount_expr(std::istream&  in,
				     amount_t&      amount,
				     xact_t *	    xact,
				     uint_least8_t  flags = 0)
  {
    expr_t expr(in, flags | static_cast<uint_least8_t>(expr_t::PARSE_PARTIAL));

    DEBUG("textual.parse", "Parsed an amount expression");

#if defined(DEBUG_ENABLED)
    DEBUG_IF("textual.parse") {
      if (_debug_stream) {
	ledger::dump_value_expr(*_debug_stream, expr);
	*_debug_stream << std::endl;
      }
    }
#endif

    if (expr) {
      amount = expr.calc(*xact).as_amount();
      DEBUG("textual.parse", "The transaction amount is " << amount);
      return expr;
    }
    return none;
  }
}

textual_parser_t::instance_t::instance_t
  (std::list<account_t *>& _account_stack,
#if defined(TIMELOG_SUPPORT)
   time_log_t&             _timelog,
#endif
   std::istream&	   _in,
   session_t&		   _session,
   journal_t&		   _journal,
   account_t *		   _master,
   const path *		   _original_file,
   instance_t *            _parent)
    : account_stack(_account_stack),
#if defined(TIMELOG_SUPPORT)
      timelog(_timelog),
#endif
      parent(_parent), in(_in), session(_session),
      journal(_journal), master(_master),
      original_file(_original_file)
{
  TRACE_CTOR(textual_parser_t::instance_t, "...");

  if (! master)
    master = journal.master;

  account_stack.push_front(master);

  pathname = journal.sources.back();
  src_idx  = journal.sources.size() - 1;
  linenum  = 1;
  beg_pos  = in.tellg();
  beg_line = linenum;
  count    = 0;
  errors   = 0;
}

textual_parser_t::instance_t::~instance_t()
{
  TRACE_DTOR(textual_parser_t::instance_t);

  account_stack.pop_front();

  if (auto_entry_finalizer.get())
    journal.remove_entry_finalizer(auto_entry_finalizer.get());
}

void textual_parser_t::instance_t::parse()
{
  INFO("Parsing file '" << pathname.string() << "'");

  TRACE_START(instance_parse, 1,
	      "Done parsing file '" << pathname.string() << "'");

  errors = 0;

  while (in.good() && ! in.eof()) {
    try {
      read_next_directive();

      beg_pos = end_pos;
    }
    catch (const std::exception& err) {
      if (parent) {
	std::list<instance_t *> instances;

	for (instance_t * instance = parent;
	     instance;
	     instance = instance->parent)
	  instances.push_front(instance);

	foreach (instance_t * instance, instances)
	  add_error_context("In file included from '"
			    << file_context(instance->pathname,
					    instance->linenum - 1) << "':");
      }
      add_error_context("While parsing file '"
			<< file_context(pathname, linenum - 1) << "':");

      std::cout.flush();
      std::cerr << ledger::error_context()
		<< "Error: " << err.what() << std::endl;
      errors++;
    }
  }

  TRACE_STOP(instance_parse, 1);
}

void textual_parser_t::instance_t::read_next_directive()
{
  char * line;

  in.getline(linebuf, MAX_LINE);
  if (in.eof())
    return;

  if (linenum == 1 && utf8::is_bom(linebuf))
    line = &linebuf[3];
  else
    line = linebuf;

  int len = std::strlen(line);
  if (line[len - 1] == '\r')
    line[--len] = '\0';

  end_pos = beg_pos;
  end_pos += len + 1;
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

  case '#':			// comment line
  case ';':			// comment line
    break;

  case '-':			// option setting
    option_directive(line);
    break;

  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
    entry_directive(line);
    break;
  case '=':			// automated entry
    automated_entry_directive(line);
    break;
  case '~':			// period entry
    period_entry_directive(line);
    break;

#if defined(TIMELOG_SUPPORT)
  case 'i':
    clock_in_directive(line, false);
    break;
  case 'I':
    clock_in_directive(line, true);
    break;

  case 'o':
    clock_out_directive(line, false);
    break;
  case 'O':
    clock_out_directive(line, true);
    break;

  case 'h':
  case 'b':
    break;
#endif // TIMELOG_SUPPORT

  case 'A':		        // a default account for unbalanced xacts
    default_account_directive(line);
    break;
  case 'C':			// a set of conversions
    price_conversion_directive(line);
    break;
  case 'D':			// a default commodity for "entry"
    default_commodity_directive(line);
    break;
  case 'N':			// don't download prices
    nomarket_directive(line);
    break;
  case 'P':			// a pricing entry
    price_entry_directive(line);
    break;
  case 'Y':			// set the current year
    year_directive(line);
    break;

  case '@':
  case '!':
    line++;
    // fall through...
  default:			// some other directive
    general_directive(line);
    break;
  }
}

#if defined(TIMELOG_SUPPORT)

void textual_parser_t::instance_t::clock_in_directive(char * line,
						      bool   capitalized)
{
  string date(line, 2, 19);

  char * p = skip_ws(line + 22);
  char * n = next_element(p, true);

  timelog.clock_in(parse_datetime(date),
		   account_stack.front()->find_account(p), n ? n : "");
}

void textual_parser_t::instance_t::clock_out_directive(char * line,
						       bool   capitalized)
{  
  string date(line, 2, 19);

  char * p = skip_ws(line + 22);
  char * n = next_element(p, true);

  timelog.clock_out(parse_datetime(date),
		    p ? account_stack.front()->find_account(p) : NULL, n);
  count++;
}
#endif // TIMELOG_SUPPORT

void textual_parser_t::instance_t::default_commodity_directive(char * line)
{
  amount_t amt(skip_ws(line + 1));
  assert(amt.valid());
  amount_t::current_pool->default_commodity = &amt.commodity();
}

void textual_parser_t::instance_t::default_account_directive(char * line)
{
  journal.basket = account_stack.front()->find_account(skip_ws(line + 1));
}

void textual_parser_t::instance_t::price_conversion_directive(char * line)
{
  if (char * p = std::strchr(line + 1, '=')) {
    *p++ = '\0';
#if 0
    // jww (2008-04-22): NYI!
    parse_conversion(line + 1, p);
#endif
  }
}

namespace {
  void parse_symbol(char *& p, string& symbol)
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
}

void textual_parser_t::instance_t::price_entry_directive(char * line)
{
  char * date_field_ptr = skip_ws(line + 1);
  char * time_field_ptr = next_element(date_field_ptr);
  if (! time_field_ptr) return;
  string date_field = date_field_ptr;

  char *     symbol_and_price;
  datetime_t datetime;

  if (std::isdigit(time_field_ptr[0])) {
    symbol_and_price = next_element(time_field_ptr);
    if (! symbol_and_price) return;
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
}

void textual_parser_t::instance_t::nomarket_directive(char * line)
{
  char * p = skip_ws(line + 1);
  string symbol;
  parse_symbol(p, symbol);

  if (commodity_t * commodity =
      amount_t::current_pool->find_or_create(symbol))
    commodity->add_flags(COMMODITY_NOMARKET);
}

void textual_parser_t::instance_t::year_directive(char * line)
{
  current_year = std::atoi(skip_ws(line + 1));
}

void textual_parser_t::instance_t::option_directive(char * line)
{
  char * p = next_element(line);
  if (! p) {
    p = std::strchr(line, '=');
    if (p)
      *p++ = '\0';
  }
  process_option(line + 2, session, p);
}

void textual_parser_t::instance_t::automated_entry_directive(char * line)
{
  if (! auto_entry_finalizer.get()) {
    auto_entry_finalizer.reset(new auto_entry_finalizer_t(&journal));
    journal.add_entry_finalizer(auto_entry_finalizer.get());
  }

  auto_entry_t * ae = new auto_entry_t(skip_ws(line + 1));
  if (parse_xacts(in, account_stack.front(), *ae, "automated",
		  end_pos)) {
    journal.auto_entries.push_back(ae);
    ae->src_idx  = src_idx;
    ae->beg_pos  = beg_pos;
    ae->beg_line = beg_line;
    ae->end_pos  = end_pos;
    ae->end_line = linenum;
  }
}

void textual_parser_t::instance_t::period_entry_directive(char * line)
{
  period_entry_t * pe = new period_entry_t(skip_ws(line + 1));
  if (! pe->period)
    throw_(parse_error, "Parsing time period '" << line << "'");

  if (parse_xacts(in, account_stack.front(), *pe,
		  "period", end_pos)) {
    if (pe->finalize()) {
      extend_entry_base(&journal, *pe, true);
      journal.period_entries.push_back(pe);
      pe->src_idx  = src_idx;
      pe->beg_pos  = beg_pos;
      pe->beg_line = beg_line;
      pe->end_pos  = end_pos;
      pe->end_line = linenum;
    } else {
      throw parse_error("Period entry failed to balance");
    }
  }
}

void textual_parser_t::instance_t::entry_directive(char * line)
{
  istream_pos_type pos = beg_pos;

  TRACE_START(entries, 1, "Time spent handling entries:");

  if (entry_t * entry = parse_entry(in, line, account_stack.front(), pos)) {
    // The entry pointer is unowned at the minute, and there is a
    // possibility that add_entry ma throw an exception, which
    // would cause us to leak without this guard.
    std::auto_ptr<entry_t> entry_ptr(entry);
    if (journal.add_entry(entry)) {
      entry_ptr.release(); // it's owned by the journal now
      entry->src_idx  = src_idx;
      entry->beg_pos  = beg_pos;
      entry->beg_line = beg_line;
      entry->end_pos  = pos;
      entry->end_line = linenum;
      count++;
    }
    // It's perfectly valid for the journal to reject the entry,
    // which it will do if the entry has no substantive effect
    // (for example, a checking entry, all of whose transactions
    // have null amounts).
  } else {
    throw parse_error("Failed to parse entry");
  }

  end_pos = pos;

  TRACE_STOP(entries, 1);
}

void textual_parser_t::instance_t::include_directive(char * line)
{
  path filename(next_element(line));

#if 0
  if (filename[0] != '/' && filename[0] != '\\' && filename[0] != '~') {
    string::size_type pos = pathname.prev.rfind('/');
    if (pos == string::npos)
      pos = pathname.prev.rfind('\\');
    if (pos != string::npos)
      filename = string(pathname.prev, 0, pos + 1) + filename;
  }
#endif
  filename = resolve_path(filename);

  DEBUG("textual.include", "Line " << linenum << ": " <<
	"Including path '" << filename << "'");

  ifstream stream(filename);

  instance_t instance(account_stack,
#if defined(TIMELOG_SUPPORT)
		      timelog,
#endif
		      stream, session, journal, master,
		      &filename, this);
  instance.parse();

  errors += instance.errors;
  count  += instance.count;
}

void textual_parser_t::instance_t::account_directive(char * line)
{
  if (account_t * acct =
      account_stack.front()->find_account(next_element(line)))
    account_stack.push_front(acct);
  else
    assert(! "Failed to create account");
}

void textual_parser_t::instance_t::end_directive(char * line)
{
  account_stack.pop_front();
}

void textual_parser_t::instance_t::alias_directive(char * line)
{
  char * b = skip_ws(line + 1);
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

void textual_parser_t::instance_t::define_directive(char * line)
{
  expr_t def(skip_ws(line + 1));
  def.compile(session);	// causes definitions to be established
}

void textual_parser_t::instance_t::general_directive(char * line)
{
  char * p = next_element(line);
  string word(line + 1);

  switch (*p) {
  case 'a':
    if (std::strcmp(p, "account") == 0) {
      account_directive(line);
      return;
    }
    else if (std::strcmp(p, "alias") == 0) {
      alias_directive(line);
      return;
    }
    break;

  case 'd':
    if (std::strcmp(p, "def") == 0) {
      define_directive(line);
      return;
    }
    break;

  case 'e':
    if (std::strcmp(p, "end") == 0) {
      end_directive(line);
      return;
    }
    break;

  case 'i':
    if (std::strcmp(p, "include") == 0) {
      include_directive(line);
      return;
    }
    break;
  }

  static const std::size_t textdir_len = std::strlen("ledger_textdir_");
  scoped_array<char> directive(new char[std::strlen(p) + textdir_len + 1]);
  std::strcpy(directive.get(), "ledger_textdir_");
  std::strcpy(directive.get() + textdir_len, p);

  if (expr_t::ptr_op_t op = lookup(directive.get())) {
    call_scope_t args(*this);
    args.push_back(string_value(p));
    op->as_function()(args);
  }
}

xact_t * textual_parser_t::instance_t::parse_xact(char *      line,
						  account_t * account,
						  entry_t *   entry)
{
  std::istringstream in(line);

  string err_desc;
  try {

  // The account will be determined later...
  std::auto_ptr<xact_t> xact(new xact_t);
  if (entry)
    xact->entry = entry;

  // Parse the state flag

  char p = peek_next_nonws(in);
  switch (p) {
  case '*':
    xact->set_state(item_t::CLEARED);
    in.get(p);
    p = peek_next_nonws(in);
    DEBUG("textual.parse",
	  "line " << linenum << ": " << "Parsed the CLEARED flag");
    break;
  case '!':
    xact->set_state(item_t::PENDING);
    in.get(p);
    p = peek_next_nonws(in);
    DEBUG("textual.parse",
	  "line " << linenum << ": " << "Parsed the PENDING flag");
    break;
  }

  // Parse the account name

  istream_pos_type account_beg = in.tellg();
  istream_pos_type account_end = account_beg;
  while (! in.eof()) {
    in.get(p);
    if (in.eof() || (std::isspace(p) &&
		     (p == '\t' || in.peek() == EOF ||
		      std::isspace(in.peek()))))
      break;
    account_end += 1;
  }

  if (account_beg == account_end)
    throw parse_error("No account was specified");

  char * b = &line[long(account_beg)];
  char * e = &line[long(account_end)];

  if ((*b == '[' && *(e - 1) == ']') ||
      (*b == '(' && *(e - 1) == ')')) {
    xact->add_flags(XACT_VIRTUAL);
    DEBUG("textual.parse",
	  "line " << linenum << ": " << "Parsed a virtual account name");

    if (*b == '[') {
      xact->add_flags(XACT_MUST_BALANCE);
      DEBUG("textual.parse",
	    "line " << linenum << ": " << "Transaction must balance");
    }
    b++; e--;
  }

  string name(b, e - b);
  DEBUG("textual.parse", "line " << linenum << ": " <<
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
      istream_pos_type beg = in.tellg();

      xact->amount_expr =
	parse_amount_expr(in, xact->amount, xact.get(),
			  static_cast<uint_least8_t>(expr_t::PARSE_NO_REDUCE) |
			  static_cast<uint_least8_t>(expr_t::PARSE_NO_ASSIGN));
      saw_amount = true;

      if (! xact->amount.is_null()) {
	xact->amount.reduce();
	DEBUG("textual.parse", "line " << linenum << ": " <<
	      "Reduced amount is " << xact->amount);
      }

      // We don't need to store the actual expression that resulted in the
      // amount if it's constant
      if (xact->amount_expr) {
	if (xact->amount_expr->is_constant())
	  xact->amount_expr = expr_t();

	istream_pos_type end = in.tellg();
	xact->amount_expr->set_text(string(line, long(beg), long(end - beg)));
      }
    }
    catch (const std::exception& err) {
      add_error_context("While parsing transaction amount:\n");
      throw;
    }
  }

  // Parse the optional cost (@ PER-UNIT-COST, @@ TOTAL-COST)

  if (in.good() && ! in.eof()) {
    p = peek_next_nonws(in);
    if (p == '@') {
      if (! saw_amount)
	throw parse_error
	  ("Transaction cannot have a cost expression with an amount");
	
      DEBUG("textual.parse", "line " << linenum << ": " <<
		  "Found a price indicator");
      bool per_unit = true;
      in.get(p);
      if (in.peek() == '@') {
	in.get(p);
	per_unit = false;
	DEBUG("textual.parse", "line " << linenum << ": " <<
		    "And it's for a total price");
      }

      if (in.good() && ! in.eof()) {
	xact->cost = amount_t();

	try {
	  istream_pos_type beg = in.tellg();

	  xact->cost_expr =
	    parse_amount_expr(in, *xact->cost, xact.get(),
			      static_cast<uint_least8_t>(expr_t::PARSE_NO_MIGRATE) |
			      static_cast<uint_least8_t>(expr_t::PARSE_NO_ASSIGN));

	  if (xact->cost_expr) {
	    istream_pos_type end = in.tellg();
	    if (per_unit)
	      xact->cost_expr->set_text(string("@") +
					string(line, long(beg), long(end - beg)));
	    else
	      xact->cost_expr->set_text(string("@@") +
					string(line, long(beg), long(end - beg)));
	  }
	}
	catch (const std::exception& err) {
	  add_error_context("While parsing transaction cost:\n");
	  throw;
	}

	if (xact->cost->sign() < 0)
	  throw parse_error("A transaction's cost may not be negative");

	amount_t per_unit_cost(*xact->cost);
	if (per_unit)
	  *xact->cost *= xact->amount;
	else
	  per_unit_cost /= xact->amount;

	commodity_t::exchange(xact->amount.commodity(),
			      per_unit_cost, datetime_t(*xact->date()));

	DEBUG("textual.parse", "line " << linenum << ": " <<
		    "Total cost is " << *xact->cost);
	DEBUG("textual.parse", "line " << linenum << ": " <<
		    "Per-unit cost is " << per_unit_cost);
	DEBUG("textual.parse", "line " << linenum << ": " <<
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
	DEBUG("textual.parse", "line " << linenum << ": " <<
	      "Found a balance assignment indicator");
	if (in.good() && ! in.eof()) {
	  xact->assigned_amount = amount_t();

	  try {
	    istream_pos_type beg = in.tellg();

	    xact->assigned_amount_expr =
	      parse_amount_expr(in, *xact->assigned_amount, xact.get(),
				static_cast<uint_least8_t>(expr_t::PARSE_NO_MIGRATE));

	    if (xact->assigned_amount->is_null())
	      throw parse_error
		("An assigned balance must evaluate to a constant value");

	    DEBUG("textual.parse", "line " << linenum << ": " <<
		  "XACT assign: parsed amt = " << *xact->assigned_amount);

	    if (xact->assigned_amount_expr) {
	      istream_pos_type end = in.tellg();
	      xact->assigned_amount_expr->set_text
		(string("=") + string(line, long(beg), long(end - beg)));
	    }

	    account_t::xdata_t& xdata(xact->account->xdata());
	    amount_t& amt(*xact->assigned_amount);

	    DEBUG("xact.assign",
		  "account balance = " << xdata.value.strip_annotations());
	    DEBUG("xact.assign",
		  "xact amount = " << amt.strip_annotations());

	    amount_t diff;
	    if (xdata.value.is_amount()) {
	      diff = amt - xdata.value.as_amount();
	    }
	    else if (xdata.value.is_balance()) {
	      if (optional<amount_t> comm_bal =
		  xdata.value.as_balance().commodity_amount(amt.commodity()))
		diff = amt - *comm_bal;
	      else
		diff = amt;
	    }
	    else if (xdata.value.is_balance_pair()) {
	      if (optional<amount_t> comm_bal =
		  xdata.value.as_balance_pair().commodity_amount(amt.commodity()))
		diff = amt - *comm_bal;
	      else
		diff = amt;
	    }
	    else {
	      diff = amt;
	    }

	    DEBUG("xact.assign", "diff = " << diff.strip_annotations());
	    DEBUG("textual.parse", "line " << linenum << ": " <<
		  "XACT assign: diff = " << diff.strip_annotations());

	    if (! diff.is_zero()) {
	      if (! xact->amount.is_null()) {
		diff -= xact->amount;
		if (! diff.is_zero()) {
		  xact_t * temp = new xact_t(xact->account, diff,
					     ITEM_GENERATED | XACT_CALCULATED);
		  entry->add_xact(temp);

		  DEBUG("textual.parse", "line " << linenum << ": " <<
			"Created balancing transaction");
		}
	      } else {
		xact->amount = diff;
		DEBUG("textual.parse", "line " << linenum << ": " <<
		      "Overwrite null transaction");
	      }
	    }
	  }
	  catch (const std::exception& err) {
	    add_error_context("While parsing assigned balance:\n");
	    throw;
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
      xact->note = &line[long(in.tellg())];
      DEBUG("textual.parse", "line " << linenum << ": " <<
		  "Parsed a note '" << *xact->note << "'");

      if (char * b = std::strchr(xact->note->c_str(), '['))
	if (char * e = std::strchr(xact->note->c_str(), ']')) {
	  char buf[256];
	  std::strncpy(buf, b + 1, e - b - 1);
	  buf[e - b - 1] = '\0';

	  DEBUG("textual.parse", "line " << linenum << ": " <<
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
    add_error_context(line_context(line, in.tellg()));
    throw;
  }
}

bool textual_parser_t::instance_t::parse_xacts(std::istream&    in,
					       account_t *	account,
					       entry_base_t&    entry,
					       const string&    kind,
					       istream_pos_type beg_pos)
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
    if (xact_t * xact = parse_xact(line, account, NULL)) {
      entry.add_xact(xact);
      added = true;
    }
  }

  TRACE_STOP(entry_xacts, 1);

  return added;
}

entry_t * textual_parser_t::instance_t::parse_entry(std::istream&     in,
						    char *	      line,
						    account_t *	      master,
						    istream_pos_type& pos)
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

  item_t::state_t state = item_t::UNCLEARED;
  if (next) {
    switch (*next) {
    case '*':
      state = item_t::CLEARED;
      next = skip_ws(++next);
      break;
    case '!':
      state = item_t::PENDING;
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

  istream_pos_type end_pos;
  unsigned long beg_line = linenum;

  while (! in.eof() && (in.peek() == ' ' || in.peek() == '\t')) {
    istream_pos_type beg_pos = in.tellg();

    line[0] = '\0';
    in.getline(line, MAX_LINE);
    if (in.eof() && line[0] == '\0')
      break;

    int len = std::strlen(line);
    if (line[len - 1] == '\r')
      line[--len] = '\0';

    end_pos = beg_pos;
    end_pos += len + 1;
    linenum++;

    if (line[0] == ' ' || line[0] == '\t') {
      char * p = skip_ws(line);
      if (! *p)
	break;
    }

    if (xact_t * xact = parse_xact(line, master, curr.get())) {
      if ((state == item_t::CLEARED && xact->state() != item_t::CLEARED) ||
	  (state == item_t::PENDING && xact->state() == item_t::UNCLEARED))
	xact->set_state(state);

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

expr_t::ptr_op_t textual_parser_t::instance_t::lookup(const string& name)
{
  return session.lookup(name);
}

void write_textual_journal(journal_t&	    journal,
			   const path&	    pathname,
			   xact_handler_ptr formatter,
			   const string&    write_hdr_format,
			   std::ostream&    out)
{
  unsigned long index = 0;
  path		found;

  // jww (2009-01-29): This function currently doesn't work

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

  istream_pos_type pos = 0;

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
	pos = in.tellg(); // pos++;
      }
    } else {
      in.get(c);
      pos = in.tellg(); // pos++;
      out.put(c);
    }
  }
}

} // namespace ledger
