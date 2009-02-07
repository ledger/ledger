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
#include "parser.h"
#include "session.h"

namespace ledger {

#if defined(TEST_FOR_PARSER)

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

#endif // TEST_FOR_PARSER

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

  session.clean_accounts();	// remove calculated totals

  TRACE_STOP(parsing_total, 1);

  // These tracers were started in textual.cc
  TRACE_FINISH(entry_text, 1);
  TRACE_FINISH(entry_details, 1);
  TRACE_FINISH(entry_xacts, 1);
  TRACE_FINISH(entries, 1);
  TRACE_FINISH(instance_parse, 1); // report per-instance timers
  TRACE_FINISH(parsing_total, 1);

  if (parsing_instance.errors > 0)
    throw static_cast<int>(parsing_instance.errors);

  return parsing_instance.count;
}

namespace {
#if defined(STORE_XACT_EXPRS)
  optional<expr_t>
#else
  void
#endif
  parse_amount_expr(session_t&    session,
		    std::istream& in,
		    amount_t&     amount,
		    xact_t *	  xact,
		    uint_least8_t flags = 0)
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
      bind_scope_t bound_scope(session, *xact);

      value_t result(expr.calc(bound_scope));
      if (! result.is_amount())
	throw_(parse_error, "Transactions may only specify simple amounts");

      amount = result.as_amount();
      DEBUG("textual.parse", "The transaction amount is " << amount);
#if defined(STORE_XACT_EXPRS)
      return expr;
#endif
    }
#if defined(STORE_XACT_EXPRS)
    return none;
#endif
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

  if (! in.good() || in.eof())
    return;

  linenum  = 0;
  errors   = 0;
  count	   = 0;
  curr_pos = in.tellg();

  while (in.good() && ! in.eof()) {
    try {
      read_next_directive();
    }
    catch (const std::exception& err) {
      string current_context = error_context();

      if (parent) {
	std::list<instance_t *> instances;

	for (instance_t * instance = parent;
	     instance;
	     instance = instance->parent)
	  instances.push_front(instance);

	foreach (instance_t * instance, instances)
	  add_error_context("In file included from "
			    << file_context(instance->pathname,
					    instance->linenum));
      }
      add_error_context("While parsing file "
			<< file_context(pathname, linenum));

      string context = error_context();
      if (! context.empty())
	std::cerr << context << std::endl;
    
      if (! current_context.empty())
	std::cerr << current_context << std::endl;

      std::cerr << "Error: " << err.what() << std::endl;
      errors++;
    }
  }

  TRACE_STOP(instance_parse, 1);
}

std::streamsize textual_parser_t::instance_t::read_line(char *& line)
{
  assert(in.good());
  assert(! in.eof());		// no one should call us in that case

  line_beg_pos = curr_pos;
  
  in.getline(linebuf, MAX_LINE);
  std::streamsize len = in.gcount();

  if (len > 0) {
    if (linenum == 0 && utf8::is_bom(linebuf))
      line = &linebuf[3];
    else
      line = linebuf;

    if (line[len - 1] == '\r')	// strip Windows CRLF down to LF
      line[--len] = '\0';

    linenum++;

    curr_pos  = line_beg_pos;
    curr_pos += len;

    return len - 1;		// LF is being silently dropped
  }
  return 0;
}

void textual_parser_t::instance_t::read_next_directive()
{
  char * line;
  std::streamsize len = read_line(line);

  if (len == 0 || line == NULL)
    return;

  switch (line[0]) {
  case '\0':
    assert(false);		// shouldn't ever reach here
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
    entry_directive(line, len);
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

  timelog.clock_in(parse_datetime(date, session.current_year),
		   account_stack.front()->find_account(p), n ? n : "");
}

void textual_parser_t::instance_t::clock_out_directive(char * line,
						       bool   capitalized)
{  
  string date(line, 2, 19);

  char * p = skip_ws(line + 22);
  char * n = next_element(p, true);

  timelog.clock_out(parse_datetime(date, session.current_year),
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
    datetime = parse_datetime(date_field + " " + time_field_ptr,
			      session.current_year);
  } else {
    symbol_and_price = time_field_ptr;
    datetime = parse_datetime(date_field, session.current_year);
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
  session.current_year = std::atoi(skip_ws(line + 1));
}

void textual_parser_t::instance_t::option_directive(char * line)
{
  char * p = next_element(line);
  if (! p) {
    p = std::strchr(line, '=');
    if (p)
      *p++ = '\0';
  }
  process_option(line + 2, session, p, line);
}

void textual_parser_t::instance_t::automated_entry_directive(char * line)
{
  if (! auto_entry_finalizer.get()) {
    auto_entry_finalizer.reset(new auto_entry_finalizer_t(&journal));
    journal.add_entry_finalizer(auto_entry_finalizer.get());
  }

  istream_pos_type pos	= curr_pos;
  std::size_t      lnum = linenum;

  std::auto_ptr<auto_entry_t>
    ae(new auto_entry_t(item_predicate<xact_t>
			(skip_ws(line + 1),
			 keep_details_t(true, true, true, true))));

  if (parse_xacts(account_stack.front(), *ae.get(), "automated")) {
    journal.auto_entries.push_back(ae.get());

    ae->pathname = pathname;
    ae->beg_pos  = pos;
    ae->beg_line = lnum;
    ae->end_pos  = curr_pos;
    ae->end_line = linenum;

    ae.release();
  }
}

void textual_parser_t::instance_t::period_entry_directive(char * line)
{
  std::auto_ptr<period_entry_t> pe(new period_entry_t(skip_ws(line + 1)));
  if (! pe->period)
    throw_(parse_error, "Parsing time period '" << line << "'");

  istream_pos_type pos	= curr_pos;
  std::size_t      lnum = linenum;

  if (parse_xacts(account_stack.front(), *pe.get(), "period")) {
    if (pe->finalize()) {
      extend_entry_base(&journal, *pe.get(), true);

      journal.period_entries.push_back(pe.get());

      pe->pathname = pathname;
      pe->beg_pos  = pos;
      pe->beg_line = lnum;
      pe->end_pos  = curr_pos;
      pe->end_line = linenum;

      pe.release();
    } else {
      throw parse_error("Period entry failed to balance");
    }
  }
}

void textual_parser_t::instance_t::entry_directive(char * line, std::streamsize len)
{
  TRACE_START(entries, 1, "Time spent handling entries:");

  if (entry_t * entry = parse_entry(line, len, account_stack.front())) {
    std::auto_ptr<entry_t> manager(entry);

    if (journal.add_entry(entry)) {
      manager.release();	// it's owned by the journal now
      count++;
    }
    // It's perfectly valid for the journal to reject the entry, which it will
    // do if the entry has no substantive effect (for example, a checking
    // entry, all of whose transactions have null amounts).
  } else {
    throw parse_error("Failed to parse entry");
  }

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

  static const std::size_t textdir_len = std::strlen("dir_");
  scoped_array<char> directive(new char[std::strlen(p) + textdir_len + 1]);
  std::strcpy(directive.get(), "dir_");
  std::strcpy(directive.get() + textdir_len, p);

  if (expr_t::ptr_op_t op = lookup(directive.get())) {
    call_scope_t args(*this);
    args.push_back(string_value(p));
    op->as_function()(args);
  }
}

xact_t * textual_parser_t::instance_t::parse_xact(char *	  line,
						  std::streamsize len,
						  account_t *	  account,
						  entry_t *	  entry)
{
  TRACE_START(xact_details, 1, "Time spent parsing transactions:");

  std::auto_ptr<xact_t> xact(new xact_t);

  xact->entry    = entry;	// this could be NULL
  xact->pathname = pathname;
  xact->beg_pos  = line_beg_pos;
  xact->beg_line = linenum;

  char buf[MAX_LINE + 1];
  std::strcpy(buf, line);
  std::size_t beg = 0;

  try {

  // Parse the state flag

  assert(line);
  assert(*line);

  char * p = skip_ws(line);

  switch (*p) {
  case '*':
    xact->set_state(item_t::CLEARED);
    p = skip_ws(p + 1);
    DEBUG("textual.parse", "line " << linenum << ": "
	  << "Parsed the CLEARED flag");
    break;

  case '!':
    xact->set_state(item_t::PENDING);
    p = skip_ws(p + 1);
    DEBUG("textual.parse", "line " << linenum << ": "
	  << "Parsed the PENDING flag");
    break;
  }

  if (entry &&
      ((entry->_state == item_t::CLEARED && xact->state() != item_t::CLEARED) ||
       (entry->_state == item_t::PENDING && xact->state() == item_t::UNCLEARED)))
    xact->set_state(entry->_state);

  // Parse the account name

  if (! *p)
    throw parse_error("Transaction has no account");

  char * next = next_element(p, true);
  char * e = p + std::strlen(p);

  if ((*p == '[' && *(e - 1) == ']') || (*p == '(' && *(e - 1) == ')')) {
    xact->add_flags(XACT_VIRTUAL);
    DEBUG("textual.parse", "line " << linenum << ": "
	  << "Parsed a virtual account name");

    if (*p == '[') {
      xact->add_flags(XACT_MUST_BALANCE);
      DEBUG("textual.parse", "line " << linenum << ": "
	    << "Transaction must balance");
    }
    p++; e--;
  }

  string name(p, e - p);
  DEBUG("textual.parse", "line " << linenum << ": "
	<< "Parsed account name " << name);

  if (account_aliases.size() > 0) {
    accounts_map::const_iterator i = account_aliases.find(name);
    if (i != account_aliases.end())
      xact->account = (*i).second;
  }
  if (! xact->account)
    xact->account = account->find_account(name);

  // Parse the optional amount

  bool saw_amount = false;

  if (next && *next && (*next != ';' && *next != '=')) {
    saw_amount = true;

    beg = next - line;
    ptristream stream(next, len - beg);

    if (*next != '(')		// indicates a value expression
      xact->amount.parse(stream, amount_t::PARSE_NO_REDUCE);
    else
      parse_amount_expr(session, stream, xact->amount, xact.get(),
			static_cast<uint_least8_t>(expr_t::PARSE_NO_REDUCE) |
			static_cast<uint_least8_t>(expr_t::PARSE_NO_ASSIGN));

    if (! xact->amount.is_null())
      xact->amount.reduce();

    DEBUG("textual.parse", "line " << linenum << ": "
	  << "xact amount = " << xact->amount);

    if (stream.eof()) {
      next = NULL;
    } else {
      next = skip_ws(next + stream.tellg());

      // Parse the optional cost (@ PER-UNIT-COST, @@ TOTAL-COST)

      if (*next == '@') {
	DEBUG("textual.parse", "line " << linenum << ": "
	      << "Found a price indicator");

	bool per_unit = true;

	if (*++next == '@') {
	  per_unit = false;
	  DEBUG("textual.parse", "line " << linenum << ": "
		<< "And it's for a total price");
	}

	beg = ++next - line;

	p = skip_ws(next);
	if (*p) {
	  xact->cost = amount_t();

	  beg = p - line;
	  ptristream cstream(p, len - beg);

	  if (*p != '(')		// indicates a value expression
	    xact->cost->parse(cstream, amount_t::PARSE_NO_MIGRATE);
	  else
	    parse_amount_expr(session, cstream, *xact->cost, xact.get(),
			      static_cast<uint_least8_t>(expr_t::PARSE_NO_MIGRATE) |
			      static_cast<uint_least8_t>(expr_t::PARSE_NO_ASSIGN));

	  if (xact->cost->sign() < 0)
	    throw parse_error("A transaction's cost may not be negative");

	  amount_t per_unit_cost(*xact->cost);
	  if (per_unit)
	    *xact->cost *= xact->amount;
	  else
	    per_unit_cost /= xact->amount;

	  commodity_t::exchange(xact->amount.commodity(),
				per_unit_cost, datetime_t(xact->date()));

	  DEBUG("textual.parse", "line " << linenum << ": "
		<< "Total cost is " << *xact->cost);
	  DEBUG("textual.parse", "line " << linenum << ": "
		<< "Per-unit cost is " << per_unit_cost);
	  DEBUG("textual.parse", "line " << linenum << ": "
		<< "Annotated amount is " << xact->amount);

	  if (cstream.eof())
	    next = NULL;
	  else
	    next = skip_ws(p + cstream.tellg());
	} else {
	  throw parse_error("Expected a cost amount");
	}
      }
    }
  }

  // Parse the optional balance assignment

  if (entry && next && *next == '=') {
    DEBUG("textual.parse", "line " << linenum << ": "
	  << "Found a balance assignment indicator");

    beg = ++next - line;

    p = skip_ws(next);
    if (*p) {
      xact->assigned_amount = amount_t();

      beg = p - line;
      ptristream stream(p, len - beg);

      if (*p != '(')		// indicates a value expression
	xact->assigned_amount->parse(stream, amount_t::PARSE_NO_MIGRATE);
      else
	parse_amount_expr(session, stream, *xact->assigned_amount, xact.get(),
			  static_cast<uint_least8_t>(expr_t::PARSE_NO_MIGRATE));

      if (xact->assigned_amount->is_null())
	throw parse_error("An assigned balance must evaluate to a constant value");

      DEBUG("textual.parse", "line " << linenum << ": "
	    << "XACT assign: parsed amt = " << *xact->assigned_amount);

      account_t::xdata_t& xdata(xact->account->xdata());
      amount_t&		  amt(*xact->assigned_amount);

      DEBUG("xact.assign", "line " << linenum << ": "
	    "account balance = " << xdata.value);
      DEBUG("xact.assign", "line " << linenum << ": "
	    "xact amount = " << amt);

      amount_t diff;

      switch (xdata.value.type()) {
      case value_t::AMOUNT:
	diff = amt - xdata.value.as_amount();
	break;

      case value_t::BALANCE:
	if (optional<amount_t> comm_bal =
	    xdata.value.as_balance().commodity_amount(amt.commodity()))
	  diff = amt - *comm_bal;
	else
	  diff = amt;
	break;

      case value_t::BALANCE_PAIR:
	if (optional<amount_t> comm_bal =
	    xdata.value.as_balance_pair().commodity_amount(amt.commodity()))
	  diff = amt - *comm_bal;
	else
	  diff = amt;
	break;

      default:
	diff = amt;
	break;
      }

      DEBUG("xact.assign",  "line " << linenum << ": "
	    << "diff = " << diff);
      DEBUG("textual.parse", "line " << linenum << ": "
	    << "XACT assign: diff = " << diff);

      if (! diff.is_zero()) {
	if (! xact->amount.is_null()) {
	  diff -= xact->amount;
	  if (! diff.is_zero()) {
	    xact_t * temp = new xact_t(xact->account, diff,
				       ITEM_GENERATED | XACT_CALCULATED);
	    entry->add_xact(temp);

	    DEBUG("textual.parse", "line " << linenum << ": "
		  << "Created balancing transaction");
	  }
	} else {
	  xact->amount = diff;
	  DEBUG("textual.parse", "line " << linenum << ": "
		<< "Overwrite null transaction");
	}
      }

      if (stream.eof())
	next = NULL;
      else
	next = skip_ws(p + stream.tellg());
    } else {
      throw parse_error("Expected an assigned balance amount");
    }
  }

  // Parse the optional note

  if (next && *next == ';') {
    xact->append_note(++next, session.current_year);
    next = line + len;
    DEBUG("textual.parse", "line " << linenum << ": "
	  << "Parsed a transaction note");
  }

  // There should be nothing more to read

  if (next && *next)
    throw_(parse_error, "Unexpected char '" << *next
	   << "' (Note: inline math requires parentheses)");

  xact->end_pos  = curr_pos;
  xact->end_line = linenum;

  TRACE_STOP(xact_details, 1);

  return xact.release();

  }
  catch (const std::exception& err) {
    add_error_context("While parsing transaction:");
    add_error_context(line_context(buf, beg, len));
    throw;
  }
}

bool textual_parser_t::instance_t::parse_xacts(account_t *	account,
					       entry_base_t&    entry,
					       const string&    kind)
{
  TRACE_START(entry_xacts, 1, "Time spent parsing transactions:");

  bool added = false;

  while (peek_whitespace_line()) {
    char * line;
    std::streamsize len = read_line(line);
    assert(len > 0);

    if (xact_t * xact = parse_xact(line, len, account, NULL)) {
      entry.add_xact(xact);
      added = true;
    }
  }

  TRACE_STOP(entry_xacts, 1);

  return added;
}

entry_t * textual_parser_t::instance_t::parse_entry(char *	    line,
						    std::streamsize len,
						    account_t *	    account)
{
  TRACE_START(entry_text, 1, "Time spent parsing entry text:");

  std::auto_ptr<entry_t> curr(new entry_t);

  curr->pathname = pathname;
  curr->beg_pos  = line_beg_pos;
  curr->beg_line = linenum;

  // Parse the date

  char * next = next_element(line);

  if (char * p = std::strchr(line, '=')) {
    *p++ = '\0';
    curr->_date_eff = parse_date(p, session.current_year);
  }
  curr->_date = parse_date(line, session.current_year);

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

  if (next && *next) {
    curr->payee = next;
    next = next_element(next, true);
  } else {
    curr->payee = "<Unspecified payee>";
  }

  // Parse the entry note

  if (next && *next == ';')
    curr->append_note(next, session.current_year);

  TRACE_STOP(entry_text, 1);

  // Parse all of the xacts associated with this entry

  TRACE_START(entry_details, 1, "Time spent parsing entry details:");

  xact_t * last_xact = NULL;

  while (peek_whitespace_line()) {
    len = read_line(line);

    char * p = skip_ws(line);
    if (! *p)
      throw parse_error("Line contains only whitespace");

    if (*p == ';') {
      item_t * item;
      if (last_xact)
	item = last_xact;
      else
	item = curr.get();

      // This is a trailing note, and possibly a metadata info tag
      item->append_note(p + 1, session.current_year);
      item->end_pos = curr_pos;
      item->end_line++;
    }
    else if (xact_t * xact = parse_xact(p, len - (p - line), account,
					curr.get())) {
      curr->add_xact(xact);
      last_xact = xact;
    }
  }

  curr->end_pos  = curr_pos;
  curr->end_line = linenum;

  TRACE_STOP(entry_details, 1);

  return curr.release();
}

expr_t::ptr_op_t textual_parser_t::instance_t::lookup(const string& name)
{
  return session.lookup(name);
}

} // namespace ledger
