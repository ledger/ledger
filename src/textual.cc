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

#include <system.hh>

#include "journal.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "option.h"
#include "pstream.h"
#include "pool.h"
#include "session.h"

#define TIMELOG_SUPPORT 1
#if defined(TIMELOG_SUPPORT)
#include "timelog.h"
#endif

namespace ledger {

namespace {
  class instance_t : public noncopyable, public scope_t
  {
    static const std::size_t MAX_LINE = 1024;

  public:
    std::list<account_t *>& account_stack;
    std::list<string>&      tag_stack;
#if defined(TIMELOG_SUPPORT)
    time_log_t&		    timelog;
#endif

    instance_t *      parent;
    std::istream&     in;
    scope_t&	      scope;
    journal_t&	      journal;
    account_t *	      master;
    const path *      original_file;
    accounts_map      account_aliases;
    bool              strict;

    path	      pathname;
    char	      linebuf[MAX_LINE + 1];
    std::size_t       linenum;
    istream_pos_type  line_beg_pos;
    istream_pos_type  curr_pos;
    std::size_t       count;
    std::size_t       errors;

    optional<date_t::year_type> current_year;

    scoped_ptr<auto_xact_finalizer_t> auto_xact_finalizer;

    instance_t(std::list<account_t *>& _account_stack,
	       std::list<string>&      _tag_stack,
#if defined(TIMELOG_SUPPORT)
	       time_log_t&             _timelog,
#endif
	       std::istream&	       _in,
	       scope_t&	               _scope,
	       journal_t&	       _journal,
	       account_t *	       _master        = NULL,
	       const path *	       _original_file = NULL,
	       bool                    _strict        = false,
	       instance_t *            _parent        = NULL);

    ~instance_t();

    void parse();
    std::streamsize read_line(char *& line);
    bool peek_whitespace_line() {
      return (in.good() && ! in.eof() &&
	      (in.peek() == ' ' || in.peek() == '\t'));
    }
    void read_next_directive(); 

#if defined(TIMELOG_SUPPORT)
    void clock_in_directive(char * line, bool capitalized);
    void clock_out_directive(char * line, bool capitalized);
#endif

    void default_commodity_directive(char * line);
    void default_account_directive(char * line);
    void price_conversion_directive(char * line);
    void price_xact_directive(char * line);
    void nomarket_directive(char * line);
    void year_directive(char * line);
    void option_directive(char * line);
    void automated_xact_directive(char * line);
    void period_xact_directive(char * line);
    void xact_directive(char * line, std::streamsize len);
    void include_directive(char * line);
    void account_directive(char * line);
    void end_directive(char * line);
    void alias_directive(char * line);
    void tag_directive(char * line);
    void pop_directive(char * line);
    void define_directive(char * line);
    bool general_directive(char * line);

    post_t * parse_post(char *		line,
			std::streamsize len,
			account_t *	account,
			xact_t *	xact,
			bool            honor_strict = true);

    bool parse_posts(account_t *   account,
		     xact_base_t& xact);

    xact_t * parse_xact(char *	  line,
			  std::streamsize len,
			  account_t *	  account);

    virtual expr_t::ptr_op_t lookup(const symbol_t::kind_t kind,
				    const string& name);
  };

  void parse_amount_expr(scope_t&             scope,
			 std::istream&        in,
			 amount_t&            amount,
			 post_t *             post,
			 const parse_flags_t& flags = PARSE_DEFAULT)
  {
    expr_t expr(in, flags.plus_flags(PARSE_PARTIAL));

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
      bind_scope_t bound_scope(scope, *post);

      value_t result(expr.calc(bound_scope));
      if (result.is_long()) {
	amount = result.to_amount();
      } else {
	if (! result.is_amount())
	  throw_(parse_error, _("Postings may only specify simple amounts"));

	amount = result.as_amount();
      }
      DEBUG("textual.parse", "The posting amount is " << amount);
    }
  }
}

instance_t::instance_t(std::list<account_t *>& _account_stack,
		       std::list<string>&      _tag_stack,
#if defined(TIMELOG_SUPPORT)
		       time_log_t&             _timelog,
#endif
		       std::istream&	       _in,
		       scope_t&	               _scope,
		       journal_t&	       _journal,
		       account_t *	       _master,
		       const path *	       _original_file,
		       bool                    _strict,
		       instance_t *            _parent)
  : account_stack(_account_stack), tag_stack(_tag_stack),
#if defined(TIMELOG_SUPPORT)
    timelog(_timelog),
#endif
    parent(_parent), in(_in), scope(_scope),
    journal(_journal), master(_master),
    original_file(_original_file), strict(_strict)
{
  TRACE_CTOR(instance_t, "...");

  if (! master)
    master = journal.master;
  account_stack.push_front(master);

  if (_original_file)
    pathname = *_original_file;
  else
    pathname = "/dev/stdin";
}

instance_t::~instance_t()
{
  TRACE_DTOR(instance_t);

  account_stack.pop_front();

  if (auto_xact_finalizer.get())
    journal.remove_xact_finalizer(auto_xact_finalizer.get());
}

void instance_t::parse()
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
	  add_error_context(_("In file included from %1")
			    << file_context(instance->pathname,
					    instance->linenum));
      }
      add_error_context(_("While parsing file %1")
			<< file_context(pathname, linenum));

      if (caught_signal != NONE_CAUGHT)
	throw;

      string context = error_context();
      if (! context.empty())
	std::cerr << context << std::endl;
    
      if (! current_context.empty())
	std::cerr << current_context << std::endl;

      std::cerr << _("Error: ") << err.what() << std::endl;
      errors++;
    }
  }

  TRACE_STOP(instance_parse, 1);
}

std::streamsize instance_t::read_line(char *& line)
{
  assert(in.good());
  assert(! in.eof());		// no one should call us in that case

  line_beg_pos = curr_pos;

  check_for_signal();

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

void instance_t::read_next_directive()
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
    xact_directive(line, len);
    break;
  case '=':			// automated xact
    automated_xact_directive(line);
    break;
  case '~':			// period xact
    period_xact_directive(line);
    break;

  case '@':
  case '!':
    line++;
    // fall through...
  default:			// some other directive
    if (! general_directive(line)) {
      switch (line[0]) {
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

      case 'A':		        // a default account for unbalanced posts
	default_account_directive(line);
	break;
      case 'C':			// a set of conversions
	price_conversion_directive(line);
	break;
      case 'D':			// a default commodity for "xact"
	default_commodity_directive(line);
	break;
      case 'N':			// don't download prices
	nomarket_directive(line);
	break;
      case 'P':			// a pricing xact
	price_xact_directive(line);
	break;
      case 'Y':			// set the current year
	year_directive(line);
	break;
      }
    }
    break;
  }
}

#if defined(TIMELOG_SUPPORT)

void instance_t::clock_in_directive(char * line,
				    bool   /*capitalized*/)
{
  string datetime(line, 2, 19);

  char * p = skip_ws(line + 22);
  char * n = next_element(p, true);

  timelog.clock_in(parse_datetime(datetime, current_year),
		   account_stack.front()->find_account(p), n ? n : "");
}

void instance_t::clock_out_directive(char * line,
				     bool   /*capitalized*/)
{  
  string datetime(line, 2, 19);

  char * p = skip_ws(line + 22);
  char * n = next_element(p, true);

  timelog.clock_out(parse_datetime(datetime, current_year),
		    p ? account_stack.front()->find_account(p) : NULL, n ? n : "");
  count++;
}

#endif // TIMELOG_SUPPORT

void instance_t::default_commodity_directive(char * line)
{
  amount_t amt(skip_ws(line + 1));
  VERIFY(amt.valid());
  amount_t::current_pool->default_commodity = &amt.commodity();
  amt.commodity().add_flags(COMMODITY_KNOWN);
}

void instance_t::default_account_directive(char * line)
{
  journal.bucket = account_stack.front()->find_account(skip_ws(line + 1));
  journal.bucket->add_flags(ACCOUNT_KNOWN);
}

void instance_t::price_conversion_directive(char * line)
{
  if (char * p = std::strchr(line + 1, '=')) {
    *p++ = '\0';
    amount_t::parse_conversion(line + 1, p);
  }
}

void instance_t::price_xact_directive(char * line)
{
  optional<price_point_t> point =
    amount_t::current_pool->parse_price_directive(skip_ws(line + 1));
  if (! point)
    throw parse_error(_("Pricing entry failed to parse"));
}

void instance_t::nomarket_directive(char * line)
{
  char * p = skip_ws(line + 1);
  string symbol;
  commodity_t::parse_symbol(p, symbol);

  if (commodity_t * commodity =
      amount_t::current_pool->find_or_create(symbol))
    commodity->add_flags(COMMODITY_NOMARKET | COMMODITY_KNOWN);
}

void instance_t::year_directive(char * line)
{
  current_year = lexical_cast<unsigned short>(skip_ws(line + 1));
}

void instance_t::option_directive(char * line)
{
  char * p = next_element(line);
  if (! p) {
    p = std::strchr(line, '=');
    if (p)
      *p++ = '\0';
  }

  if (! process_option(pathname.string(), line + 2, scope, p, line))
    throw_(option_error, _("Illegal option --%1") << line + 2);
}

void instance_t::automated_xact_directive(char * line)
{
  istream_pos_type pos	= line_beg_pos;
  std::size_t      lnum = linenum;

  bool reveal_context = true;

  try {

  if (! auto_xact_finalizer.get()) {
    auto_xact_finalizer.reset(new auto_xact_finalizer_t(&journal));
    journal.add_xact_finalizer(auto_xact_finalizer.get());
  }

  std::auto_ptr<auto_xact_t> ae
    (new auto_xact_t(predicate_t(skip_ws(line + 1),
				 keep_details_t(true, true, true))));

  reveal_context = false;

  if (parse_posts(account_stack.front(), *ae.get())) {
    reveal_context = true;

    journal.auto_xacts.push_back(ae.get());

    ae->journal	      = &journal;
    ae->pos	      = position_t();
    ae->pos->pathname = pathname;
    ae->pos->beg_pos  = pos;
    ae->pos->beg_line = lnum;
    ae->pos->end_pos  = curr_pos;
    ae->pos->end_line = linenum;

    ae.release();
  }

  }
  catch (const std::exception& err) {
    if (reveal_context) {
      add_error_context(_("While parsing periodic transaction:"));
      add_error_context(source_context(pathname, pos, curr_pos, "> "));
    }
    throw;
  }
}

void instance_t::period_xact_directive(char * line)
{
  istream_pos_type pos	= line_beg_pos;
  std::size_t      lnum = linenum;

  bool reveal_context = true;

  try {

  std::auto_ptr<period_xact_t> pe(new period_xact_t(skip_ws(line + 1)));

  reveal_context = false;

  if (parse_posts(account_stack.front(), *pe.get())) {
    reveal_context = true;
    pe->journal = &journal;

    if (pe->finalize()) {
      extend_xact_base(&journal, *pe.get(), true);

      journal.period_xacts.push_back(pe.get());

      pe->pos           = position_t();
      pe->pos->pathname = pathname;
      pe->pos->beg_pos  = pos;
      pe->pos->beg_line = lnum;
      pe->pos->end_pos  = curr_pos;
      pe->pos->end_line = linenum;

      pe.release();
    } else {
      pe->journal = NULL;
      throw parse_error(_("Period transaction failed to balance"));
    }
  }

  }
  catch (const std::exception& err) {
    if (reveal_context) {
      add_error_context(_("While parsing periodic transaction:"));
      add_error_context(source_context(pathname, pos, curr_pos, "> "));
    }
    throw;
  }
}

void instance_t::xact_directive(char * line, std::streamsize len)
{
  TRACE_START(xacts, 1, "Time spent handling transactions:");

  if (xact_t * xact = parse_xact(line, len, account_stack.front())) {
    std::auto_ptr<xact_t> manager(xact);

    if (journal.add_xact(xact)) {
      manager.release();	// it's owned by the journal now
      count++;
    }
    // It's perfectly valid for the journal to reject the xact, which it will
    // do if the xact has no substantive effect (for example, a checking
    // xact, all of whose postings have null amounts).
  } else {
    throw parse_error(_("Failed to parse transaction"));
  }

  TRACE_STOP(xacts, 1);
}

void instance_t::include_directive(char * line)
{
  path filename(line);

  filename = resolve_path(filename);

  DEBUG("textual.include", "Line " << linenum << ": " <<
	"Including path '" << filename << "'");

  if (! exists(filename))
    throw_(std::runtime_error,
	   _("File to include was not found: '%1'" << filename));

  ifstream stream(filename);

  instance_t instance(account_stack, tag_stack,
#if defined(TIMELOG_SUPPORT)
		      timelog,
#endif
		      stream, scope, journal, master,
		      &filename, strict, this);
  instance.parse();

  errors += instance.errors;
  count  += instance.count;
}

void instance_t::account_directive(char * line)
{
  if (account_t * acct = account_stack.front()->find_account(line))
    account_stack.push_front(acct);
  else
    assert(! "Failed to create account");
}

void instance_t::end_directive(char *)
{
  if (account_stack.empty())
    throw_(std::runtime_error,
	   _("'end' directive found, but no account currently active"));
  else
    account_stack.pop_back();
}

void instance_t::alias_directive(char * line)
{
  char * b = skip_ws(line);
  if (char * e = std::strchr(b, '=')) {
    char * z = e - 1;
    while (std::isspace(*z))
      *z-- = '\0';
    *e++ = '\0';
    e = skip_ws(e);

    // Once we have an alias name (b) and the target account
    // name (e), add a reference to the account in the
    // `account_aliases' map, which is used by the post
    // parser to resolve alias references.
    account_t * acct = account_stack.front()->find_account(e);
    std::pair<accounts_map::iterator, bool> result
      = account_aliases.insert(accounts_map::value_type(b, acct));
    assert(result.second);
  }
}

void instance_t::tag_directive(char * line)
{
  tag_stack.push_back(trim_ws(line));
}

void instance_t::pop_directive(char *)
{
  if (tag_stack.empty())
    throw_(std::runtime_error,
	   _("'pop' directive found, but no tags currently active"));
  else
    tag_stack.pop_back();
}

void instance_t::define_directive(char * line)
{
  expr_t def(skip_ws(line));
  def.compile(scope);	// causes definitions to be established
}

bool instance_t::general_directive(char * line)
{
  char * p = line;
  if (*p == '@' || *p == '!')
    p++;

  switch (*p) {
  case 'a':
    if (std::strcmp(p, "account") == 0) {
      account_directive(next_element(line));
      return true;
    }
    else if (std::strcmp(p, "alias") == 0) {
      alias_directive(next_element(line));
      return true;
    }
    break;

  case 'd':
    if (std::strcmp(p, "def") == 0) {
      define_directive(next_element(line));
      return true;
    }
    break;

  case 'e':
    if (std::strcmp(p, "end") == 0) {
      end_directive(next_element(line));
      return true;
    }
    break;

  case 'i':
    if (std::strcmp(p, "include") == 0) {
      include_directive(next_element(line));
      return true;
    }
    break;

  case 'p':
    if (std::strcmp(p, "pop") == 0) {
      pop_directive(next_element(line));
      return true;
    }
    break;

  case 't':
    if (std::strcmp(p, "tag") == 0) {
      tag_directive(next_element(line));
      return true;
    }
    break;
  }

  if (expr_t::ptr_op_t op = lookup(symbol_t::DIRECTIVE, p)) {
    call_scope_t args(*this);
    args.push_back(string_value(p));
    op->as_function()(args);
    return true;
  }

  return false;
}

post_t * instance_t::parse_post(char *		line,
				std::streamsize len,
				account_t *	account,
				xact_t *	xact,
				bool            honor_strict)
{
  TRACE_START(post_details, 1, "Time spent parsing postings:");

  std::auto_ptr<post_t> post(new post_t);

  post->xact	      = xact;	// this could be NULL
  post->pos	      = position_t();
  post->pos->pathname = pathname;
  post->pos->beg_pos  = line_beg_pos;
  post->pos->beg_line = linenum;

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
    post->set_state(item_t::CLEARED);
    p = skip_ws(p + 1);
    DEBUG("textual.parse", "line " << linenum << ": "
	  << "Parsed the CLEARED flag");
    break;

  case '!':
    post->set_state(item_t::PENDING);
    p = skip_ws(p + 1);
    DEBUG("textual.parse", "line " << linenum << ": "
	  << "Parsed the PENDING flag");
    break;
  }

  if (xact &&
      ((xact->_state == item_t::CLEARED && post->_state != item_t::CLEARED) ||
       (xact->_state == item_t::PENDING && post->_state == item_t::UNCLEARED)))
    post->set_state(xact->_state);

  // Parse the account name

  if (! *p)
    throw parse_error(_("Posting has no account"));

  char * next = next_element(p, true);
  char * e = p + std::strlen(p);

  while (e > p && std::isspace(*(e - 1)))
    e--;

  if ((*p == '[' && *(e - 1) == ']') || (*p == '(' && *(e - 1) == ')')) {
    post->add_flags(POST_VIRTUAL);
    DEBUG("textual.parse", "line " << linenum << ": "
	  << "Parsed a virtual account name");

    if (*p == '[') {
      post->add_flags(POST_MUST_BALANCE);
      DEBUG("textual.parse", "line " << linenum << ": "
	    << "Posting must balance");
    }
    p++; e--;
  }

  string name(p, e - p);
  DEBUG("textual.parse", "line " << linenum << ": "
	<< "Parsed account name " << name);

  if (account_aliases.size() > 0) {
    accounts_map::const_iterator i = account_aliases.find(name);
    if (i != account_aliases.end())
      post->account = (*i).second;
  }
  if (! post->account)
    post->account = account->find_account(name);

  if (honor_strict && strict && ! post->account->has_flags(ACCOUNT_KNOWN)) {
    if (post->_state == item_t::UNCLEARED)
      warning_(_("\"%1\", line %2: Unknown account '%3'")
	       << pathname << linenum << post->account->fullname());
    post->account->add_flags(ACCOUNT_KNOWN);
  }

  // Parse the optional amount

  bool saw_amount = false;

  if (next && *next && (*next != ';' && *next != '=')) {
    saw_amount = true;

    beg = next - line;
    ptristream stream(next, len - beg);

    if (*next != '(')		// indicates a value expression
      post->amount.parse(stream, PARSE_NO_REDUCE);
    else
      parse_amount_expr(scope, stream, post->amount, post.get(),
			PARSE_NO_REDUCE | PARSE_SINGLE | PARSE_NO_ASSIGN);

    if (! post->amount.is_null() && honor_strict && strict &&
	post->amount.has_commodity() &&
	! post->amount.commodity().has_flags(COMMODITY_KNOWN)) {
      if (post->_state == item_t::UNCLEARED)
	warning_(_("\"%1\", line %2: Unknown commodity '%3'")
		 << pathname << linenum << post->amount.commodity());
      post->amount.commodity().add_flags(COMMODITY_KNOWN);
    }

    DEBUG("textual.parse", "line " << linenum << ": "
	  << "post amount = " << post->amount);

    if (stream.eof()) {
      next = NULL;
    } else {
      next = skip_ws(next + static_cast<std::ptrdiff_t>(stream.tellg()));

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
	  post->cost = amount_t();

	  beg = p - line;
	  ptristream cstream(p, len - beg);

	  if (*p != '(')		// indicates a value expression
	    post->cost->parse(cstream, PARSE_NO_MIGRATE);
	  else
	    parse_amount_expr(scope, cstream, *post->cost, post.get(),
			      PARSE_NO_MIGRATE | PARSE_SINGLE |
			      PARSE_NO_ASSIGN);

	  if (post->cost->sign() < 0)
	    throw parse_error(_("A posting's cost may not be negative"));

	  post->cost->in_place_unround();

	  if (per_unit) {
	    // For the sole case where the cost might be uncommoditized,
	    // guarantee that the commodity of the cost after multiplication
	    // is the same as it was before.
	    commodity_t& cost_commodity(post->cost->commodity());
	    *post->cost *= post->amount;
	    post->cost->set_commodity(cost_commodity);
	  }

	  DEBUG("textual.parse", "line " << linenum << ": "
		<< "Total cost is " << *post->cost);
	  DEBUG("textual.parse", "line " << linenum << ": "
		<< "Annotated amount is " << post->amount);

	  if (cstream.eof())
	    next = NULL;
	  else
	    next = skip_ws(p + static_cast<std::ptrdiff_t>(cstream.tellg()));
	} else {
	  throw parse_error(_("Expected a cost amount"));
	}
      }
    }
  }

  // Parse the optional balance assignment

  if (xact && next && *next == '=') {
    DEBUG("textual.parse", "line " << linenum << ": "
	  << "Found a balance assignment indicator");

    beg = ++next - line;

    p = skip_ws(next);
    if (*p) {
      post->assigned_amount = amount_t();

      beg = p - line;
      ptristream stream(p, len - beg);

      if (*p != '(')		// indicates a value expression
	post->assigned_amount->parse(stream, PARSE_NO_MIGRATE);
      else
	parse_amount_expr(scope, stream, *post->assigned_amount, post.get(),
			  PARSE_SINGLE | PARSE_NO_MIGRATE);

      if (post->assigned_amount->is_null()) {
	if (post->amount.is_null())
	  throw parse_error(_("Balance assignment must evaluate to a constant"));
	else
	  throw parse_error(_("Balance assertion must evaluate to a constant"));
      }

      DEBUG("textual.parse", "line " << linenum << ": "
	    << "POST assign: parsed amt = " << *post->assigned_amount);

      amount_t&	amt(*post->assigned_amount);
      value_t   account_total(post->account->amount(false)
			      .strip_annotations(keep_details_t()));

      DEBUG("post.assign",
	    "line " << linenum << ": " "account balance = " << account_total);
      DEBUG("post.assign",
	    "line " << linenum << ": " "post amount = " << amt);

      amount_t diff = amt;

      switch (account_total.type()) {
      case value_t::AMOUNT:
	diff -= account_total.as_amount();
	break;

      case value_t::BALANCE:
	if (optional<amount_t> comm_bal =
	    account_total.as_balance().commodity_amount(amt.commodity()))
	  diff -= *comm_bal;
	break;

      default:
	break;
      }

      DEBUG("post.assign",
	    "line " << linenum << ": " << "diff = " << diff);
      DEBUG("textual.parse",
	    "line " << linenum << ": " << "POST assign: diff = " << diff);

      if (! diff.is_zero()) {
	if (! post->amount.is_null()) {
	  diff -= post->amount;
	  if (! diff.is_zero())
	    throw_(parse_error, _("Balance assertion off by %1") << diff);
	} else {
	  post->amount = diff;
	  DEBUG("textual.parse", "line " << linenum << ": "
		<< "Overwrite null posting");
	}
      }

      if (stream.eof())
	next = NULL;
      else
	next = skip_ws(p + static_cast<std::ptrdiff_t>(stream.tellg()));
    } else {
      throw parse_error(_("Expected an balance assignment/assertion amount"));
    }
  }

  // Parse the optional note

  if (next && *next == ';') {
    post->append_note(++next, current_year);
    next = line + len;
    DEBUG("textual.parse", "line " << linenum << ": "
	  << "Parsed a posting note");
  }

  // There should be nothing more to read

  if (next && *next)
    throw_(parse_error,
	   _("Unexpected char '%1' (Note: inline math requires parentheses)")
	   << *next);

  post->pos->end_pos  = curr_pos;
  post->pos->end_line = linenum;

  if (! tag_stack.empty()) {
    foreach (const string& tag, tag_stack)
      post->parse_tags(tag.c_str());
  }

  TRACE_STOP(post_details, 1);

  return post.release();

  }
  catch (const std::exception& err) {
    add_error_context(_("While parsing posting:"));
    add_error_context(line_context(buf, beg, len));
    throw;
  }
}

bool instance_t::parse_posts(account_t *   account,
			     xact_base_t& xact)
{
  TRACE_START(xact_posts, 1, "Time spent parsing postings:");

  bool added = false;

  while (peek_whitespace_line()) {
    char * line;
    std::streamsize len = read_line(line);
    assert(len > 0);

    if (post_t * post = parse_post(line, len, account, NULL, false)) {
      xact.add_post(post);
      added = true;
    }
  }

  TRACE_STOP(xact_posts, 1);

  return added;
}

xact_t * instance_t::parse_xact(char *		line,
				std::streamsize len,
				account_t *	account)
{
  TRACE_START(xact_text, 1, "Time spent parsing transaction text:");

  std::auto_ptr<xact_t> xact(new xact_t);

  xact->pos	      = position_t();
  xact->pos->pathname = pathname;
  xact->pos->beg_pos  = line_beg_pos;
  xact->pos->beg_line = linenum;

  bool reveal_context = true;

  try {

  // Parse the date

  char * next = next_element(line);

  if (char * p = std::strchr(line, '=')) {
    *p++ = '\0';
    xact->_date_eff = parse_date(p, current_year);
  }
  xact->_date = parse_date(line, current_year);

  // Parse the optional cleared flag: *

  if (next) {
    switch (*next) {
    case '*':
      xact->_state = item_t::CLEARED;
      next = skip_ws(++next);
      break;
    case '!':
      xact->_state = item_t::PENDING;
      next = skip_ws(++next);
      break;
    }
  }

  // Parse the optional code: (TEXT)

  if (next && *next == '(') {
    if (char * p = std::strchr(next++, ')')) {
      *p++ = '\0';
      xact->code = next;
      next = skip_ws(p);
    }
  }

  // Parse the description text

  if (next && *next) {
    char * p = next_element(next, true);
    xact->payee = next;
    next = p;
  } else {
    xact->payee = _("<Unspecified payee>");
  }

  // Parse the xact note

  if (next && *next == ';')
    xact->append_note(++next, current_year);

  TRACE_STOP(xact_text, 1);

  // Parse all of the posts associated with this xact

  TRACE_START(xact_details, 1, "Time spent parsing transaction details:");

  post_t * last_post = NULL;

  while (peek_whitespace_line()) {
    len = read_line(line);

    char * p = skip_ws(line);
    if (! *p)
      break;

    if (*p == ';') {
      item_t * item;
      if (last_post)
	item = last_post;
      else
	item = xact.get();

      // This is a trailing note, and possibly a metadata info tag
      item->append_note(p + 1, current_year);
      item->pos->end_pos = curr_pos;
      item->pos->end_line++;
    } else {
      reveal_context = false;

      if (post_t * post =
	  parse_post(p, len - (p - line), account, xact.get())) {
	xact->add_post(post);
	last_post = post;
      }
    }
  }

  if (xact->_state == item_t::UNCLEARED) {
    item_t::state_t result = item_t::CLEARED;

    foreach (post_t * post, xact->posts) {
      if (post->_state == item_t::UNCLEARED) {
	result = item_t::UNCLEARED;
	break;
      }
      else if (post->_state == item_t::PENDING) {
	result = item_t::PENDING;
      }
    }
  }

  xact->pos->end_pos  = curr_pos;
  xact->pos->end_line = linenum;

  if (! tag_stack.empty()) {
    foreach (const string& tag, tag_stack)
      xact->parse_tags(tag.c_str());
  }

  TRACE_STOP(xact_details, 1);

  return xact.release();

  }
  catch (const std::exception& err) {
    if (reveal_context) {
      add_error_context(_("While parsing transaction:"));
      add_error_context(source_context(xact->pos->pathname,
				       xact->pos->beg_pos, curr_pos, "> "));
    }
    throw;
  }
}

expr_t::ptr_op_t instance_t::lookup(const symbol_t::kind_t kind,
				    const string& name)
{
  return scope.lookup(kind, name);
}

std::size_t journal_t::parse(std::istream& in,
			     scope_t&      scope,
			     account_t *   master,
			     const path *  original_file,
			     bool          strict)
{
  TRACE_START(parsing_total, 1, "Total time spent parsing text:");

  std::list<account_t *> account_stack;
  std::list<string>      tag_stack;
#if defined(TIMELOG_SUPPORT)
  time_log_t		 timelog(*this);
#endif

  instance_t parsing_instance(account_stack, tag_stack,
#if defined(TIMELOG_SUPPORT)
			      timelog,
#endif
			      in, scope, *this, master,
			      original_file, strict);
  parsing_instance.parse();

  TRACE_STOP(parsing_total, 1);

  // These tracers were started in textual.cc
  TRACE_FINISH(xact_text, 1);
  TRACE_FINISH(xact_details, 1);
  TRACE_FINISH(xact_posts, 1);
  TRACE_FINISH(xacts, 1);
  TRACE_FINISH(instance_parse, 1); // report per-instance timers
  TRACE_FINISH(parsing_total, 1);

  if (parsing_instance.errors > 0)
    throw static_cast<int>(parsing_instance.errors);

  return parsing_instance.count;
}

} // namespace ledger
