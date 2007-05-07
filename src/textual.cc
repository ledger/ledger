#include "textual.h"
#include "session.h"

namespace ledger {

#define MAX_LINE 1024

static path	    pathname;
static unsigned int linenum;
static unsigned int src_idx;
static accounts_map account_aliases;

static std::list<std::pair<path, int> > include_stack;

#define TIMELOG_SUPPORT 1
#ifdef TIMELOG_SUPPORT

struct time_entry_t {
  moment_t    checkin;
  account_t * account;
  string desc;
};

std::list<time_entry_t> time_entries;

#endif // TIMELOG_SUPPORT

inline char * next_element(char * buf, bool variable = false)
{
  for (char * p = buf; *p; p++) {
    if (! (*p == ' ' || *p == '\t'))
      continue;

    if (! variable) {
      *p = '\0';
      return skip_ws(p + 1);
    }
    else if (*p == '\t') {
      *p = '\0';
      return skip_ws(p + 1);
    }
    else if (*(p + 1) == ' ') {
      *p = '\0';
      return skip_ws(p + 2);
    }
  }
  return NULL;
}

static inline void
parse_amount_expr(std::istream& in, journal_t *,
		  transaction_t& xact, amount_t& amount,
		  unsigned short flags = 0)
{
  xml::xpath_t xpath(in, flags | XPATH_PARSE_RELAXED | XPATH_PARSE_PARTIAL);

  DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
	 "Parsed an amount expression");

#if 0
  IF_DEBUG("ledger.textual.parse") {
    if (_debug_stream) {
      xpath.dump(*_debug_stream);
      *_debug_stream << std::endl;
    }
  }
#endif

  amount = xpath.calc(xact.data).to_amount();

  DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
	 "The transaction amount is " << amount);
}

transaction_t * parse_transaction(char *      line,
				  journal_t * journal,
				  account_t * account,
				  entry_t *   entry = NULL)
{
  // The account will be determined later...
  std::auto_ptr<transaction_t> xact(new transaction_t(NULL));

  // First cut up the input line into its various parts.

  char * state	      = NULL;
  char * account_path = NULL;
  char * amount	      = NULL;
  char * note	      = NULL;

  char * p = line;

  if (*p == '*' || *p == '!')
    state = p++;

  account_path = skip_ws(p);

  amount = next_element(account_path, true);
  if (amount) {
    char * p = amount;
    while (*p && *p != ';')
      p++;

    if (*p == ';') {
      *p++ = '\0';
      note = skip_ws(p);
    }

    p = amount + (std::strlen(amount) - 1);
    while (p > amount && std::isspace(*p))
      p--;

    if (std::isspace(*(p + 1)))
      *++p = '\0';
  }

  string err_desc;
#if 0
  try {
#endif

  xact->entry = entry;		// this might be NULL

  // Parse the state flag

  if (state)
    switch (*state) {
    case '*':
      xact->state = transaction_t::CLEARED;
      DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
	     "Parsed the CLEARED flag");
      break;
    case '!':
      xact->state = transaction_t::PENDING;
      DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
	     "Parsed the PENDING flag");
      break;
    }

  // Parse the account name

  char * b = &account_path[0];
  char * e = &account_path[std::strlen(account_path) - 1];
  if ((*b == '[' && *e == ']') ||
      (*b == '(' && *e == ')')) {
    xact->flags |= TRANSACTION_VIRTUAL;
    DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
	   "Parsed a virtual account name");
    if (*b == '[') {
      xact->flags |= TRANSACTION_BALANCE;
      DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
	     "Parsed a balanced virtual account name");
    }
    *account_path++ = '\0';
    *e = '\0';
  }

  DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
	 "Parsed account name " << account_path);
  if (account_aliases.size() > 0) {
    accounts_map::const_iterator i = account_aliases.find(account_path);
    if (i != account_aliases.end())
      xact->account = (*i).second;
  }
  if (! xact->account)
    xact->account = account->find_account(account_path);

  // Parse the optional amount

  if (amount && *amount) {
    std::istringstream in(amount);

    PUSH_CONTEXT();

    // jww (2006-09-15): Make sure it doesn't gobble up the upcoming @ symbol

    unsigned long beg = (long)in.tellg();

    amount_t temp;
    temp.parse(in, AMOUNT_PARSE_NO_REDUCE);
    xact->amount = temp;

    char c;
    if (! in.eof() && (c = peek_next_nonws(in)) != '@' &&
	c != ';' && ! in.eof()) {
      in.seekg(beg, std::ios::beg);

      if (xact->entry) {
	// Create a report item for this entry, so the transaction
	// below may refer to it

	if (! xact->entry->data)
	  xact->entry->data = xml::wrap_node(journal->document, xact->entry,
					     journal->document->top);

	xact->data = xml::wrap_node(journal->document, xact.get(),
				    xact->entry->data);
      }

      assert(xact->amount);
      parse_amount_expr(in, journal, *xact, *xact->amount,
			XPATH_PARSE_NO_REDUCE);

      if (xact->entry) {
	checked_delete(xact->data);
	xact->data = NULL;
      }

      unsigned long end = (long)in.tellg();

      xact->amount_expr = string(line, beg, end - beg);
    }

    // jww (2007-04-30): This should be a string context, or perhaps a
    // file context
    POP_CONTEXT(context("While parsing transaction amount"));

    // Parse the optional cost (@ PER-UNIT-COST, @@ TOTAL-COST)

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
	  PUSH_CONTEXT();

	  unsigned long beg = (long)in.tellg();

	  amount_t temp;
	  temp.parse(in);
	  xact->cost = temp;

	  unsigned long end = (long)in.tellg();

	  if (per_unit)
	    xact->cost_expr = (string("@") +
			       string(amount, beg, end - beg));
	  else
	    xact->cost_expr = (string("@@") +
			       string(amount, beg, end - beg));

	  POP_CONTEXT(context("While parsing transaction cost"));

	  if (xact->cost->sign() < 0)
	    throw_(parse_error, "A transaction's cost may not be negative");

	  assert(xact->amount);

	  amount_t per_unit_cost(*xact->cost);
	  if (per_unit)
	    *xact->cost *= xact->amount->number();
	  else
	    per_unit_cost /= xact->amount->number();

	  if (xact->amount->commodity() &&
	      ! xact->amount->commodity().annotated)
	    xact->amount->annotate_commodity(annotation_t(per_unit_cost,
							  xact->entry->actual_date(),
							  xact->entry->code));

	  DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
		 "Total cost is " << *xact->cost);
	  DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
		 "Per-unit cost is " << per_unit_cost);
	  DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
		 "Annotated amount is " << *xact->amount);
	  DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
		 "Bare amount is " << xact->amount->number());
	}
      }
    }

    if (xact->amount) {
      xact->amount->in_place_reduce();

      DEBUG("ledger.textual.parse", "line " << linenum << ": " <<
	    "Reduced amount is " << *xact->amount);
    }
  }

  // Parse the optional note

  if (note) {
    xact->note = note;
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
	  xact->_date_eff = parse_datetime(p);
	}
	if (buf[0])
	  xact->_date = parse_datetime(buf);
      }
  }

  return xact.release();

#if 0
  }
  catch (error * err) {
    err->context.push_back
      (new line_context(line, -1, ! err_desc.empty() ?
			err_desc : "While parsing transaction:"));
    throw err;
  }
#endif
}

bool parse_transactions(std::istream& in,
			journal_t *   journal,
			account_t *   account,
			entry_base_t& entry,
			const string& /* kind */,
			unsigned long beg_pos)
{
  static char line[MAX_LINE + 1];
  bool	      added = false;

  while (! in.eof() && (in.peek() == ' ' || in.peek() == '\t')) {
    in.getline(line, MAX_LINE);
    if (in.eof())
      break;

    beg_pos += std::strlen(line) + 1;
    linenum++;

    char * p = skip_ws(line);
    if (! *p || *p == '\r' || *p == '\n')
      break;

    if (transaction_t * xact = parse_transaction(p, journal, account)) {
      entry.add_transaction(xact);
      added = true;
    }
  }

  return added;
}

entry_t * parse_entry(std::istream& in, char * line, journal_t * journal,
		      account_t * master, textual_parser_t& /* parser */,
		      unsigned long beg_pos)
{
  TRACE_START(entry_text, 1, "Time spent preparing entry text:");

  std::auto_ptr<entry_t> curr(new entry_t);

  // First cut up the input line into its various parts.

  char * date	  = NULL;
  char * date_eff = NULL;
  char * statep   = NULL;
  char * code	  = NULL;
  char * payee	  = NULL;

  date = line;

  char * p = line;
  
  while (*p && (std::isdigit(*p) || *p == '/' || *p == '.' || *p == '-'))
    p++;
  assert(*p);

  if (*p == '=') {
    *p++ = '\0';
    date_eff = p;

    while (*p && (std::isdigit(*p) || *p == '/' || *p == '.' || *p == '-'))
      p++;
    assert(*p);
  } else {
    *p++ = '\0';
  }

  p = skip_ws(p);

  if (*p == '*' || *p == '!') {
    statep = p;
    p++; *p++ = '\0';

    p = skip_ws(p);
  }

  if (*p == '(') {
    code = ++p;
    while (*p && *p != ')')
      p++;
    assert(*p);
    *p++ = '\0';

    p = skip_ws(p);
  }

  payee = p;

  p = payee + (std::strlen(payee) - 1);
  while (p > payee && std::isspace(*p))
    p--;

  if (std::isspace(*(p + 1)))
    *++p = '\0';

  TRACE_STOP(entry_text, 1);

  // Parse the date

  TRACE_START(entry_date, 1, "Time spent parsing entry dates:");

  curr->_date = parse_datetime(date);

  if (date_eff)
    curr->_date_eff = parse_datetime(date_eff);

  TRACE_STOP(entry_date, 1);

  // Parse the optional cleared flag: *

  TRACE_START(entry_details, 1, "Time spent parsing entry details:");

  transaction_t::state_t state = transaction_t::UNCLEARED;
  if (statep) {
    switch (*statep) {
    case '*':
      state = transaction_t::CLEARED;
      break;
    case '!':
      state = transaction_t::PENDING;
      break;
    }
  }

  // Parse the optional code: (TEXT)

  if (code)
    curr->code = code;

  // Parse the payee/description text

  assert(payee);
  curr->payee = *payee != '\0' ? payee : "<Unspecified payee>";

  TRACE_STOP(entry_details, 1);

  // Parse all of the transactions associated with this entry

  TRACE_START(entry_xacts, 1, "Time spent parsing transactions:");

  unsigned long end_pos;
  unsigned long beg_line = linenum;

  while (! in.eof() && (in.peek() == ' ' || in.peek() == '\t')) {
    line[0] = '\0';
    in.getline(line, MAX_LINE);
    if (in.eof() || line[0] == '\0')
      break;
    end_pos = beg_pos + std::strlen(line) + 1;
    linenum++;

    char * p = skip_ws(line);
    if (! *p || *p == '\r' || *p == '\n')
      break;

    if (transaction_t * xact = parse_transaction(p, journal, master,
						 curr.get())) {
      if (state != transaction_t::UNCLEARED &&
	  xact->state == transaction_t::UNCLEARED)
	xact->state = state;

      xact->beg_pos  = beg_pos;
      xact->beg_line = beg_line;
      xact->end_pos  = end_pos;
      xact->end_line = linenum;
      beg_pos = end_pos;

      curr->add_transaction(xact);
    }

    if (in.eof())
      break;
  }

  if (curr->data) {
    checked_delete(curr->data);
    curr->data = NULL;
  }

  TRACE_STOP(entry_xacts, 1);

  return curr.release();
}

template <typename T>
struct push_var {
  T& var;
  T prev;
  push_var(T& _var) : var(_var), prev(var) {}
  ~push_var() { var = prev; }
};

static inline void parse_symbol(char *& p, string& symbol)
{
  if (*p == '"') {
    char * q = std::strchr(p + 1, '"');
    if (! q)
      throw_(parse_error, "Quoted commodity symbol lacks closing quote");
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
    throw_(parse_error, "Failed to parse commodity");
}

bool textual_parser_t::test(std::istream& in) const
{
  char buf[5];

  in.read(buf, 5);
  if (std::strncmp(buf, "<?xml", 5) == 0) {
#if defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)
    throw_(parse_error, "Ledger file contains XML data, but format was not recognized");
#else
    throw_(parse_error, "Ledger file contains XML data, but no XML support present");
#endif
  }

  in.clear();
  in.seekg(0, std::ios::beg);
  assert(in.good());
  return true;
}

static void clock_out_from_timelog(const moment_t& when,
				   account_t *	account,
				   const char *	desc,
				   journal_t *	journal)
{
  time_entry_t event;

  if (time_entries.size() == 1) {
    event = time_entries.back();
    time_entries.clear();
  }
  else if (time_entries.empty()) {
    throw_(parse_error, "Timelog check-out event without a check-in");
  }
  else if (! account) {
    throw_(parse_error,
	   "When multiple check-ins are active, checking out requires an account");
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
      throw_(parse_error,
	     "Timelog check-out event does not match any current check-ins");
  }

  if (desc && event.desc.empty()) {
    event.desc = desc;
    desc = NULL;
  }

  std::auto_ptr<entry_t> curr(new entry_t);
  curr->_date = when;
  curr->code  = desc ? desc : "";
  curr->payee = event.desc;

  if (curr->_date < event.checkin)
    throw_(parse_error,
	   "Timelog check-out date less than corresponding check-in");

  char buf[32];
  std::sprintf(buf, "%lds", (long)(curr->_date - event.checkin).total_seconds());
  amount_t amt;
  amt.parse(buf);

  transaction_t * xact
    = new transaction_t(event.account, amt, TRANSACTION_VIRTUAL);
  xact->state = transaction_t::CLEARED;
  curr->add_transaction(xact);

  if (! journal->add_entry(curr.get()))
    throw_(parse_error, "Failed to record 'out' timelog entry");
  else
    curr.release();
}

unsigned int textual_parser_t::parse(std::istream&	   in,
				     journal_t *	   journal,
				     account_t *	   master,
				     const optional<path>& original)
{
  static bool  added_auto_entry_hook = false;
  static char  line[MAX_LINE + 1];
  unsigned int count  = 0;

  TRACE_START(parsing_total, 1, "Total time spent parsing text:");

  std::list<account_t *> account_stack;

  auto_entry_finalizer_t auto_entry_finalizer(journal);

  if (! master && journal)
    master = journal->master;

  account_stack.push_front(master);

  pathname = (journal ? journal->sources.back() :
	      (assert(original), *original));
  src_idx  = journal ? journal->sources.size() - 1 : 0;
  linenum  = 1;

  INFO("Parsing file '" << pathname.string() << "'");

  unsigned long beg_pos = in.tellg();
  unsigned long end_pos;
  unsigned long beg_line = linenum;

  while (in.good() && ! in.eof()) {
    in.getline(line, MAX_LINE);
    if (in.eof())
      break;
    end_pos = beg_pos + std::strlen(line) + 1;
    linenum++;

    PUSH_CONTEXT();

    switch (line[0]) {
    case '\0':
    case '\r':
      break;

    case ' ':
    case '\t': {
      char * p = skip_ws(line);
      if (*p && *p != '\r')
	throw_(parse_error, "Line begins with whitespace");
      break;
    }

#ifdef TIMELOG_SUPPORT
    case 'i':
    case 'I': {
      string date(line, 2, 19);

      char * p = skip_ws(line + 22);
      char * n = next_element(p, true);

      time_entry_t event;
      event.desc    = n ? n : "";
      event.checkin = parse_datetime(date);
      event.account = account_stack.front()->find_account(p);

      if (! time_entries.empty())
	for (std::list<time_entry_t>::iterator i = time_entries.begin();
	     i != time_entries.end();
	     i++)
	  if (event.account == (*i).account)
	    throw_(parse_error, "Cannot double check-in to the same account");

      time_entries.push_back(event);
      break;
    }

    case 'o':
    case 'O':
      if (time_entries.empty()) {
	throw_(parse_error, "Timelog check-out event without a check-in");
      } else {
	string date(line, 2, 19);

	char * p = skip_ws(line + 22);
	char * n = next_element(p, true);

	clock_out_from_timelog
	  (parse_datetime(date),
	   p ? account_stack.front()->find_account(p) : NULL, n, journal);
	count++;
      }
      break;
#endif // TIMELOG_SUPPORT

    case 'D':	{		// a default commodity for "entry"
      amount_t amt(skip_ws(line + 1));
      amount_t::current_pool->default_commodity = &amt.commodity();
      break;
    }

    case 'A':		        // a default account for unbalanced xacts
      journal->basket =
	account_stack.front()->find_account(skip_ws(line + 1));
      break;

    case 'C':			// a set of conversions
      if (char * p = std::strchr(line + 1, '=')) {
	*p++ = '\0';
	amount_t::parse_conversion(line + 1, p);
      }
      break;

    case 'P': {		// a pricing entry
      char * date_field_ptr = skip_ws(line + 1);
      char * time_field_ptr = next_element(date_field_ptr);
      if (! time_field_ptr) break;
      string date_field = date_field_ptr;

      char * symbol_and_price;
      moment_t  datetime;

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

    case 'Y':			// set current year
#if 0
      // jww (2007-04-18): Need to set this up again
      date_t::current_year = lexical_cast<int>(skip_ws(line + 1));
#endif
      break;

#ifdef TIMELOG_SUPPORT
    case 'h':
    case 'b':
#endif
    case ';':			// comment
      break;

    case '-':			// option setting
      throw_(parse_error, "Option settings are not allowed in journal files");

    case '=': {		// automated entry
      if (! added_auto_entry_hook) {
	journal->add_entry_finalizer(&auto_entry_finalizer);
	added_auto_entry_hook = true;
      }

      std::auto_ptr<auto_entry_t> ae(new auto_entry_t(skip_ws(line + 1)));
      if (parse_transactions(in, journal, account_stack.front(), *ae,
			     "automated", end_pos)) {
	ae->src_idx  = src_idx;
	ae->beg_pos  = beg_pos;
	ae->beg_line = beg_line;
	ae->end_pos  = end_pos;
	ae->end_line = linenum;
	journal->auto_entries.push_back(ae.release());
      }
      break;
    }

    case '~': {		// period entry
      std::auto_ptr<period_entry_t> pe(new period_entry_t(skip_ws(line + 1)));
      if (! pe->period)
	throw_(parse_error, string("Parsing time period '") + skip_ws(line + 1) + "'");

      if (parse_transactions(in, journal, account_stack.front(), *pe,
			     "period", end_pos)) {
	if (pe->finalize()) {
	  extend_entry_base(journal, *pe, true);
	  pe->src_idx  = src_idx;
	  pe->beg_pos  = beg_pos;
	  pe->beg_line = beg_line;
	  pe->end_pos  = end_pos;
	  pe->end_line = linenum;
	  journal->period_entries.push_back(pe.release());
	} else {
	  throw_(parse_error, "Period entry failed to balance");
	}
      }
      break;
    }

    case '@':
    case '!': {                 // directive
      char * p = next_element(line);
      string word(line + 1);
      if (word == "include") {
	push_var<path>		save_path(pathname);
	push_var<unsigned int>  save_src_idx(src_idx);
	push_var<unsigned long> save_beg_pos(beg_pos);
	push_var<unsigned long> save_end_pos(end_pos);
	push_var<unsigned int>  save_linenum(linenum);

	if (*p != '~' && *p != '/')
	  pathname = (pathname.branch_path() / path(p)).normalize();
	else
	  pathname = resolve_path(p);

	DEBUG("ledger.textual.include", "Line " << linenum << ": " <<
	      "Including path '" << pathname.string() << "'");

	include_stack.push_back
	  (std::pair<path, int>(journal->sources.back(), linenum - 1));

	try {
	  count += journal->session->read_journal(pathname, journal,
						  account_stack.front());
	  include_stack.pop_back();
	}
	catch (...) {
	  include_stack.pop_back();
	  throw;
	}
      }
      else if (word == "account") {
	if (account_t * acct = account_stack.front()->find_account(p))
	  account_stack.push_front(acct);
	else
	  ; // jww (2007-04-30): throw an error here
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
	  // `account_aliases' map, which is used by the transaction
	  // parser to resolve alias references.
	  if (account_t * acct = account_stack.front()->find_account(e)) {
	    std::pair<accounts_map::iterator, bool> result
	      = account_aliases.insert(accounts_pair(b, acct));
	    assert(result.second);
	  } else {
	    ; // jww (2007-04-30): throw an error here
	  }
	}
      }
      else if (word == "def" || word == "eval") {
	// jww (2006-09-13): Read the string after and evaluate it.
	// But also keep a list of these value expressions, and a
	// way to know where they fall in the transaction sequence.
	// This will be necessary so that binary file reading can
	// re-evaluate them at the appopriate time.

	// compile(&journal->defs);
      }
      break;
    }

    default: {
      TRACE_START(entries, 1, "Time spent handling entries:");

      std::auto_ptr<entry_t> entry
	(parse_entry(in, line, journal, account_stack.front(),
		     *this, end_pos));
      if (entry.get()) {
	entry->src_idx  = src_idx;
	entry->beg_pos  = beg_pos;
	entry->beg_line = beg_line;
	entry->end_pos  = end_pos;
	entry->end_line = linenum;

	if (journal->add_entry(entry.get())) {
	  entry.release();
	  count++;
	} else {
	  throw_(parse_error, "Entry does not balance");
	}
      } else {
	throw_(parse_error, "Failed to parse entry");
      }

      TRACE_STOP(entries, 1);
      break;
    }
    }

    POP_CONTEXT(file_context(pathname, beg_line, linenum,
			     beg_pos, end_pos));

    beg_pos  = end_pos;
    beg_line = linenum;
  }

  if (! time_entries.empty()) {
    for (std::list<time_entry_t>::iterator i = time_entries.begin();
	 i != time_entries.end();
	 i++)
      clock_out_from_timelog(now, (*i).account, NULL, journal);
    time_entries.clear();
  }

  if (added_auto_entry_hook)
    journal->remove_entry_finalizer(&auto_entry_finalizer);

  TRACE_STOP(parsing_total, 1);

  return count;
}

} // namespace ledger
