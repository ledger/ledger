#ifdef USE_PCH
#include "pch.h"
#else
#if defined(__GNUG__) && __GNUG__ < 3
#define _XOPEN_SOURCE
#endif

#include "session.h"
#include "journal.h"
#include "repitem.h"
#include "textual.h"
#include "datetime.h"
#include "valexpr.h"
#include "error.h"
#include "option.h"
#include "timing.h"
#include "util.h"
#include "acconf.h"

#include <fstream>
#include <sstream>
#include <cstring>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#endif

#ifdef HAVE_REALPATH
extern "C" char *realpath(const char *, char resolved_path[]);
#endif

#define TIMELOG_SUPPORT 1

namespace ledger {

#define MAX_LINE 1024

static std::string  path;
static unsigned int linenum;
static unsigned int src_idx;
static accounts_map account_aliases;

static std::list<std::pair<std::string, int> > include_stack;

#ifdef TIMELOG_SUPPORT
struct time_entry_t {
  datetime_t  checkin;
  account_t * account;
  std::string desc;
};
std::list<time_entry_t> time_entries;
#endif

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
parse_amount_expr(std::istream& in, journal_t * journal,
		  transaction_t& xact, amount_t& amount,
		  unsigned short flags = 0)
{
  valexpr_t valexpr(in, flags | PARSE_VALEXPR_RELAXED | PARSE_VALEXPR_PARTIAL);

  DEBUG_PRINT("ledger.textual.parse", "line " << linenum << ": " <<
	      "Parsed an amount expression");

#ifdef DEBUG_ENABLED
  DEBUG_IF("ledger.textual.parse") {
    if (_debug_stream) {
      valexpr.dump(*_debug_stream);
      *_debug_stream << std::endl;
    }
  }
#endif

  amount = valexpr.calc(static_cast<repitem_t *>(xact.data)).to_amount();

  DEBUG_PRINT("ledger.textual.parse", "line " << linenum << ": " <<
	      "The transaction amount is " << amount);
}

transaction_t * parse_transaction(char *      line,
				  journal_t * journal,
				  account_t * account,
				  entry_t *   entry = NULL)
{
  // The account will be determined later...
  std::auto_ptr<transaction_t> xact(new transaction_t(NULL));

  std::istringstream in(line);
  std::string err_desc;
  try {

  xact->entry = entry;		// this might be NULL
  xact->data  = repitem_t::wrap(xact.get(), entry ?
				static_cast<repitem_t *>(entry->data) :
				static_cast<repitem_t *>(journal->data));
  // Parse the state flag

  char p = peek_next_nonws(in);
  switch (p) {
  case '*':
    xact->state = transaction_t::CLEARED;
    in.get(p);
    p = peek_next_nonws(in);
    DEBUG_PRINT("ledger.textual.parse", "line " << linenum << ": " <<
		"Parsed the CLEARED flag");
    break;
  case '!':
    xact->state = transaction_t::PENDING;
    in.get(p);
    p = peek_next_nonws(in);
    DEBUG_PRINT("ledger.textual.parse", "line " << linenum << ": " <<
		"Parsed the PENDING flag");
    break;
  }

  // Parse the account name

  unsigned long account_beg = in.tellg();
  unsigned long account_end = account_beg;
  while (! in.eof()) {
    in.get(p);
    if (in.eof() || (std::isspace(p) &&
		     (p == '\t' || std::isspace(in.peek()))))
      break;
    account_end++;
  }

  if (account_beg == account_end)
    throw new parse_error("No account was specified");

  char * b = &line[account_beg];
  char * e = &line[account_end];
  if ((*b == '[' && *(e - 1) == ']') ||
      (*b == '(' && *(e - 1) == ')')) {
    xact->flags |= TRANSACTION_VIRTUAL;
    DEBUG_PRINT("ledger.textual.parse", "line " << linenum << ": " <<
		"Parsed a virtual account name");
    if (*b == '[') {
      xact->flags |= TRANSACTION_BALANCE;
      DEBUG_PRINT("ledger.textual.parse", "line " << linenum << ": " <<
		  "Parsed a balanced virtual account name");
    }
    b++; e--;
  }

  std::string name(b, e - b);
  DEBUG_PRINT("ledger.textual.parse", "line " << linenum << ": " <<
	      "Parsed account name " << name);
  if (account_aliases.size() > 0) {
    accounts_map::const_iterator i = account_aliases.find(name);
    if (i != account_aliases.end())
      xact->account = (*i).second;
  }
  if (! xact->account)
    xact->account = account->find_account(name);

  // Parse the optional amount

  if (in.good() && ! in.eof()) {
    p = peek_next_nonws(in);
    if (in.eof())
      goto finished;
    if (p == ';')
      goto parse_note;

    try {
      // jww (2006-09-15): Make sure it doesn't gobble up the upcoming @ symbol

      unsigned long beg = (long)in.tellg();
      parse_amount_expr(in, journal, *xact, xact->amount,
			PARSE_VALEXPR_NO_REDUCE);
      unsigned long end = (long)in.tellg();
      xact->amount_expr = std::string(line, beg, end - beg);
    }
    catch (error * err) {
      err_desc = "While parsing transaction amount:";
      throw err;
    }
  }

  // Parse the optional cost (@ PER-UNIT-COST, @@ TOTAL-COST)

  if (in.good() && ! in.eof()) {
    p = peek_next_nonws(in);
    if (p == '@') {
      DEBUG_PRINT("ledger.textual.parse", "line " << linenum << ": " <<
		  "Found a price indicator");
      bool per_unit = true;
      in.get(p);
      if (in.peek() == '@') {
	in.get(p);
	per_unit = false;
	DEBUG_PRINT("ledger.textual.parse", "line " << linenum << ": " <<
		    "And it's for a total price");
      }

      if (in.good() && ! in.eof()) {
	xact->cost = new amount_t;

	try {
	  unsigned long beg = (long)in.tellg();

	  parse_amount_expr(in, journal, *xact, *xact->cost,
			    PARSE_VALEXPR_NO_MIGRATE);

	  unsigned long end = (long)in.tellg();

	  if (per_unit)
	    xact->cost_expr = (std::string("@") +
			       std::string(line, beg, end - beg));
	  else
	    xact->cost_expr = (std::string("@@") +
			       std::string(line, beg, end - beg));
	}
	catch (error * err) {
	  err_desc = "While parsing transaction cost:";
	  throw err;
	}

	if (*xact->cost < 0)
	  throw new parse_error("A transaction's cost may not be negative");

	amount_t per_unit_cost(*xact->cost);
	if (per_unit)
	  *xact->cost *= xact->amount;
	else
	  per_unit_cost /= xact->amount;

	if (xact->amount.commodity() &&
	    ! xact->amount.commodity().annotated)
	  xact->amount.annotate_commodity(per_unit_cost,
					  xact->entry->actual_date(),
					  xact->entry->code);

	DEBUG_PRINT("ledger.textual.parse", "line " << linenum << ": " <<
		    "Total cost is " << *xact->cost);
	DEBUG_PRINT("ledger.textual.parse", "line " << linenum << ": " <<
		    "Per-unit cost is " << per_unit_cost);
	DEBUG_PRINT("ledger.textual.parse", "line " << linenum << ": " <<
		    "Annotated amount is " << xact->amount);
      }
    }
  }

  xact->amount.reduce();
  DEBUG_PRINT("ledger.textual.parse", "line " << linenum << ": " <<
	      "Reduced amount is " << xact->amount);

  // Parse the optional note

 parse_note:
  if (in.good() && ! in.eof()) {
    p = peek_next_nonws(in);
    if (p == ';') {
      in.get(p);
      p = peek_next_nonws(in);
      xact->note = &line[in.tellg()];
      DEBUG_PRINT("ledger.textual.parse", "line " << linenum << ": " <<
		  "Parsed a note '" << xact->note << "'");

      if (char * b = std::strchr(xact->note.c_str(), '['))
	if (char * e = std::strchr(xact->note.c_str(), ']')) {
	  char buf[256];
	  std::strncpy(buf, b + 1, e - b - 1);
	  buf[e - b - 1] = '\0';

	  DEBUG_PRINT("ledger.textual.parse", "line " << linenum << ": " <<
		      "Parsed a transaction date " << buf);

	  if (char * p = std::strchr(buf, '=')) {
	    *p++ = '\0';
	    xact->_date_eff = p;
	  }
	  if (buf[0])
	    xact->_date = buf;
	}
    }
  }

 finished:
  if (! xact->entry) {
    delete static_cast<repitem_t *>(xact->data);
    xact->data = NULL;
  }
  return xact.release();

  }
  catch (error * err) {
    delete static_cast<repitem_t *>(xact->data);
    xact->data = NULL;

    err->context.push_back
      (new line_context(line, (long)in.tellg() - 1,
			! err_desc.empty() ?
			err_desc : "While parsing transaction:"));
    throw err;
  }
}

bool parse_transactions(std::istream&	   in,
			journal_t *	   journal,
			account_t *	   account,
			entry_base_t&	   entry,
			const std::string& kind,
			unsigned long      beg_pos)
{
  static char line[MAX_LINE + 1];
  bool	      added = false;

  while (! in.eof() && (in.peek() == ' ' || in.peek() == '\t')) {
    in.getline(line, MAX_LINE);
    if (in.eof())
      break;
    beg_pos += std::strlen(line) + 1;
    linenum++;

    if (line[0] == ' ' || line[0] == '\t' || line[0] == '\r') {
      char * p = skip_ws(line);
      if (! *p || *p == '\r')
	break;
    }
    if (transaction_t * xact = parse_transaction(line, journal, account)) {
      entry.add_transaction(xact);
      added = true;
    }
  }

  return added;
}

namespace {
  TIMER_DEF(parsing_total, "total parsing time");
  TIMER_DEF(entry_xacts,   "parsing transactions");
  TIMER_DEF(entry_details, "parsing entry details");
  TIMER_DEF(entry_date,    "parsing entry date");
}

entry_t * parse_entry(std::istream& in, char * line, journal_t * journal,
		      account_t * master, textual_parser_t& parser,
		      unsigned long beg_pos)
{
  std::auto_ptr<entry_t> curr(new entry_t);

  std::istringstream line_in(line);
  char c;

  // Parse the date

  TIMER_START(entry_date);

  curr->_date.parse(line_in);

  if (peek_next_nonws(line_in) == '=') {
    line_in.get(c);
    curr->_date_eff.parse(line_in);
  }

  TIMER_STOP(entry_date);

  // Parse the optional cleared flag: *

  TIMER_START(entry_details);

  transaction_t::state_t state = transaction_t::UNCLEARED;
  switch (peek_next_nonws(line_in)) {
  case '*':
    state = transaction_t::CLEARED;
    line_in.get(c);
    break;
  case '!':
    state = transaction_t::PENDING;
    line_in.get(c);
    break;
  }

  // Parse the optional code: (TEXT)

  char buf[256];

  if (peek_next_nonws(line_in) == '(') {
    line_in.get(c);
    READ_INTO(line_in, buf, 255, c, c != ')');
    curr->code = buf;
    if (c == ')')
      line_in.get(c);
    peek_next_nonws(line_in);
  }

  // Parse the payee/description text

  std::memset(buf, 0, 255);
  line_in.read(buf, 255);
  curr->payee = buf[0] != '\0' ? buf : "<Unspecified payee>";

  TIMER_STOP(entry_details);

  // Create a report item for this entry, so the transaction below may
  // refer to it

  curr->data =
    repitem_t::wrap(curr.get(), static_cast<repitem_t *>(journal->data));

  // Parse all of the transactions associated with this entry

  TIMER_START(entry_xacts);

  unsigned long end_pos;
  unsigned long beg_line = linenum;

  while (! in.eof() && (in.peek() == ' ' || in.peek() == '\t')) {
    line[0] = '\0';
    in.getline(line, MAX_LINE);
    if (in.eof() && line[0] == '\0')
      break;
    end_pos = beg_pos + std::strlen(line) + 1;
    linenum++;

    if (line[0] == ' ' || line[0] == '\t' || line[0] == '\r') {
      char * p = skip_ws(line);
      if (! *p || *p == '\r')
	break;
    }

    if (transaction_t * xact =
	parse_transaction(line, journal, master, curr.get())) {
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

  TIMER_STOP(entry_xacts);

  return curr.release();
}

template <typename T>
struct push_var {
  T& var;
  T prev;
  push_var(T& _var) : var(_var), prev(var) {}
  ~push_var() { var = prev; }
};

static inline void parse_symbol(char *& p, std::string& symbol)
{
  if (*p == '"') {
    char * q = std::strchr(p + 1, '"');
    if (! q)
      throw new parse_error("Quoted commodity symbol lacks closing quote");
    symbol = std::string(p + 1, 0, q - p - 1);
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
    throw new parse_error("Failed to parse commodity");
}

bool textual_parser_t::test(std::istream& in) const
{
  char buf[5];

  in.read(buf, 5);
  if (std::strncmp(buf, "<?xml", 5) == 0) {
#if defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)
    throw new parse_error("Ledger file contains XML data, but format was not recognized");
#else
    throw new parse_error("Ledger file contains XML data, but no XML support present");
#endif
  }

  in.clear();
  in.seekg(0, std::ios::beg);
  assert(in.good());
  return true;
}

static void clock_out_from_timelog(const datetime_t& when,
				   account_t *	     account,
				   const char *	     desc,
				   journal_t *	     journal)
{
  time_entry_t event;

  if (time_entries.size() == 1) {
    event = time_entries.back();
    time_entries.clear();
  }
  else if (time_entries.empty()) {
    throw new parse_error("Timelog check-out event without a check-in");
  }
  else if (! account) {
    throw new parse_error
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
      throw new parse_error
	("Timelog check-out event does not match any current check-ins");
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
    throw new parse_error
      ("Timelog check-out date less than corresponding check-in");

  char buf[32];
  std::sprintf(buf, "%lds", curr->_date - event.checkin);
  amount_t amt;
  amt.parse(buf);

  transaction_t * xact
    = new transaction_t(event.account, amt, TRANSACTION_VIRTUAL);
  xact->state = transaction_t::CLEARED;
  curr->add_transaction(xact);

  if (! journal->add_entry(curr.get()))
    throw new parse_error("Failed to record 'out' timelog entry");
  else
    curr.release();
}

unsigned int textual_parser_t::parse(std::istream&	 in,
				     journal_t *	 journal,
				     account_t *	 master,
				     const std::string * original_file)
{
  static bool  added_auto_entry_hook = false;
  static char  line[MAX_LINE + 1];
  char	       c;
  unsigned int count  = 0;
  unsigned int errors = 0;

  TIMER_START(parsing_total);

  std::list<account_t *> account_stack;

  auto_entry_finalizer_t auto_entry_finalizer(journal);

  if (! master && journal)
    master = journal->master;

  account_stack.push_front(master);

  path	  = journal ? journal->sources.back() : *original_file;
  src_idx = journal ? journal->sources.size() - 1 : 0;
  linenum = 1;

  unsigned long beg_pos = in.tellg();
  unsigned long end_pos;
  unsigned long beg_line = linenum;

  while (in.good() && ! in.eof()) {
    try {
      in.getline(line, MAX_LINE);
      if (in.eof())
	break;
      end_pos = beg_pos + std::strlen(line) + 1;
      linenum++;

      switch (line[0]) {
      case '\0':
      case '\r':
	break;

      case ' ':
      case '\t': {
	char * p = skip_ws(line);
	if (*p && *p != '\r')
	  throw new parse_error("Line begins with whitespace");
	break;
      }

#ifdef TIMELOG_SUPPORT
      case 'i':
      case 'I': {
	std::string date(line, 2, 19);

	char * p = skip_ws(line + 22);
	char * n = next_element(p, true);

	time_entry_t event;
	event.desc    = n ? n : "";
	event.checkin = date;
	event.account = account_stack.front()->find_account(p);

	if (! time_entries.empty())
	  for (std::list<time_entry_t>::iterator i = time_entries.begin();
	       i != time_entries.end();
	       i++)
	    if (event.account == (*i).account)
	      throw new parse_error
		("Cannot double check-in to the same account");

	time_entries.push_back(event);
	break;
      }

      case 'o':
      case 'O':
	if (time_entries.empty()) {
	  throw new parse_error("Timelog check-out event without a check-in");
	} else {
	  std::string date(line, 2, 19);

	  char * p = skip_ws(line + 22);
	  char * n = next_element(p, true);

	  clock_out_from_timelog
	    (date, p ? account_stack.front()->find_account(p) : NULL, n,
	     journal);
	  count++;
	}
	break;
#endif // TIMELOG_SUPPORT

      case 'D':	{		// a default commodity for "entry"
	amount_t amt(skip_ws(line + 1));
	commodity_t::default_commodity = &amt.commodity();
	break;
      }

      case 'A':		        // a default account for unbalanced xacts
	journal->basket =
	  account_stack.front()->find_account(skip_ws(line + 1));
	break;

      case 'C':			// a set of conversions
	if (char * p = std::strchr(line + 1, '=')) {
	  *p++ = '\0';
	  parse_conversion(line + 1, p);
	}
	break;

      case 'P': {		// a pricing entry
	char * date_field_ptr = skip_ws(line + 1);
	char * time_field_ptr = next_element(date_field_ptr);
	if (! time_field_ptr) break;
	std::string date_field = date_field_ptr;

	char *     symbol_and_price;
	datetime_t datetime;

	if (std::isdigit(time_field_ptr[0])) {
	  symbol_and_price = next_element(time_field_ptr);
	  if (! symbol_and_price) break;
	  datetime = date_field + " " + time_field_ptr;
	} else {
	  symbol_and_price = time_field_ptr;
	  datetime = date_t(date_field);
	}

	std::string symbol;
	parse_symbol(symbol_and_price, symbol);
	amount_t price(symbol_and_price);

	if (commodity_t * commodity = commodity_t::find_or_create(symbol))
	  commodity->add_price(datetime, price);
	break;
      }

      case 'N': {			// don't download prices
	char * p = skip_ws(line + 1);
	std::string symbol;
	parse_symbol(p, symbol);

	if (commodity_t * commodity = commodity_t::find_or_create(symbol))
	  commodity->add_flags(COMMODITY_STYLE_NOMARKET);
	break;
      }

      case 'Y':			// set current year
	date_t::current_year = std::atoi(skip_ws(line + 1)) - 1900;
	break;

#ifdef TIMELOG_SUPPORT
      case 'h':
      case 'b':
#endif
      case ';':			// comment
	break;

      case '-':			// option setting
	throw new parse_error("Option settings are not allowed in journal files");

      case '=': {		// automated entry
	if (! added_auto_entry_hook) {
	  journal->add_entry_finalizer(&auto_entry_finalizer);
	  added_auto_entry_hook = true;
	}

	auto_entry_t * ae = new auto_entry_t(skip_ws(line + 1));
	if (parse_transactions(in, journal, account_stack.front(), *ae,
			       "automated", end_pos)) {
	  journal->auto_entries.push_back(ae);
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
	  throw new parse_error(std::string("Parsing time period '") + line + "'");

	if (parse_transactions(in, journal, account_stack.front(), *pe,
			       "period", end_pos)) {
	  if (pe->finalize()) {
	    extend_entry_base(journal, *pe, true);
	    journal->period_entries.push_back(pe);
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
	std::string word(line + 1);
	if (word == "include") {
	  push_var<std::string>	  save_path(path);
	  push_var<unsigned int>  save_src_idx(src_idx);
	  push_var<unsigned long> save_beg_pos(beg_pos);
	  push_var<unsigned long> save_end_pos(end_pos);
	  push_var<unsigned int>  save_linenum(linenum);

	  path = p;
	  if (path[0] != '/' && path[0] != '\\' && path[0] != '~') {
	    std::string::size_type pos = save_path.prev.rfind('/');
	    if (pos == std::string::npos)
	      pos = save_path.prev.rfind('\\');
	    if (pos != std::string::npos)
	      path = std::string(save_path.prev, 0, pos + 1) + path;
	  }
	  path = resolve_path(path);

	  DEBUG_PRINT("ledger.textual.include", "line " << linenum << ": " <<
		      "Including path '" << path << "'");

	  include_stack.push_back(std::pair<std::string, int>
				  (journal->sources.back(), linenum - 1));
	  count += journal->session->read_journal(path, journal,
						  account_stack.front());
	  include_stack.pop_back();
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
	    // `account_aliases' map, which is used by the transaction
	    // parser to resolve alias references.
	    account_t * acct = account_stack.front()->find_account(e);
	    std::pair<accounts_map::iterator, bool> result
	      = account_aliases.insert(accounts_pair(b, acct));
	    assert(result.second);
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
	unsigned int first_line = linenum;
	unsigned long pos = end_pos;
	if (entry_t * entry = parse_entry(in, line, journal,
					  account_stack.front(),
					  *this, pos)) {
	  if (journal->add_entry(entry)) {
	    entry->src_idx  = src_idx;
	    entry->beg_pos  = beg_pos;
	    entry->beg_line = beg_line;
	    entry->end_pos  = end_pos;
	    entry->end_line = linenum;
	    count++;
	  } else {
	    delete entry;
	    throw new parse_error("Entry does not balance");
	  }
	} else {
	  throw new parse_error("Failed to parse entry");
	}
	end_pos = pos;
	break;
      }
      }
    }
    catch (error * err) {
      for (std::list<std::pair<std::string, int> >::reverse_iterator i =
	     include_stack.rbegin();
	   i != include_stack.rend();
	   i++)
	err->context.push_back(new include_context((*i).first, (*i).second,
						    "In file included from"));
      err->context.push_front(new file_context(path, linenum - 1));

      std::cout.flush();
      if (errors > 0 && err->context.size() > 1)
	std::cerr << std::endl;
      err->reveal_context(std::cerr, "Error");
      std::cerr << err->what() << std::endl;
      delete err;
      errors++;
    }
    beg_pos = end_pos;
  }

 done:
  if (! time_entries.empty()) {
    for (std::list<time_entry_t>::iterator i = time_entries.begin();
	 i != time_entries.end();
	 i++)
      clock_out_from_timelog(datetime_t::now, (*i).account, NULL, journal);
    time_entries.clear();
  }

  if (added_auto_entry_hook)
    journal->remove_entry_finalizer(&auto_entry_finalizer);

  if (errors > 0)
    throw (int)errors;

  TIMER_STOP(parsing_total);

  return count;
}

} // namespace ledger
