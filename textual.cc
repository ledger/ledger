#include "journal.h"
#include "textual.h"
#include "datetime.h"
#include "autoxact.h"
#include "valexpr.h"
#include "error.h"
#include "option.h"
#include "config.h"
#include "timing.h"
#include "util.h"
#ifdef USE_BOOST_PYTHON
#include "python.h"
#endif

#include <fstream>
#include <sstream>
#include <cstring>
#include <ctime>
#include <cctype>

#define TIMELOG_SUPPORT 1

namespace ledger {

#define MAX_LINE 1024

static std::string  path;
static unsigned int linenum;

#ifdef TIMELOG_SUPPORT
static std::time_t time_in;
static account_t * last_account;
static std::string last_desc;
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

transaction_t * parse_transaction_text(char * line, account_t * account,
				       entry_t * entry)
{
  // The account will be determined later...

  std::auto_ptr<transaction_t> xact(new transaction_t(NULL));

  // The call to `next_element' will skip past the account name,
  // and return a pointer to the beginning of the amount.  Once
  // we know where the amount is, we can strip off any
  // transaction note, and parse it.

  char * p = skip_ws(line);
  if (char * cost_str = next_element(p, true)) {
    if (char * note_str = std::strchr(cost_str, ';')) {
      *note_str++ = '\0';
      xact->note = skip_ws(note_str);
    }

    char * price_str = std::strchr(cost_str, '@');
    bool   per_unit  = true;
    if (price_str) {
      *price_str++ = '\0';
      if (*price_str == '@') {
	per_unit = false;
	price_str++;
      }
      xact->cost = new amount_t;
      xact->cost->parse(price_str);
    }

    xact->amount.parse(cost_str);
    if (price_str && per_unit)
      *xact->cost *= xact->amount;
  }

  if (*p == '[' || *p == '(') {
    xact->flags |= TRANSACTION_VIRTUAL;
    if (*p == '[')
      xact->flags |= TRANSACTION_BALANCE;
    p++;

    char * e = p + (std::strlen(p) - 1);
    assert(*e == ')' || *e == ']');
    *e = '\0';
  }

  xact->account = account->find_account(p);

  return xact.release();
}

transaction_t * parse_transaction(std::istream& in, account_t * account,
				  entry_t * entry)
{
  static char line[MAX_LINE + 1];
  in.getline(line, MAX_LINE);
  linenum++;

  // Skip a possible blank line
  if (*skip_ws(line) == '\0')
    return NULL;

  return parse_transaction_text(line, account, entry);
}

void parse_automated_transactions(std::istream& in,
				  account_t *	account,
				  automated_transactions_t& auto_xacts)
{
  static char line[MAX_LINE + 1];
  in.getline(line, MAX_LINE);
  linenum++;

  transactions_deque xacts;

  while (! in.eof() && (in.peek() == ' ' || in.peek() == '\t'))
    if (transaction_t * xact = parse_transaction(in, account, NULL)) {
      if (! xact->amount)
	throw parse_error(path, linenum,
			  "All automated transactions must have values");
      else
	xacts.push_back(xact);
    }

  if (! xacts.empty())
    auto_xacts.
      add_automated_transaction(new automated_transaction_t(line + 1, xacts));
}

namespace {
  TIMER_DEF(entry_finish,  "finalizing entry");
  TIMER_DEF(entry_xacts,   "parsing transactions");
  TIMER_DEF(entry_details, "parsing entry details");
  TIMER_DEF(entry_date,    "parsing entry date");
}

entry_t * parse_entry(std::istream& in, account_t * master,
		      textual_parser_t& parser)
{
  std::auto_ptr<entry_t> curr(new entry_t);

  static char line[MAX_LINE + 1];
  in.getline(line, MAX_LINE);
  linenum++;

  // Parse the date

  TIMER_START(entry_date);

  char * next = next_element(line);

  if (! quick_parse_date(line, &curr->date))
    throw parse_error(path, linenum, "Failed to parse date");

  TIMER_STOP(entry_date);

  // Parse the optional cleared flag: *

  TIMER_START(entry_details);

  if (*next == '*') {
    curr->state = entry_t::CLEARED;
    next = skip_ws(++next);
  }

  // Parse the optional code: (TEXT)

  if (*next == '(') {
    if (char * p = std::strchr(next++, ')')) {
      *p++ = '\0';
      curr->code = next;
      next = skip_ws(p);
    }
  }

  // Parse the description text

  curr->payee = next;

  TIMER_STOP(entry_details);

  // Parse all of the transactions associated with this entry

  TIMER_START(entry_xacts);

  while (! in.eof() && (in.peek() == ' ' || in.peek() == '\t'))
    if (transaction_t * xact = parse_transaction(in, master, curr.get()))
      curr->add_transaction(xact);

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

unsigned int textual_parser_t::parse(std::istream&	 in,
				     journal_t *	 journal,
				     account_t *	 master,
				     const std::string * original_file)
{
  static bool   added_autoxact_hook = false;
  static char   line[MAX_LINE + 1];
  char		c;
  unsigned int  count = 0;
  unsigned int  errors = 0;
  commodity_t *	time_commodity = NULL;

  std::deque<account_t *>  account_stack;
  automated_transactions_t auto_xacts;

  current_auto_xacts = &auto_xacts;

  if (! master)
    master = journal->master;

  account_stack.push_front(master);

  path	  = journal->sources.back();
  linenum = 1;

  while (! in.eof()) {
    try {
      switch (in.peek()) {
      case -1:                    // end of file
	goto done;

      case ' ':
      case '\t':
	if (peek_next_nonws(in) != '\n') {
	  in.getline(line, MAX_LINE);
	  linenum++;
	  throw parse_error(path, linenum, "Line begins with whitespace");
	}
	// fall through...

      case '\n':
	linenum++;
      case '\r':                  // skip blank lines
	in.get(c);
	break;

#ifdef TIMELOG_SUPPORT
      case 'i':
      case 'I': {
	std::string date, time;

	in >> c;
	in >> date;
	in >> time;
	date += " ";
	date += time;

	in.getline(line, MAX_LINE);
	linenum++;

	char * p = skip_ws(line);
	char * n = next_element(p, true);
	last_desc = n ? n : "";

	struct std::tm when;
	if (strptime(date.c_str(), "%Y/%m/%d %H:%M:%S", &when)) {
	  time_in      = std::mktime(&when);
	  last_account = account_stack.front()->find_account(p);
	} else {
	  last_account = NULL;
	  throw parse_error(path, linenum, "Cannot parse timelog entry date");
	}
	break;
      }

      case 'o':
      case 'O':
	if (last_account) {
	  std::string date, time;

	  in >> c;
	  in >> date;
	  in >> time;
	  date += " ";
	  date += time;

	  in.getline(line, MAX_LINE);
	  linenum++;

	  struct std::tm when;
	  if (strptime(date.c_str(), "%Y/%m/%d %H:%M:%S", &when)) {
	    std::auto_ptr<entry_t> curr(new entry_t);
	    curr->date  = std::mktime(&when);
	    curr->state = entry_t::CLEARED;
	    curr->code  = "";
	    curr->payee = last_desc;

	    double diff = std::difftime(curr->date, time_in) / 60.0 / 60.0;
	    char   buf[32];
	    std::sprintf(buf, "%fh", diff);
	    amount_t amt;
	    amt.parse(buf);
	    time_commodity = &amt.commodity();

	    transaction_t * xact
	      = new transaction_t(last_account, amt, TRANSACTION_VIRTUAL);
	    curr->add_transaction(xact);

	    if (! journal->add_entry(curr.release()))
	      throw parse_error(path, linenum,
				"Failed to record 'out' timelog entry");

	    count++;
	  } else {
	    throw parse_error(path, linenum, "Cannot parse timelog entry date");
	  }

	  last_account = NULL;
	} else {
	  in.getline(line, MAX_LINE);
	  linenum++;
	}
	break;
#endif // TIMELOG_SUPPORT

      case 'P': {			// a pricing entry
	in >> c;

	std::time_t date;
	std::string symbol;

	in >> line;		// the date
	if (! quick_parse_date(line, &date))
	  throw parse_error(path, linenum, "Failed to parse date");

	int hour, min, sec;

	in >> hour;		// the time
	in >> c;
	in >> min;
	in >> c;
	in >> sec;

	date = std::time_t(((unsigned long) date) +
			   hour * 3600 + min * 60 + sec);

	parse_commodity(in, symbol);

	in.getline(line, MAX_LINE);
	linenum++;

	amount_t price;
	price.parse(skip_ws(line));

	commodity_t * commodity = commodity_t::find_commodity(symbol, true);
	commodity->add_price(date, price);
	break;
      }

      case 'N': {			// don't download prices
	std::string symbol;

	in >> c;
	parse_commodity(in, symbol);

	commodity_t * commodity = commodity_t::find_commodity(symbol, true);
	commodity->flags |= COMMODITY_STYLE_NOMARKET;
	break;
      }

      case 'C': {			// a flat conversion
	in >> c;

	std::string symbol;
	amount_t    price;

	parse_commodity(in, symbol);

	in.getline(line, MAX_LINE);
	linenum++;
	price.parse(skip_ws(line));

	commodity_t * commodity = commodity_t::find_commodity(symbol, true);
	commodity->conversion = price;
	break;
      }

      case 'Y':                   // set the current year
	in >> c;
	in >> now_year;
	now_year -= 1900;
	break;

#ifdef TIMELOG_SUPPORT
      case 'h':
      case 'b':
#endif
      case ';':                   // a comment line
	in.getline(line, MAX_LINE);
	linenum++;
	break;

      case '-': {                 // option setting
	std::string opt;
	in >> c >> c;
	in >> opt;
	in.getline(line, MAX_LINE);
	linenum++;
	char * p = skip_ws(line);
	process_option(config_options, opt, *p == '\n' ? NULL : p);
	break;
      }

      case '=':                   // automated transactions
	if (! added_autoxact_hook) {
	  add_hook(journal->entry_finalize_hooks, handle_auto_xacts);
	  added_autoxact_hook = true;
	}
	parse_automated_transactions(in, account_stack.front(), auto_xacts);
	break;

      case '!': {                 // directive
	std::string word;
	in.get(c);
	in >> word;
	if (word == "include") {
	  in.getline(line, MAX_LINE);
	  linenum++;

	  push_var<unsigned int> save_linenum(linenum);
	  push_var<std::string>  save_path(path);
	  push_var<automated_transactions_t *>
	    save_current_auto_xacts(current_auto_xacts);

	  count += parse_journal_file(skip_ws(line), journal,
				      account_stack.front());
	}
	else if (word == "account") {
	  in.getline(line, MAX_LINE);
	  linenum++;

	  account_t * acct;
	  acct = account_stack.front()->find_account(skip_ws(line));
	  account_stack.push_front(acct);
	}
	else if (word == "end") {
	  account_stack.pop_front();
	}
#ifdef USE_BOOST_PYTHON
	else if (word == "python") {
	  in.getline(line, MAX_LINE);
	  python_eval(in);
	}
#endif
	break;
      }

      default: {
	unsigned int first_line = linenum;
	if (entry_t * entry = parse_entry(in, account_stack.front(), *this)) {
	  if (journal->add_entry(entry))
	    count++;
	  else
	    throw parse_error(path, first_line, "Entry does not balance");
	} else {
	  throw parse_error(path, first_line, "Failed to parse entry");
	}
	break;
      }
      }
    }
    catch (const parse_error& err) {
      std::cerr << "Error: " << err.what() << std::endl;
      errors++;
    }
  }

 done:
  if (time_commodity) {
    time_commodity->precision = 2;
    time_commodity->flags |= COMMODITY_STYLE_NOMARKET;
  }

  if (errors > 0)
    throw error(std::string("Errors parsing file '") + path + "'");

  return count;
}

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>

using namespace boost::python;
using namespace ledger;

BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS(textual_parse_overloads,
				       textual_parser_t::parse, 2, 4)

void export_textual() {
  class_< textual_parser_t, bases<parser_t> > ("TextualParser")
    .def("test", &textual_parser_t::test)
    .def("parse", &textual_parser_t::parse, textual_parse_overloads())
    ;
}

#endif // USE_BOOST_PYTHON
