#include "ledger.h"
#include "constraint.h"
#include "textual.h"

#include <vector>
#include <fstream>
#include <sstream>
#include <cstring>
#include <ctime>
#include <cctype>

#define TIMELOG_SUPPORT 1

namespace ledger {

#if 0
static const std::string entry1_fmt = "%?10d %p";
static const std::string entryn_fmt = "    %-30a  %15t";
#endif

#define MAX_LINE 1024

std::string             path;
unsigned int		linenum;

#ifdef TIMELOG_SUPPORT
static std::time_t	time_in;
static account_t *	last_account;
static std::string	last_desc;
#endif

static std::time_t	now	  = std::time(NULL);
static struct std::tm * now_tm	  = std::localtime(&now);

static std::time_t	base      = -1;
static int		base_year = -1;

static const int	month_days[12] = {
  31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
};

static const char *	formats[] = {
  "%Y/%m/%d",
  "%m/%d",
  "%Y.%m.%d",
  "%m.%d",
  "%Y-%m-%d",
  "%m-%d",
  "%a",
  "%A",
  "%b",
  "%B",
  "%Y",
  NULL
};

inline char * skip_ws(char * ptr)
{
  while (*ptr == ' ' || *ptr == '\t' || *ptr == '\n')
    ptr++;
  return ptr;
}

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

bool parse_date_mask(const char * date_str, struct std::tm * result)
{
  for (const char ** f = formats; *f; f++) {
    memset(result, INT_MAX, sizeof(struct std::tm));
    if (strptime(date_str, *f, result))
      return true;
  }
  return false;
}

bool parse_date(const char * date_str, std::time_t * result, const int year)
{
  struct std::tm when;

  if (! parse_date_mask(date_str, &when))
    return false;

  when.tm_hour = 0;
  when.tm_min  = 0;
  when.tm_sec  = 0;

  if (when.tm_year == -1)
    when.tm_year = ((year == -1) ? now_tm->tm_year : (year - 1900));

  if (when.tm_mon == -1)
    when.tm_mon = 0;

  if (when.tm_mday == -1)
    when.tm_mday = 1;

  *result = std::mktime(&when);

  return true;
}

static bool quick_parse_date(char * date_str, std::time_t * result)
{
  int year = -1, month = -1, day, num = 0;

  for (char * p = date_str; *p; p++) {
    if (*p == '/' || *p == '-' || *p == '.') {
      if (year == -1)
	year = num;
      else
	month = num;
      num = 0;
    }
    else if (*p < '0' || *p > '9') {
      return false;
    }
    else {
      num *= 10;
      num += *p - '0';
    }
  }

  day = num;

  if (month == -1) {
    month = year;
    year  = -1;
  }

  if (base == -1 || year != base_year) {
    struct std::tm when;

    when.tm_hour = 0;
    when.tm_min  = 0;
    when.tm_sec  = 0;

    base_year    = year == -1 ? now_tm->tm_year + 1900 : year;
    when.tm_year = year == -1 ? now_tm->tm_year : year - 1900;
    when.tm_mon  = 0;
    when.tm_mday = 1;

    base = std::mktime(&when);
  }

  *result = base;

  --month;
  while (--month >= 0) {
    *result += month_days[month] * 24 * 60 * 60;
    if (month == 1 && year % 4 == 0 && year != 2000) // february in leap years
      *result += 24 * 60 * 60;
  }

  if (--day)
    *result += day * 24 * 60 * 60;

  return true;
}

inline char peek_next_nonws(std::istream& in)
{
  char c = in.peek();
  while (! in.eof() && std::isspace(c) && c != '\n') {
    in.get(c);
    c = in.peek();
  }
  return c;
}

transaction_t * parse_transaction_text(char * line, ledger_t * ledger,
				       account_t * account, entry_t * entry)
{
  // The account will be determined later...

  transaction_t * xact = new transaction_t(entry, NULL);

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
    if (price_str) {
      *price_str++ = '\0';
      xact->cost.parse(price_str, ledger);
    }

    xact->amount.parse(cost_str, ledger);

    if (price_str)
      xact->cost *= xact->amount;
    else
      xact->cost = xact->amount;
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

  if (! xact->amount.commodity)
    xact->amount.commodity = ledger->find_commodity("", true);
  if (! xact->cost.commodity)
    xact->cost.commodity = ledger->find_commodity("", true);

  return xact;
}

transaction_t * parse_transaction(std::istream& in, ledger_t * ledger,
				  account_t * account, entry_t * entry)
{
  static char line[MAX_LINE + 1];
  in.getline(line, MAX_LINE);
  linenum++;

  return parse_transaction_text(line, ledger, account, entry);
}

class automated_transaction_t
{
public:
  masks_list        masks;
  transactions_list transactions;

  automated_transaction_t(masks_list& _masks,
			  transactions_list& _transactions) {
    masks.insert(masks.begin(), _masks.begin(), _masks.end());
    transactions.insert(transactions.begin(),
			_transactions.begin(), _transactions.end());
    // Take over ownership of the pointers
    _transactions.clear();
  }

  ~automated_transaction_t() {
    for (transactions_list::iterator i = transactions.begin();
	 i != transactions.end();
	 i++)
      delete *i;
  }

  void extend_entry(entry_t * entry);
};

typedef std::vector<automated_transaction_t *>
  automated_transactions_vector;

void automated_transaction_t::extend_entry(entry_t * entry)
{
  for (transactions_list::iterator i = entry->transactions.begin();
       i != entry->transactions.end();
       i++)
    if (matches(masks, *((*i)->account))) {
      for (transactions_list::iterator t = transactions.begin();
	   t != transactions.end();
	   t++) {
	amount_t amt;
	if ((*t)->amount.commodity->symbol.empty())
	  amt = (*i)->amount * (*t)->amount;
	else
	  amt = (*t)->amount;

	transaction_t * xact
	  = new transaction_t(entry, (*t)->account, amt, amt, (*t)->flags);
	entry->add_transaction(xact);
      }
    }
}

class automated_transactions_t
{
public:
  automated_transactions_vector automated_transactions;

  ~automated_transactions_t() {
    for (automated_transactions_vector::iterator i
	   = automated_transactions.begin();
	 i != automated_transactions.end();
	 i++)
      delete *i;
  }

  void extend_entry(entry_t * entry) {
    for (automated_transactions_vector::iterator i
	   = automated_transactions.begin();
	 i != automated_transactions.end();
	 i++)
      (*i)->extend_entry(entry);
  }

  void add_automated_transaction(automated_transaction_t * auto_xact) {
    automated_transactions.push_back(auto_xact);
  }
  bool remove_automated_transaction(automated_transaction_t * auto_xact) {
    for (automated_transactions_vector::iterator i
	   = automated_transactions.begin();
	 i != automated_transactions.end();
	 i++) {
      if (*i == auto_xact) {
	automated_transactions.erase(i);
	return true;
      }
    }
    return false;
  }
};

void parse_automated_transactions(std::istream& in, ledger_t * ledger,
				  account_t * account,
				  automated_transactions_t& auto_xacts)
{
  static char line[MAX_LINE + 1];

  masks_list masks;

  while (! in.eof() && in.peek() == '=') {
    in.getline(line, MAX_LINE);
    linenum++;

    char * p = line + 1;
    p = skip_ws(p);

    masks.push_back(mask_t(p));
  }

  transactions_list xacts;

  while (! in.eof() && (in.peek() == ' ' || in.peek() == '\t')) {
    if (transaction_t * xact = parse_transaction(in, ledger, account, NULL)) {
      if (! xact->amount) {
	std::cerr << "Error in " << path << ", line " << (linenum - 1)
		  << ": All automated transactions must have a value."
		  << std::endl;
      } else {
	xacts.push_back(xact);
      }
    }
  }

  if (! masks.empty() && ! xacts.empty()) {
    automated_transaction_t * auto_xact
      = new automated_transaction_t(masks, xacts);
    auto_xacts.add_automated_transaction(auto_xact);
  }
}

bool finalize_entry(entry_t * entry)
{
  // Scan through and compute the total balance for the entry.  This
  // is used for auto-calculating the value of entries with no cost,
  // and the per-unit price of unpriced commodities.

  balance_t balance;

  for (transactions_list::const_iterator x = entry->transactions.begin();
       x != entry->transactions.end();
       x++)
    if (! ((*x)->flags & TRANSACTION_VIRTUAL) ||
	((*x)->flags & TRANSACTION_BALANCE))
      balance += (*x)->cost;

  // If one transaction of a two-line transaction is of a different
  // commodity than the others, and it has no per-unit price,
  // determine its price by dividing the unit count into the value of
  // the balance.  This is done for the last eligible commodity.

  if (! balance.amounts.empty() && balance.amounts.size() == 2)
    for (transactions_list::const_iterator x = entry->transactions.begin();
	 x != entry->transactions.end();
	 x++) {
      if ((*x)->cost != (*x)->amount || ((*x)->flags & TRANSACTION_VIRTUAL))
	continue;

      for (amounts_map::const_iterator i = balance.amounts.begin();
	   i != balance.amounts.end();
	   i++)
	if ((*i).second.commodity != (*x)->amount.commodity) {
	  assert((*x)->amount);
	  balance -= (*x)->cost;
	  (*x)->cost = - (*i).second;
	  balance += (*x)->cost;
	  break;
	}

      break;
    }

  // Walk through each of the transactions, fixing up any that we
  // can, and performing any on-the-fly calculations.

  bool empty_allowed = true;

  for (transactions_list::const_iterator x = entry->transactions.begin();
       x != entry->transactions.end();
       x++) {
    if ((*x)->amount || ((*x)->flags & TRANSACTION_VIRTUAL))
      continue;

    if (! empty_allowed || balance.amounts.empty() ||
	balance.amounts.size() != 1)
      return false;

    empty_allowed = false;

    // If one transaction gives no value at all -- and all the
    // rest are of the same commodity -- then its value is the
    // inverse of the computed value of the others.

    amounts_map::const_iterator i = balance.amounts.begin();
    (*x)->amount = (*x)->cost = - balance.amount((*i).first);

    balance = 0;
  }

  return ! balance;
}

entry_t * parse_entry(std::istream& in, ledger_t * ledger,
		      account_t * master)
{
  entry_t * curr = new entry_t;

  static char line[MAX_LINE + 1];
  in.getline(line, MAX_LINE);
  linenum++;

  // Parse the date

  char * next = next_element(line);

  if (! quick_parse_date(line, &curr->date)) {
    std::cerr << "Error in " << path << ", line " << (linenum - 1)
	      << ": Failed to parse date: " << line << std::endl;
    return NULL;
  }

  // Parse the optional cleared flag: *

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

  // Parse all of the transactions associated with this entry

  while (! in.eof() && (in.peek() == ' ' || in.peek() == '\t'))
    if (transaction_t * xact = parse_transaction(in, ledger, master, curr))
      curr->add_transaction(xact);

  // If there were no transactions, throw away the entry

  if (curr->transactions.empty() || ! finalize_entry(curr)) {
    delete curr;
    return NULL;
  }

  return curr;
}

//////////////////////////////////////////////////////////////////////
//
// Textual ledger parser
//

unsigned int parse_textual_ledger(std::istream& in, ledger_t *& ledger,
				  account_t * master)
{
  static char   line[MAX_LINE + 1];
  char		c;
  int		count = 0;
  commodity_t *	time_commodity = NULL;

  std::list<account_t *>   account_stack;
  automated_transactions_t auto_xacts;

  if (! ledger)
    ledger = new ledger_t;

  if (! master)
    master = ledger->master;

  account_stack.push_front(master);

  path	  = ledger->sources.back();
  linenum = 1;

  while (! in.eof()) {
    switch (in.peek()) {
    case -1:                    // end of file
      goto done;

    case ' ':
    case '\t':
      if (peek_next_nonws(in) != '\n') {
	std::cerr << "Error in " << path << ", line " << (linenum - 1)
		  << ": Ignoring entry beginning with whitespace."
		  << std::endl;
	in.getline(line, MAX_LINE);
	linenum++;
	break;
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

      static struct std::tm when;
      if (strptime(date.c_str(), "%Y/%m/%d %H:%M:%S", &when)) {
	time_in      = std::mktime(&when);
	last_account = account_stack.front()->find_account(p);
      } else {
	std::cerr << "Error in " << path << ", line " << (linenum - 1)
		  << ": Cannot parse timelog entry date." << std::endl;
	last_account = NULL;
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

	in.getline(line, MAX_LINE);
	linenum++;

	date += " ";
	date += time;

	static struct std::tm when;
	if (strptime(date.c_str(), "%Y/%m/%d %H:%M:%S", &when)) {
	  entry_t * curr = new entry_t;
	  curr->date  = std::mktime(&when);
	  curr->state = entry_t::CLEARED;
	  curr->code  = "";
	  curr->payee = last_desc;

	  double diff = std::difftime(curr->date, time_in) / 60.0 / 60.0;
	  char   buf[32];
	  std::sprintf(buf, "%fh", diff);
	  amount_t amt;
	  amt.parse(buf, ledger);
	  time_commodity = amt.commodity;

	  transaction_t * xact = new transaction_t(curr, last_account, amt, amt,
						   TRANSACTION_VIRTUAL);
	  curr->add_transaction(xact);

	  if (! finalize_entry(curr) || ! ledger->add_entry(curr))
	    assert(0);

	  count++;
	} else {
	  std::cerr << "Error in " << path << ", line " << (linenum - 1)
		    << ": Cannot parse timelog entry date." << std::endl;
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
      if (! quick_parse_date(line, &date)) {
	std::cerr << "Error in " << path << ", line " << (linenum - 1)
		  << ": Failed to parse date: " << line << std::endl;
	break;
      }

      int hour, min, sec;

      in >> hour;		// the time
      in >> c;
      in >> min;
      in >> c;
      in >> sec;

      date = std::time_t(((unsigned long) date) +
			 hour * 3600 + min * 60 + sec);

      amount_t price;

      parse_commodity(in, symbol);
      in >> line;		// the price
      price.parse(line, ledger);

      commodity_t * commodity = ledger->find_commodity(symbol, true);
      commodity->add_price(date, price);
      break;
    }

    case 'N': {			// don't download prices
      std::string symbol;

      in >> c;
      parse_commodity(in, symbol);

      commodity_t * commodity = ledger->find_commodity(line, true);
      commodity->flags |= (COMMODITY_STYLE_CONSULTED |
			   COMMODITY_STYLE_NOMARKET);
      break;
    }

    case 'C': {			// a flat conversion
      in >> c;

      std::string symbol;
      amount_t    price;

      parse_commodity(in, symbol);
      in >> line;		// the price
      price.parse(line, ledger);

      commodity_t * commodity = ledger->find_commodity(symbol, true);
      commodity->set_conversion(price);
      break;
    }

    case 'Y':                   // set the current year
      in >> c;
      in >> now_tm->tm_year;
      now_tm->tm_year -= 1900;
      break;

#ifdef TIMELOG_SUPPORT
    case 'h':
    case 'b':
#endif
    case ';':                   // a comment line
      in.getline(line, MAX_LINE);
      linenum++;
      break;

    case '=':                   // automated transactions
      parse_automated_transactions(in, ledger, account_stack.front(),
				   auto_xacts);
      break;

    case '@': {                 // account specific
      in >> c;
      if (in.peek() == '@') {
	in.get(c);
	account_stack.pop_front();
	break;
      }

      in.getline(line, MAX_LINE);
      linenum++;

      account_t * acct = account_stack.front()->find_account(skip_ws(line));
      account_stack.push_front(acct);
      break;
    }

    case '!':                   // directive
      in >> line;
      if (std::string(line) == "!include") {
	in.getline(line, MAX_LINE);
	linenum++;

	char * path = skip_ws(line);
	std::ifstream stream(path);

	ledger->sources.push_back(path);

	unsigned int curr_linenum = linenum;
	count += parse_textual_ledger(stream, ledger, account_stack.front());
	linenum = curr_linenum;
      }
      break;

    default: {
      unsigned int first_line = linenum;
      if (entry_t * entry = parse_entry(in, ledger, account_stack.front())) {
	if (! auto_xacts.automated_transactions.empty())
	  auto_xacts.extend_entry(entry);

	if (ledger->add_entry(entry))
	  count++;
	else
	  std::cerr << "Error in " << path << ", line " << first_line
		    << ": Entry does not balance." << std::endl;
      } else {
	std::cerr << "Error in " << path << ", line " << first_line
		  << ": Failed to parse entry." << std::endl;
      }
      break;
    }
    }
  }

 done:
  if (time_commodity) {
    time_commodity->precision = 2;
    time_commodity->flags |= (COMMODITY_STYLE_CONSULTED |
			      COMMODITY_STYLE_NOMARKET);
  }

  return count;
}

//////////////////////////////////////////////////////////////////////
//
// Textual ledger printing code
//

void print_transaction(std::ostream& out, transaction_t * xact,
		       bool display_amount, bool display_cost)
{
  std::ostringstream s;
  s << *(xact->account);
  std::string acct_name = s.str();

  if (xact->flags & TRANSACTION_VIRTUAL) {
    if (xact->flags & TRANSACTION_BALANCE)
      acct_name = std::string("[") + acct_name + "]";
    else
      acct_name = std::string("(") + acct_name + ")";
  }

  out.width(30);
  out.fill(' ');
  out << std::left << acct_name;

  if (xact->amount && display_amount) {
    out << "  ";
    out.width(12);
    out.fill(' ');
    std::ostringstream s;
    s << xact->amount;
    out << std::right << s.str();
  }

  if (xact->amount && display_cost &&
      xact->amount != xact->cost) {
    out << " @ ";
    out << xact->cost / xact->amount;
  }

  if (! xact->note.empty())
    out << "  ; " << xact->note;

  out << std::endl;
}

void print_textual_entry(std::ostream& out, entry_t * entry, bool shortcut)
{
  char buf[32];
  std::strftime(buf, 31, "%Y/%m/%d ", std::gmtime(&entry->date));
  out << buf;

  if (entry->state == entry_t::CLEARED)
    out << "* ";
  if (! entry->code.empty())
    out << '(' << entry->code << ") ";
  if (! entry->payee.empty())
    out << entry->payee;

  out << std::endl;

  const commodity_t * comm = NULL;
  int size = 0;

  for (transactions_list::const_iterator x
	 = entry->transactions.begin();
       x != entry->transactions.end();
       x++) {
    if ((*x)->flags & TRANSACTION_VIRTUAL &&
	! ((*x)->flags & TRANSACTION_BALANCE))
      continue;

    if (! comm)
      comm = (*x)->amount.commodity;
    else if (comm != (*x)->amount.commodity)
      shortcut = false;

    size++;
  }

  if (shortcut && size != 2)
    shortcut = false;

  for (transactions_list::const_iterator x
	 = entry->transactions.begin();
       x != entry->transactions.end();
       x++) {
    out << "    ";
    print_transaction(out, *x,
		      (! shortcut || x == entry->transactions.begin() ||
		       ((*x)->flags & TRANSACTION_VIRTUAL &&
			! ((*x)->flags & TRANSACTION_BALANCE))),
		      size != 2);
  }

  out << std::endl;
}

void print_textual_ledger(std::ostream& out, ledger_t * ledger,
			  bool shortcut)
{
  for (entries_list::const_iterator i = ledger->entries.begin();
       i != ledger->entries.end();
       i++)
    print_textual_entry(out, *i, shortcut);
}

} // namespace ledger

#ifdef PARSE_TEST

int main(int argc, char *argv[])
{
  book.sources.push_back(argv[1]);
  std::ifstream stream(argv[1]);
  ledger::ledger_t book;
  int count = parse_textual_ledger(stream, &book, book.master);
  std::cout << "Read " << count << " entries." << std::endl;
  print_textual_ledger(std::cout, &book, true);
}

#endif // PARSE_TEST
