#include "ledger.h"

#include <fstream>
#include <cstring>
#include <ctime>
#include <cctype>

#define TIMELOG_SUPPORT 1

namespace ledger {

static inline char * skip_ws(char * ptr)
{
  while (std::isspace(*ptr))
    ptr++;
  return ptr;
}

static inline char * next_element(char * buf, bool variable = false)
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

static const char *formats[] = {
  "%Y-%m-%d",
  "%m-%d",
  "%Y/%m/%d",
  "%m/%d",
  "%Y.%m.%d",
  "%m.%d",
  "%a",
  "%A",
  "%b",
  "%B",
  "%Y",
  NULL
};

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

  static std::time_t now = std::time(NULL);
  static struct std::tm * now_tm = std::localtime(&now);

  when.tm_hour = 0;
  when.tm_min  = 0;
  when.tm_sec  = 0;

  if (when.tm_year == -1)
    when.tm_year = ((year == -1) ? now_tm->tm_year : (year - 1900));

  if (when.tm_mon == -1)
    when.tm_mon = now_tm->tm_mon;

  if (when.tm_mday == -1)
    when.tm_mday = now_tm->tm_mday;

  *result = std::mktime(&when);

  return true;
}

void record_price(const std::string& symbol, amount * price,
		  std::time_t * date = NULL)
{
  commodity * comm = NULL;
  commodities_map_iterator item = main_ledger->commodities.find(symbol);
  if (item == main_ledger->commodities.end())
    comm = new commodity(symbol);
  else
    comm = (*item).second;

  assert(comm);
  comm->set_price(price, date);
}

void parse_price_setting(const std::string& setting)
{
  char buf[128];
  std::strcpy(buf, setting.c_str());

  assert(setting.length() < 128);

  char * c = buf;
  char * p = std::strchr(buf, '=');
  if (! p) {
    std::cerr << "Warning: Invalid price setting: " << setting << std::endl;
  } else {
    *p++ = '\0';
    record_price(c, create_amount(p));
  }
}

#define MAX_LINE 1024

       int         linenum;
static bool        do_compute;
static std::string account_prefix;

transaction * parse_transaction_text(char * line, book * ledger)
{
  transaction * xact = new transaction();

  // The call to `next_element' will skip past the account name,
  // and return a pointer to the beginning of the amount.  Once
  // we know where the amount is, we can strip off any
  // transaction note, and parse it.

  char * p = skip_ws(line);
  char * cost_str = next_element(p, true);
  char * note_str;

  // If there is no amount given, it is intended as an implicit
  // amount; we must use the opposite of the value of the
  // preceding transaction.

  if (! cost_str || ! *cost_str || *cost_str == ';') {
    if (cost_str && *cost_str) {
      while (*cost_str == ';' || std::isspace(*cost_str))
	cost_str++;
      xact->note = cost_str;
    }

    xact->cost = NULL;
  }
  else {
    note_str = std::strchr(cost_str, ';');
    if (note_str) {
      *note_str++ = '\0';
      xact->note = skip_ws(note_str);
    }

    for (char * t = cost_str + (std::strlen(cost_str) - 1);
	 std::isspace(*t);
	 t--)
      *t = '\0';

    xact->cost = create_amount(cost_str);
  }

  if (*p == '[' || *p == '(') {
    xact->is_virtual   = true;
    xact->specified    = true;
    xact->must_balance = *p == '[';
    p++;

    char * e = p + (std::strlen(p) - 1);
    assert(*e == ')' || *e == ']');
    *e = '\0';
  }

  std::string name = account_prefix + p;
  xact->acct = ledger->find_account(name.c_str());

  if (do_compute && xact->cost)
    xact->acct->balance.credit(xact->cost);

  return xact;
}

transaction * parse_transaction(std::istream& in, book * ledger)
{
  static char line[MAX_LINE + 1];
  in.getline(line, MAX_LINE);
  linenum++;

  return parse_transaction_text(line, ledger);
}

entry * parse_entry(std::istream& in, book * ledger)
{
  entry * curr = new entry(ledger);

  static char line[MAX_LINE + 1];
  in.getline(line, MAX_LINE);
  linenum++;

  // Parse the date

  char * next = next_element(line);
  if (! parse_date(line, &curr->date, ledger->current_year)) {
    std::cerr << "Error, line " << linenum
	      << ": Failed to parse date: " << line << std::endl;
    return NULL;
  }

  // Parse the optional cleared flag: *

  if (*next == '*') {
    curr->cleared = true;
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

  curr->desc = next;

  // Parse all of the transactions associated with this entry

  while (! in.eof() && (in.peek() == ' ' || in.peek() == '\t'))
    if (transaction * xact = parse_transaction(in, ledger))
      curr->xacts.push_back(xact);

  // If there were no transactions, throw away the entry

  if (curr->xacts.empty() || ! curr->finalize(do_compute)) {
    delete curr;
    return NULL;
  }

  return curr;
}

void parse_automated_transactions(std::istream& in, book * ledger)
{
  static char line[MAX_LINE + 1];

  regexps_list * masks = NULL;

  while (! in.eof() && in.peek() == '=') {
    in.getline(line, MAX_LINE);
    linenum++;

    char * p = line + 1;
    p = skip_ws(p);

    if (! masks)
      masks = new regexps_list;
    masks->push_back(mask(p));
  }

  std::list<transaction *> * xacts = NULL;

  while (! in.eof() && (in.peek() == ' ' || in.peek() == '\t')) {
    if (transaction * xact = parse_transaction(in, ledger)) {
      if (! xacts)
	xacts = new std::list<transaction *>;

      if (! xact->cost) {
	std::cerr << "Error, line " << (linenum - 1)
		  << ": All automated transactions must have a value."
		  << std::endl;
      } else {
	xacts->push_back(xact);
      }
    }
  }

  if (masks && xacts)
    ledger->virtual_mapping.insert(book::virtual_map_pair(masks, xacts));
  else if (masks)
    delete masks;
  else if (xacts)
    delete xacts;
}

//////////////////////////////////////////////////////////////////////
//
// Ledger parser
//

#ifdef TIMELOG_SUPPORT
static std::time_t  time_in;
static account *    last_account;
static std::string  last_desc;
#endif

int parse_ledger(book * ledger, std::istream& in,
		 regexps_list& regexps, bool compute_balances,
		 const char * acct_prefix)
{
  static char   line[MAX_LINE + 1];
  char 		c;
  int 		count = 0;
  std::string   old_account_prefix = account_prefix;

  linenum    = 1;
  do_compute = compute_balances;
  if (acct_prefix) {
    account_prefix += acct_prefix;
    account_prefix += ":";
  }

  while (! in.eof()) {
    switch (in.peek()) {
    case -1:                    // end of file
      goto done;

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
	last_account = ledger->find_account(p);
      } else {
	std::cerr << "Error, line " << (linenum - 1)
		  << ": Cannot parse timelog entry date."
		  << std::endl;
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
	date += " ";
	date += time;

	static struct std::tm when;
	if (strptime(date.c_str(), "%Y/%m/%d %H:%M:%S", &when)) {
	  entry * curr = new entry(ledger);

	  curr->date = std::mktime(&when);

	  double diff = (curr->date - time_in) / 60.0 / 60.0;
	  char   buf[128];
	  std::sprintf(buf, "%fh", diff);

	  curr->cleared = true;
	  curr->code    = "";
	  curr->desc    = last_desc;

	  std::string xact_line = "(";
	  xact_line += last_account->as_str();
	  xact_line += ")  ";
	  xact_line += buf;

	  std::strcpy(buf, xact_line.c_str());

	  if (transaction * xact = parse_transaction_text(buf, ledger)) {
	    curr->xacts.push_back(xact);

	    // Make sure numbers are reported only to 1 decimal place.
	    commodity * cmdty = xact->cost->commdty();
	    cmdty->precision = 1;
	  }

	  ledger->entries.push_back(curr);
	  count++;
	} else {
	  std::cerr << "Error, line " << (linenum - 1)
		    << ": Cannot parse timelog entry date."
		    << std::endl;
	}

	last_account = NULL;
      }
      break;
#endif // TIMELOG_SUPPORT

    case 'P': {			// a pricing entry
      in >> c;

      time_t      date;
      std::string symbol;

      in >> line;		// the date
      if (! parse_date(line, &date, ledger->current_year)) {
	std::cerr << "Error, line " << linenum
		  << ": Failed to parse date: " << line << std::endl;
	break;
      }
      in >> symbol;		// the commodity
      in >> line;		// the price

      // Add this pricing entry to the history for the given
      // commodity.
      record_price(symbol, create_amount(line), &date);
      break;
    }

    case 'N': {			// don't download prices
      in >> c;
      in >> line;		// the symbol

      commodity * comm = NULL;
      commodities_map_iterator item = main_ledger->commodities.find(line);
      if (item == main_ledger->commodities.end())
	comm = new commodity(line);
      else
	comm = (*item).second;

      assert(comm);
      if (comm)
	comm->sought = true;
      break;
    }

    case 'C': {			// a flat conversion
      in >> c;

      std::string symbol;
      in >> symbol;		// the commodity
      in >> line;		// the price

      // Add this pricing entry to the given commodity
      record_price(symbol, create_amount(line));
      break;
    }

    case 'Y':                   // set the current year
      in >> c;
      in >> ledger->current_year;
      break;

#ifdef TIMELOG_SUPPORT
    case 'h':
    case 'b':
#endif
    case ';':                   // a comment line
      in.getline(line, MAX_LINE);
      linenum++;
      break;

    case '-':
    case '+':                   // permanent regexps
      in.getline(line, MAX_LINE);
      linenum++;

      // Add the regexp to whatever masks currently exist
      regexps.push_back(mask(line));
      break;

    case '=':                   // automated transactions
      do_compute = false;
      parse_automated_transactions(in, ledger);
      do_compute = compute_balances;
      break;

    case '!':                   // directive
      in >> line;
      if (std::string(line) == "!include") {
	std::string path;
	bool        has_prefix = false;

	in >> path;

	if (in.peek() == ' ') {
	  has_prefix = true;
	  in.getline(line, MAX_LINE);
	}

	int curr_linenum = linenum;
	count += parse_ledger_file(ledger, path, regexps, compute_balances,
				   has_prefix ? skip_ws(line) : NULL);
	linenum = curr_linenum;
      }
      break;

    default:
      if (entry * ent = parse_entry(in, ledger)) {
	ledger->entries.push_back(ent);
	count++;
      }
      break;
    }
  }

 done:
  account_prefix = old_account_prefix;
  
  return count;
}

int parse_ledger_file(book * ledger, const std::string& file,
		      regexps_list& regexps, bool compute_balances,
		      const char * acct_prefix)
{
  std::ifstream stream(file.c_str());

  // Parse the ledger

#ifdef READ_GNUCASH
  char buf[32];
  stream.get(buf, 31);
  stream.seekg(0);

  if (std::strncmp(buf, "<?xml version=\"1.0\"?>", 21) == 0)
    return parse_gnucash(ledger, stream, compute_balances);
  else
#endif
    return parse_ledger(ledger, stream, regexps, compute_balances,
			acct_prefix);
}

//////////////////////////////////////////////////////////////////////
//
// Read other kinds of data from files
//

void read_regexps(const std::string& path, regexps_list& regexps)
{
  std::ifstream file(path.c_str());

  while (! file.eof()) {
    char buf[80];
    file.getline(buf, 79);
    if (*buf && ! std::isspace(*buf))
      regexps.push_back(mask(buf));
  }
}

} // namespace ledger
