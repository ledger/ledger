#include "ledger.h"

#include <fstream>
#include <cstring>
#include <ctime>
#include <cctype>

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

    commodity * comm = NULL;

    commodities_map_iterator item = main_ledger->commodities.find(c);
    if (item == main_ledger->commodities.end()) {
      comm = new commodity(c);
    } else {
      comm = (*item).second;
    }

    assert(comm);
    comm->price = create_amount(p);
  }
}

#define MAX_LINE 1024

int  linenum;

static bool do_compute;

transaction * parse_transaction(std::istream& in, book * ledger)
{
  transaction * xact = new transaction();

  static char line[MAX_LINE + 1];
  in.getline(line, MAX_LINE);
  linenum++;

  char * p = line;
  p = skip_ws(p);

  // The call to `next_element' will skip past the account name,
  // and return a pointer to the beginning of the amount.  Once
  // we know where the amount is, we can strip off any
  // transaction note, and parse it.

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

  xact->acct = ledger->find_account(p);

  if (do_compute && xact->cost)
    xact->acct->balance.credit(xact->cost);

  return xact;
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

  regexps_map * masks = NULL;

  while (! in.eof() && in.peek() == '=') {
    in.getline(line, MAX_LINE);
    linenum++;

    char * p = line + 1;
    p = skip_ws(p);

    if (! masks)
      masks = new regexps_map;
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

book * parse_ledger(std::istream& in, regexps_map& regexps,
		     bool compute_balances)
{
  static char line[MAX_LINE + 1];
  char c;

  book * ledger = new book;

  main_ledger = ledger;
  do_compute  = compute_balances;
  linenum     = 0;

  while (! in.eof()) {
    switch (in.peek()) {
    case -1:                    // end of file
      return ledger;

    case '\n':
      linenum++;
    case '\r':                  // skip blank lines
      in.get(c);
      break;

    case 'Y':                   // set the current year
      in >> c;
      in >> ledger->current_year;
      break;

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
      parse_automated_transactions(in, ledger);
      break;

    default:
      if (entry * ent = parse_entry(in, ledger))
	ledger->entries.push_back(ent);
      break;
    }
  }
  return ledger;
}

} // namespace ledger
