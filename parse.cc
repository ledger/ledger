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

static int  linenum;
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
  entry * curr = new entry;

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

  if (curr->xacts.empty()) {
    delete curr;
    return NULL;
  }

  // Scan through and compute the total balance for the entry.  This
  // is used for auto-calculating the value of entries with no cost,
  // and the per-unit price of unpriced commodities.

  totals balance;

  for (std::list<transaction *>::iterator x = curr->xacts.begin();
       x != curr->xacts.end();
       x++)
    if ((*x)->cost && ! (*x)->is_virtual) {
      amount * value = (*x)->cost->value();
      balance.credit(value);
      delete value;
    }

  // If one transaction is of a different commodity than the others,
  // and it has no per-unit price, determine its price by dividing
  // the unit count into the value of the balance.
  //
  // NOTE: We don't do this for prefix-style or joined-symbol
  // commodities.  Also, do it for the last eligible commodity first,
  // if it meets the criteria.

  if (! balance.amounts.empty() && balance.amounts.size() == 2) {
    for (std::list<transaction *>::iterator x = curr->xacts.begin();
	 x != curr->xacts.end();
	 x++) {
      if ((*x)->is_virtual)
	continue;

      if (! (*x)->cost->has_price() &&
	  ! (*x)->cost->commdty()->prefix &&
	  (*x)->cost->commdty()->separate) {
	for (totals::iterator i = balance.amounts.begin();
	     i != balance.amounts.end();
	     i++) {
	  if ((*i).second->commdty() != (*x)->cost->commdty()) {
	    (*x)->cost->set_value((*i).second);
	    break;
	  }
	}
	break;
      }
    }
  }

  // Walk through each of the transactions, fixing up any that we
  // can, and performing any on-the-fly calculations.

  bool empty_allowed = true;

  for (std::list<transaction *>::iterator x = curr->xacts.begin();
       x != curr->xacts.end();
       x++) {
    if ((*x)->is_virtual || (*x)->cost)
      continue;

    if (! empty_allowed || balance.amounts.empty() ||
	balance.amounts.size() != 1) {
      std::cerr << "Error, line " << linenum
		<< ": Transaction entry is lacking an amount."
		<< std::endl;
      return NULL;
    }
    empty_allowed = false;

    // If one transaction gives no value at all -- and all the
    // rest are of the same commodity -- then its value is the
    // inverse of the computed value of the others.

    totals::iterator i = balance.amounts.begin();
    (*x)->cost = (*i).second->value();
    (*x)->cost->negate();

    if (do_compute)
      (*x)->acct->balance.credit((*x)->cost);
  }

  // If virtual accounts are being supported, walk through the
  // transactions and create new virtual transactions for all that
  // apply.

  for (book::virtual_map_iterator m = ledger->virtual_mapping.begin();
       m != ledger->virtual_mapping.end();
       m++) {
    std::list<transaction *> xacts;

    for (std::list<transaction *>::iterator x = curr->xacts.begin();
	 x != curr->xacts.end();
	 x++) {
      if ((*x)->is_virtual ||
	  ! ledger::matches(*((*m).first), (*x)->acct->as_str()))
	continue;

      for (std::list<transaction *>::iterator i = (*m).second->begin();
	   i != (*m).second->end();
	   i++) {
	transaction * t;

	if ((*i)->cost->commdty()) {
	  t = new transaction((*i)->acct, (*i)->cost);
	} else {
	  amount * temp = (*x)->cost->value();
	  t = new transaction((*i)->acct, temp->value((*i)->cost));
	  delete temp;
	}

	t->is_virtual   = (*i)->is_virtual;
	t->must_balance = (*i)->must_balance;

	// If there is already a virtual transaction for the
	// account under consideration, and it's `must_balance'
	// flag matches, then simply add this amount to that
	// transaction.

	bool added = false;

	for (std::list<transaction *>::iterator y = xacts.begin();
	     y != xacts.end();
	     y++) {
	  if ((*y)->is_virtual && (*y)->acct == t->acct &&
	      (*y)->must_balance == t->must_balance) {
	    (*y)->cost->credit(t->cost);
	    delete t;
	    added = true;
	    break;
	  }
	}

	if (! added)
	  for (std::list<transaction *>::iterator y = curr->xacts.begin();
	       y != curr->xacts.end();
	       y++) {
	    if ((*y)->is_virtual && (*y)->acct == t->acct &&
		(*y)->must_balance == t->must_balance) {
	      (*y)->cost->credit(t->cost);
	      delete t;
	      added = true;
	      break;
	    }
	  }

	if (! added)
	  xacts.push_back(t);
      }
    }

    // Add to the current entry any virtual transactions which were
    // created.  We have to do this afterward, otherwise the
    // iteration above is screwed up if we try adding new
    // transactions during the traversal.

    for (std::list<transaction *>::iterator x = xacts.begin();
	 x != xacts.end();
	 x++) {
      curr->xacts.push_back(*x);

      if (do_compute)
	(*x)->acct->balance.credit((*x)->cost);
    }
  }

  // Compute the balances again, just to make sure it all comes out
  // right (i.e., zero for every commodity).

  if (! curr->validate()) {
    std::cerr << "Error, line " << (linenum - 1)
	      << ": Failed to balance the following transaction:"
	      << std::endl;
    curr->print(std::cerr);
    curr->validate(true);
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
