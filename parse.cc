#include "ledger.h"

#include <fstream>
#include <cstring>
#include <ctime>
#include <cctype>

namespace ledger {

static inline char * next_element(char * buf, bool variable = false)
{
  char * p;

  // Convert any tabs to spaces, for simplicity's sake
  for (p = buf; *p; p++)
    if (*p == '\t')
      *p = ' ';

  if (variable)
    p = std::strstr(buf, "  ");
  else
    p = std::strchr(buf, ' ');

  if (! p)
    return NULL;

  *p++ = '\0';
  while (std::isspace(*p))
    p++;

  return p;
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

bool parse_date(const std::string& date_str, std::time_t * result,
		const int year = -1)
{
  struct std::tm when;

  std::time_t now = std::time(NULL);
  struct std::tm * now_tm = std::localtime(&now);

  for (const char ** f = formats; *f; f++) {
    memset(&when, INT_MAX, sizeof(struct std::tm));
    if (strptime(date_str.c_str(), *f, &when)) {
      when.tm_hour = 0;
      when.tm_min  = 0;
      when.tm_sec  = 0;

      if (when.tm_year == -1)
	when.tm_year = year == -1 ? now_tm->tm_year : year - 1900;

      if (std::strcmp(*f, "%Y") == 0) {
	when.tm_mon  = 0;
	when.tm_mday = 1;
      } else {
	if (when.tm_mon == -1)
	  when.tm_mon = now_tm->tm_mon;
	if (when.tm_mday == -1)
	  when.tm_mday = now_tm->tm_mday;
      }
      *result = std::mktime(&when);
      return true;
    }
  }
  return false;
}

static int linenum = 0;

static void finalize_entry(entry * curr, bool compute_balances)
{
  assert(curr);
  assert(! curr->xacts.empty());

  // Scan through and compute the total balance for the entry.

  totals balance;

  for (std::list<transaction *>::iterator x = curr->xacts.begin();
       x != curr->xacts.end();
       x++)
    if ((*x)->cost && ! (*x)->is_virtual)
      balance.credit((*x)->cost->value());

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
	  ! (*x)->cost->comm()->prefix &&
	  (*x)->cost->comm()->separate) {
	for (totals::iterator i = balance.amounts.begin();
	     i != balance.amounts.end();
	     i++) {
	  if ((*i).second->comm() != (*x)->cost->comm()) {
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
      std::cerr << "Error, line " << (linenum - 1)
		<< ": Transaction entry is lacking an amount."
		<< std::endl;
      return;
    }
    empty_allowed = false;

    // If one transaction gives no value at all -- and all the
    // rest are of the same commodity -- then its value is the
    // inverse of the computed value of the others.

    totals::iterator i = balance.amounts.begin();
    (*x)->cost = (*i).second->value();
    (*x)->cost->negate();

    if (compute_balances)
      (*x)->acct->balance.credit((*x)->cost);
  }

  // If virtual accounts are being supported, walk through the
  // transactions and create new virtual transactions for all that
  // apply.

  if (main_ledger.compute_virtual) {
    for (state::virtual_map_iterator m = main_ledger.virtual_mapping.begin();
	 m != main_ledger.virtual_mapping.end();
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

	  assert((*i)->is_virtual);
	  assert((*i)->cost);

	  if ((*i)->cost->comm()) {
	    t = new transaction((*i)->acct, (*i)->cost);
	  } else {
	    amount * temp = (*x)->cost->value();
	    t = new transaction((*i)->acct, temp->value((*i)->cost));
	    delete temp;
	  }

	  t->is_virtual   = true;
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

	if (compute_balances)
	  (*x)->acct->balance.credit((*x)->cost);
      }
    }
  }

  // Compute the balances again, just to make sure it all comes out
  // right (i.e., to zero for every commodity).

  if (! curr->validate()) {
    std::cerr << "Error, line " << (linenum - 1)
	      << ": Failed to balance the following transaction:"
	      << std::endl;
    curr->print(std::cerr);
    curr->validate(true);
    return;
  }

  // If it's OK, add it to the general ledger's list of entries.

  main_ledger.entries.push_back(curr);
}

//////////////////////////////////////////////////////////////////////
//
// Ledger parser
//

bool parse_ledger(std::istream& in, bool compute_balances)
{
  char    line[1024];
  int     current_year = -1;
  entry * curr = NULL;

  while (! in.eof()) {
    in.getline(line, 1023);
    linenum++;

    if (line[0] == '\n') {
      continue;
    }
    else if (std::isdigit(line[0])) {
      if (curr && ! curr->xacts.empty())
	finalize_entry(curr, compute_balances);
      curr = new entry;

      char * next = next_element(line);
      if (! parse_date(line, &curr->date, current_year)) {
	std::cerr << "Error, line " << linenum
		  << ": Failed to parse date: " << line << std::endl;
	continue;
      }

      if (*next == '*') {
	curr->cleared = true;

	next++;
	while (std::isspace(*next))
	  next++;
      }

      if (*next == '(') {
	char * p = std::strchr(next, ')');
	if (p) {
	  *p++ = '\0';
	  curr->code = next;
	  next = p;

	  next++;
	  while (std::isspace(*next))
	    next++;
	}
      }

      curr->desc = next;
    }
    else if (curr && std::isspace(line[0])) {
      transaction * xact = new transaction();

      char * p = line;
      while (std::isspace(*p))
	p++;

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
	  while (std::isspace(*note_str))
	    note_str++;
	  xact->note = note_str;
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

      if (xact->is_virtual && ! main_ledger.compute_virtual) {
	delete xact;
      } else {
	xact->acct = main_ledger.find_account(p);
	if (compute_balances && xact->cost)
	  xact->acct->balance.credit(xact->cost);

	curr->xacts.push_back(xact);
      }
    }
    else if (line[0] == 'Y') {
      current_year = std::atoi(line + 2);
    }
  }

  if (curr && ! curr->xacts.empty())
    finalize_entry(curr, compute_balances);

  return true;
}

void parse_virtual_mappings(const std::string& path)
{
  main_ledger.mapping_file = path;

  std::ifstream maps(main_ledger.mapping_file.c_str());

  char line[1024];
  int  linenum = 0;

  std::list<mask> * masks = NULL;
  std::list<transaction *> * xacts = NULL;

  while (! maps.eof()) {
    maps.getline(line, 1023);
    linenum++;

    // The format of each entry is:
    //
    //   REGEXP1
    //   REGEXP2...
    //     ACCOUNT      AMOUNT
    //     ACCOUNT      AMOUNT...
    //
    // If AMOUNT has a commodity, that exact amount is always
    // transacted whenever a REGEXP is matched.  If it has no
    // commodity, then it is taken as the multiplier, the result of
    // which is transacted instead.
    //
    // If one of REGEXP is the word "{BEGIN}", then those
    // transactions will be entered before parsing has begin.

    if (std::isspace(line[0])) {
      if (! xacts)
	xacts = new std::list<transaction *>;

      char * p = line;
      while (std::isspace(*p))
	p++;

      char *        cost_str = next_element(p, true);
      account *     acct     = main_ledger.find_account(p);
      transaction * xact     = new transaction(acct, create_amount(cost_str));

      xact->is_virtual   = true;
      xact->must_balance = false;

      assert(masks);
      assert(! masks->empty());
      if (masks->size() == 1 &&
	  masks->front().pattern == "{BEGIN}") {
	entry * opening = new entry;

	opening->date    = std::time(NULL);
	opening->cleared = true;
	opening->desc    = "Opening Balance";

	opening->xacts.push_back(xact);
	main_ledger.entries.push_back(opening);
      } else {
	xacts->push_back(xact);
      }
    }
    else if (line[0] != '\0') {
      if (xacts) {
	std::pair<state::virtual_map_iterator, bool> result =
	  main_ledger.virtual_mapping.insert
	    (state::virtual_map_pair(masks, xacts));
	assert(result.second);

	masks = NULL;
	xacts = NULL;
      }

      if (! masks)
	masks = new std::list<mask>;

      masks->push_back(mask(line));
    }
  }

  if (xacts) {
    std::pair<state::virtual_map_iterator, bool> result =
      main_ledger.virtual_mapping.insert
	(state::virtual_map_pair(masks, xacts));
    assert(result.second);
  }
}

} // namespace ledger
