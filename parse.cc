#include "ledger.h"

#include <cstring>
#include <ctime>
#include <cctype>

namespace ledger {

static char * next_element(char * buf, bool variable = false)
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

static int linenum = 0;

static void finalize_entry(entry * curr, bool compute_balances)
{
  assert(curr);

  // Certain shorcuts are allowed in the case of exactly two
  // transactions.

  if (! curr->xacts.empty() && curr->xacts.size() == 2) {
    transaction * first  = curr->xacts.front();
    transaction * second = curr->xacts.back();

    // If one transaction gives no value at all, then its value is
    // the inverse of the computed value of the other.

    if (! first->cost && second->cost) {
      first->cost = second->cost->value();
      first->cost->negate();

      if (compute_balances)
	first->acct->balance.credit(first->cost);
    }
    else if (! second->cost && first->cost) {
      second->cost = first->cost->value();
      second->cost->negate();

      if (compute_balances)
	second->acct->balance.credit(second->cost);
    }
    else if (first->cost && second->cost) {
      // If one transaction is of a different commodity than the
      // other, and it has no per-unit price, and its not of the
      // default commodity, then determine its price by dividing the
      // unit count into the total, to balance the transaction.

      if (first->cost->comm() != second->cost->comm()) {
	if (! second->cost->has_price() &&
	    second->cost->comm_symbol() != DEFAULT_COMMODITY) {
	  second->cost->set_value(first->cost);
	}
	else if (! first->cost->has_price() &&
		 first->cost->comm_symbol() != DEFAULT_COMMODITY) {
	  first->cost->set_value(second->cost);
	}
      }
    }
  }

  if (! curr->validate()) {
    std::cerr << "Error, line " << (linenum - 1)
	      << ": Failed to balance the following transaction:"
	      << std::endl;
    curr->print(std::cerr);
    curr->validate(true);
    return;
  }

#ifdef HUQUQULLAH
  if (main_ledger.compute_huquq) {
    for (std::list<transaction *>::iterator x = curr->xacts.begin();
	 x != curr->xacts.end();
	 x++) {
      if (! (*x)->exempt_or_necessary || ! (*x)->cost)
	continue;

      // Reflect the exempt or necessary transaction in the
      // Huququ'llah account, using the H commodity, which is 19% of
      // whichever DEFAULT_COMMODITY ledger was compiled with.

      amount * temp = (*x)->cost->value();

      transaction * t
	= new transaction(main_ledger.huquq_account,
			  temp->value(main_ledger.huquq));
      curr->xacts.push_back(t);

      if (compute_balances)
	t->acct->balance.credit(t->cost);

      // Balance the above transaction by recording the inverse in
      // Expenses:Huququ'llah.

      t = new transaction(main_ledger.huquq_expenses_account,
			  temp->value(main_ledger.huquq));
      t->cost->negate();
      curr->xacts.push_back(t);

      if (compute_balances)
	t->acct->balance.credit(t->cost);

      delete temp;
    }
  }
#endif

  main_ledger.entries.push_back(curr);
}

//////////////////////////////////////////////////////////////////////
//
// Ledger parser
//

bool parse_ledger(std::istream& in, bool compute_balances)
{
  std::time_t      now          = std::time(NULL);
  struct std::tm * now_tm       = std::localtime(&now);
  int              current_year = now_tm->tm_year + 1900;

  char line[1024];

  struct std::tm moment;

  entry * curr = NULL;

  // Compile the regular expression used for parsing amounts
  const char *error;
  int erroffset;
  static const std::string regexp =
    "^(([0-9]{4})[./])?([0-9]+)[./]([0-9]+)\\s+(\\*\\s+)?"
    "(\\(([^)]+)\\)\\s+)?(.+)";
  pcre * entry_re = pcre_compile(regexp.c_str(), 0,
				 &error, &erroffset, NULL);

  while (! in.eof()) {
    in.getline(line, 1023);
    linenum++;

    if (line[0] == '\n') {
      continue;
    }
    else if (std::isdigit(line[0])) {
      static char buf[256];
      int ovector[60];

      int matched = pcre_exec(entry_re, NULL, line, std::strlen(line),
			      0, 0, ovector, 60);
      if (! matched) {
	std::cerr << "Error, line " << linenum
		  << ": Failed to parse: " << line << std::endl;
	continue;
      }

      // If we haven't finished with the last entry yet, do so now

      if (curr)
	finalize_entry(curr, compute_balances);

      curr = new entry;

      // Parse the date

      int year = current_year;
      if (ovector[1 * 2] >= 0) {
	pcre_copy_substring(line, ovector, matched, 2, buf, 255);
	year = std::atoi(buf);
      }

      assert(ovector[3 * 2] >= 0);
      pcre_copy_substring(line, ovector, matched, 3, buf, 255);
      int mon = std::atoi(buf);

      assert(ovector[4 * 2] >= 0);
      pcre_copy_substring(line, ovector, matched, 4, buf, 255);
      int mday = std::atoi(buf);

      memset(&moment, 0, sizeof(struct std::tm));

      moment.tm_mday = mday;
      moment.tm_mon  = mon - 1;
      moment.tm_year = year - 1900;

      curr->date = std::mktime(&moment);

      // Parse the remaining entry details

      if (ovector[5 * 2] >= 0)
	curr->cleared = true;

      if (ovector[6 * 2] >= 0) {
	pcre_copy_substring(line, ovector, matched, 7, buf, 255);
	curr->code = buf;
      }

      if (ovector[8 * 2] >= 0) {
	pcre_copy_substring(line, ovector, matched, 8, buf, 255);
	curr->desc = buf;
      }
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

#ifdef HUQUQULLAH
      if (*p == '!') {
	xact->exempt_or_necessary = true;
	p++;
      }
#endif

      xact->acct = main_ledger.find_account(p);
#ifdef HUQUQULLAH
      if (xact->acct->exempt_or_necessary)
	xact->exempt_or_necessary = true;
#endif
      if (compute_balances && xact->cost)
	xact->acct->balance.credit(xact->cost);

      curr->xacts.push_back(xact);
    }
    else if (line[0] == 'Y') {
      current_year = std::atoi(line + 2);
    }
  }

  if (curr)
    finalize_entry(curr, compute_balances);

  return true;
}

} // namespace ledger
