#include "ledger.h"

#include <cstring>
#include <ctime>
#include <cctype>

namespace ledger {

static char * next_element(char * buf, bool variable = false)
{
  char * p;

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

static inline void finalize_entry(entry * curr)
{
  if (curr) {
    if (! curr->validate()) {
      std::cerr << "Failed to balance the following transaction, "
		<< "ending on line " << (linenum - 1) << std::endl;
      curr->print(std::cerr);
    } else {
      ledger.push_back(curr);
    }
  }
}

static account * find_account(const char * name)
{
  char * buf = new char[std::strlen(name) + 1];
  std::strcpy(buf, name);

  account * current = NULL;
  for (char * tok = std::strtok(buf, ":");
       tok;
       tok = std::strtok(NULL, ":")) {
    if (! current) {
      accounts_iterator i = accounts.find(tok);
      if (i == accounts.end()) {
	current = new account(tok);
	accounts.insert(accounts_entry(tok, current));
      } else {
	current = (*i).second;
      }
    } else {
      account::iterator i = current->children.find(tok);
      if (i == current->children.end()) {
	current = new account(tok, current);
	current->parent->children.insert(accounts_entry(tok, current));
      } else {
	current = (*i).second;
      }
    }
  }

  delete[] buf;

  return current;
}

//////////////////////////////////////////////////////////////////////
//
// Ledger parser
//

bool parse_ledger(std::istream& in)
{
  static std::time_t now = std::time(NULL);
  static struct std::tm * now_tm = std::localtime(&now);
  static int current_year = now_tm->tm_year + 1900;

  static char line[1024];

  static struct std::tm moment;
  memset(&moment, 0, sizeof(struct std::tm));

  entry * curr = NULL;

  // Compile the regular expression used for parsing amounts
  static pcre * entry_re = NULL;
  if (! entry_re) {
    const char *error;
    int erroffset;
    static const std::string regexp =
      "^(([0-9]{4})[./])?([0-9]+)[./]([0-9]+)\\s+(\\*\\s+)?"
      "(\\(([^)]+)\\)\\s+)?(.+)";
    entry_re = pcre_compile(regexp.c_str(), 0, &error, &erroffset, NULL);
  }

  while (! in.eof()) {
    in.getline(line, 1023);
    linenum++;

    if (in.eof()) {
      break;
    }
    else if (line[0] == '\n') {
      continue;
    }
    else if (std::isdigit(line[0])) {
      static char buf[256];
      int ovector[60];

      int matched = pcre_exec(entry_re, NULL, line, std::strlen(line),
			      0, 0, ovector, 60);
      if (! matched) {
	std::cerr << "Failed to parse, line " << linenum << ": "
		  << line << std::endl;
	continue;
      }

      if (curr)
	finalize_entry(curr);
      curr = new entry;

      // Parse the date

      int mday, mon, year = current_year;

      if (ovector[1 * 2] >= 0) {
	pcre_copy_substring(line, ovector, matched, 2, buf, 255);
	year = std::atoi(buf);
      }

      if (ovector[3 * 2] >= 0) {
	pcre_copy_substring(line, ovector, matched, 3, buf, 255);
	mon = std::atoi(buf);
      }

      if (ovector[4 * 2] >= 0) {
	pcre_copy_substring(line, ovector, matched, 4, buf, 255);
	mday = std::atoi(buf);
      }

      moment.tm_mday = mday;
      moment.tm_mon  = mon - 1;
      moment.tm_year = year - 1900;

      curr->date = std::mktime(&moment);

      if (ovector[5 * 2] >= 0)
	curr->cleared = true;

      if (ovector[6 * 2] >= 0) {
	pcre_copy_substring(line, ovector, matched, 7, buf, 255);
	curr->code = buf;
      }

      if (ovector[8 * 2] >= 0) {
	int result = pcre_copy_substring(line, ovector, matched, 8, buf, 255);
	assert(result >= 0);
	curr->desc = buf;
      }
    }
    else if (std::isspace(line[0])) {
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
	xact->cost = curr->xacts.front()->cost->copy();
	xact->cost->negate();
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
      bool exempt = false;
      if (compute_huquq) {
	if (*p == '!') {
	  exempt = true;
	  p++;
	}
	else if (matches(huquq_categories, p)) {
	  exempt = true;
	}
      }
#endif

      xact->acct = find_account(p);
      curr->xacts.push_back(xact);

#ifdef HUQUQULLAH
      if (exempt) {
	static amount * huquq = create_amount("H 1.00");
	amount * temp;

	transaction * t = new transaction();
	t->acct = find_account("Huququ'llah");
	temp = xact->cost->value();
	t->cost = temp->value(huquq);
	delete temp;
	curr->xacts.push_back(t);

	t = new transaction();
	t->acct = find_account("Expenses:Huququ'llah");
	temp = xact->cost->value();
	t->cost = temp->value(huquq);
	delete temp;
	t->cost->negate();
	curr->xacts.push_back(t);
      }
#endif
    }
    else if (line[0] == 'Y') {
      current_year = std::atoi(line + 2);
    }
  }

  if (curr)
    finalize_entry(curr);

  return true;
}

} // namespace ledger
