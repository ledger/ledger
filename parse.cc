#include <cstring>
#include <ctime>
#include <cctype>

#include "ledger.h"

#include <pcre.h>               // Perl regular expression library

namespace ledger {

//////////////////////////////////////////////////////////////////////
//
// Ledger parser
//

char * next_element(char * buf, bool variable = false)
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

inline void finalize_entry(entry * curr) {
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
      "^(([0-9]{4})[./])?([0-9]{2})[./]([0-9]{2})\\s+(\\*\\s+)?"
      "(\\(([^)]+)\\)\\s+)?(.+)";
    entry_re = pcre_compile(regexp.c_str(), 0, &error, &erroffset, NULL);
  }

  while (! in.eof()) {
    in.getline(line, 1023);
    linenum++;

    if (in.eof()) {
      break;
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

      xact->cost = create_amount(next_element(line, true));

      // jww (2003-09-28): Reverse parse the account name to find the
      // correct account.  This means that each account needs to know
      // its children.
      account * current = NULL;
      for (char * tok = std::strtok(line, ":");
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
      xact->acct = current;

      curr->xacts.push_back(xact);
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
