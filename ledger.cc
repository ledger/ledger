#include "ledger.h"

#include <fstream>

namespace ledger {

commodities_t commodities;
accounts_t    accounts;
ledger_t      ledger;

bool use_warnings = false;

#ifdef HUQUQULLAH
bool compute_huquq;
std::list<mask> huquq_categories;
#endif

void entry::print(std::ostream& out) const
{
  char buf[32];
  std::strftime(buf, 31, "%Y.%m.%d ", std::localtime(&date));
  out << buf;

  if (cleared)
    out << "* ";
  if (! code.empty())
    out << '(' << code << ") ";
  if (! desc.empty())
    out << " " << desc;

  out << std::endl;

  bool shortcut = xacts.size() == 2;
  if (shortcut &&
      xacts.front()->cost->comm() != xacts.back()->cost->comm())
    shortcut = false;

  for (std::list<transaction *>::const_iterator i = xacts.begin();
       i != xacts.end();
       i++) {
    out << "    ";

    std::string acct_name;
    for (account * acct = (*i)->acct;
	 acct;
	 acct = acct->parent) {
      if (acct_name.empty())
	acct_name = acct->name;
      else
	acct_name = acct->name + ":" + acct_name;
    }

    out.width(30);
    out << std::left << acct_name;

    if (! shortcut || i == xacts.begin()) {
      out << "  ";
      out.width(10);
      out << std::right << (*i)->cost->as_str(true);
    }

    if (! (*i)->note.empty())
      out << "  ; " << (*i)->note;

    out << std::endl;
  }
  out << std::endl;
}

bool entry::validate() const
{
  totals balance;

  for (std::list<transaction *>::const_iterator i = xacts.begin();
       i != xacts.end();
       i++)
    balance.credit((*i)->cost->value());

  if (balance) {
    std::cerr << "Totals are:" << std::endl;
    balance.print(std::cerr);
    std::cerr << std::endl;
  }
  return ! balance;             // must balance to 0.0
}

void totals::credit(const totals& other)
{
  for (const_iterator_t i = other.amounts.begin();
       i != other.amounts.end();
       i++) {
    credit((*i).second);
  }
}

totals::operator bool() const
{
  for (const_iterator_t i = amounts.begin(); i != amounts.end(); i++)
    if (*((*i).second))
      return true;
  return false;
}

void totals::print(std::ostream& out) const
{
  bool first = true;
  for (const_iterator_t i = amounts.begin(); i != amounts.end(); i++)
    if (*((*i).second)) {
      if (first)
	first = false;
      else
	out << std::endl;

      out.width(20);
      out << std::right << *((*i).second);
    }
}

amount * totals::value(const std::string& commodity)
{
  // Render all of the amounts into the given commodity.  This
  // requires known prices for each commodity.

  amount * total = create_amount((commodity + " 0.00").c_str());

  for (iterator_t i = amounts.begin(); i != amounts.end(); i++)
    *total += *((*i).second);

  return total;
}

// Print out the entire ledger that was read in, sorted by date.
// This can be used to "wash" ugly ledger files.

void print_ledger(int argc, char *argv[], std::ostream& out)
{
  std::list<mask> regexps;

  int c;
  optind = 1;
  while (-1 != (c = getopt(argc, argv, "i:"))) {
    switch (char(c)) {
    // -i path-to-file-of-regexps
    case 'i':
      read_regexps(optarg, regexps);
      break;
    }
  }

  // Compile the list of specified regular expressions, which can be
  // specified on the command line, or using an include/exclude file

  for (; optind < argc; optind++)
    record_regexp(argv[optind], regexps);

  // Sort the list of entries by date, then print them in order.

  std::sort(ledger.begin(), ledger.end(), cmp_entry_date());

  for (std::vector<entry *>::const_iterator i = ledger.begin();
       i != ledger.end();
       i++) {
    if (regexps.empty() ||
	(matches(regexps, (*i)->code) ||
	 matches(regexps, (*i)->desc))) {
      (*i)->print(out);
    }
    else {
      bool match = false;

      for (std::list<transaction *>::const_iterator x = (*i)->xacts.begin();
	   x != (*i)->xacts.end();
	   x++) {
	if (matches(regexps, (*x)->acct->name) ||
	    matches(regexps, (*x)->note)) {
	  match = true;
	  break;
	}
      }

      if (match)
	(*i)->print(out);
    }
  }
}

void record_regexp(char * pattern, std::list<mask>& regexps)
{
  bool exclude = false;

  char * pat = pattern;
  if (*pat == '-') {
    exclude = true;
    pat++;
    while (std::isspace(*pat))
      pat++;
  }
  else if (*pat == '+') {
    pat++;
    while (std::isspace(*pat))
      pat++;
  }

  const char *error;
  int erroffset;
  pcre * re = pcre_compile(pat, PCRE_CASELESS, &error, &erroffset, NULL);
  if (! re)
    std::cerr << "Warning: Failed to compile regexp: " << pattern
	      << std::endl;
  else
    regexps.push_back(mask(exclude, re));
}

void read_regexps(const char * path, std::list<mask>& regexps)
{
  if (access(path, R_OK) != -1) {
    std::ifstream file(path);

    while (! file.eof()) {
      char buf[80];
      file.getline(buf, 79);
      if (*buf && ! std::isspace(*buf))
	record_regexp(buf, regexps);
    }
  }
}

bool matches(const std::list<mask>& regexps, const std::string& str)
{
  // If the first pattern is an exclude, we assume all patterns match
  // if they don't match the exclude.  If the first pattern is an
  // include, then only accounts matching the include will match.

  bool match = (*regexps.begin()).exclude;

  for (std::list<mask>::const_iterator r = regexps.begin();
       r != regexps.end();
       r++) {
    int ovec[3];
    if (pcre_exec((*r).regexp, NULL, str.c_str(), str.length(),
		  0, 0, ovec, 3) >= 0)
      match = ! (*r).exclude;
  }

  return match;
}

} // namespace ledger
