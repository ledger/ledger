#include "ledger.h"

#include <fstream>

namespace ledger {

bool use_warnings = false;

state main_ledger;

std::list<mask> regexps;

void entry::print(std::ostream& out, bool shortcut) const
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

  if (shortcut &&
      (xacts.size() != 2 ||
       xacts.front()->cost->comm() != xacts.back()->cost->comm()))
    shortcut = false;

  for (std::list<transaction *>::const_iterator x = xacts.begin();
       x != xacts.end();
       x++) {
#ifdef HUQUQULLAH
    if ((*x)->acct->exempt_or_necessary &&
	(! shortcut || ! ledger::matches(main_ledger.huquq_categories,
					 (*x)->acct->as_str())))
      out << "   !";
    else
#endif
      out << "    ";

    out.width(30);
    out << std::left << (*x)->acct->as_str();

    if ((*x)->cost && (! shortcut || x == xacts.begin())) {
      out << "  ";
      out.width(10);
      out << std::right << (*x)->cost->as_str(true);
    }

    if (! (*x)->note.empty())
      out << "  ; " << (*x)->note;

    out << std::endl;
  }
  out << std::endl;
}

bool entry::validate(bool show_unaccounted) const
{
  totals balance;

  for (std::list<transaction *>::const_iterator x = xacts.begin();
       x != xacts.end();
       x++)
    if ((*x)->cost)
      balance.credit((*x)->cost->value());

  if (show_unaccounted && balance) {
    std::cerr << "Unaccounted-for balances are:" << std::endl;
    balance.print(std::cerr, 20);
    std::cerr << std::endl << std::endl;
  }
  return ! balance;             // must balance to 0.0
}

bool entry::matches(const std::list<mask>& regexps) const
{
  if (regexps.empty() || (ledger::matches(regexps, code) ||
			  ledger::matches(regexps, desc))) {
    return true;
  }
  else {
    bool match = false;

    for (std::list<transaction *>::const_iterator x = xacts.begin();
	 x != xacts.end();
	 x++) {
      if (ledger::matches(regexps, (*x)->acct->name) ||
	  ledger::matches(regexps, (*x)->note)) {
	match = true;
	break;
      }
    }
    return match;
  }
}

#ifdef DO_CLEANUP

totals::~totals()
{
  for (iterator i = amounts.begin(); i != amounts.end(); i++)
    delete (*i).second;
}

#endif // DO_CLEANUP

void totals::credit(const totals& other)
{
  for (const_iterator i = other.amounts.begin();
       i != other.amounts.end();
       i++)
    credit((*i).second);
}

totals::operator bool() const
{
  for (const_iterator i = amounts.begin(); i != amounts.end(); i++)
    if (*((*i).second))
      return true;
  return false;
}

void totals::print(std::ostream& out, int width) const
{
  bool first = true;
  for (const_iterator i = amounts.begin(); i != amounts.end(); i++)
    if (*((*i).second)) {
      if (first)
	first = false;
      else
	out << std::endl;

      out.width(width);
      out << std::right << *((*i).second);
    }
}

// Print out the entire ledger that was read in, sorted by date.
// This can be used to "wash" ugly ledger files.

void print_ledger(int argc, char *argv[], std::ostream& out)
{
  bool use_shortcuts = true;

  optind = 1;

  int c;
  while (-1 != (c = getopt(argc, argv, "n"))) {
    switch (char(c)) {
    case 'n': use_shortcuts = false; break;
    }
  }

  // Compile the list of specified regular expressions, which can be
  // specified on the command line, or using an include/exclude file

  for (; optind < argc; optind++)
    record_regexp(argv[optind], regexps);

  // Sort the list of entries by date, then print them in order.

  std::sort(main_ledger.entries.begin(), main_ledger.entries.end(),
	    cmp_entry_date());

  for (entries_iterator i = main_ledger.entries.begin();
       i != main_ledger.entries.end();
       i++)
    if ((*i)->matches(regexps))
      (*i)->print(out, use_shortcuts);
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

bool matches(const std::list<mask>& regexps, const std::string& str,
	     bool * exclude)
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
		  0, 0, ovec, 3) >= 0) {
      if (exclude)
	*exclude = (*r).exclude;
      match = ! (*r).exclude;
    }
  }

  return match;
}

#ifdef DO_CLEANUP

state::~state()
{
  for (commodities_iterator i = commodities.begin();
       i != commodities.end();
       i++)
    delete (*i).second;

  for (accounts_iterator i = accounts.begin();
       i != accounts.end();
       i++)
    delete (*i).second;

  for (entries_iterator i = entries.begin();
       i != entries.end();
       i++)
    delete *i;
}

#endif // DO_CLEANUP

void state::record_price(const char * setting)
{
  char buf[128];
  std::strcpy(buf, setting);

  assert(std::strlen(setting) < 128);

  char * c = buf;
  char * p = std::strchr(buf, '=');
  if (! p) {
    std::cerr << "Warning: Invalid price setting: " << setting << std::endl;
  } else {
    *p++ = '\0';
    prices.amounts.insert(totals::pair(c, create_amount(p)));
  }
}

account * state::find_account(const char * name, bool create)
{
  accounts_iterator i = accounts_cache.find(name);
  if (i != accounts_cache.end())
    return (*i).second;

  char * buf = new char[std::strlen(name) + 1];
  std::strcpy(buf, name);

  account * current = NULL;
  for (char * tok = std::strtok(buf, ":");
       tok;
       tok = std::strtok(NULL, ":")) {
    if (! current) {
      accounts_iterator i = accounts.find(tok);
      if (i == accounts.end()) {
	if (! create)
	  return NULL;
	current = new account(tok);
	accounts.insert(accounts_entry(tok, current));
      } else {
	current = (*i).second;
      }
    } else {
      accounts_iterator i = current->children.find(tok);
      if (i == current->children.end()) {
	if (! create)
	  return NULL;
	current = new account(tok, current);
	current->parent->children.insert(accounts_entry(tok, current));
      } else {
	current = (*i).second;
      }
    }
  }

  delete[] buf;

  if (current) {
    accounts_cache.insert(accounts_entry(name, current));

#ifdef HUQUQULLAH
    if (matches(main_ledger.huquq_categories, name))
      current->exempt_or_necessary = true;
#endif
  }
  return current;
}

} // namespace ledger
