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
    if ((*x)->exempt_or_necessary ||
	(! shortcut && (*x)->acct->exempt_or_necessary))
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

  if (show_unaccounted && ! balance.is_zero()) {
    std::cerr << "Unaccounted-for balances are:" << std::endl;
    balance.print(std::cerr, 20);
    std::cerr << std::endl << std::endl;
  }
  return balance.is_zero();     // must balance to 0.0
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

bool totals::is_zero() const
{
  for (const_iterator i = amounts.begin(); i != amounts.end(); i++)
    if (! (*i).second->is_zero())
      return false;
  return true;
}

void totals::print(std::ostream& out, int width) const
{
  bool first = true;

  for (const_iterator i = amounts.begin(); i != amounts.end(); i++) {
    if ((*i).second->is_zero())
      continue;

    if (first)
      first = false;
    else
      out << std::endl;

    out.width(width);
    out << std::right << (*i).second->as_str();
  }
}

// Print out the entire ledger that was read in, sorted by date.
// This can be used to "wash" ugly ledger files.

void print_ledger(int argc, char ** argv, regexps_t& regexps,
		  std::ostream& out)
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

mask::mask(const std::string& pat) : exclude(false)
{
  const char * p = pat.c_str();
  if (*p == '-') {
    exclude = true;
    p++;
    while (std::isspace(*p))
      p++;
  }
  else if (*p == '+') {
    p++;
    while (std::isspace(*p))
      p++;
  }
  pattern = p;

  const char *error;
  int erroffset;
  regexp = pcre_compile(pattern.c_str(), PCRE_CASELESS,
			&error, &erroffset, NULL);
  if (! regexp)
    std::cerr << "Warning: Failed to compile regexp: " << pattern
	      << std::endl;
}

void record_regexp(const std::string& pattern, regexps_t& regexps)
{
  regexps.push_back(mask(pattern));
}

void read_regexps(const std::string& path, regexps_t& regexps)
{
  if (access(path.c_str(), R_OK) != -1) {
    std::ifstream file(path.c_str());

    while (! file.eof()) {
      char buf[80];
      file.getline(buf, 79);
      if (*buf && ! std::isspace(*buf))
	record_regexp(buf, regexps);
    }
  }
}

bool matches(const regexps_t& regexps, const std::string& str,
	     bool * by_exclusion)
{
  assert(! regexps.empty());

  // If the first pattern is an exclude, we assume all patterns match
  // if they don't match the exclude -- and by_exclusion will be set
  // to true to reflect this "by default" behavior.  But if the first
  // pattern is an include, only accounts matching the include will
  // match, and these are a positive match.

  bool match = (*regexps.begin()).exclude;
  if (match && by_exclusion)
    *by_exclusion = true;

  for (std::list<mask>::const_iterator r = regexps.begin();
       r != regexps.end();
       r++) {
    int ovec[3];
    if (pcre_exec((*r).regexp, NULL, str.c_str(), str.length(),
		  0, 0, ovec, 3) >= 0) {
      if (by_exclusion)
	*by_exclusion = (*r).exclude;
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

void state::record_price(const std::string& setting)
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
    prices.amounts.insert(totals::pair(c, create_amount(p)));
  }
}

account * state::find_account(const std::string& name, bool create)
{
  accounts_iterator i = accounts_cache.find(name);
  if (i != accounts_cache.end())
    return (*i).second;

  char * buf = new char[name.length() + 1];
  std::strcpy(buf, name.c_str());

  account * current = NULL;
  for (char * tok = std::strtok(buf, ":");
       tok;
       tok = std::strtok(NULL, ":")) {
    if (! current) {
      accounts_iterator i = accounts.find(tok);
      if (i == accounts.end()) {
	if (! create) {
	  delete[] buf;
	  return NULL;
	}
	current = new account(tok);
	accounts.insert(accounts_entry(tok, current));
      } else {
	current = (*i).second;
      }
    } else {
      accounts_iterator i = current->children.find(tok);
      if (i == current->children.end()) {
	if (! create) {
	  delete[] buf;
	  return NULL;
	}
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
