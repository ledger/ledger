#include "ledger.h"

#include <fstream>

namespace ledger {

bool    use_warnings = false;
book * main_ledger;

const std::string transaction::acct_as_str() const
{
  char * begin = NULL;
  char * end = NULL;

  if (is_virtual) {
    if (must_balance) {
      begin = "[";
      end   = "]";
    } else {
      begin = "(";
      end   = ")";
    }
  }

  if (begin)
    return std::string(begin) + acct->as_str() + end;
  else
    return acct->as_str();
}

void transaction::print(std::ostream& out, bool display_quantity,
			bool display_price) const
{
  out.width(30);
  out << std::left << acct_as_str();

  if (cost && display_quantity) {
    out << "  ";
    out.width(10);

    std::string value = cost->as_str(true);
    if (! display_price) {
      int index = value.find('@');
      value = std::string(value, index - 1);
    }
    out << std::right << value;
  }

  if (! note.empty())
    out << "  ; " << note;

  out << std::endl;
}

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

  if (shortcut && (xacts.size() != 2 ||
		   (xacts.front()->cost->commdty() !=
		    xacts.back()->cost->commdty())))
    shortcut = false;

  for (std::list<transaction *>::const_iterator x = xacts.begin();
       x != xacts.end();
       x++) {
    if ((*x)->is_virtual && ! (*x)->specified)
      continue;

    out << "    ";

    // jww (2003-10-03): If we are shortcutting, don't print the
    // "per-unit price" of a commodity, if it is not necessary.

    (*x)->print(out, shortcut && x == xacts.begin());
  }

  out << std::endl;
}

bool entry::validate(bool show_unaccounted) const
{
  totals balance;

  for (std::list<transaction *>::const_iterator x = xacts.begin();
       x != xacts.end();
       x++)
    if ((*x)->cost && (*x)->must_balance)
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

totals::~totals()
{
  for (iterator i = amounts.begin(); i != amounts.end(); i++)
    delete (*i).second;
}

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

void book::print(std::ostream& out, regexps_map& regexps,
		  bool shortcut) const
{
  for (entries_list_const_iterator i = entries.begin();
       i != entries.end();
       i++)
    if ((*i)->matches(regexps))
      (*i)->print(out, shortcut);
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

void read_regexps(const std::string& path, regexps_map& regexps)
{
  if (access(path.c_str(), R_OK) != -1) {
    std::ifstream file(path.c_str());

    while (! file.eof()) {
      char buf[80];
      file.getline(buf, 79);
      if (*buf && ! std::isspace(*buf))
	regexps.push_back(mask(buf));
    }
  }
}

bool matches(const regexps_map& regexps, const std::string& str,
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

book::~book()
{
  for (commodities_map_iterator i = commodities.begin();
       i != commodities.end();
       i++)
    delete (*i).second;

  for (accounts_map_iterator i = accounts.begin();
       i != accounts.end();
       i++)
    delete (*i).second;

  for (entries_list_iterator i = entries.begin();
       i != entries.end();
       i++)
    delete *i;
}

account * book::find_account(const std::string& name, bool create)
{
  accounts_map_iterator i = accounts_cache.find(name);
  if (i != accounts_cache.end())
    return (*i).second;

  char * buf = new char[name.length() + 1];
  std::strcpy(buf, name.c_str());

  account * current = NULL;
  for (char * tok = std::strtok(buf, ":");
       tok;
       tok = std::strtok(NULL, ":")) {
    if (! current) {
      accounts_map_iterator i = accounts.find(tok);
      if (i == accounts.end()) {
	if (! create) {
	  delete[] buf;
	  return NULL;
	}
	current = new account(tok);
	accounts.insert(accounts_map_pair(tok, current));
      } else {
	current = (*i).second;
      }
    } else {
      accounts_map_iterator i = current->children.find(tok);
      if (i == current->children.end()) {
	if (! create) {
	  delete[] buf;
	  return NULL;
	}
	current = new account(tok, current);
	current->parent->children.insert(accounts_map_pair(tok, current));
      } else {
	current = (*i).second;
      }
    }
  }

  delete[] buf;

  if (current)
    accounts_cache.insert(accounts_map_pair(name, current));

  return current;
}

} // namespace ledger
