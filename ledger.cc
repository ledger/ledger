#include "ledger.h"

#include <fstream>

namespace ledger {

book * main_ledger;

commodity::~commodity()
{
  if (price)
    delete price;
}

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

  // jww (2003-10-11): Unspecified virtual transactions should not
  // factor into the size computation.

  commodity * comm = NULL;
  int         size = 0;
  
  for (std::list<transaction *>::const_iterator x = xacts.begin();
       x != xacts.end();
       x++) {
    if ((*x)->is_virtual && ! (*x)->specified)
      continue;

    if (! comm) {
      comm = (*x)->cost->commdty();
    }
    else if (comm != (*x)->cost->commdty()) {
      shortcut = false;
      break;
    }

    size++;
  }

  if (shortcut && size != 2)
    shortcut = false;

  for (std::list<transaction *>::const_iterator x = xacts.begin();
       x != xacts.end();
       x++) {
    if ((*x)->is_virtual && ! (*x)->specified)
      continue;

    out << "    ";

    // jww (2003-10-03): If we are shortcutting, don't print the
    // "per-unit price" of a commodity, if it is not necessary.

    (*x)->print(out, ! shortcut || x == xacts.begin());
  }

  out << std::endl;
}

bool entry::validate(bool show_unaccounted) const
{
  totals balance;

  for (std::list<transaction *>::const_iterator x = xacts.begin();
       x != xacts.end();
       x++)
    if ((*x)->cost && (*x)->must_balance) {
      amount * value = (*x)->cost->value();
      balance.credit(value);
      delete value;
    }

  if (show_unaccounted && ! balance.is_zero()) {
    std::cerr << "Unaccounted-for balances are:" << std::endl;
    balance.print(std::cerr, 20);
    std::cerr << std::endl << std::endl;
  }
  return balance.is_zero();     // must balance to 0.0
}

bool entry::finalize(bool do_compute)
{
  // Scan through and compute the total balance for the entry.  This
  // is used for auto-calculating the value of entries with no cost,
  // and the per-unit price of unpriced commodities.

  totals balance;

  for (std::list<transaction *>::iterator x = xacts.begin();
       x != xacts.end();
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
    for (std::list<transaction *>::iterator x = xacts.begin();
	 x != xacts.end();
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

  for (std::list<transaction *>::iterator x = xacts.begin();
       x != xacts.end();
       x++) {
    if ((*x)->is_virtual || (*x)->cost)
      continue;

    if (! empty_allowed || balance.amounts.empty() ||
	balance.amounts.size() != 1) {
      std::cerr << "Error, line " //<< linenum
		<< ": Transaction entry is lacking an amount."
		<< std::endl;
      return false;
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

  for (book::virtual_map_iterator m = main_ledger->virtual_mapping.begin();
       m != main_ledger->virtual_mapping.end();
       m++) {
    std::list<transaction *> xacts;

    for (std::list<transaction *>::iterator x = xacts.begin();
	 x != xacts.end();
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
      xacts.push_back(*x);

      if (do_compute)
	(*x)->acct->balance.credit((*x)->cost);
    }
  }

  // Compute the balances again, just to make sure it all comes out
  // right (i.e., zero for every commodity).

  if (! validate()) {
    std::cerr << "Error, line " //<< (linenum - 1)
	      << ": Failed to balance the following transaction:"
	      << std::endl;
    validate(true);
    return false;
  }

  return true;
}

bool entry::matches(const regexps_map& regexps) const
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

void totals::credit(const amount * val)
{
  iterator i = amounts.find(val->commdty());
  if (i != amounts.end())
    (*i).second->credit(val);
#ifndef DEBUG
  else
    amounts.insert(pair(val->commdty(), val->copy()));
#else
  else {
    std::pair<iterator, bool> result =
      amounts.insert(pair(val->commdty(), val->copy()));
    assert(result.second);
  }
#endif
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

account::~account()
{
  for (accounts_map_iterator i = children.begin();
       i != children.end();
       i++)
    delete (*i).second;
}

const std::string account::as_str() const
{
  if (! parent)
    return name;
  else if (full_name.empty())
    full_name = parent->as_str() + ":" + name;

  return full_name;
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

mask::mask(const mask& m) : exclude(m.exclude), pattern(m.pattern)
{
  const char *error;
  int erroffset;
  regexp = pcre_compile(pattern.c_str(), PCRE_CASELESS,
			&error, &erroffset, NULL);
  assert(regexp);
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

bool mask::match(const std::string& str) const
{
  static int ovec[30];
  int result = pcre_exec(regexp, NULL, str.c_str(), str.length(),
			 0, 0, ovec, 30);
  return result >= 0 && ! exclude;
}

bool matches(const regexps_map& regexps, const std::string& str,
	     bool * by_exclusion)
{
  assert(! regexps.empty());

  bool match    = false;
  bool definite = false;

//  std::ofstream out("regex.out", std::ios_base::app);
//  out << "Matching against: " << str << std::endl;

  for (regexps_map_const_iterator r = regexps.begin();
       r != regexps.end();
       r++) {
//    out << "  Trying: " << (*r).pattern << std::endl;

    static int ovec[30];
    int result = pcre_exec((*r).regexp, NULL, str.c_str(), str.length(),
			   0, 0, ovec, 30);
    if (result >= 0) {
//      out << "    Definite ";

      match = ! (*r).exclude;
//      if (match)
//	out << "match";
//      else
//	out << "unmatch";

      definite = true;
    } else {
      assert(result == -1);

//      out << "    failure code = " << result << std::endl;

      if ((*r).exclude) {
	if (! match) {
	  match = ! definite;
//	  if (match)
//	    out << "    indefinite match by exclusion" << std::endl;
	}
      } else {
	definite = true;
      }
    }

//    out << "  Current status: "
//	  << (definite ? "definite " : "")
//	  << (match ? "match" : "not match") << std::endl;
  }

  if (by_exclusion) {
    if (match && ! definite && by_exclusion) {
//      out << "  Note: Matched by exclusion rule" << std::endl;
      *by_exclusion = true;
    } else {
      *by_exclusion = false;
    }
  }

//  out << "  Final result: " << (match ? "match" : "not match")
//	<< std::endl;

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

  for (virtual_map_iterator i = virtual_mapping.begin();
       i != virtual_mapping.end();
       i++) {
    delete (*i).first;

    for (std::list<transaction *>::iterator j = (*i).second->begin();
	 j != (*i).second->end();
	 j++) {
      delete *j;
    }
    delete (*i).second;
  }

  for (entries_list_iterator i = entries.begin();
       i != entries.end();
       i++)
    delete *i;
}

account * book::re_find_account(const std::string& regex)
{
  mask acct_regex(regex);

  for (entries_list_reverse_iterator i = entries.rbegin();
       i != entries.rend();
       i++)
    for (std::list<transaction *>::iterator x = (*i)->xacts.begin();
         x != (*i)->xacts.end();
         x++)
      if (acct_regex.match((*x)->acct->as_str()))
        return (*x)->acct;

  return NULL;
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
