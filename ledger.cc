#include "ledger.h"

#include <fstream>
#include <unistd.h>

namespace ledger {

book * main_ledger;

extern int linenum;

commodity::~commodity()
{
  if (conversion)
    delete conversion;

  for (price_map::iterator i = history.begin();
       i != history.end();
       i++)
    delete (*i).second;
}

void commodity::set_price(amount * price, std::time_t * when)
{
  assert(price);
  if (when)
    history.insert(price_map_pair(*when, price));
  else
    conversion = price;
}

amount * commodity::price(std::time_t * when,
			  bool use_history, bool download) const
{
  if (conversion || ! when || ! use_history)
    return conversion;

  std::time_t age;
  amount *    price = NULL;
  
  for (price_map::reverse_iterator i = history.rbegin();
       i != history.rend();
       i++)
    if (std::difftime(*when, (*i).first) >= 0) {
      age   = (*i).first;
      price = (*i).second;
      break;
    }

  extern long pricing_leeway;
  time_t now = time(NULL);	// the time of the query

  if (download && ! sought &&
      std::difftime(now, *when) < pricing_leeway &&
      (! price || std::difftime(*when, age) > pricing_leeway)) {
    using namespace std;

    // Only consult the Internet once for any commodity
    sought = true;

    char buf[256];
    buf[0] = '\0';

    if (FILE * fp = popen((string("getquote ") + symbol).c_str(), "r")) {
      if (feof(fp) || ! fgets(buf, 255, fp)) {
	fclose(fp);
	return price;
      }
      fclose(fp);
    }

    if (buf[0]) {
      char * p = strchr(buf, '\n');
      if (p) *p = '\0';

      price = create_amount(buf);
      const_cast<commodity *>(this)->set_price(price, &now);

      extern string price_db;
      if (! price_db.empty()) {
	char buf[128];
	strftime(buf, 127, "%Y/%m/%d %H:%M:%S", localtime(&now));
	ofstream database(price_db.c_str(), ios_base::out | ios_base::app);
	database << "P " << buf << " " << symbol << " "
		 << price->as_str() << endl;
      }
    }
  }

  return price;
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
    out.width(12);

    std::string value = cost->as_str(true);
    if (! display_price) {
      int index = value.find('@');
      if (index != -1)
	value = std::string(value, 0, index - 1);
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
  std::strftime(buf, 31, "%Y/%m/%d ", std::localtime(&date));
  out << buf;

  if (cleared)
    out << "* ";
  if (! code.empty())
    out << '(' << code << ") ";
  if (! desc.empty())
    out << desc;

  out << std::endl;

  commodity * 	comm = NULL;
  int 		size = 0;
  
  for (std::list<transaction *>::const_iterator x = xacts.begin();
       x != xacts.end();
       x++) {
    if ((*x)->is_virtual && ! (*x)->must_balance)
      continue;

    if (! comm)
      comm = (*x)->cost->commdty();
    else if (comm != (*x)->cost->commdty())
      shortcut = false;

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

    (*x)->print(out, (! shortcut || x == xacts.begin() ||
		      ((*x)->is_virtual && ! (*x)->must_balance)),
		size != 2);
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

  // If one transaction of a two-line transaction is of a different
  // commodity than the others, and it has no per-unit price,
  // determine its price by dividing the unit count into the value of
  // the balance.  This is done for the last eligible commodity.

  if (! balance.amounts.empty() && balance.amounts.size() == 2) {
    for (std::list<transaction *>::iterator x = xacts.begin();
	 x != xacts.end();
	 x++) {
      if ((*x)->is_virtual || (*x)->cost->has_price())
	continue;

      for (totals::iterator i = balance.amounts.begin();
	   i != balance.amounts.end();
	   i++)
	if ((*i).second->commdty() != (*x)->cost->commdty()) {
	  (*x)->cost->set_value((*i).second);
	  assert((*x)->cost->has_price());
	  (*x)->cost->commdty()->set_price((*x)->cost->per_item_price(),
					   &date);
	  break;
	}

      break;
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
      std::cerr << "Error, line " << linenum
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

  // If automated transactions are being used, walk through the
  // current transaction lines and create new transactions for all
  // that match.

  for (book::virtual_map_iterator m = ledger->virtual_mapping.begin();
       m != ledger->virtual_mapping.end();
       m++) {
    std::list<transaction *> new_xacts;

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

        new_xacts.push_back(t);
      }
    }

    // Add to the current entry any virtual transactions which were
    // created.  We have to do this afterward, otherwise the
    // iteration above is screwed up if we try adding new
    // transactions during the traversal.

    for (std::list<transaction *>::iterator x = new_xacts.begin();
	 x != new_xacts.end();
	 x++) {
      xacts.push_back(*x);

      if (do_compute)
	(*x)->acct->balance.credit((*x)->cost);
    }
  }

  // Compute the balances again, just to make sure it all comes out
  // right (i.e., zero for every commodity).

  if (! validate()) {
    std::cerr << "Error, line " << (linenum - 1)
	      << ": Failed to balance the following transaction:"
	      << std::endl;
    validate(true);
    return false;
  }

  return true;
}

bool entry::matches(const regexps_list& regexps) const
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
      if (ledger::matches(regexps, (*x)->acct->as_str()) ||
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

void totals::negate()
{
  for (const_iterator i = amounts.begin(); i != amounts.end(); i++)
    (*i).second->negate();
}

bool totals::is_zero() const
{
  for (const_iterator i = amounts.begin(); i != amounts.end(); i++)
    if (! (*i).second->is_zero())
      return false;
  return true;
}

bool totals::is_negative() const
{
  bool all_negative = true;
  bool some_negative = false;
  for (const_iterator i = amounts.begin(); i != amounts.end(); i++) {
    if ((*i).second->is_negative())
      some_negative = true;
    else if (! (*i).second->is_zero())
      all_negative = false;
  }
  return some_negative && all_negative;
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

totals * totals::value() const
{
  totals * cost_basis = new totals;

  for (const_iterator i = amounts.begin(); i != amounts.end(); i++) {
    if ((*i).second->is_zero())
      continue;

    amount * value = (*i).second->value();
    cost_basis->credit(value);
    delete value;
  }

  return cost_basis;
}

totals * totals::street(std::time_t * when, bool use_history,
			bool download) const
{
  totals * street_balance = new totals;

  for (const_iterator i = amounts.begin(); i != amounts.end(); i++) {
    if ((*i).second->is_zero())
      continue;

    amount * street = (*i).second->street(when, use_history, download);
    street_balance->credit(street);
    delete street;
  }

  return street_balance;
}

account::~account()
{
  for (accounts_map_iterator i = children.begin();
       i != children.end();
       i++)
    delete (*i).second;
}

const std::string account::as_str(const account * stop) const
{
  if (! parent || this == stop)
    return name;
  else if (stop)
    return parent->as_str(stop) + ":" + name;
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

bool mask::match(const std::string& str) const
{
  static int ovec[30];
  int result = pcre_exec(regexp, NULL, str.c_str(), str.length(),
			 0, 0, ovec, 30);
  return result >= 0 && ! exclude;
}

bool matches(const regexps_list& regexps, const std::string& str,
	     bool * by_exclusion)
{
  if (regexps.empty())
    return false;

  bool match    = false;
  bool definite = false;

  for (regexps_list_const_iterator r = regexps.begin();
       r != regexps.end();
       r++) {
    static int ovec[30];
    int result = pcre_exec((*r).regexp, NULL, str.c_str(), str.length(),
			   0, 0, ovec, 30);
    if (result >= 0) {
      match     = ! (*r).exclude;
      definite  = true;
    }
    else if ((*r).exclude) {
      if (! match)
	match = ! definite;
    }
    else {
      definite = true;
    }
  }

  if (by_exclusion)
    *by_exclusion = match && ! definite && by_exclusion;

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
