#include "ledger.h"

#include <sstream>
#include <deque>

namespace ledger {

unsigned long account_t::next_ident;

account_t::~account_t()
{
  for (accounts_map::iterator i = accounts.begin();
       i != accounts.end();
       i++)
    delete (*i).second;
}

account_t * account_t::find_account(const std::string& name,
				    const bool	       auto_create)
{
  accounts_map::const_iterator i = accounts.find(name);
  if (i != accounts.end())
    return (*i).second;

  static char buf[256];

  std::string::size_type sep = name.find(':');
  const char * first, * rest;
  if (sep == std::string::npos) {
    first = name.c_str();
    rest  = NULL;
  } else {
    std::strncpy(buf, name.c_str(), sep);
    buf[sep] = '\0';

    first = buf;
    rest  = name.c_str() + sep + 1;
  }

  account_t * account;

  i = accounts.find(first);
  if (i == accounts.end()) {
    if (! auto_create)
      return NULL;
    account = new account_t(this, first);
    accounts.insert(accounts_pair(first, account));
  } else {
    account = (*i).second;
  }

  if (rest)
    account = account->find_account(rest, auto_create);

  return account;
}

bool account_t::remove_transaction(transaction_t * xact)
{
  for (transactions_list::iterator i = transactions.begin();
       i != transactions.end();
       i++)
    if (*i == xact) {
      transactions.erase(i);
      return true;
    }

  return false;
}

std::string account_t::fullname() const
{
  if (! _fullname.empty()) {
    return _fullname;
  } else {
    const account_t *	first	 = this;
    std::string		fullname = name;

    while (first->parent) {
      first = first->parent;
      if (! first->name.empty())
	fullname = first->name + ":" + fullname;
    }

    _fullname = fullname;

    return fullname;
  }
}

} // namespace ledger
