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

account_t * account_t::find_account(const std::string& ident,
				    const bool	       auto_create)
{
  accounts_map::const_iterator c = accounts_cache.find(ident);
  if (c != accounts_cache.end())
    return (*c).second;

  accounts_map::const_iterator i = accounts.find(ident);
  if (i != accounts.end())
    return (*i).second;

  static char buf[256];

  std::string::size_type sep = ident.find(':');
  const char * first, * rest;
  if (sep == std::string::npos) {
    first = ident.c_str();
    rest  = NULL;
  } else {
    std::strncpy(buf, ident.c_str(), sep);
    buf[sep] = '\0';

    first = buf;
    rest  = ident.c_str() + sep + 1;
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

  accounts_cache.insert(accounts_pair(ident, account));

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
  const account_t *	first	 = this;
  std::string		fullname = name;

  while (first->parent) {
    first = first->parent;
    if (! first->name.empty())
      fullname = first->name + ":" + fullname;
  }

  return fullname;
}

} // namespace ledger
