/*
 * Copyright (c) 2003-2008, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of New Artisans LLC nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "account.h"
#include "report.h"

namespace ledger {

account_t::~account_t()
{
  TRACE_DTOR(account_t);

  foreach (accounts_map::value_type& pair, accounts)
    checked_delete(pair.second);
}

account_t * account_t::find_account(const string& name,
				    const bool	  auto_create)
{
  accounts_map::const_iterator i = accounts.find(name);
  if (i != accounts.end())
    return (*i).second;

  char buf[256];

  string::size_type sep = name.find(':');
  assert(sep < 256|| sep == string::npos);

  const char * first, * rest;
  if (sep == string::npos) {
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

    account = new account_t(owner, this, first);
    std::pair<accounts_map::iterator, bool> result
      = accounts.insert(accounts_map::value_type(first, account));
    assert(result.second);
  } else {
    account = (*i).second;
  }

  if (rest)
    account = account->find_account(rest, auto_create);

  return account;
}

string account_t::fullname() const
{
  if (! _fullname.empty()) {
    return _fullname;
  } else {
    const account_t *	first	 = this;
    string		fullname = name;

    while (first->parent) {
      first = first->parent;
      if (! first->name.empty())
	fullname = first->name + ":" + fullname;
    }

    _fullname = fullname;

    return fullname;
  }
}

std::ostream& operator<<(std::ostream& out, const account_t& account)
{
  out << account.fullname();
  return out;
}

namespace {
  value_t get_partial_name(account_t& account) {
    string name;

    for (account_t * acct = &account;
	 acct && acct->parent;
	 acct = acct->parent) {
      if (acct->has_xdata() &&
	  acct->xdata().has_flags(ACCOUNT_EXT_DISPLAYED))
	break;

      if (name.empty())
	name = acct->name;
      else
	name = acct->name + ":" + name;
    }

    return string_value(name);
  }

  value_t get_total(account_t& account) {
    assert(account.xdata_);
    return account.xdata_->total;
  }

  value_t get_amount(account_t& account) {
    assert(account.xdata_);
    return account.xdata_->value;
  }

  value_t get_depth_spacer(account_t& account) {
    std::ostringstream out;
    for (account_t * acct = &account;
	 acct;
	 acct = acct->parent)
      if (acct->has_xdata() &&
	  acct->xdata().has_flags(ACCOUNT_EXT_DISPLAYED))
	out << "  ";
    return string_value(out.str());
  }

  template <value_t (*Func)(account_t&)>
  value_t get_wrapper(call_scope_t& scope) {
    return (*Func)(find_scope<account_t>(scope));
  }
}

expr_t::ptr_op_t account_t::lookup(const string& name)
{
  switch (name[0]) {
  case 'a':
    if (name == "amount")
      return WRAP_FUNCTOR(get_wrapper<&get_amount>);
    break;

  case 'f':
    if (name.find("fmt_") == 0) {
      switch (name[4]) {
      case '_':
	return WRAP_FUNCTOR(get_wrapper<&get_depth_spacer>);
      case 'T':
	return WRAP_FUNCTOR(get_wrapper<&get_total>);
      case 'a':
	return WRAP_FUNCTOR(get_wrapper<&get_partial_name>);
      }
    }
    break;

  case 't':
    if (name == "total")
      return WRAP_FUNCTOR(get_wrapper<&get_total>);
    break;
  }
  return expr_t::ptr_op_t();
}

bool account_t::valid() const
{
  if (depth > 256) {
    DEBUG("ledger.validate", "account_t: depth > 256");
    return false;
  }

  foreach (const accounts_map::value_type& pair, accounts) {
    if (this == pair.second) {
      DEBUG("ledger.validate", "account_t: parent refers to itself!");
      return false;
    }

    if (! pair.second->valid()) {
      DEBUG("ledger.validate", "account_t: child not valid");
      return false;
    }
  }

  return true;
}

void account_t::calculate_sums()
{
  xdata_t& xd(xdata());

  foreach (accounts_map::value_type& pair, accounts) {
    (*pair.second).calculate_sums();

    xdata_t& child_xd((*pair.second).xdata());
    add_or_set_value(xd.total, child_xd.total);
    xd.total_count += child_xd.total_count + child_xd.count;
  }

  call_scope_t args(*this);
  value_t amount(owner->current_report->get_amount_expr(args));
  if (! amount.is_null()) {
    add_or_set_value(xd.total, amount);
    xd.total_count += xd.count;
  } else {
    assert(xd.count == 0);
  }
}

} // namespace ledger
