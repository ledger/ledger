/*
 * Copyright (c) 2003-2009, John Wiegley.  All rights reserved.
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
#include "interactive.h"

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

    account = new account_t(this, first);
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

namespace {
  account_t * find_account_re_(account_t * account, const mask_t& regexp)
  {
    if (regexp.match(account->fullname()))
      return account;

    foreach (accounts_map::value_type& pair, account->accounts)
      if (account_t * a = find_account_re_(pair.second, regexp))
	return a;

    return NULL;
  }
}

account_t * account_t::find_account_re(const string& regexp)
{
  return find_account_re_(this, mask_t(regexp));
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

string account_t::partial_name(bool flat) const
{
  string pname = name;

  for (const account_t * acct = parent;
       acct && acct->parent;
       acct = acct->parent) {
    if (! flat) {
      std::size_t count = acct->children_with_flags(ACCOUNT_EXT_MATCHING);
      assert(count > 0);
      if (count > 1)
	break;
    }
    pname = acct->name + ":" + pname;
  }
  return pname;
}

std::ostream& operator<<(std::ostream& out, const account_t& account)
{
  out << account.fullname();
  return out;
}

namespace {
  value_t get_partial_name(call_scope_t& scope)
  {
    in_context_t<account_t> env(scope, "&b");
    return string_value(env->partial_name(env.has(0) ?
					  env.get<bool>(0) : false));
  }

  value_t get_account(account_t& account) { // this gets the name
    return string_value(account.fullname());
  }

  value_t get_account_base(account_t& account) {
    return string_value(account.name);
  }

  value_t get_total(account_t& account) {
    assert(account.xdata_);
    if (account.xdata_->total.is_null())
      return 0L;
    else
      return account.xdata_->total;
  }

  value_t get_count(account_t& account) {
    assert(account.xdata_);
    return long(account.xdata_->total_count);
  }

  value_t get_subcount(account_t& account) {
    assert(account.xdata_);
    return long(account.xdata_->count);
  }

  value_t get_amount(account_t& account) {
    assert(account.xdata_);
    if (account.xdata_->value.is_null())
      return 0L;
    else
      return account.xdata_->value;
  }

  value_t get_depth(account_t& account) {
    return long(account.depth);
  }

  value_t ignore(account_t&) {
    return false;
  }

  value_t get_depth_spacer(account_t& account)
  {
    std::size_t depth = 0;
    for (const account_t * acct = account.parent;
	 acct && acct->parent;
	 acct = acct->parent) {
      std::size_t count = acct->children_with_flags(ACCOUNT_EXT_MATCHING);
      assert(count > 0);
      if (count > 1)
	depth++;
    }

    std::ostringstream out;
    for (std::size_t i = 0; i < depth; i++)
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
    else if (name == "account")
      return WRAP_FUNCTOR(get_wrapper<&get_account>);
    else if (name == "account_base")
      return WRAP_FUNCTOR(get_wrapper<&get_account_base>);
    break;

  case 'c':
    if (name == "count")
      return WRAP_FUNCTOR(get_wrapper<&get_count>);
    break;

  case 'd':
    if (name == "depth")
      return WRAP_FUNCTOR(get_wrapper<&get_depth>);
    else if (name == "depth_spacer")
      return WRAP_FUNCTOR(get_wrapper<&get_depth_spacer>);
    break;

  case 'p':
    if (name == "partial_account")
      return WRAP_FUNCTOR(get_partial_name);
    break;

  case 's':
    if (name == "subcount")
      return WRAP_FUNCTOR(get_wrapper<&get_subcount>);
    break;

  case 't':
    if (name == "total")
      return WRAP_FUNCTOR(get_wrapper<&get_total>);
    break;

  case 'u':
    if (name == "use_direct_amount")
      return WRAP_FUNCTOR(get_wrapper<&ignore>);
    break;
  }

  return NULL;
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

std::size_t account_t::children_with_flags(xdata_t::flags_t flags) const
{
  std::size_t count = 0;
  bool        grandchildren_visited = false;

  foreach (const accounts_map::value_type& pair, accounts) {
    if (pair.second->has_flags(flags) ||
	pair.second->children_with_flags(flags))
      count++;
  }

  // Although no immediately children were visited, if any progeny at all were
  // visited, it counts as one.
  if (count == 0 && grandchildren_visited)
    count = 1;

  return count;
}

void account_t::calculate_sums(expr_t& amount_expr)
{
  xdata_t& xd(xdata());

  foreach (accounts_map::value_type& pair, accounts) {
    (*pair.second).calculate_sums(amount_expr);

    xdata_t& child_xd((*pair.second).xdata());
    if (! child_xd.total.is_null()) {
      add_or_set_value(xd.total, child_xd.total);
      xd.total_count += child_xd.total_count;
    } else {
      assert(child_xd.total_count == 0);
      assert(child_xd.count == 0);
    }
  }

  bind_scope_t bound_scope(*amount_expr.get_context(), *this);
  value_t amount(amount_expr.calc(bound_scope));

  if (! amount.is_null()) {
    DEBUG("account.sums", "Added " << amount << " to " << fullname());
    add_or_set_value(xd.total, amount);
    xd.total_count += xd.count;
  } else {
    assert(xd.count == 0);
  }
}

} // namespace ledger
