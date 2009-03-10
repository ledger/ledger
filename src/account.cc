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

#include <system.hh>

#include "account.h"
#include "post.h"
#include "xact.h"
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
      std::size_t count = acct->children_with_flags(ACCOUNT_EXT_TO_DISPLAY);
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

  value_t get_amount(account_t& account) {
    return VALUE_OR_ZERO(account.self_total());
  }

  value_t get_total(account_t& account) {
    return VALUE_OR_ZERO(account.family_total());
  }

  value_t get_subcount(account_t& account) {
    return long(account.self_details().posts_count);
  }

  value_t get_count(account_t& account) {
    return long(account.family_details().posts_count);
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
      std::size_t count = acct->children_with_flags(ACCOUNT_EXT_TO_DISPLAY);
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

account_t::xdata_t::details_t&
account_t::xdata_t::details_t::operator+=(const details_t& other)
{
  posts_count    	 += other.posts_count;
  posts_virtuals_count	 += other.posts_virtuals_count;
  posts_cleared_count	 += other.posts_cleared_count;
  posts_last_7_count	 += other.posts_last_7_count;
  posts_last_30_count	 += other.posts_last_30_count;
  posts_this_month_count += other.posts_this_month_count;

  if (! is_valid(earliest_post) ||
      (is_valid(other.earliest_post) &&
       other.earliest_post < earliest_post))
    earliest_post = other.earliest_post;
  if (! is_valid(earliest_cleared_post) ||
      (is_valid(other.earliest_cleared_post) &&
       other.earliest_cleared_post < earliest_cleared_post))
    earliest_cleared_post = other.earliest_cleared_post;

  if (! is_valid(latest_post) ||
      (is_valid(other.latest_post) &&
       other.latest_post > latest_post))
    latest_post = other.latest_post;
  if (! is_valid(latest_cleared_post) ||
      (is_valid(other.latest_cleared_post) &&
       other.latest_cleared_post > latest_cleared_post))
    latest_cleared_post = other.latest_cleared_post;

  filenames.insert(other.filenames.begin(), other.filenames.end());
  accounts_referenced.insert(other.accounts_referenced.begin(),
			     other.accounts_referenced.end());
  payees_referenced.insert(other.payees_referenced.begin(),
			   other.payees_referenced.end());
  return *this;
}

value_t account_t::self_total(const optional<expr_t&>& expr) const
{
  if (xdata_ && xdata_->has_flags(ACCOUNT_EXT_VISITED)) {
    if (! xdata_) xdata_ = xdata_t();

    posts_deque::const_iterator i =
      posts.begin() + xdata_->self_details.last_size;

    for (; i != posts.end(); i++) {
      if ((*i)->xdata().has_flags(POST_EXT_VISITED) &&
	  ! (*i)->xdata().has_flags(POST_EXT_CONSIDERED)) {
	(*i)->add_to_value(xdata_->self_details.total, expr);
	(*i)->xdata().add_flags(POST_EXT_CONSIDERED);
      }
    }

    xdata_->self_details.last_size = posts.size();

    return xdata_->self_details.total;
  } else {
    return NULL_VALUE;
  }
}

value_t account_t::family_total(const optional<expr_t&>& expr) const
{
  if (! (xdata_ && xdata_->family_details.calculated)) {
    const_cast<account_t&>(*this).xdata().family_details.calculated = true;

    value_t temp;
    foreach (const accounts_map::value_type& pair, accounts) {
      temp = pair.second->family_total(expr);
      if (! temp.is_null())
	add_or_set_value(xdata_->family_details.total, temp);
    }

    temp = self_total(expr);
    if (! temp.is_null())
      add_or_set_value(xdata_->family_details.total, temp);
  }
  return xdata_->family_details.total;
}

const account_t::xdata_t::details_t&
account_t::self_details(bool gather_all) const
{
  if (! (xdata_ && xdata_->self_details.gathered)) {
    const_cast<account_t&>(*this).xdata().self_details.gathered = true;

    foreach (const post_t * post, posts)
      xdata_->self_details.update(const_cast<post_t&>(*post), gather_all);
  }
  return xdata_->self_details;
}

const account_t::xdata_t::details_t&
account_t::family_details(bool gather_all) const
{
  if (! (xdata_ && xdata_->family_details.gathered)) {
    const_cast<account_t&>(*this).xdata().family_details.gathered = true;

    foreach (const accounts_map::value_type& pair, accounts)
      xdata_->family_details += pair.second->family_details(gather_all);

    xdata_->family_details += self_details(gather_all);
  }
  return xdata_->family_details;
}

void account_t::xdata_t::details_t::update(post_t& post,
					   bool	   gather_all)
{
  posts_count++;

  if (post.has_flags(POST_VIRTUAL))
    posts_virtuals_count++;

  if (gather_all)
    filenames.insert(post.pathname);

  date_t date = post.date();

  if (date.year() == CURRENT_DATE().year() &&
      date.month() == CURRENT_DATE().month())
    posts_this_month_count++;

  if ((CURRENT_DATE() - date).days() <= 30)
    posts_last_30_count++;
  if ((CURRENT_DATE() - date).days() <= 7)
    posts_last_7_count++;

  if (! is_valid(earliest_post) || post.date() < earliest_post)
    earliest_post = post.date();
  if (! is_valid(latest_post) || post.date() > latest_post)
    latest_post = post.date();

  if (post.state() == item_t::CLEARED) {
    posts_cleared_count++;

    if (! is_valid(earliest_cleared_post) ||
	post.date() < earliest_cleared_post)
      earliest_cleared_post = post.date();
    if (! is_valid(latest_cleared_post) ||
	post.date() > latest_cleared_post)
      latest_cleared_post = post.date();
  }

  if (gather_all) {
    accounts_referenced.insert(post.account->fullname());
    payees_referenced.insert(post.xact->payee);
  }
}

} // namespace ledger
