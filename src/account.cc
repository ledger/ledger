/*
 * Copyright (c) 2003-2010, John Wiegley.  All rights reserved.
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

namespace ledger {

account_t::~account_t()
{
  TRACE_DTOR(account_t);

  foreach (accounts_map::value_type& pair, accounts) {
    if (! pair.second->has_flags(ACCOUNT_TEMP) ||
        has_flags(ACCOUNT_TEMP)) {
      checked_delete(pair.second);
    }      
  }
}

account_t * account_t::find_account(const string& name,
                                    const bool    auto_create)
{
  accounts_map::const_iterator i = accounts.find(name);
  if (i != accounts.end())
    return (*i).second;

  char buf[8192];

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

    // An account created within a temporary or generated account is itself
    // temporary or generated, so that the whole tree has the same status.
    if (has_flags(ACCOUNT_TEMP))
      account->add_flags(ACCOUNT_TEMP);
    if (has_flags(ACCOUNT_GENERATED))
      account->add_flags(ACCOUNT_GENERATED);

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

void account_t::add_post(post_t * post)
{
  posts.push_back(post);

  // Adding a new post changes the possible totals that may have been
  // computed before.
  if (xdata_) {
    xdata_->self_details.gathered     = false;
    xdata_->self_details.calculated   = false;
    xdata_->family_details.gathered   = false;
    xdata_->family_details.calculated = false;
  }
}

bool account_t::remove_post(post_t * post)
{
  assert(! posts.empty());
  posts.remove(post);
  post->account = NULL;
  return true;
}

string account_t::fullname() const
{
  if (! _fullname.empty()) {
    return _fullname;
  } else {
    const account_t *   first    = this;
    string              fullname = name;

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
      if (count > 1 || acct->has_xflags(ACCOUNT_EXT_TO_DISPLAY))
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
  value_t get_partial_name(call_scope_t& args)
  {
    return string_value(args.context<account_t>()
                        .partial_name(args.has<bool>(0) &&
                                      args.get<bool>(0)));
  }

  value_t get_account(call_scope_t& args) { // this gets the name
    account_t& account(args.context<account_t>());
    if (args.has<string>(0)) {
      account_t * acct = account.parent;
      for (; acct && acct->parent; acct = acct->parent) ;
      if (args[0].is_string())
        return scope_value(acct->find_account(args.get<string>(0), false));
      else if (args[0].is_mask())
        return scope_value(acct->find_account_re(args.get<mask_t>(0).str()));
      else
        return NULL_VALUE;
    }
    else if (args.type_context() == value_t::SCOPE) {
      return scope_value(&account);
    }
    else {
      return string_value(account.fullname());
    }
  }

  value_t get_account_base(account_t& account) {
    return string_value(account.name);
  }

  value_t get_amount(account_t& account) {
    return SIMPLIFIED_VALUE_OR_ZERO(account.amount());
  }

  value_t get_total(account_t& account) {
    return SIMPLIFIED_VALUE_OR_ZERO(account.total());
  }

  value_t get_subcount(account_t& account) {
    return long(account.self_details().posts_count);
  }

  value_t get_count(account_t& account) {
    return long(account.family_details().posts_count);
  }
  value_t get_cost(account_t&) {
    throw_(calc_error, _("An account does not have a 'cost' value"));
    return false;
  }

  value_t get_depth(account_t& account) {
    return long(account.depth);
  }

  value_t ignore(account_t&) {
    return false;
  }

  value_t get_true(account_t&) {
    return true;
  }

  value_t get_addr(account_t& account) {
    return long(&account);
  }

  value_t get_depth_spacer(account_t& account)
  {
    std::size_t depth = 0;
    for (const account_t * acct = account.parent;
         acct && acct->parent;
         acct = acct->parent) {
      std::size_t count = acct->children_with_flags(ACCOUNT_EXT_TO_DISPLAY);
      assert(count > 0);
      if (count > 1 || acct->has_xflags(ACCOUNT_EXT_TO_DISPLAY))
        depth++;
    }

    std::ostringstream out;
    for (std::size_t i = 0; i < depth; i++)
      out << "  ";

    return string_value(out.str());
  }

  value_t get_latest_cleared(account_t& account)
  {
    return account.self_details().latest_cleared_post;
  }

  template <value_t (*Func)(account_t&)>
  value_t get_wrapper(call_scope_t& args) {
    return (*Func)(args.context<account_t>());
  }

  value_t get_parent(account_t& account) {
    return scope_value(account.parent);
  }

  value_t fn_any(call_scope_t& args)
  {
    account_t& account(args.context<account_t>());
    expr_t::ptr_op_t expr(args.get<expr_t::ptr_op_t>(0));

    foreach (post_t * p, account.posts) {
      bind_scope_t bound_scope(args, *p);
      if (expr->calc(bound_scope, args.locus, args.depth).to_boolean())
        return true;
    }
    return false;
  }

  value_t fn_all(call_scope_t& args)
  {
    account_t& account(args.context<account_t>());
    expr_t::ptr_op_t expr(args.get<expr_t::ptr_op_t>(0));

    foreach (post_t * p, account.posts) {
      bind_scope_t bound_scope(args, *p);
      if (! expr->calc(bound_scope, args.locus, args.depth).to_boolean())
        return false;
    }
    return true;
  }
}

expr_t::ptr_op_t account_t::lookup(const symbol_t::kind_t kind,
                                   const string& name)
{
  if (kind != symbol_t::FUNCTION)
    return NULL;

  switch (name[0]) {
  case 'a':
    if (name[1] == '\0' || name == "amount")
      return WRAP_FUNCTOR(get_wrapper<&get_amount>);
    else if (name == "account")
      return WRAP_FUNCTOR(&get_account);
    else if (name == "account_base")
      return WRAP_FUNCTOR(get_wrapper<&get_account_base>);
    else if (name == "addr")
      return WRAP_FUNCTOR(get_wrapper<&get_addr>);
    else if (name == "any")
      return WRAP_FUNCTOR(&fn_any);
    else if (name == "all")
      return WRAP_FUNCTOR(&fn_all);
    break;

  case 'c':
    if (name == "count")
      return WRAP_FUNCTOR(get_wrapper<&get_count>);
    else if (name == "cost")
      return WRAP_FUNCTOR(get_wrapper<&get_cost>);
    break;

  case 'd':
    if (name == "depth")
      return WRAP_FUNCTOR(get_wrapper<&get_depth>);
    else if (name == "depth_spacer")
      return WRAP_FUNCTOR(get_wrapper<&get_depth_spacer>);
    break;

  case 'i':
    if (name == "is_account")
      return WRAP_FUNCTOR(get_wrapper<&get_true>);
    else if (name == "is_index")
      return WRAP_FUNCTOR(get_wrapper<&get_subcount>);
    break;

  case 'l':
    if (name == "latest_cleared")
      return WRAP_FUNCTOR(get_wrapper<&get_latest_cleared>);
    else if (name[1] == '\0')
      return WRAP_FUNCTOR(get_wrapper<&get_depth>);
    break;

  case 'n':
    if (name[1] == '\0')
      return WRAP_FUNCTOR(get_wrapper<&get_subcount>);
    break;

  case 'p':
    if (name == "partial_account")
      return WRAP_FUNCTOR(get_partial_name);
    else if (name == "parent")
      return WRAP_FUNCTOR(get_wrapper<&get_parent>);
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

  case 'N':
    if (name[1] == '\0')
      return WRAP_FUNCTOR(get_wrapper<&get_count>);
    break;

  case 'O':
    if (name[1] == '\0')
      return WRAP_FUNCTOR(get_wrapper<&get_total>);
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

bool account_t::children_with_xdata() const
{
  foreach (const accounts_map::value_type& pair, accounts)
    if (pair.second->has_xdata() ||
        pair.second->children_with_xdata())
      return true;

  return false;
}

std::size_t account_t::children_with_flags(xdata_t::flags_t flags) const
{
  std::size_t count = 0;
  bool        grandchildren_visited = false;

  foreach (const accounts_map::value_type& pair, accounts)
    if (pair.second->has_xflags(flags) ||
        pair.second->children_with_flags(flags))
      count++;

  // Although no immediately children were visited, if any progeny at all were
  // visited, it counts as one.
  if (count == 0 && grandchildren_visited)
    count = 1;

  return count;
}

account_t::xdata_t::details_t&
account_t::xdata_t::details_t::operator+=(const details_t& other)
{
  posts_count            += other.posts_count;
  posts_virtuals_count   += other.posts_virtuals_count;
  posts_cleared_count    += other.posts_cleared_count;
  posts_last_7_count     += other.posts_last_7_count;
  posts_last_30_count    += other.posts_last_30_count;
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

void account_t::clear_xdata()
{
  xdata_ = none;

  foreach (accounts_map::value_type& pair, accounts)
    if (! pair.second->has_flags(ACCOUNT_TEMP))
      pair.second->clear_xdata();
}

value_t account_t::amount(const optional<expr_t&>& expr) const
{
  if (xdata_ && xdata_->has_flags(ACCOUNT_EXT_VISITED)) {
    posts_list::const_iterator i;
    if (xdata_->self_details.last_post)
      i = *xdata_->self_details.last_post;
    else
      i = posts.begin();

    for (; i != posts.end(); i++) {
      if ((*i)->xdata().has_flags(POST_EXT_VISITED)) {
        if (! (*i)->xdata().has_flags(POST_EXT_CONSIDERED)) {
          (*i)->add_to_value(xdata_->self_details.total, expr);
          (*i)->xdata().add_flags(POST_EXT_CONSIDERED);
        }
      }
      xdata_->self_details.last_post = i;
    }

    if (xdata_->self_details.last_reported_post)
      i = *xdata_->self_details.last_reported_post;
    else
      i = xdata_->reported_posts.begin();

    for (; i != xdata_->reported_posts.end(); i++) {
      if ((*i)->xdata().has_flags(POST_EXT_VISITED)) {
        if (! (*i)->xdata().has_flags(POST_EXT_CONSIDERED)) {
          (*i)->add_to_value(xdata_->self_details.total, expr);
          (*i)->xdata().add_flags(POST_EXT_CONSIDERED);
        }
      }
      xdata_->self_details.last_reported_post = i;
    }

    return xdata_->self_details.total;
  } else {
    return NULL_VALUE;
  }
}

value_t account_t::total(const optional<expr_t&>& expr) const
{
  if (! (xdata_ && xdata_->family_details.calculated)) {
    const_cast<account_t&>(*this).xdata().family_details.calculated = true;

    value_t temp;
    foreach (const accounts_map::value_type& pair, accounts) {
      temp = pair.second->total(expr);
      if (! temp.is_null())
        add_or_set_value(xdata_->family_details.total, temp);
    }

    temp = amount(expr);
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
                                           bool    gather_all)
{
  posts_count++;

  if (post.has_flags(POST_VIRTUAL))
    posts_virtuals_count++;

  if (gather_all && post.pos)
    filenames.insert(post.pos->pathname);

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
