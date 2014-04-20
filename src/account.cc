/*
 * Copyright (c) 2003-2014, John Wiegley.  All rights reserved.
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

account_t * account_t::find_account(const string& acct_name,
                                    const bool    auto_create)
{
  accounts_map::const_iterator i = accounts.find(acct_name);
  if (i != accounts.end())
    return (*i).second;

  char buf[8192];

  string::size_type sep = acct_name.find(':');
  assert(sep < 256|| sep == string::npos);

  const char * first, * rest;
  if (sep == string::npos) {
    first = acct_name.c_str();
    rest  = NULL;
  } else {
    std::strncpy(buf, acct_name.c_str(), sep);
    buf[sep] = '\0';

    first = buf;
    rest  = acct_name.c_str() + sep + 1;
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

#if DEBUG_ON
    std::pair<accounts_map::iterator, bool> result =
#endif
      accounts.insert(accounts_map::value_type(first, account));
#if DEBUG_ON
    assert(result.second);
#endif
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

void account_t::add_deferred_post(const string& uuid, post_t * post)
{
  if (! deferred_posts)
    deferred_posts = deferred_posts_map_t();

  deferred_posts_map_t::iterator i = deferred_posts->find(uuid);
  if (i == deferred_posts->end()) {
    posts_list lst;
    lst.push_back(post);
    deferred_posts->insert(deferred_posts_map_t::value_type(uuid, lst));
  } else {
    (*i).second.push_back(post);
  }
}

void account_t::apply_deferred_posts()
{
  if (deferred_posts) {
    foreach (deferred_posts_map_t::value_type& pair, *deferred_posts) {
      foreach (post_t * post, pair.second)
        post->account->add_post(post);
    }
    deferred_posts = none;
  }

  // Also apply in child accounts
  foreach (const accounts_map::value_type& pair, accounts)
    pair.second->apply_deferred_posts();
}

bool account_t::remove_post(post_t * post)
{
  // It's possible that 'post' wasn't yet in this account, but try to
  // remove it anyway.  This can happen if there is an error during
  // parsing, when the posting knows what it's account is, but
  // xact_t::finalize has not yet added that posting to the account.
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

  value_t get_note(account_t& account) {
    return account.note ? string_value(*account.note) : NULL_VALUE;
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

  value_t get_earliest(account_t& account)
  {
    return account.self_details().earliest_post;
  }
  value_t get_earliest_checkin(account_t& account)
  {
    return (! account.self_details().earliest_checkin.is_not_a_date_time() ?
            value_t(account.self_details().earliest_checkin) : NULL_VALUE);
  }

  value_t get_latest(account_t& account)
  {
    return account.self_details().latest_post;
  }
  value_t get_latest_checkout(account_t& account)
  {
    return (! account.self_details().latest_checkout.is_not_a_date_time() ?
            value_t(account.self_details().latest_checkout) : NULL_VALUE);
  }
  value_t get_latest_checkout_cleared(account_t& account)
  {
    return account.self_details().latest_checkout_cleared;
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
                                   const string& fn_name)
{
  if (kind != symbol_t::FUNCTION)
    return NULL;

  switch (fn_name[0]) {
  case 'a':
    if (fn_name[1] == '\0' || fn_name == "amount")
      return WRAP_FUNCTOR(get_wrapper<&get_amount>);
    else if (fn_name == "account")
      return WRAP_FUNCTOR(&get_account);
    else if (fn_name == "account_base")
      return WRAP_FUNCTOR(get_wrapper<&get_account_base>);
    else if (fn_name == "addr")
      return WRAP_FUNCTOR(get_wrapper<&get_addr>);
    else if (fn_name == "any")
      return WRAP_FUNCTOR(&fn_any);
    else if (fn_name == "all")
      return WRAP_FUNCTOR(&fn_all);
    break;

  case 'c':
    if (fn_name == "count")
      return WRAP_FUNCTOR(get_wrapper<&get_count>);
    else if (fn_name == "cost")
      return WRAP_FUNCTOR(get_wrapper<&get_cost>);
    break;

  case 'd':
    if (fn_name == "depth")
      return WRAP_FUNCTOR(get_wrapper<&get_depth>);
    else if (fn_name == "depth_spacer")
      return WRAP_FUNCTOR(get_wrapper<&get_depth_spacer>);
    break;

  case 'e':
    if (fn_name == "earliest")
      return WRAP_FUNCTOR(get_wrapper<&get_earliest>);
    else if (fn_name == "earliest_checkin")
      return WRAP_FUNCTOR(get_wrapper<&get_earliest_checkin>);
    break;

  case 'i':
    if (fn_name == "is_account")
      return WRAP_FUNCTOR(get_wrapper<&get_true>);
    else if (fn_name == "is_index")
      return WRAP_FUNCTOR(get_wrapper<&get_subcount>);
    break;

  case 'l':
    if (fn_name[1] == '\0')
      return WRAP_FUNCTOR(get_wrapper<&get_depth>);
    else if (fn_name == "latest_cleared")
      return WRAP_FUNCTOR(get_wrapper<&get_latest_cleared>);
    else if (fn_name == "latest")
      return WRAP_FUNCTOR(get_wrapper<&get_latest>);
    else if (fn_name == "latest_checkout")
      return WRAP_FUNCTOR(get_wrapper<&get_latest_checkout>);
    else if (fn_name == "latest_checkout_cleared")
      return WRAP_FUNCTOR(get_wrapper<&get_latest_checkout_cleared>);
    break;

  case 'n':
    if (fn_name[1] == '\0')
      return WRAP_FUNCTOR(get_wrapper<&get_subcount>);
    else if (fn_name == "note")
      return WRAP_FUNCTOR(get_wrapper<&get_note>);
    break;

  case 'p':
    if (fn_name == "partial_account")
      return WRAP_FUNCTOR(get_partial_name);
    else if (fn_name == "parent")
      return WRAP_FUNCTOR(get_wrapper<&get_parent>);
    break;

  case 's':
    if (fn_name == "subcount")
      return WRAP_FUNCTOR(get_wrapper<&get_subcount>);
    break;

  case 't':
    if (fn_name == "total")
      return WRAP_FUNCTOR(get_wrapper<&get_total>);
    break;

  case 'u':
    if (fn_name == "use_direct_amount")
      return WRAP_FUNCTOR(get_wrapper<&ignore>);
    break;

  case 'N':
    if (fn_name[1] == '\0')
      return WRAP_FUNCTOR(get_wrapper<&get_count>);
    break;

  case 'O':
    if (fn_name[1] == '\0')
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

  if (post.checkin && (earliest_checkin.is_not_a_date_time() ||
                       *post.checkin < earliest_checkin))
    earliest_checkin = *post.checkin;

  if (post.checkout && (latest_checkout.is_not_a_date_time() ||
                        *post.checkout > latest_checkout)) {
    latest_checkout = *post.checkout;
    latest_checkout_cleared = post.state() == item_t::CLEARED;
  }

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
    payees_referenced.insert(post.payee());
  }
}

void put_account(property_tree::ptree& st, const account_t& acct,
                 function<bool(const account_t&)> pred)
{
  if (pred(acct)) {
    std::ostringstream buf;
    buf.width(sizeof(unsigned long) * 2);
    buf.fill('0');
    buf << std::hex << reinterpret_cast<unsigned long>(&acct);

    st.put("<xmlattr>.id", buf.str());

    st.put("name", acct.name);
    st.put("fullname", acct.fullname());

    value_t total = acct.amount();
    if (! total.is_null())
      put_value(st.put("account-amount", ""), total);

    total = acct.total();
    if (! total.is_null())
      put_value(st.put("account-total", ""), total);

    foreach (const accounts_map::value_type& pair, acct.accounts)
      put_account(st.add("account", ""), *pair.second, pred);
  }
}

} // namespace ledger
