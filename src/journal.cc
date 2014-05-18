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

#include "journal.h"
#include "context.h"
#include "amount.h"
#include "commodity.h"
#include "pool.h"
#include "xact.h"
#include "post.h"
#include "account.h"

namespace ledger {

journal_t::journal_t()
{
  initialize();
  TRACE_CTOR(journal_t, "");
}

#if 0
journal_t::journal_t(const path& pathname)
{
  initialize();
  read(pathname);
  TRACE_CTOR(journal_t, "path");
}

journal_t::journal_t(const string& str)
{
  initialize();
  read(str);
  TRACE_CTOR(journal_t, "string");
}
#endif

journal_t::~journal_t()
{
  TRACE_DTOR(journal_t);

  // Don't bother unhooking each xact's posts from the accounts they refer to,
  // because all accounts are about to be deleted.
  foreach (xact_t * xact, xacts)
    checked_delete(xact);

  foreach (auto_xact_t * xact, auto_xacts)
    checked_delete(xact);

  foreach (period_xact_t * xact, period_xacts)
    checked_delete(xact);

  checked_delete(master);
}

void journal_t::initialize()
{
  master            = new account_t;
  bucket            = NULL;
  fixed_accounts    = false;
  fixed_payees      = false;
  fixed_commodities = false;
  fixed_metadata    = false;
  current_context   = NULL;
  was_loaded        = false;
  force_checking    = false;
  check_payees      = false;
  day_break         = false;
  checking_style    = CHECK_NORMAL;
  recursive_aliases = false;
  no_aliases        = false;
}

void journal_t::add_account(account_t * acct)
{
  master->add_account(acct);
}

bool journal_t::remove_account(account_t * acct)
{
  return master->remove_account(acct);
}

account_t * journal_t::find_account(const string& name, bool auto_create)
{
  return master->find_account(name, auto_create);
}

account_t * journal_t::find_account_re(const string& regexp)
{
  return master->find_account_re(regexp);
}

account_t * journal_t::register_account(const string& name, post_t * post,
                                        account_t * master_account)
{
  // If there are any account aliases, substitute before creating an account
  // object.
  account_t * result = expand_aliases(name);

  // Create the account object and associate it with the journal; this
  // is registering the account.
  if (! result)
    result = master_account->find_account(name);

  // If the account name being registered is "Unknown", check whether
  // the payee indicates an account that should be used.
  if (result->name == _("Unknown")) {
    foreach (account_mapping_t& value, payees_for_unknown_accounts) {
      if (post && value.first.match(post->xact->payee)) {
        result = value.second;
        break;
      }
    }
  }

  // Now that we have an account, make certain that the account is
  // "known", if the user has requested validation of that fact.
  if (checking_style == CHECK_WARNING || checking_style == CHECK_ERROR) {
    if (! result->has_flags(ACCOUNT_KNOWN)) {
      if (! post) {
        if (force_checking)
          fixed_accounts = true;
        result->add_flags(ACCOUNT_KNOWN);
      }
      else if (! fixed_accounts && post->_state != item_t::UNCLEARED) {
        result->add_flags(ACCOUNT_KNOWN);
      }
      else if (checking_style == CHECK_WARNING) {
        current_context->warning(_f("Unknown account '%1%'") % result->fullname());
      }
      else if (checking_style == CHECK_ERROR) {
        throw_(parse_error, _f("Unknown account '%1%'") % result->fullname());
      }
    }
  }
  return result;
}

account_t * journal_t::expand_aliases(string name) {
  // Aliases are expanded recursively, so if both alias Foo=Bar:Foo and
  // alias Bar=Baaz:Bar are in effect, first Foo will be expanded to Bar:Foo,
  // then Bar:Foo will be expanded to Baaz:Bar:Foo.
  // The expansion loop keeps a list of already expanded names in order to
  // prevent infinite excursion. Each alias may only be expanded at most once.
  account_t * result = NULL;

  if (no_aliases)
    return result;

  bool keep_expanding = true;
  std::list<string> already_seen;
  // loop until no expansion can be found
  do {
    if (account_aliases.size() > 0) {
      accounts_map::const_iterator i = account_aliases.find(name);
      if (i != account_aliases.end()) {
        if (std::find(already_seen.begin(), already_seen.end(), name) != already_seen.end()) {
          throw_(std::runtime_error,
                 _f("Infinite recursion on alias expansion for %1%")
                 % name);
        }
        // there is an alias for the full account name, including colons
        already_seen.push_back(name);
        result = (*i).second;
        name = result->fullname();
      } else {
        // only check the very first account for alias expansion, in case
        // that can be expanded successfully
        size_t colon = name.find(':');
        if (colon != string::npos) {
          string first_account_name = name.substr(0, colon);
          accounts_map::const_iterator j = account_aliases.find(first_account_name);
          if (j != account_aliases.end()) {
            if (std::find(already_seen.begin(), already_seen.end(), first_account_name) != already_seen.end()) {
              throw_(std::runtime_error,
                     _f("Infinite recursion on alias expansion for %1%")
                     % first_account_name);
            }
            already_seen.push_back(first_account_name);
            result = find_account((*j).second->fullname() + name.substr(colon));
            name = result->fullname();
          } else {
            keep_expanding = false;
          }
        } else {
          keep_expanding = false;
        }
      }
    } else {
      keep_expanding = false;
    }
  } while(keep_expanding && recursive_aliases);
  return result;
}

string journal_t::register_payee(const string& name, xact_t * xact)
{
  string payee;

  if (check_payees &&
      (checking_style == CHECK_WARNING || checking_style == CHECK_ERROR)) {
    std::set<string>::iterator i = known_payees.find(name);

    if (i == known_payees.end()) {
      if (! xact) {
        if (force_checking)
          fixed_payees = true;
        known_payees.insert(name);
      }
      else if (! fixed_payees && xact->_state != item_t::UNCLEARED) {
        known_payees.insert(name);
      }
      else if (checking_style == CHECK_WARNING) {
        current_context->warning(_f("Unknown payee '%1%'") % name);
      }
      else if (checking_style == CHECK_ERROR) {
        throw_(parse_error, _f("Unknown payee '%1%'") % name);
      }
    }
  }

  foreach (payee_alias_mapping_t& value, payee_alias_mappings) {
    if (value.first.match(name)) {
      payee = value.second;
      break;
    }
  }

  return payee.empty() ? name : payee;
}

void journal_t::register_commodity(commodity_t& comm,
                                   variant<int, xact_t *, post_t *> context)
{
  if (checking_style == CHECK_WARNING || checking_style == CHECK_ERROR) {
    if (! comm.has_flags(COMMODITY_KNOWN)) {
      if (context.which() == 0) {
        if (force_checking)
          fixed_commodities = true;
        comm.add_flags(COMMODITY_KNOWN);
      }
      else if (! fixed_commodities &&
               ((context.which() == 1 &&
                 boost::get<xact_t *>(context)->_state != item_t::UNCLEARED) ||
                (context.which() == 2 &&
                 boost::get<post_t *>(context)->_state != item_t::UNCLEARED))) {
        comm.add_flags(COMMODITY_KNOWN);
      }
      else if (checking_style == CHECK_WARNING) {
        current_context->warning(_f("Unknown commodity '%1%'") % comm);
      }
      else if (checking_style == CHECK_ERROR) {
        throw_(parse_error, _f("Unknown commodity '%1%'") % comm);
      }
    }
  }
}

void journal_t::register_metadata(const string& key, const value_t& value,
                                  variant<int, xact_t *, post_t *> context)
{
  if (checking_style == CHECK_WARNING || checking_style == CHECK_ERROR) {
    std::set<string>::iterator i = known_tags.find(key);

    if (i == known_tags.end()) {
      if (context.which() == 0) {
        if (force_checking)
          fixed_metadata = true;
        known_tags.insert(key);
      }
      else if (! fixed_metadata &&
               ((context.which() == 1 &&
                 boost::get<xact_t *>(context)->_state != item_t::UNCLEARED) ||
                (context.which() == 2 &&
                 boost::get<post_t *>(context)->_state != item_t::UNCLEARED))) {
        known_tags.insert(key);
      }
      else if (checking_style == CHECK_WARNING) {
        current_context->warning(_f("Unknown metadata tag '%1%'") % key);
      }
      else if (checking_style == CHECK_ERROR) {
        throw_(parse_error, _f("Unknown metadata tag '%1%'") % key);
      }
    }
  }

  if (! value.is_null()) {
    std::pair<tag_check_exprs_map::iterator,
              tag_check_exprs_map::iterator> range =
      tag_check_exprs.equal_range(key);

    for (tag_check_exprs_map::iterator i = range.first;
         i != range.second;
         ++i) {
      bind_scope_t bound_scope
        (*current_context->scope,
         context.which() == 1 ?
         static_cast<scope_t&>(*boost::get<xact_t *>(context)) :
         static_cast<scope_t&>(*boost::get<post_t *>(context)));
      value_scope_t val_scope(bound_scope, value);

      if (! (*i).second.first.calc(val_scope).to_boolean()) {
        if ((*i).second.second == expr_t::EXPR_ASSERTION)
          throw_(parse_error,
                 _f("Metadata assertion failed for (%1%: %2%): %3%")
                 % key % value % (*i).second.first);
        else
          current_context->warning
            (_f("Metadata check failed for (%1%: %2%): %3%")
             % key % value % (*i).second.first);
      }
    }
  }
}

namespace {
  void check_all_metadata(journal_t& journal,
                          variant<int, xact_t *, post_t *> context)
  {
    xact_t * xact = context.which() == 1 ? boost::get<xact_t *>(context) : NULL;
    post_t * post = context.which() == 2 ? boost::get<post_t *>(context) : NULL;

    if ((xact || post) && xact ? xact->metadata : post->metadata) {
      foreach (const item_t::string_map::value_type& pair,
               xact ? *xact->metadata : *post->metadata) {
        const string& key(pair.first);

        if (optional<value_t> value = pair.second.first)
          journal.register_metadata(key, *value, context);
        else
          journal.register_metadata(key, NULL_VALUE, context);
      }
    }
  }
}

bool lt_posting_account(post_t * left, post_t * right) {
  return left->account < right->account;
}

bool is_equivalent_posting(post_t * left, post_t * right)
{
  if (left->account != right->account)
    return false;

  if (left->amount != right->amount)
    return false;

  return true;
}

bool journal_t::add_xact(xact_t * xact)
{
  xact->journal = this;

  if (! xact->finalize()) {
    xact->journal = NULL;
    return false;
  }

  extend_xact(xact);
  check_all_metadata(*this, xact);

  foreach (post_t * post, xact->posts) {
    extend_post(*post, *this);
    check_all_metadata(*this, post);
  }

  // If a transaction with this UUID has already been seen, simply do
  // not add this one to the journal.  However, all automated checks
  // will have been performed by extend_xact, so asserts can still be
  // applied to it.
  if (optional<value_t> ref = xact->get_tag(_("UUID"))) {
    std::string uuid = ref->to_string();
    std::pair<checksum_map_t::iterator, bool> result
      = checksum_map.insert(checksum_map_t::value_type(uuid, xact));
    if (! result.second) {
      // This UUID has been seen before; apply any postings which the
      // earlier version may have deferred.
      foreach (post_t * post, xact->posts) {
        account_t * acct = post->account;
        if (acct->deferred_posts) {
          auto i = acct->deferred_posts->find(uuid);
          if (i != acct->deferred_posts->end()) {
            for (post_t * rpost : (*i).second)
              if (acct == rpost->account)
                acct->add_post(rpost);
            acct->deferred_posts->erase(i);
          }
        }
      }

      xact_t * other = (*result.first).second;

      // Copy the two lists of postings (which should be relatively
      // short), and make sure that the intersection is the empty set
      // (i.e., that they are the same list).
      std::vector<post_t *> this_posts(xact->posts.begin(),
                                       xact->posts.end());
      std::sort(this_posts.begin(), this_posts.end(),
                lt_posting_account);
      std::vector<post_t *> other_posts(other->posts.begin(),
                                        other->posts.end());
      std::sort(other_posts.begin(), other_posts.end(),
                lt_posting_account);
      bool match = std::equal(this_posts.begin(), this_posts.end(),
                              other_posts.begin(), is_equivalent_posting);

      if (! match || this_posts.size() != other_posts.size()) {
        add_error_context(_("While comparing this previously seen transaction:"));
        add_error_context(source_context(other->pos->pathname,
                                         other->pos->beg_pos,
                                         other->pos->end_pos, "> "));
        add_error_context(_("to this later transaction:"));
        add_error_context(source_context(xact->pos->pathname,
                                         xact->pos->beg_pos,
                                         xact->pos->end_pos, "> "));
        throw_(std::runtime_error,
               _f("Transactions with the same UUID must have equivalent postings"));
      }

      xact->journal = NULL;
      return false;
    }
  }

  xacts.push_back(xact);

  return true;
}

void journal_t::extend_xact(xact_base_t * xact)
{
  foreach (auto_xact_t * auto_xact, auto_xacts)
    auto_xact->extend_xact(*xact, *current_context);
}

bool journal_t::remove_xact(xact_t * xact)
{
  bool found = false;
  xacts_list::iterator i;
  for (i = xacts.begin(); i != xacts.end(); i++)
    if (*i == xact) {
      found = true;
      break;
    }
  if (! found)
    return false;

  xacts.erase(i);
  xact->journal = NULL;

  return true;
}

std::size_t journal_t::read(parse_context_stack_t& context)
{
  std::size_t count = 0;
  try {
    parse_context_t& current(context.get_current());
    current_context = &current;

    current.count = 0;
    if (! current.scope)
      current.scope = scope_t::default_scope;

    if (! current.scope)
      throw_(std::runtime_error,
             _f("No default scope in which to read journal file '%1%'")
             % current.pathname);

    if (! current.master)
      current.master = master;

    count = read_textual(context);
    if (count > 0) {
      if (! current.pathname.empty())
        sources.push_back(fileinfo_t(current.pathname));
      else
        sources.push_back(fileinfo_t());
    }
  }
  catch (...) {
    clear_xdata();
    current_context = NULL;
    throw;
  }

  // xdata may have been set for some accounts and transaction due to the use
  // of balance assertions or other calculations performed in valexpr-based
  // posting amounts.
  clear_xdata();

  return count;
}

bool journal_t::has_xdata()
{
  foreach (xact_t * xact, xacts)
    if (xact->has_xdata())
      return true;

  foreach (auto_xact_t * xact, auto_xacts)
    if (xact->has_xdata())
      return true;

  foreach (period_xact_t * xact, period_xacts)
    if (xact->has_xdata())
      return true;

  if (master->has_xdata() || master->children_with_xdata())
    return true;

  return false;
}

void journal_t::clear_xdata()
{
  foreach (xact_t * xact, xacts)
    if (! xact->has_flags(ITEM_TEMP))
      xact->clear_xdata();

  foreach (auto_xact_t * xact, auto_xacts)
    if (! xact->has_flags(ITEM_TEMP))
      xact->clear_xdata();

  foreach (period_xact_t * xact, period_xacts)
    if (! xact->has_flags(ITEM_TEMP))
      xact->clear_xdata();

  master->clear_xdata();
}

bool journal_t::valid() const
{
  if (! master->valid()) {
    DEBUG("ledger.validate", "journal_t: master not valid");
    return false;
  }

  foreach (const xact_t * xact, xacts)
    if (! xact->valid()) {
      DEBUG("ledger.validate", "journal_t: xact not valid");
      return false;
    }

  return true;
}

} // namespace ledger
