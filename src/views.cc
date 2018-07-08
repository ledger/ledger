/*
 * Copyright (c) 2003-2018, John Wiegley.  All rights reserved.
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

#if DOCUMENT_MODEL

#include <system.hh>

#include "views.h"
#include "report.h"
#include "journal.h"
#include "xact.h"
#include "post.h"
#include "account.h"

namespace ledger {

r_xact_ptr r_journal_t::create_xact(xact_t * xact)
{
  r_xact_ptr x = new r_xact_t(this, xact);
  add_xact(x);
  assert(xact->data == NULL);
  xact->data = &x;
  return x;
}

void r_journal_t::add_xact(r_xact_ptr xact)
{
  xacts.push_back(xact);
}

r_post_ptr r_journal_t::add_post(post_t * post)
{
  r_xact_ptr x;
  if (post->xact->data)
    x = *static_cast<r_xact_ptr *>(post->xact->data);
  else
    x = create_xact(post->xact);

  r_post_ptr p = create_post(post, x, create_account(post->account));
  return p;
}

void r_journal_t::add_post(r_post_ptr post)
{
  posts.push_back(post);
}

r_post_ptr r_journal_t::create_post(post_t * post, r_xact_ptr xact,
                                    r_account_ptr account)
{
  r_post_ptr p = new r_post_t(this, post, xact, account);

  add_post(p);
  xact->add_post(p);
  account->add_post(p);

  return p;
}

r_post_ptr r_journal_t::create_post(r_post_ptr post, r_xact_ptr xact,
                                    r_account_ptr account)
{
  r_post_ptr temp(new r_post_t(*post.get()));

  add_post(temp);

  temp->set_xact(xact);
  xact->add_post(temp);

  temp->set_account(account);
  account->add_post(temp);

  return temp;
}

r_account_ptr r_journal_t::create_account(account_t * account)
{
  return create_account(account->fullname());
}

r_account_ptr r_journal_t::create_account(const std::string& name)
{
  return master_ptr->create_account(name);
}


const optional<position_t> r_item_t::position() const
{
  return ptr()->pos;
}

date_t r_item_t::date() const
{
  return ptr()->date();
}

void r_item_t::set_date(const date_t& when)
{
}

item_t::state_t r_item_t::state() const
{
  return ptr()->state();
}

void r_item_t::set_state(item_t::state_t val)
{
}

string r_item_t::payee() const
{
  if (optional<value_t> desc = get_tag(_("Payee")))
    return desc->as_string();
  else
    return empty_string;
}

void r_item_t::set_payee(const string& name)
{
}

void r_item_t::define(const symbol_t::kind_t, const string& name,
                      expr_t::ptr_op_t def)
{
  bind_scope_t bound_scope(*scope_t::default_scope, *this);
  set_tag(name, def->calc(bound_scope));
}

expr_t::ptr_op_t r_item_t::lookup(const symbol_t::kind_t kind,
                                  const string& name)
{
  if (kind != symbol_t::FUNCTION)
    return NULL;

  switch (name[0]) {
  }

  return base_item->lookup(kind, name);
}


string r_xact_t::description()
{
  return ptr()->description();
}

void r_xact_t::add_post(r_post_ptr post)
{
  posts.push_back(post);
}

string r_xact_t::payee() const
{
  string desc(r_item_t::payee());
  if (desc.empty())
    return ptr()->payee;
  else
    return desc;
}


string r_post_t::description()
{
  return ptr()->description();
}

string r_post_t::payee() const
{
  string desc(r_item_t::payee());
  if (desc.empty())
    return const_cast<r_post_t *>(this)->xact()->payee();
  else
    return desc;
}


string r_account_t::description()
{
  return string(_("account ")) + fullname();
}

void r_account_t::add_post(r_post_ptr post)
{
  posts.push_back(post);
}

r_account_ptr r_account_t::create_account(const std::string& fname)
{
  string::size_type sep = fname.find(':');
  string head, tail;
  if (sep == string::npos) {
    head = fname;
  } else {
    head = string(fname, 0, sep);
    tail = string(fname, sep + 1);
  }

  std::pair<r_accounts_map::iterator, bool> result =
    accounts.insert(r_accounts_map::value_type
                    (head, new r_account_t(journal_ptr, this, name)));

  r_account_ptr acct((*result.first).second);
  if (tail.empty())
    return acct;
  else
    return acct->create_account(tail);
}

string r_account_t::fullname() const
{
  if (! _fullname.empty()) {
    return _fullname;
  } else {
    r_account_ptr first = NULL;
    string        fname = name;

    while (! first || first->parent_ptr) {
      first = first ? first->parent_ptr : parent_ptr;
      if (! first->name.empty())
        fname = first->name + ":" + fname;
    }

    _fullname = fname;

    return fname;
  }
}

} // namespace ledger

#endif /* DOCUMENT_MODEL */
