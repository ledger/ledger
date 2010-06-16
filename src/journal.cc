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

#include "journal.h"
#include "amount.h"
#include "commodity.h"
#include "pool.h"
#include "xact.h"
#include "account.h"

namespace ledger {

journal_t::journal_t()
{
  TRACE_CTOR(journal_t, "");
  initialize();
}

journal_t::journal_t(const path& pathname)
{
  TRACE_CTOR(journal_t, "path");
  initialize();
  read(pathname);
}

journal_t::journal_t(const string& str)
{
  TRACE_CTOR(journal_t, "string");
  initialize();
  read(str);
}

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
  master     = new account_t;
  bucket     = NULL;
  was_loaded = false;
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

bool journal_t::add_xact(xact_t * xact)
{
  xact->journal = this;

  if (! xact->finalize()) {
    xact->journal = NULL;
    return false;
  }

  extend_xact(xact);
  xacts.push_back(xact);

  return true;
}

void journal_t::extend_xact(xact_base_t * xact)
{
  foreach (auto_xact_t * auto_xact, auto_xacts)
    auto_xact->extend_xact(*xact);
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

std::size_t journal_t::read(std::istream& in,
                            const path&   pathname,
                            account_t *   master_alt,
                            scope_t *     scope)
{
  std::size_t count = 0;
  try {
    if (! scope)
      scope = scope_t::default_scope;

    if (! scope)
      throw_(std::runtime_error,
             _("No default scope in which to read journal file '%1'")
             << pathname);

    value_t strict = expr_t("strict").calc(*scope);

    count = parse(in, *scope, master_alt ? master_alt : master,
                  &pathname, strict.to_boolean());
  }
  catch (...) {
    clear_xdata();
    throw;
  }

  // xdata may have been set for some accounts and transaction due to the use
  // of balance assertions or other calculations performed in valexpr-based
  // posting amounts.
  clear_xdata();

  return count;
}

std::size_t journal_t::read(const path& pathname,
                            account_t * master,
                            scope_t *   scope)
{
  path filename = resolve_path(pathname);

  if (! exists(filename))
    throw_(std::runtime_error,
           _("Cannot read journal file '%1'") << filename);

  ifstream stream(filename);
  std::size_t count = read(stream, filename, master, scope);
  if (count > 0)
    sources.push_back(fileinfo_t(filename));
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
