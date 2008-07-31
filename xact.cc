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

#include "xact.h"
#include "journal.h"
#include "account.h"

namespace ledger {

bool xact_t::use_effective_date = false;

xact_t::~xact_t()
{
  TRACE_DTOR(xact_t);
}

datetime_t xact_t::actual_date() const
{
  if (! _date && entry)
    return entry->actual_date();
  return *_date;
}

datetime_t xact_t::effective_date() const
{
  if (! _date_eff && entry)
    return entry->effective_date();
  return *_date_eff;
}

namespace {
  value_t get_amount(call_scope_t& scope)
  {
    xact_t& xact(downcast<xact_t>(*scope.parent));
    return xact.amount;
  }

  value_t get_date(call_scope_t& scope)
  {
    xact_t& xact(downcast<xact_t>(*scope.parent));
    return xact.entry->date();
  }

  value_t get_payee(call_scope_t& scope)
  {
    xact_t& xact(downcast<xact_t>(*scope.parent));
    return string_value(xact.entry->payee);
  }

  value_t get_account(call_scope_t& scope)
  {
    xact_t& xact(downcast<xact_t>(*scope.parent));

    string name = xact.account->fullname();

    if (xact.has_flags(XACT_VIRTUAL)) {
      if (xact.must_balance())
	name = string("[") + name + "]";
      else
	name = string("(") + name + ")";
    }
    return string_value(name);
  }

  value_t get_account_base(call_scope_t& scope)
  {
    assert(false);
    return NULL_VALUE;
  }
}

expr_t::ptr_op_t xact_t::lookup(const string& name)
{
  switch (name[0]) {
  case 'a':
    if (name[1] == '\0' || name == "amount")
      return WRAP_FUNCTOR(bind(get_amount, _1));
    else if (name == "account")
      return WRAP_FUNCTOR(bind(get_account, _1));
    else if (name == "account_base")
      return WRAP_FUNCTOR(bind(get_account_base, _1));
    break;
  case 'd':
    if (name[1] == '\0' || name == "date")
      return WRAP_FUNCTOR(bind(get_date, _1));
    break;
  case 'p':
    if (name[1] == '\0' || name == "payee")
      return WRAP_FUNCTOR(bind(get_payee, _1));
    break;
  }
  return entry->lookup(name);
}

bool xact_t::valid() const
{
  if (! entry) {
    DEBUG("ledger.validate", "xact_t: ! entry");
    return false;
  }

  if (state != UNCLEARED && state != CLEARED && state != PENDING) {
    DEBUG("ledger.validate", "xact_t: state is bad");
    return false;
  }

  xacts_list::const_iterator i =
    std::find(entry->xacts.begin(),
	      entry->xacts.end(), this);
  if (i == entry->xacts.end()) {
    DEBUG("ledger.validate", "xact_t: ! found");
    return false;
  }

  if (! account) {
    DEBUG("ledger.validate", "xact_t: ! account");
    return false;
  }

  if (! amount.valid()) {
    DEBUG("ledger.validate", "xact_t: ! amount.valid()");
    return false;
  }

  if (cost && ! cost->valid()) {
    DEBUG("ledger.validate", "xact_t: cost && ! cost->valid()");
    return false;
  }

  if (flags() & ~0x003f) {
    DEBUG("ledger.validate", "xact_t: flags are bad");
    return false;
  }

  return true;
}

#if 0
xact_context::xact_context(const xact_t& _xact, const string& desc) throw()
  : file_context("", 0, desc), xact(_xact)
{
  const paths_list& sources(xact.entry->journal->sources);
  unsigned int x = 0;
  for (paths_list::const_iterator i = sources.begin();
       i != sources.end();
       i++, x++)
    if (x == xact.entry->src_idx) {
      file = *i;
      break;
    }
  line = xact.beg_line;
}
#endif

} // namespace ledger
