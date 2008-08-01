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

#ifndef _XACT_H
#define _XACT_H

#include "utils.h"
#include "scope.h"

namespace ledger {

// These flags persist with the object
#define XACT_NORMAL     0x0000	// no flags at all, a basic transaction
#define XACT_VIRTUAL    0x0001  // the account was specified with (parens)
#define XACT_BALANCE    0x0002  // the account was specified with [brackets]
#define XACT_AUTO       0x0004  // transaction created by automated entry
#define XACT_IN_CACHE   0x0008  // transaction allocated by the binary cache
#define XACT_CALCULATED 0x0010  // transaction's amount was auto-calculated
#define XACT_GENERATED  0x0020  // transaction was not found in a journal
#define XACT_TEMP       0x0040  // transaction is a temporary object

class entry_t;
class account_t;

class xact_t : public supports_flags<>, public scope_t
{
 public:
  enum state_t { UNCLEARED, CLEARED, PENDING };

  entry_t *	     entry;
  state_t	     state;
  account_t *	     account;
  optional<date_t>   _date;
  optional<date_t>   _date_eff;
  amount_t	     amount;
  optional<expr_t>   amount_expr;
  optional<amount_t> cost;
  optional<expr_t>   cost_expr;
  optional<string>   note;
  istream_pos_type   beg_pos;
  unsigned long	     beg_line;
  istream_pos_type   end_pos;
  unsigned long	     end_line;

  mutable void * data;
  static bool	 use_effective_date;

  xact_t(account_t * _account = NULL,
		flags_t     _flags   = XACT_NORMAL)
    : supports_flags<>(_flags), entry(NULL),
      state(UNCLEARED), account(_account),
      beg_pos(0), beg_line(0), end_pos(0), end_line(0), data(NULL)
  {
    TRACE_CTOR(xact_t, "account_t *, flags_t");
  }
  xact_t(account_t *	_account,
		const amount_t& _amount,
		flags_t         _flags = XACT_NORMAL,
		const optional<string>& _note = none)
    : supports_flags<>(_flags), entry(NULL), state(UNCLEARED),
      account(_account), amount(_amount), note(_note),
      beg_pos(0), beg_line(0), end_pos(0), end_line(0), data(NULL)
  {
    TRACE_CTOR(xact_t,
	       "account_t *, const amount_t&, flags_t, const string&");
  }
  xact_t(const xact_t& xact)
    : supports_flags<>(xact),
      scope_t(),
      entry(xact.entry),
      state(xact.state),
      account(xact.account),
      _date(xact._date),
      _date_eff(xact._date_eff),
      amount(xact.amount),
      cost(xact.cost),
      note(xact.note),
      beg_pos(xact.beg_pos),
      beg_line(xact.beg_line),
      end_pos(xact.end_pos),
      end_line(xact.end_line),
      data(xact.data)		// jww (2008-07-19): What are the copy semantics?
  {
    TRACE_CTOR(xact_t, "copy");
  }
  ~xact_t();

  date_t actual_date() const;
  date_t effective_date() const;
  date_t date() const {
    if (use_effective_date)
      return effective_date();
    else
      return actual_date();
  }

  bool must_balance() const {
    return ! has_flags(XACT_VIRTUAL) || has_flags(XACT_BALANCE);
  }

  virtual expr_t::ptr_op_t lookup(const string& name);

  bool valid() const;
};

} // namespace ledger

#endif // _XACT_H
