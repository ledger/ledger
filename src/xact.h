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

class entry_t;
class account_t;

class xact_t;
typedef std::list<xact_t *> xacts_list;

class xact_t : public supports_flags<>, public scope_t
{
public:
#define XACT_NORMAL     0x0000	// no flags at all, a basic transaction
#define XACT_VIRTUAL    0x0001  // the account was specified with (parens)
#define XACT_BALANCE    0x0002  // the account was specified with [brackets]
#define XACT_AUTO       0x0004  // transaction created by automated entry
#define XACT_IN_CACHE   0x0008  // transaction allocated by the binary cache
#define XACT_CALCULATED 0x0010  // transaction's amount was auto-calculated
#define XACT_GENERATED  0x0020  // transaction was not found in a journal
#define XACT_TEMP       0x0040  // transaction is a temporary object

  enum state_t { UNCLEARED, CLEARED, PENDING };

  entry_t *	     entry;
  account_t *	     account;
  state_t	     state;

  optional<date_t>   _date;
  optional<date_t>   _date_eff;
  optional<string>   note;

  amount_t	     amount;
  optional<expr_t>   amount_expr;
  optional<amount_t> cost;
  optional<expr_t>   cost_expr;

  istream_pos_type   beg_pos;
  unsigned long	     beg_line;
  istream_pos_type   end_pos;
  unsigned long	     end_line;

  static  bool	     use_effective_date;

  xact_t(account_t * _account = NULL,
	 flags_t     _flags   = XACT_NORMAL)
    : supports_flags<>(_flags), entry(NULL), account(_account),
      state(UNCLEARED), beg_pos(0), beg_line(0), end_pos(0), end_line(0)
  {
    TRACE_CTOR(xact_t, "account_t *, flags_t");
  }
  xact_t(account_t *	_account,
	 const amount_t& _amount,
	 flags_t         _flags = XACT_NORMAL,
	 const optional<string>& _note = none)
    : supports_flags<>(_flags), entry(NULL), account(_account),
      state(UNCLEARED), note(_note), amount(_amount),
      beg_pos(0), beg_line(0), end_pos(0), end_line(0)
  {
    TRACE_CTOR(xact_t,
	       "account_t *, const amount_t&, flags_t, const string&");
  }
  xact_t(const xact_t& xact)
    : supports_flags<>(xact),
      scope_t(),
      entry(xact.entry),
      account(xact.account),
      state(xact.state),
      _date(xact._date),
      _date_eff(xact._date_eff),
      note(xact.note),
      amount(xact.amount),
      cost(xact.cost),
      beg_pos(xact.beg_pos),
      beg_line(xact.beg_line),
      end_pos(xact.end_pos),
      end_line(xact.end_line),
      xdata_(xact.xdata_)    // jww (2008-07-19): What are the copy semantics?
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

  struct xdata_t : public supports_flags<>
  {
#define XACT_EXT_RECEIVED   0x01
#define XACT_EXT_HANDLED    0x02
#define XACT_EXT_TO_DISPLAY 0x04
#define XACT_EXT_DISPLAYED  0x08
#define XACT_EXT_NO_TOTAL   0x10
#define XACT_EXT_SORT_CALC  0x20
#define XACT_EXT_COMPOUND   0x40
#define XACT_EXT_MATCHES    0x80

    value_t	 total;
    value_t	 sort_value;
    value_t	 value;
    unsigned int index;
    date_t	 date;
    account_t *	 account;
    void *	 ptr;

    optional<xacts_list> component_xacts;

    xdata_t() : supports_flags<>(), index(0), account(NULL), ptr(NULL) {
      TRACE_CTOR(xdata_t, "");
    }
    ~xdata_t() throw() {
      TRACE_DTOR(xdata_t);
    }

    void remember_xact(xact_t& xact) {
      if (! component_xacts)
	component_xacts = xacts_list();
      component_xacts->push_back(&xact);
    }

    bool has_component_xacts() const {
      return component_xacts && ! component_xacts->empty();
    }

    void copy_component_xacts(xacts_list& xacts) {
      foreach (xact_t * xact, xacts)
	remember_xact(*xact);
    }

#if 0
    void walk_component_xacts(item_handler<xact_t>& handler) const {
      foreach (xact_t * xact, *component_xacts)
	handler(*xact);
    }
#endif
  };

  // This variable holds optional "extended data" which is usually produced
  // only during reporting, and only for the transaction set being reported.
  // It's a memory-saving measure to delay allocation until the last possible
  // moment.
  mutable optional<xdata_t> xdata_;

  bool has_xdata() const {
    return xdata_;
  }
  void clear_xdata() {
    xdata_ = none;
  }
  xdata_t& xdata() {
    if (! xdata_)
      xdata_ = xdata_t();
    return *xdata_;
  }

  void add_to_value(value_t& value);

  date_t reported_date() const {
    if (xdata_ && is_valid(xdata_->date))
      return xdata_->date;
    return
      date();
  }

  account_t * reported_account() {
    if (xdata_)
      if (account_t * acct = xdata_->account)
	return acct;
    return account;
  }

  const account_t * reported_account() const {
    return const_cast<xact_t *>(this)->reported_account();
  }
};

} // namespace ledger

#endif // _XACT_H
