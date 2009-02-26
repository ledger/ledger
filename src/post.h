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

/**
 * @addtogroup data
 */

/**
 * @file   post.h
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _POST_H
#define _POST_H

#include "item.h"

namespace ledger {

class xact_t;
class account_t;

class post_t;
typedef std::list<post_t *> posts_list;

/**
 * @brief Brief
 *
 * Long.
 */
class post_t : public item_t
{
public:
#define POST_VIRTUAL	  0x10 // the account was specified with (parens)
#define POST_MUST_BALANCE 0x20 // posting must balance in the transaction
#define POST_CALCULATED	  0x40 // posting's amount was calculated
#define POST_PRICED	  0x80 // posting's cost was calculated

  xact_t *	     xact;	// only set for posts of regular xacts
  account_t *	     account;

  amount_t	     amount;	// can be null until finalization
  optional<amount_t> cost;
  optional<amount_t> assigned_amount;

  post_t(account_t * _account = NULL,
	 flags_t     _flags   = ITEM_NORMAL)
    : item_t(_flags),
      xact(NULL), account(_account)
  {
    TRACE_CTOR(post_t, "account_t *, flags_t");
  }
  post_t(account_t *	         _account,
	 const amount_t&         _amount,
	 flags_t                 _flags = ITEM_NORMAL,
	 const optional<string>& _note = none)
    : item_t(_flags, _note),
      xact(NULL), account(_account), amount(_amount)
  {
    TRACE_CTOR(post_t, "account_t *, const amount_t&, flags_t, const optional<string>&");
  }
  post_t(const post_t& post)
    : item_t(post),
      xact(post.xact),
      account(post.account),
      amount(post.amount),
      cost(post.cost),
      assigned_amount(post.assigned_amount),
      xdata_(post.xdata_)
  {
    TRACE_CTOR(post_t, "copy");
  }
  ~post_t() {
    TRACE_DTOR(post_t);
  }

  virtual bool has_tag(const string& tag) const;
  virtual bool has_tag(const mask_t& tag_mask,
		       const optional<mask_t>& value_mask = none) const;

  virtual optional<string> get_tag(const string& tag) const;
  virtual optional<string> get_tag(const mask_t& tag_mask,
				   const optional<mask_t>& value_mask = none) const;

  virtual date_t date() const;
  virtual optional<date_t> effective_date() const;

  virtual state_t state() const;

  bool must_balance() const {
    return ! has_flags(POST_VIRTUAL) || has_flags(POST_MUST_BALANCE);
  }

  virtual expr_t::ptr_op_t lookup(const string& name);

  bool valid() const;

  struct xdata_t : public supports_flags<>
  {
#define POST_EXT_RECEIVED   0x01
#define POST_EXT_HANDLED    0x02
#define POST_EXT_TO_DISPLAY 0x04
#define POST_EXT_DISPLAYED  0x08
#define POST_EXT_DIRECT_AMT 0x10
#define POST_EXT_SORT_CALC  0x20
#define POST_EXT_COMPOUND   0x40
#define POST_EXT_MATCHES    0x80

    value_t	total;
    std::size_t	count;
    value_t	value;
    date_t	date;
    account_t *	account;
    void *	ptr;

    std::list<sort_value_t> sort_values;

    xdata_t()
      : supports_flags<>(), count(0), account(NULL), ptr(NULL) {
      TRACE_CTOR(post_t::xdata_t, "");
    }
    xdata_t(const xdata_t& other)
      : supports_flags<>(other.flags()),
	total(other.total),
	count(other.count),
	value(other.value),
	date(other.date),
	account(other.account),
	ptr(NULL),
	sort_values(other.sort_values)
    {
      TRACE_CTOR(post_t::xdata_t, "copy");
    }
    ~xdata_t() throw() {
      TRACE_DTOR(post_t::xdata_t);
    }
  };

  // This variable holds optional "extended data" which is usually produced
  // only during reporting, and only for the posting set being reported.
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

  void add_to_value(value_t& value, expr_t& expr);

  account_t * reported_account() {
    if (xdata_)
      if (account_t * acct = xdata_->account)
	return acct;
    return account;
  }

  const account_t * reported_account() const {
    return const_cast<post_t *>(this)->reported_account();
  }

  friend class xact_t;
};

} // namespace ledger

#endif // _POST_H
