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

/**
 * @addtogroup data
 */

/**
 * @file   post.h
 * @author John Wiegley
 *
 * @ingroup data
 */
#ifndef _POST_H
#define _POST_H

#include "item.h"

namespace ledger {

class xact_t;
class account_t;

class post_t : public item_t
{
public:
#define POST_VIRTUAL         0x0010 // the account was specified with (parens)
#define POST_MUST_BALANCE    0x0020 // posting must balance in the transaction
#define POST_CALCULATED      0x0040 // posting's amount was calculated
#define POST_COST_CALCULATED 0x0080 // posting's cost was calculated
#define POST_COST_IN_FULL    0x0100 // cost specified using @@
#define POST_COST_FIXATED    0x0200 // cost is fixed using = indicator
#define POST_COST_VIRTUAL    0x0400 // cost is virtualized: (@)
#define POST_ANONYMIZED      0x0800 // a temporary, anonymous posting
#define POST_DEFERRED        0x1000 // the account was specified with <angles>

  xact_t *    xact;             // only set for posts of regular xacts
  account_t * account;

  amount_t             amount;  // can be null until finalization
  optional<expr_t>     amount_expr;
  optional<amount_t>   cost;
  optional<amount_t>   assigned_amount;
  optional<datetime_t> checkin;
  optional<datetime_t> checkout;

  post_t(account_t * _account = NULL,
         flags_t     _flags   = ITEM_NORMAL)
    : item_t(_flags), xact(NULL), account(_account)
  {
    TRACE_CTOR(post_t, "account_t *, flags_t");
  }
  post_t(account_t *             _account,
         const amount_t&         _amount,
         flags_t                 _flags = ITEM_NORMAL,
         const optional<string>& _note = none)
    : item_t(_flags, _note), xact(NULL), account(_account), amount(_amount)
  {
    TRACE_CTOR(post_t, "account_t *, amount_t, flags_t, optional<string>");
  }
  post_t(const post_t& post)
    : item_t(post),
      xact(post.xact),
      account(post.account),
      amount(post.amount),
      cost(post.cost),
      assigned_amount(post.assigned_amount),
      checkin(post.checkin),
      checkout(post.checkout),
      xdata_(post.xdata_)
  {
    copy_details(post);
    TRACE_CTOR(post_t, "copy");
  }
  virtual ~post_t() {
    TRACE_DTOR(post_t);
  }

  virtual string description() {
    if (pos) {
      std::ostringstream buf;
      buf << _f("posting at line %1%") % pos->beg_line;
      return buf.str();
    } else {
      return string(_("generated posting"));
    }
  }

  virtual bool has_tag(const string& tag,
                       bool          inherit = true) const;
  virtual bool has_tag(const mask_t&           tag_mask,
                       const optional<mask_t>& value_mask = none,
                       bool                    inherit    = true) const;

  virtual optional<value_t> get_tag(const string& tag,
                                    bool          inherit = true) const;
  virtual optional<value_t> get_tag(const mask_t&           tag_mask,
                                    const optional<mask_t>& value_mask = none,
                                    bool                    inherit    = true) const;

  virtual date_t value_date() const;
  virtual date_t date() const;
  virtual date_t primary_date() const;
  virtual optional<date_t> aux_date() const;

  string payee() const;

  bool must_balance() const {
    return ! has_flags(POST_VIRTUAL) || has_flags(POST_MUST_BALANCE);
  }

  virtual expr_t::ptr_op_t lookup(const symbol_t::kind_t kind,
                                  const string& name);

  amount_t resolve_expr(scope_t& scope, expr_t& expr);

  std::size_t xact_id() const;
  std::size_t account_id() const;

  virtual void copy_details(const item_t& item) {
    const post_t& post(dynamic_cast<const post_t&>(item));
    xdata_ = post.xdata_;
    item_t::copy_details(item);
  }

  bool valid() const;

  struct xdata_t : public supports_flags<uint_least16_t>
  {
#define POST_EXT_RECEIVED   0x0001
#define POST_EXT_HANDLED    0x0002
#define POST_EXT_DISPLAYED  0x0004
#define POST_EXT_DIRECT_AMT 0x0008
#define POST_EXT_SORT_CALC  0x0010
#define POST_EXT_COMPOUND   0x0020
#define POST_EXT_VISITED    0x0040
#define POST_EXT_MATCHES    0x0080
#define POST_EXT_CONSIDERED 0x0100

    value_t     visited_value;
    value_t     compound_value;
    value_t     total;
    std::size_t count;
    date_t      date;
    date_t      value_date;
    datetime_t  datetime;
    account_t * account;

    std::list<sort_value_t> sort_values;

    xdata_t()
      : supports_flags<uint_least16_t>(), count(0), account(NULL) {
      TRACE_CTOR(post_t::xdata_t, "");
    }
    xdata_t(const xdata_t& other)
      : supports_flags<uint_least16_t>(other.flags()),
        visited_value(other.visited_value),
        compound_value(other.compound_value),
        total(other.total),
        count(other.count),
        date(other.date),
        account(other.account),
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
  const xdata_t& xdata() const {
    return const_cast<post_t *>(this)->xdata();
  }

  void add_to_value(value_t& value,
                    const optional<expr_t&>& expr = none) const;

  void set_reported_account(account_t * account);

  account_t * reported_account() {
    if (xdata_)
      if (account_t * acct = xdata_->account)
        return acct;
    assert(account);
    return account;
  }

  const account_t * reported_account() const {
    return const_cast<post_t *>(this)->reported_account();
  }

  friend class xact_t;

  struct compare_by_date_and_sequence
  {
    bool operator()(const post_t * left, const post_t * right) const {
      gregorian::date_duration duration =
        left->primary_date() - right->primary_date();
      if (duration.days() == 0) {
        return ((left->pos ? left->pos->sequence : 0) <
                (right->pos ? right->pos->sequence : 0));
      } else {
        return duration.days() < 0;
      }
    }
  };

#if HAVE_BOOST_SERIALIZATION
private:
  /** Serialization. */

  friend class boost::serialization::access;

  template<class Archive>
  void serialize(Archive& ar, const unsigned int /* version */) {
    ar & boost::serialization::base_object<item_t>(*this);
    ar & xact;
    ar & account;
    ar & amount;
    ar & amount_expr;
    ar & cost;
    ar & assigned_amount;
  }
#endif // HAVE_BOOST_SERIALIZATION
};

class journal_t;
void extend_post(post_t& post, journal_t& journal);
void put_post(property_tree::ptree& pt, const post_t& post);

} // namespace ledger

#endif // _POST_H
