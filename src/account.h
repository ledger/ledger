/*
 * Copyright (c) 2003-2023, John Wiegley.  All rights reserved.
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
 * @file   account.h
 * @author John Wiegley
 *
 * @ingroup data
 */
#pragma once

#include "scope.h"

namespace ledger {

using namespace boost::placeholders;

class account_t;
class xact_t;
class post_t;

typedef std::list<post_t *> posts_list;
typedef std::map<string, account_t *> accounts_map;
typedef std::map<string, posts_list> deferred_posts_map_t;

class account_t : public flags::supports_flags<>, public scope_t
{
#define ACCOUNT_NORMAL    0x00  // no flags at all, a basic account
#define ACCOUNT_KNOWN     0x01
#define ACCOUNT_TEMP      0x02  // account is a temporary object
#define ACCOUNT_GENERATED 0x04  // account never actually existed

public:
  account_t *                    parent;
  string                         name;
  optional<string>               note;
  unsigned short                 depth;
  accounts_map                   accounts;
  posts_list                     posts;
  optional<deferred_posts_map_t> deferred_posts;
  optional<expr_t>               value_expr;

  mutable string   _fullname;

  account_t(account_t *             _parent = NULL,
            const string&           _name   = "",
            const optional<string>& _note   = none)
    : supports_flags<>(), scope_t(), parent(_parent),
      name(_name), note(_note),
      depth(static_cast<unsigned short>(parent ? parent->depth + 1 : 0))
  {
    TRACE_CTOR(account_t, "account_t *, const string&, const string&");
  }
  account_t(const account_t& other)
    : supports_flags<>(other.flags()), scope_t(),
      parent(other.parent),
      name(other.name),
      note(other.note),
      depth(other.depth),
      accounts(other.accounts)
  {
    TRACE_CTOR(account_t, "copy");
  }
  virtual ~account_t();

  virtual string description() {
    return string(_("account ")) + fullname();
  }

  operator string() const {
    return fullname();
  }
  string fullname() const;
  string partial_name(bool flat = false) const;

  void add_account(account_t * acct) {
    accounts.insert(accounts_map::value_type(acct->name, acct));
  }
  bool remove_account(account_t * acct) {
    accounts_map::size_type n = accounts.erase(acct->name);
    return n > 0;
  }

  account_t * find_account(const string& name, bool auto_create = true);
  account_t * find_account_re(const string& regexp);

  typedef transform_iterator<function<account_t *(accounts_map::value_type&)>,
                             accounts_map::iterator>
    accounts_map_seconds_iterator;

  accounts_map_seconds_iterator accounts_begin() {
    return make_transform_iterator
      (accounts.begin(), boost::bind(&accounts_map::value_type::second, _1));
  }
  accounts_map_seconds_iterator accounts_end() {
    return make_transform_iterator
      (accounts.end(), boost::bind(&accounts_map::value_type::second, _1));
  }

  void add_post(post_t * post);
  void add_deferred_post(const string& uuid, post_t * post);
  void apply_deferred_posts();
  bool remove_post(post_t * post);

  posts_list::iterator posts_begin() {
    return posts.begin();
  }
  posts_list::iterator posts_end() {
    return posts.end();
  }

  virtual expr_t::ptr_op_t lookup(const symbol_t::kind_t kind,
                                  const string& name);

  bool valid() const;

  friend class journal_t;

  struct xdata_t : public supports_flags<>
  {
#define ACCOUNT_EXT_SORT_CALC        0x01
#define ACCOUNT_EXT_HAS_NON_VIRTUALS 0x02
#define ACCOUNT_EXT_HAS_UNB_VIRTUALS 0x04
#define ACCOUNT_EXT_AUTO_VIRTUALIZE  0x08
#define ACCOUNT_EXT_VISITED          0x10
#define ACCOUNT_EXT_MATCHING         0x20
#define ACCOUNT_EXT_TO_DISPLAY       0x40
#define ACCOUNT_EXT_DISPLAYED        0x80

    struct details_t
    {
      value_t            total;
      value_t            real_total;
      bool               calculated;
      bool               gathered;

      std::size_t        posts_count;
      std::size_t        posts_virtuals_count;
      std::size_t        posts_cleared_count;
      std::size_t        posts_last_7_count;
      std::size_t        posts_last_30_count;
      std::size_t        posts_this_month_count;

      date_t             earliest_post;
      date_t             earliest_cleared_post;
      date_t             latest_post;
      date_t             latest_cleared_post;

      datetime_t         earliest_checkin;
      datetime_t         latest_checkout;
      bool               latest_checkout_cleared;

      std::set<path>     filenames;
      std::set<string>   accounts_referenced;
      std::set<string>   payees_referenced;

      optional<posts_list::const_iterator> last_post;
      optional<posts_list::const_iterator> last_reported_post;

      details_t()
        : calculated(false),
          gathered(false),

          posts_count(0),
          posts_virtuals_count(0),
          posts_cleared_count(0),
          posts_last_7_count(0),
          posts_last_30_count(0),
          posts_this_month_count(0),
          latest_checkout_cleared(false)
      {
        TRACE_CTOR(account_t::xdata_t::details_t, "");
      }
      // A copy copies nothing
      details_t(const details_t&)
        : calculated(false),
          gathered(false),

          posts_count(0),
          posts_virtuals_count(0),
          posts_cleared_count(0),
          posts_last_7_count(0),
          posts_last_30_count(0),
          posts_this_month_count(0),
          latest_checkout_cleared(false)
      {
        TRACE_CTOR(account_t::xdata_t::details_t, "copy");
      }
      ~details_t() throw() {
        TRACE_DTOR(account_t::xdata_t::details_t);
      }

      details_t& operator+=(const details_t& other);

      void update(post_t& post, bool gather_all = false);
    };

    details_t  self_details;
    details_t  family_details;
    posts_list reported_posts;

    std::list<sort_value_t> sort_values;

    xdata_t() : supports_flags<>()
    {
      TRACE_CTOR(account_t::xdata_t, "");
    }
    xdata_t(const xdata_t& other)
      : supports_flags<>(other.flags()),
        self_details(other.self_details),
        family_details(other.family_details),
        sort_values(other.sort_values)
    {
      TRACE_CTOR(account_t::xdata_t, "copy");
    }
    ~xdata_t() throw() {
      TRACE_DTOR(account_t::xdata_t);
    }
  };

  // This variable holds optional "extended data" which is usually produced
  // only during reporting, and only for the posting set being reported.
  // It's a memory-saving measure to delay allocation until the last possible
  // moment.
  mutable optional<xdata_t> xdata_;

  bool has_xdata() const {
    return static_cast<bool>(xdata_);
  }
  void clear_xdata();
  xdata_t& xdata() {
    if (! xdata_)
      xdata_ = xdata_t();
    return *xdata_;
  }
  const xdata_t& xdata() const {
    assert(xdata_);
    return *xdata_;
  }

  value_t amount(const optional<bool> real_only = false, const optional<expr_t&>& expr = none) const;
  value_t total(const optional<expr_t&>& expr = none) const;

  const xdata_t::details_t& self_details(bool gather_all = true) const;
  const xdata_t::details_t& family_details(bool gather_all = true) const;

  bool has_xflags(xdata_t::flags_t flags) const {
    return xdata_ && xdata_->has_flags(flags);
  }
  bool children_with_xdata() const;
  std::size_t children_with_flags(xdata_t::flags_t flags) const;
};

std::ostream& operator<<(std::ostream& out, const account_t& account);

void put_account(property_tree::ptree& pt, const account_t& acct,
                 function<bool(const account_t&)> pred);

//simple struct added to allow std::map to compare accounts in the accounts report
struct account_compare {
  bool operator() (const account_t& lhs, const account_t& rhs) const {
    return (lhs.fullname().compare(rhs.fullname()) < 0);
  }
};

} // namespace ledger
