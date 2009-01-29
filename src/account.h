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

#ifndef _ACCOUNT_H
#define _ACCOUNT_H

#include "utils.h"
#include "scope.h"

namespace ledger {

class account_t;
class session_t;

typedef std::map<const string, account_t *> accounts_map;

class account_t : public scope_t
{
 public:
  typedef unsigned long ident_t;

  account_t *	   parent;
  string	   name;
  optional<string> note;
  unsigned short   depth;
  accounts_map	   accounts;

  mutable void *   data;
  mutable ident_t  ident;
  mutable string   _fullname;

  account_t(account_t *   _parent = NULL,
	    const string& _name   = "",
	    const optional<string>& _note = none)
    : scope_t(), parent(_parent), name(_name), note(_note),
      depth(parent ? parent->depth + 1 : 0), data(NULL), ident(0) {
    TRACE_CTOR(account_t, "account_t *, const string&, const string&");
  }
  account_t(const account_t& other)
    : scope_t(),
      parent(other.parent),
      name(other.name),
      note(other.note),
      depth(other.depth),
      accounts(other.accounts),
      data(NULL),
      ident(0) {
    TRACE_CTOR(account_t, "copy");
    assert(other.data == NULL);
    assert(other.ident == 0);
  }
  ~account_t();

  operator string() const {
    return fullname();
  }
  string fullname() const;

  void add_account(account_t * acct) {
    accounts.insert(accounts_map::value_type(acct->name, acct));
  }
  bool remove_account(account_t * acct) {
    accounts_map::size_type n = accounts.erase(acct->name);
    return n > 0;
  }

  account_t * find_account(const string& name, bool auto_create = true);

  virtual expr_t::ptr_op_t lookup(const string& name);

  bool valid() const;

  friend class journal_t;

  struct xdata_t : public supports_flags<>
  {
#define ACCOUNT_EXT_TO_DISPLAY	     0x01
#define ACCOUNT_EXT_DISPLAYED	     0x02
#define ACCOUNT_EXT_SORT_CALC	     0x04
#define ACCOUNT_EXT_HAS_NON_VIRTUALS 0x08
#define ACCOUNT_EXT_HAS_UNB_VIRTUALS 0x10

    value_t	  value;
    value_t	  total;
    value_t	  sort_value;
    std::size_t   count;	// xacts counted toward amount
    std::size_t   total_count;	// xacts counted toward total
    std::size_t   virtuals;
    uint_least8_t dflags;

    xdata_t()
      : supports_flags<>(), count(0), total_count(0),
	virtuals(0), dflags(0)
    {
      TRACE_CTOR(account_t::xdata_t, "");
    }
    xdata_t(const xdata_t& other)
      : supports_flags<>(other.flags()),
	value(other.value),
	total(other.total),
	sort_value(other.sort_value),
	count(other.count),
	total_count(other.total_count),
	virtuals(other.virtuals),
	dflags(other.dflags)
    {
      TRACE_CTOR(account_t::xdata_t, "copy");
    }

    ~xdata_t() throw() {
      TRACE_DTOR(account_t::xdata_t);
    }
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

  void calculate_sums();
};

std::ostream& operator<<(std::ostream& out, const account_t& account);

} // namespace ledger

#endif // _ACCOUNT_H
