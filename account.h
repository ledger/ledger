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

#ifndef _ACCOUNT_H
#define _ACCOUNT_H

#include "utils.h"

namespace ledger {

class account_t;

typedef std::map<const string, account_t *> accounts_map;

class account_t
{
 public:
  typedef unsigned long ident_t;

  account_t *	   parent;
  string	   name;
  optional<string> note;
  unsigned short   depth;
  accounts_map	   accounts;

  mutable void *  data;
  mutable ident_t ident;
  mutable string  _fullname;

  account_t(account_t *   _parent = NULL,
	    const string& _name   = "",
	    const optional<string>& _note = none)
    : parent(_parent), name(_name), note(_note),
      depth(parent ? parent->depth + 1 : 0), data(NULL), ident(0) {
    TRACE_CTOR(account_t, "account_t *, const string&, const string&");
  }
  account_t(const account_t& other)
    : parent(other.parent),
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

  bool valid() const;

  friend class journal_t;
};

std::ostream& operator<<(std::ostream& out, const account_t& account);

} // namespace ledger

#endif // _ACCOUNT_H
