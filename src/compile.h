/*
 * Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
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

#ifndef _COMPILE_H
#define _COMPILE_H

#include "node.h"
#include "journal.h"

namespace ledger {
namespace xml {

#if 0
class commodity_node_t : public parent_node_t
{
public:
  commodity_t * commodity;

  commodity_node_t(document_t *    _document,
		   commodity_t *   _commodity,
		   parent_node_t * _parent = NULL)
    : parent_node_t(_document, _parent), commodity(_commodity) {
    TRACE_CTOR(commodity_node_t, "document_t *, commodity_t *, parent_node_t *");
    set_name(document_t::COMMODITY);
  }
  virtual ~commodity_node_t() {
    TRACE_DTOR(commodity_node_t);
  }

  virtual node_t * children() const;
};

class amount_node_t : public parent_node_t
{
public:
  amount_t * amount;

  amount_node_t(document_t *    _document,
		amount_t *      _amount,
		parent_node_t * _parent = NULL)
    : parent_node_t(_document, _parent), amount(_amount) {
    TRACE_CTOR(amount_node_t, "document_t *, amount_t *, parent_node_t *");
    set_name(document_t::AMOUNT);
  }
  virtual ~amount_node_t() {
    TRACE_DTOR(amount_node_t);
  }

  virtual node_t * children() const;

  virtual value_t to_value() const {
    return *amount;
  }
};
#endif

class entry_node_t : public parent_node_t
{
public:
  shared_ptr<entry_t> entry;

  entry_node_t(nameid_t    _name_id,
	       document_t& _document,
	       const optional<parent_node_t&>& _parent = none,
	       entry_t *   _entry = NULL)
    : parent_node_t(_name_id, _document, _parent), entry(_entry) {
    TRACE_CTOR(entry_node_t, "document_t&, parent_node_t, entry_t *");
    assert(_name_id == ENTRY_NODE);
  }
  virtual ~entry_node_t() {
    TRACE_DTOR(entry_node_t);
  }

  virtual void compile();
};

class transaction_node_t : public parent_node_t
{
public:
  shared_ptr<transaction_t> transaction;

  transaction_node_t(nameid_t        _name_id,
		     document_t&     _document,
		     const optional<parent_node_t&>& _parent = none,
		     transaction_t * _transaction = NULL)
    : parent_node_t(_name_id, _document, _parent),
      transaction(_transaction) {
    TRACE_CTOR(transaction_node_t,
	       "document_t&, parent_node_t, transaction_t *");
    assert(_name_id == TRANSACTION_NODE);
  }
  virtual ~transaction_node_t() {
    TRACE_DTOR(transaction_node_t);
  }
};

#if 0
class entry_node_t : public parent_node_t
{
  entry_t *  entry;

public:
  entry_node_t(document_t * _document, entry_t * _entry,
	       parent_node_t * _parent = NULL)
    : parent_node_t(_document, _parent), entry(_entry) {
    TRACE_CTOR(entry_node_t, "document_t *, entry_t *, parent_node_t *");
    set_name(document_t::ENTRY);
  }
  virtual ~entry_node_t() {
    TRACE_DTOR(entry_node_t);
  }

  virtual node_t * children() const;
  virtual node_t * lookup_child(int _name_id) const;

  friend class transaction_node_t;
};

class account_node_t : public parent_node_t
{
  account_t * account;

public:
  account_node_t(document_t * _document, account_t * _account,
		 parent_node_t * _parent = NULL)
    : parent_node_t(_document, _parent), account(_account) {
    TRACE_CTOR(account_node_t, "document_t *, account_t *, parent_node_t *");
    set_name(document_t::ACCOUNT);
  }
  virtual ~account_node_t() {
    TRACE_DTOR(account_node_t);
  }

  virtual node_t * children() const;
};

class journal_node_t : public parent_node_t
{
  journal_t * journal;

public:
  journal_node_t(document_t * _document, journal_t * _journal,
		 parent_node_t * _parent = NULL)
    : parent_node_t(_document, _parent), journal(_journal) {
    TRACE_CTOR(journal_node_t, "document_t *, journal_t *, parent_node_t *");
    set_name(document_t::JOURNAL);
  }
  virtual ~journal_node_t() {
    TRACE_DTOR(journal_node_t);
  }

  virtual node_t * children() const;

  friend class transaction_node_t;
};

template <typename T>
inline typename T::node_type *
wrap_node(document_t * doc, T * item, void * parent_node = NULL) {
  assert(false);
  return NULL;
}

template <>
inline transaction_t::node_type *
wrap_node(document_t * doc, transaction_t * xact, void * parent_node) {
  return new transaction_node_t(doc, xact, (parent_node_t *)parent_node);
}

template <>
inline entry_t::node_type *
wrap_node(document_t * doc, entry_t * entry, void * parent_node) {
  return new entry_node_t(doc, entry, (parent_node_t *)parent_node);
}

template <>
inline account_t::node_type *
wrap_node(document_t * doc, account_t * account, void * parent_node) {
  return new account_node_t(doc, account, (parent_node_t *)parent_node);
}

template <>
inline journal_t::node_type *
wrap_node(document_t * doc, journal_t * journal, void * parent_node) {
  return new journal_node_t(doc, journal, (parent_node_t *)parent_node);
}
#endif

} // namespace xml
} // namespace ledger

#endif // _COMPILE_H
