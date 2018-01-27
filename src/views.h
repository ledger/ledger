/*
 * Copyright (c) 2003-2018, John Wiegley.  All rights reserved.
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
 * @addtogroup views
 */

/**
 * @file   views.h
 * @author John Wiegley
 *
 * @ingroup views
 */
#ifndef _VIEWS_H
#define _VIEWS_H

#include "utils.h"

#if DOCUMENT_MODEL

#include "scope.h"
#include "item.h"
#include "report.h"
#include "post.h"
#include "predicate.h"

namespace ledger {

class journal_t;
class xact_t;
class post_t;
class account_t;
class report_t;

class r_base_t : public supports_flags<uint_least16_t>,
                 public scope_t
{
public:
  r_base_t() : refc(0) {
    TRACE_CTOR(r_base_t, "");
  }
  r_base_t(const r_base_t& other) : refc(0) {
    TRACE_CTOR(r_base_t, "copy");
  }
  virtual ~r_base_t() {
    TRACE_DTOR(r_base_t);
  }

protected:
  /**
   * `refc' holds the current reference count for each object.
   */
  mutable int refc;

  /**
   * Reference counting methods.  The intrusive_ptr_* methods are used
   * by boost::intrusive_ptr to manage the calls to acquire and release.
   */
  void acquire() const {
    VERIFY(refc >= 0);
    refc++;
  }
  void release() const {
    VERIFY(refc > 0);
    if (--refc == 0)
      checked_delete(this);
  }

  friend inline void intrusive_ptr_add_ref(r_base_t * r_ptr) {
    r_ptr->acquire();
  }
  friend inline void intrusive_ptr_release(r_base_t * r_ptr) {
    r_ptr->release();
  }
};

class r_journal_t;
class r_item_t;
class r_xact_t;
class r_post_t;
class r_account_t;

typedef intrusive_ptr<r_journal_t> r_journal_ptr;
typedef intrusive_ptr<r_item_t>    r_item_ptr;
typedef intrusive_ptr<r_xact_t>    r_xact_ptr;
typedef intrusive_ptr<r_post_t>    r_post_ptr;
typedef intrusive_ptr<r_account_t> r_account_ptr;

typedef std::list<r_xact_ptr> r_xacts_list;
typedef std::list<r_post_ptr> r_posts_list;

class r_journal_t : public r_base_t
{
  journal_t * base_journal;

  journal_t * ptr() {
    return base_journal;
  }
  const journal_t * ptr() const {
    return base_journal;
  }

  r_account_ptr master_ptr;
  r_xacts_list  xacts;
  r_posts_list  posts;

  void set_master(r_account_ptr ptr) {
    master_ptr = ptr;
  }

public:
  r_journal_t(journal_t * _journal, r_account_ptr _master)
    : r_base_t(), base_journal(_journal), master_ptr(_master) {
    TRACE_CTOR(r_journal_t, "journal_t *, account_t *");
  }
  r_journal_t(const r_journal_t& other)
    : r_base_t(other),
      base_journal(other.base_journal),
      master_ptr(other.master_ptr),
      xacts(other.xacts),
      posts(other.posts) {
    TRACE_CTOR(r_journal_t, "copy");
  }
  virtual ~r_journal_t() {
    TRACE_DTOR(r_journal_t);
  }

  r_xact_ptr create_xact(xact_t * xact = NULL);

  void add_xact(r_xact_ptr xact);

  r_xacts_list::iterator xacts_begin() {
    return xacts.begin();
  }
  r_xacts_list::iterator xacts_end() {
    return xacts.end();
  }

  r_post_ptr add_post(post_t * post);
  void add_post(r_post_ptr post);

  r_post_ptr create_post(post_t * post = NULL, r_xact_ptr xact = NULL,
                         r_account_ptr account = NULL);
  r_post_ptr create_post(r_post_ptr post, r_xact_ptr xact = NULL,
                         r_account_ptr account = NULL);

  r_posts_list::iterator posts_begin() {
    return posts.begin();
  }
  r_posts_list::iterator posts_end() {
    return posts.end();
  }

  r_account_ptr create_account(account_t * account = NULL);
  r_account_ptr create_account(const std::string& name);

  friend void put_journal(property_tree::ptree& pt, r_journal_ptr journal);
};

class r_item_t : public r_base_t
{
protected:
  item_t * base_item;

  item_t * ptr() {
    return base_item;
  }
  const item_t * ptr() const {
    return base_item;
  }

  r_journal_ptr journal_ptr;

public:
  r_item_t(r_journal_ptr _journal_ptr, item_t * _item)
    : r_base_t(), base_item(_item), journal_ptr(_journal_ptr) {
    TRACE_CTOR(r_item_t, "r_journal_ptr, item_t *");
  }
  r_item_t(const r_item_t& other)
    : r_base_t(other),
      base_item(other.base_item),
      journal_ptr(other.journal_ptr) {
    TRACE_CTOR(r_item_t, "copy");
  }
  virtual ~r_item_t() {
    TRACE_DTOR(r_item_t);
  }

  const optional<position_t> position() const;

  string id() const {
    return ptr()->id();
  }
  std::size_t seq() const {
    return ptr()->seq();
  }

  date_t date() const;
  void set_date(const date_t& when);

  item_t::state_t state() const;
  void set_state(item_t::state_t val);

  string payee() const;
  void set_payee(const string& name);

  optional<string> note() const {
    return ptr()->note;
  }

  bool has_tag(const string& tag) const {
    return ptr()->has_tag(tag);
  }
  bool has_tag(const mask_t&           tag_mask,
               const optional<mask_t>& value_mask = none) const {
    return ptr()->has_tag(tag_mask, value_mask);
  }

  optional<value_t> get_tag(const string& tag) const {
    return ptr()->get_tag(tag);
  }
  optional<value_t> get_tag(const mask_t&           tag_mask,
                            const optional<mask_t>& value_mask = none) const {
    return ptr()->get_tag(tag_mask, value_mask);
  }

  void set_tag(const string&            tag,
               const optional<value_t>& value              = none,
               const bool               overwrite_existing = true) {
    ptr()->set_tag(tag, value, overwrite_existing);
  }

  /**
   * Symbol scope methods.
   */
  virtual void define(const symbol_t::kind_t, const string&,
                      expr_t::ptr_op_t);
  virtual expr_t::ptr_op_t lookup(const symbol_t::kind_t kind,
                                  const string& name);

  friend class r_journal_t;
  friend void put_item(property_tree::ptree& pt, r_item_ptr journal);
};

class r_xact_t : public r_item_t
{
  xact_t * ptr() {
    return reinterpret_cast<xact_t *>(base_item);
  }
  const xact_t * ptr() const {
    return reinterpret_cast<const xact_t *>(base_item);
  }

  r_posts_list posts;

public:
  r_xact_t(r_journal_ptr journal_ptr, xact_t * _xact)
    : r_item_t(journal_ptr, reinterpret_cast<item_t *>(_xact)) {
    TRACE_CTOR(r_xact_t, "r_journal_ptr, xact_t *");
  }
  r_xact_t(const r_xact_t& other)
    : r_item_t(other),
      posts(other.posts) {
    TRACE_CTOR(r_xact_t, "copy");
  }
  virtual ~r_xact_t() {
    TRACE_DTOR(r_xact_t);
  }

  virtual string description();

  void add_post(r_post_ptr post);

#if 0
  r_post_ptr create_post(post_t * post = NULL, r_account_ptr account = NULL);
  r_post_ptr create_post(r_post_ptr post, r_account_ptr account = NULL);
#endif

  r_posts_list::iterator posts_begin() {
    return posts.begin();
  }
  r_posts_list::iterator posts_end() {
    return posts.end();
  }

  string code() const;
  string payee() const;

  friend class r_journal_t;
  friend void put_xact(property_tree::ptree& pt, r_xact_ptr journal);
};

class r_post_t : public r_item_t
{
  post_t * ptr() {
    return reinterpret_cast<post_t *>(base_item);
  }
  const post_t * ptr() const {
    return reinterpret_cast<const post_t *>(base_item);
  }

  r_xact_ptr    xact_ptr;
  r_account_ptr account_ptr;

  void set_xact(r_xact_ptr ptr) {
    xact_ptr = ptr;
  }
  void set_account(r_account_ptr ptr) {
    account_ptr = ptr;
  }

public:
  r_post_t(r_journal_ptr journal_ptr, post_t * _post,
           r_xact_ptr _xact_ptr, r_account_ptr _account_ptr)
    : r_item_t(journal_ptr, reinterpret_cast<item_t *>(_post)),
      xact_ptr(_xact_ptr), account_ptr(_account_ptr) {
    TRACE_CTOR(r_post_t, "r_journal_ptr, post_t *, r_xact_ptr, r_account_ptr");
  }
  r_post_t(const r_post_t& other)
    : r_item_t(other),
      xact_ptr(other.xact_ptr),
      account_ptr(other.account_ptr) {
    TRACE_CTOR(r_post_t, "copy");
  }
  virtual ~r_post_t() {
    TRACE_DTOR(r_post_t);
  }

  virtual string description();

  string payee() const;

  r_xact_ptr    xact();
  r_account_ptr account();

  value_t amount() const;
  value_t cost() const;

  std::size_t count() const;
  value_t     running_total() const;

  optional<datetime_t> checkin() const;
  optional<datetime_t> checkout() const;

  friend class r_journal_t;
  friend void put_post(property_tree::ptree& pt, r_post_ptr journal);
};

typedef std::map<string, r_account_ptr> r_accounts_map;

class r_account_t : public r_base_t
{
  r_journal_ptr  journal_ptr;
  r_account_ptr  parent_ptr;
  r_accounts_map accounts;
  r_posts_list   posts;

  string name;

  mutable string _fullname;

public:
  r_account_t(r_journal_ptr _journal_ptr, r_account_ptr _parent_ptr,
              string _name)
    : r_base_t(), journal_ptr(_journal_ptr), parent_ptr(_parent_ptr),
      name(_name) {
    TRACE_CTOR(r_account_t, "r_journal_ptr, r_account_ptr, string");
  }
  r_account_t(const r_account_t& other)
    : r_base_t(other),
      journal_ptr(other.journal_ptr),
      parent_ptr(other.parent_ptr),
      accounts(other.accounts),
      posts(other.posts),
      name(other.name),
      _fullname(other._fullname) {
    TRACE_CTOR(r_account_t, "copy");
  }
  virtual ~r_account_t() {
    TRACE_DTOR(r_account_t);
  }

  virtual string description();

  void add_post(r_post_ptr post);

  r_posts_list::iterator posts_begin() {
    return posts.begin();
  }
  r_posts_list::iterator posts_end() {
    return posts.end();
  }

  r_account_ptr create_account(const std::string& name);

  string fullname() const;

  /**
   * Symbol scope methods.
   */
  virtual void define(const symbol_t::kind_t, const string&,
                      expr_t::ptr_op_t) {}
  virtual expr_t::ptr_op_t lookup(const symbol_t::kind_t kind,
                                  const string& fname) {
    return NULL;
  }

  friend class r_journal_t;
  friend void put_account(property_tree::ptree& pt, r_account_ptr journal);
};

template <typename PostsIterator>
void populate_journal(r_journal_ptr journal, report_t& report,
                      PostsIterator iter, predicate_t& pred)
{
  while (post_t * post = *iter) {
    bind_scope_t bound_scope(report, *post);
    if (pred.calc(bound_scope))
      journal->add_post(post);

    iter.increment();
  }
}

} // namespace ledger

#endif /* DOCUMENT_MODEL */

#endif // _VIEWS_H
