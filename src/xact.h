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
 * @file   xact.h
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _XACT_H
#define _XACT_H

#include "post.h"
#include "predicate.h"

namespace ledger {

class journal_t;

/**
 * @brief Brief
 *
 * Long.
 */
class xact_base_t : public item_t
{
public:
  journal_t * journal;

  posts_list  posts;

  xact_base_t() : item_t(), journal(NULL) {
    TRACE_CTOR(xact_base_t, "");
  }
  xact_base_t(const xact_base_t& e);  

  virtual ~xact_base_t();

  virtual void add_post(post_t * post);
  virtual bool remove_post(post_t * post);

  virtual bool finalize();
  virtual bool valid() const = 0;
};

/**
 * @brief Brief
 *
 * Long.
 */
class xact_t : public xact_base_t
{
public:
  optional<string> code;
  string	   payee;

  xact_t() {
    TRACE_CTOR(xact_t, "");
  }
  xact_t(const xact_t& e);

  virtual ~xact_t() {
    TRACE_DTOR(xact_t);
  }

  virtual void add_post(post_t * post);

  virtual expr_t::ptr_op_t lookup(const string& name);

  virtual bool valid() const;
};

/**
 * @brief Brief
 *
 * Long.
 */
struct xact_finalizer_t {
  virtual ~xact_finalizer_t() {}
  virtual bool operator()(xact_t& xact, bool post) = 0;
};

/**
 * @brief Brief
 *
 * Long.
 */
class auto_xact_t : public xact_base_t
{
public:
  item_predicate predicate;

  auto_xact_t() {
    TRACE_CTOR(auto_xact_t, "");
  }
  auto_xact_t(const auto_xact_t& other)
    : xact_base_t(), predicate(other.predicate) {
    TRACE_CTOR(auto_xact_t, "copy");
  }
  auto_xact_t(const item_predicate& _predicate)
    : predicate(_predicate)
  {
    TRACE_CTOR(auto_xact_t, "const item_predicate<post_t>&");
  }

  virtual ~auto_xact_t() {
    TRACE_DTOR(auto_xact_t);
  }

  virtual void extend_xact(xact_base_t& xact, bool post);
  virtual bool valid() const {
    return true;
  }
};

/**
 * @brief Brief
 *
 * Long.
 */
struct auto_xact_finalizer_t : public xact_finalizer_t
{
  journal_t * journal;

  auto_xact_finalizer_t() : journal(NULL) {
    TRACE_CTOR(auto_xact_finalizer_t, "");
  }
  auto_xact_finalizer_t(const auto_xact_finalizer_t& other)
    : xact_finalizer_t(), journal(other.journal) {
    TRACE_CTOR(auto_xact_finalizer_t, "copy");
  }
  auto_xact_finalizer_t(journal_t * _journal) : journal(_journal) {
    TRACE_CTOR(auto_xact_finalizer_t, "journal_t *");
  }
  ~auto_xact_finalizer_t() throw() {
    TRACE_DTOR(auto_xact_finalizer_t);
  }

  virtual bool operator()(xact_t& xact, bool post);
};

/**
 * @brief Brief
 *
 * Long.
 */
class period_xact_t : public xact_base_t
{
 public:
  interval_t period;
  string     period_string;

  period_xact_t() {
    TRACE_CTOR(period_xact_t, "");
  }
  period_xact_t(const period_xact_t& e)
    : xact_base_t(e), period(e.period), period_string(e.period_string) {
    TRACE_CTOR(period_xact_t, "copy");
  }
  period_xact_t(const string& _period)
    : period(_period), period_string(_period) {
    TRACE_CTOR(period_xact_t, "const string&");
  }

  virtual ~period_xact_t() throw() {
    TRACE_DTOR(period_xact_t);
  }

  virtual bool valid() const {
    return period;
  }
};

/**
 * @brief Brief
 *
 * Long.
 */
class func_finalizer_t : public xact_finalizer_t
{
  func_finalizer_t();

public:
  typedef function<bool (xact_t& xact, bool post)> func_t;

  func_t func;

  func_finalizer_t(func_t _func) : func(_func) {
    TRACE_CTOR(func_finalizer_t, "func_t");
  }
  func_finalizer_t(const func_finalizer_t& other) :
    xact_finalizer_t(), func(other.func) {
    TRACE_CTOR(func_finalizer_t, "copy");
  }
  ~func_finalizer_t() throw() {
    TRACE_DTOR(func_finalizer_t);
  }

  virtual bool operator()(xact_t& xact, bool post) {
    return func(xact, post);
  }
};

void extend_xact_base(journal_t * journal, xact_base_t& xact, bool post);

inline bool auto_xact_finalizer_t::operator()(xact_t& xact, bool post) {
  extend_xact_base(journal, xact, post);
  return true;
}

typedef std::list<xact_t *>	    xacts_list;
typedef std::list<auto_xact_t *>   auto_xacts_list;
typedef std::list<period_xact_t *> period_xacts_list;

} // namespace ledger

#endif // _XACT_H
