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

#ifndef _ENTRY_H
#define _ENTRY_H

#include "xact.h"
#include "predicate.h"

namespace ledger {

class journal_t;

typedef std::list<xact_t *> xacts_list;

class entry_base_t : public supports_flags<>
{
public:
#define ENTRY_IN_CACHE 0x1

  journal_t *      journal;
  string	   note;
  unsigned long    src_idx;
  istream_pos_type beg_pos;
  unsigned long    beg_line;
  istream_pos_type end_pos;
  unsigned long    end_line;
  xacts_list	   xacts;

  entry_base_t() : journal(NULL),
    beg_pos(0), beg_line(0), end_pos(0), end_line(0)
  {
    TRACE_CTOR(entry_base_t, "");
  }
  entry_base_t(const entry_base_t& e);  

  virtual ~entry_base_t();

  bool operator==(const entry_base_t& entry) {
    return this == &entry;
  }
  bool operator!=(const entry_base_t& entry) {
    return ! (*this == entry);
  }

  virtual void add_xact(xact_t * xact);
  virtual bool remove_xact(xact_t * xact);

  virtual bool finalize();
  virtual bool valid() const = 0;
};

class entry_t : public entry_base_t, public scope_t
{
public:
  datetime_t	       _date;
  optional<datetime_t> _date_eff;
  optional<string>     code;
  string	       payee;

  entry_t() {
    TRACE_CTOR(entry_t, "");
  }
  entry_t(const entry_t& e);

  virtual ~entry_t() {
    TRACE_DTOR(entry_t);
  }

  datetime_t actual_date() const {
    return _date;
  }
  datetime_t effective_date() const {
    if (! _date_eff)
      return _date;
    return *_date_eff;
  }
  datetime_t date() const {
    if (xact_t::use_effective_date)
      return effective_date();
    else
      return actual_date();
  }

  bool get_state(xact_t::state_t * state) const;

  virtual void add_xact(xact_t * xact);

  virtual expr_t::ptr_op_t lookup(const string& name);

  virtual bool valid() const;
};

struct entry_finalizer_t {
  virtual ~entry_finalizer_t() {}
  virtual bool operator()(entry_t& entry, bool post) = 0;
};

class entry_context : public error_context {
 public:
  const entry_base_t& entry;

  entry_context(const entry_base_t& _entry,
		const string& _desc = "") throw()
    : error_context(_desc), entry(_entry) {}
  virtual ~entry_context() throw() {}

  virtual void describe(std::ostream& out) const throw();
};

class auto_entry_t : public entry_base_t
{
public:
  item_predicate<xact_t> predicate;

  auto_entry_t() {
    TRACE_CTOR(auto_entry_t, "");
  }
  auto_entry_t(const auto_entry_t& other)
    : entry_base_t(), predicate(other.predicate) {
    TRACE_CTOR(auto_entry_t, "copy");
  }
  auto_entry_t(const string& _predicate)
    : predicate(_predicate)
  {
    TRACE_CTOR(auto_entry_t, "const string&");
  }

  virtual ~auto_entry_t() {
    TRACE_DTOR(auto_entry_t);
  }

  virtual void extend_entry(entry_base_t& entry, bool post);
  virtual bool valid() const {
    return true;
  }
};

struct auto_entry_finalizer_t : public entry_finalizer_t
{
  journal_t * journal;

  auto_entry_finalizer_t() : journal(NULL) {
    TRACE_CTOR(auto_entry_finalizer_t, "");
  }
  auto_entry_finalizer_t(const auto_entry_finalizer_t& other)
    : entry_finalizer_t(), journal(other.journal) {
    TRACE_CTOR(auto_entry_finalizer_t, "copy");
  }
  auto_entry_finalizer_t(journal_t * _journal) : journal(_journal) {
    TRACE_CTOR(auto_entry_finalizer_t, "journal_t *");
  }
  ~auto_entry_finalizer_t() throw() {
    TRACE_DTOR(auto_entry_finalizer_t);
  }

  virtual bool operator()(entry_t& entry, bool post);
};

class period_entry_t : public entry_base_t
{
 public:
  interval_t period;
  string     period_string;

  period_entry_t() {
    TRACE_CTOR(period_entry_t, "");
  }
  period_entry_t(const period_entry_t& e)
    : entry_base_t(e), period(e.period), period_string(e.period_string) {
    TRACE_CTOR(period_entry_t, "copy");
  }
  period_entry_t(const string& _period)
    : period(_period), period_string(_period) {
    TRACE_CTOR(period_entry_t, "const string&");
  }

  virtual ~period_entry_t() throw() {
    TRACE_DTOR(period_entry_t);
  }

  virtual bool valid() const {
    return period;
  }
};

class func_finalizer_t : public entry_finalizer_t
{
  func_finalizer_t();

public:
  typedef function<bool (entry_t& entry, bool post)> func_t;

  func_t func;

  func_finalizer_t(func_t _func) : func(_func) {
    TRACE_CTOR(func_finalizer_t, "func_t");
  }
  func_finalizer_t(const func_finalizer_t& other) :
    entry_finalizer_t(), func(other.func) {
    TRACE_CTOR(func_finalizer_t, "copy");
  }
  ~func_finalizer_t() throw() {
    TRACE_DTOR(func_finalizer_t);
  }

  virtual bool operator()(entry_t& entry, bool post) {
    return func(entry, post);
  }
};

typedef std::list<entry_t *>	    entries_list;
typedef std::list<auto_entry_t *>   auto_entries_list;
typedef std::list<period_entry_t *> period_entries_list;

void extend_entry_base(journal_t * journal, entry_base_t& entry, bool post);

inline bool auto_entry_finalizer_t::operator()(entry_t& entry, bool post) {
  extend_entry_base(journal, entry, post);
  return true;
}

} // namespace ledger

#endif // _ENTRY_H
