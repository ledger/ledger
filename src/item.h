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

#ifndef _ITEM_H
#define _ITEM_H

#include "utils.h"
#include "scope.h"

namespace ledger {

class entry_t;
class account_t;

class item_t;
typedef std::list<item_t *> items_list;

class item_t : public supports_flags<>, public scope_t
{
public:
#define ITEM_NORMAL     0x0000	// no flags at all, a basic transaction
#define ITEM_IN_CACHE   0x0001  // transaction allocated by the binary cache
#define ITEM_GENERATED  0x0002  // transaction was not found in a journal
#define ITEM_TEMP       0x0004  // transaction is a temporary object

  enum state_t { UNCLEARED = 0, CLEARED, PENDING };

  state_t	     _state;

  optional<date_t>   _date;
  optional<date_t>   _date_eff;
  optional<string>   note;

  unsigned short     src_idx;
  istream_pos_type   beg_pos;
  unsigned long	     beg_line;
  istream_pos_type   end_pos;
  unsigned long	     end_line;

  static  bool	     use_effective_date;

  item_t(flags_t _flags = ITEM_NORMAL, const optional<string>& _note = none)
    : supports_flags<>(_flags), _state(UNCLEARED), note(_note),
      beg_pos(0), beg_line(0), end_pos(0), end_line(0)
  {
    TRACE_CTOR(item_t, "flags_t, const string&");
  }
  item_t(const item_t& item) : supports_flags<>(), scope_t()
  {
    TRACE_CTOR(item_t, "copy");
    copy_details(item);
  }
  virtual ~item_t() {
    TRACE_DTOR(item_t);
  }

  void copy_details(const item_t& item)
  {
    set_flags(item.flags());
    set_state(item.state());

    _date     = item._date;
    _date_eff = item._date_eff;

    note      = item.note;

    beg_pos   = item.beg_pos;
    beg_line  = item.beg_line;
    end_pos   = item.end_pos;
    end_line  = item.end_line;
  }

  virtual bool operator==(const item_t& entry) {
    return this == &entry;
  }
  virtual bool operator!=(const item_t& entry) {
    return ! (*this == entry);
  }

  virtual optional<date_t> actual_date() const {
    return _date;
  }
  virtual optional<date_t> effective_date() const {
    return _date_eff;
  }
  optional<date_t> date() const {
    if (use_effective_date && _date_eff)
      return effective_date();
    else
      return actual_date();
  }

  void set_state(state_t new_state) {
    _state = new_state;
  }
  virtual state_t state() const {
    return _state;
  }

  virtual expr_t::ptr_op_t lookup(const string& name);

  bool valid() const;
};

} // namespace ledger

#endif // _ITEM_H
