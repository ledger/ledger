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
 * @defgroup data Data representation
 */

/**
 * @file   item.h
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _ITEM_H
#define _ITEM_H

#include "utils.h"
#include "scope.h"

namespace ledger {

/**
 * @brief Brief
 *
 * Long.
 */
class item_t : public supports_flags<>, public scope_t
{
public:
#define ITEM_NORMAL     0x00	// no flags at all, a basic transaction
#define ITEM_GENERATED  0x01	// transaction was not found in a journal
#define ITEM_TEMP       0x02	// transaction is a temporary object

  enum state_t { UNCLEARED = 0, CLEARED, PENDING };

  state_t	     _state;

  optional<date_t>   _date;
  optional<date_t>   _date_eff;
  optional<string>   note;

  typedef std::map<string, optional<string> > string_map;
  optional<string_map> metadata;

  path               pathname;
  istream_pos_type   beg_pos;
  std::size_t	     beg_line;
  istream_pos_type   end_pos;
  std::size_t	     end_line;

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

    pathname  = item.pathname;
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

  virtual bool has_tag(const string& tag) const;
  virtual bool has_tag(const mask_t& tag_mask,
		       const optional<mask_t>& value_mask = none) const;

  virtual optional<string> get_tag(const string& tag) const;
  virtual optional<string> get_tag(const mask_t& tag_mask,
				   const optional<mask_t>& value_mask = none) const;

  virtual void set_tag(const string& tag,
		       const optional<string>& value = none);

  virtual void parse_tags(const char * p, int current_year = -1);
  virtual void append_note(const char * p, int current_year = -1);

  static bool use_effective_date;

  virtual date_t date() const {
    assert(_date);
    if (use_effective_date)
      if (optional<date_t> effective = effective_date())
	return *effective;
    return *_date;
  }
  virtual optional<date_t> effective_date() const {
    return _date_eff;
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

value_t get_comment(item_t& item);
void	print_item(std::ostream& out, const item_t& item,
		   const string& prefix = "");
string	item_context(const item_t& item, const string& desc);

} // namespace ledger

#endif // _ITEM_H
