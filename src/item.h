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
 * @defgroup data Data representation
 */

/**
 * @file   item.h
 * @author John Wiegley
 *
 * @ingroup data
 */
#ifndef _ITEM_H
#define _ITEM_H

#include "scope.h"

namespace ledger {

struct position_t
{
  path             pathname;
  istream_pos_type beg_pos;
  std::size_t      beg_line;
  istream_pos_type end_pos;
  std::size_t      end_line;
  std::size_t      sequence;

  position_t()
    : beg_pos(0), beg_line(0), end_pos(0), end_line(0), sequence(0) {
    TRACE_CTOR(position_t, "");
  }
  position_t(const position_t& pos) {
    *this = pos;
    TRACE_CTOR(position_t, "copy");
  }
  ~position_t() throw() {
    TRACE_DTOR(position_t);
  }

  position_t& operator=(const position_t& pos) {
    if (this != &pos) {
      pathname  = pos.pathname;
      beg_pos   = pos.beg_pos;
      beg_line  = pos.beg_line;
      end_pos   = pos.end_pos;
      end_line  = pos.end_line;
      sequence  = pos.sequence;
    }
    return *this;
  }
};

class item_t : public supports_flags<uint_least16_t>, public scope_t
{
public:
#define ITEM_NORMAL            0x00 // no flags at all, a basic posting
#define ITEM_GENERATED         0x01 // posting was not found in a journal
#define ITEM_TEMP              0x02 // posting is a managed temporary
#define ITEM_NOTE_ON_NEXT_LINE 0x04 // did we see a note on the next line?

  enum state_t { UNCLEARED = 0, CLEARED, PENDING };

  typedef std::pair<optional<value_t>, bool> tag_data_t;
  typedef std::map<string, tag_data_t,
                   std::function<bool(string, string)> > string_map;

  state_t              _state;
  optional<date_t>     _date;
  optional<date_t>     _date_aux;
  optional<string>     note;
  optional<position_t> pos;
  optional<string_map> metadata;

  item_t(flags_t _flags = ITEM_NORMAL, const optional<string>& _note = none)
    : supports_flags<uint_least16_t>(_flags), _state(UNCLEARED), note(_note)
  {
    TRACE_CTOR(item_t, "flags_t, const string&");
  }
  item_t(const item_t& item) : supports_flags<uint_least16_t>(), scope_t()
  {
    copy_details(item);
    TRACE_CTOR(item_t, "copy");
  }
  virtual ~item_t() {
    TRACE_DTOR(item_t);
  }

  virtual void copy_details(const item_t& item)
  {
    set_flags(item.flags());
    set_state(item.state());

    _date     = item._date;
    _date_aux = item._date_aux;
    note      = item.note;
    pos       = item.pos;
    metadata  = item.metadata;
  }

  virtual bool operator==(const item_t& xact) {
    return this == &xact;
  }
  virtual bool operator!=(const item_t& xact) {
    return ! (*this == xact);
  }

  string id() const {
    if (optional<value_t> ref = get_tag(_("UUID"))) {
      return ref->to_string();
    } else {
      std::ostringstream buf;
      buf << seq();
      return buf.str();
    }
  }
  std::size_t seq() const {
    return pos ? pos->sequence : 0L;
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

  virtual string_map::iterator
  set_tag(const string&            tag,
          const optional<value_t>& value              = none,
          const bool               overwrite_existing = true);

  virtual void parse_tags(const char * p,
                          scope_t&     scope,
                          bool         overwrite_existing = true);
  virtual void append_note(const char * p,
                           scope_t&     scope,
                           bool         overwrite_existing = true);

  static bool use_aux_date;

  virtual bool has_date() const {
    return static_cast<bool>(_date);
  }

  virtual date_t date() const {
    assert(_date);
    if (use_aux_date)
      if (optional<date_t> aux = aux_date())
        return *aux;
    return *_date;
  }
  virtual date_t primary_date() const {
    assert(_date);
    return *_date;
  }
  virtual optional<date_t> aux_date() const {
    return _date_aux;
  }

  void set_state(state_t new_state) {
    _state = new_state;
  }
  virtual state_t state() const {
    return _state;
  }

  virtual void define(const symbol_t::kind_t, const string&,
                      expr_t::ptr_op_t);
  virtual expr_t::ptr_op_t lookup(const symbol_t::kind_t kind,
                                  const string& name);

  bool valid() const;
};

value_t get_comment(item_t& item);
void    print_item(std::ostream& out, const item_t& item,
                   const string& prefix = "");
string  item_context(const item_t& item, const string& desc);
void    put_metadata(property_tree::ptree& pt, const item_t::string_map& metadata);

} // namespace ledger

#endif // _ITEM_H
