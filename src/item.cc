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

#include <system.hh>

#include "item.h"

namespace ledger {

bool item_t::use_aux_date = false;

bool item_t::has_tag(const string& tag, bool) const
{
  DEBUG("item.meta", "Checking if item has tag: " << tag);
  if (! metadata) {
    DEBUG("item.meta", "Item has no metadata at all");
    return false;
  }
  string_map::const_iterator i = metadata->find(tag);
#if DEBUG_ON
  if (SHOW_DEBUG("item.meta")) {
    if (i == metadata->end())
      DEBUG("item.meta", "Item does not have this tag");
    else
      DEBUG("item.meta", "Item has the tag!");
  }
#endif
  return i != metadata->end();
}

bool item_t::has_tag(const mask_t& tag_mask,
                     const optional<mask_t>& value_mask, bool) const
{
  if (metadata) {
    foreach (const string_map::value_type& data, *metadata) {
      if (tag_mask.match(data.first)) {
        if (! value_mask)
          return true;
        else if (data.second.first)
          return value_mask->match(data.second.first->to_string());
      }
    }
  }
  return false;
}

optional<value_t> item_t::get_tag(const string& tag, bool) const
{
  DEBUG("item.meta", "Getting item tag: " << tag);
  if (metadata) {
    DEBUG("item.meta", "Item has metadata");
    string_map::const_iterator i = metadata->find(tag);
    if (i != metadata->end()) {
      DEBUG("item.meta", "Found the item!");
      return (*i).second.first;
    }
  }
  return none;
}

optional<value_t> item_t::get_tag(const mask_t& tag_mask,
                                  const optional<mask_t>& value_mask,
                                  bool) const
{
  if (metadata) {
    foreach (const string_map::value_type& data, *metadata) {
      if (tag_mask.match(data.first) &&
          (! value_mask ||
           (data.second.first &&
            value_mask->match(data.second.first->to_string())))) {
        return data.second.first;
      }
    }
  }
  return none;
}

namespace {
  struct CaseInsensitiveKeyCompare
    : public std::binary_function<string, string, bool>
  {
    bool operator()(const string& s1, const string& s2) const {
      return boost::algorithm::ilexicographical_compare(s1, s2);
    }
  };
}

item_t::string_map::iterator
item_t::set_tag(const string&            tag,
                const optional<value_t>& value,
                const bool               overwrite_existing)
{
  assert(! tag.empty());

  if (! metadata)
    metadata = string_map(CaseInsensitiveKeyCompare());

  DEBUG("item.meta", "Setting tag '" << tag << "' to value '"
        << (value ? *value : string_value("<none>")) << "'");

  optional<value_t> data = value;
  if (data && (data->is_null() ||
               (data->is_string() && data->as_string().empty())))
    data = none;

  string_map::iterator i = metadata->find(tag);
  if (i == metadata->end()) {
    std::pair<string_map::iterator, bool> result
      = metadata->insert(string_map::value_type(tag, tag_data_t(data, false)));
    assert(result.second);
    return result.first;
  } else {
    if (overwrite_existing)
      (*i).second = tag_data_t(data, false);
    return i;
  }
}

void item_t::parse_tags(const char * p,
                        scope_t&     scope,
                        bool         overwrite_existing)
{
  if (! std::strchr(p, ':')) {
    if (const char * b = std::strchr(p, '[')) {
      if (*(b + 1) != '\0' &&
          (std::isdigit(*(b + 1)) || *(b + 1) == '=')) {
        if (const char * e = std::strchr(p, ']')) {
          char buf[256];
          std::strncpy(buf, b + 1, static_cast<std::size_t>(e - b - 1));
          buf[e - b - 1] = '\0';

          if (char * pp = std::strchr(buf, '=')) {
            *pp++ = '\0';
            _date_aux = parse_date(pp);
          }
          if (buf[0])
            _date = parse_date(buf);
        }
      }
    }
    return;
  }

  scoped_array<char> buf(new char[std::strlen(p) + 1]);

  std::strcpy(buf.get(), p);

  string tag;
  bool   by_value = false;
  bool   first = true;
  for (char * q = std::strtok(buf.get(), " \t");
       q;
       q = std::strtok(NULL, " \t")) {
    const string::size_type len = std::strlen(q);
    if (len < 2) continue;
    if (q[0] == ':' && q[len - 1] == ':') { // a series of tags
      for (char * r = std::strtok(q + 1, ":");
           r;
           r = std::strtok(NULL, ":")) {
        string_map::iterator i = set_tag(r, none, overwrite_existing);
        (*i).second.second = true;
      }
    }
    else if (first && q[len - 1] == ':') { // a metadata setting
      std::size_t index = 1;
      if (q[len - 2] == ':') {
        by_value = true;
        index    = 2;
      }
      tag = string(q, len - index);

      string_map::iterator i;
      string field(p + len + index);
      trim(field);
      if (by_value) {
        bind_scope_t bound_scope(scope, *this);
        i = set_tag(tag, expr_t(field).calc(bound_scope), overwrite_existing);
      } else {
        i = set_tag(tag, string_value(field), overwrite_existing);
      }
      (*i).second.second = true;
      break;
    }
    first = false;
  }
}

void item_t::append_note(const char * p,
                         scope_t&     scope,
                         bool         overwrite_existing)
{
  if (note) {
    *note += '\n';
    *note += p;
  } else {
    note = p;
  }

  parse_tags(p, scope, overwrite_existing);
}

namespace {
  value_t get_status(item_t& item) {
    return long(item.state());
  }
  value_t get_uncleared(item_t& item) {
    return item.state() == item_t::UNCLEARED;
  }
  value_t get_cleared(item_t& item) {
    return item.state() == item_t::CLEARED;
  }
  value_t get_pending(item_t& item) {
    return item.state() == item_t::PENDING;
  }

  value_t get_actual(item_t& item) {
    return ! item.has_flags(ITEM_GENERATED | ITEM_TEMP);
  }

  value_t get_date(item_t& item) {
    return item.date();
  }
  value_t get_primary_date(item_t& item) {
    return item.primary_date();
  }
  value_t get_aux_date(item_t& item) {
    if (optional<date_t> aux_date = item.aux_date())
      return *aux_date;
    return NULL_VALUE;
  }
  value_t get_note(item_t& item) {
    return item.note ? string_value(*item.note) : NULL_VALUE;
  }

  value_t has_tag(call_scope_t& args) {
    item_t& item(find_scope<item_t>(args));

    if (args.size() == 1) {
      if (args[0].is_string())
        return item.has_tag(args.get<string>(0));
      else if (args[0].is_mask())
        return item.has_tag(args.get<mask_t>(0));
      else
        throw_(std::runtime_error,
               _f("Expected string or mask for argument 1, but received %1%")
               % args[0].label());
    }
    else if (args.size() == 2) {
      if (args[0].is_mask() && args[1].is_mask())
        return item.has_tag(args.get<mask_t>(0), args.get<mask_t>(1));
      else
        throw_(std::runtime_error,
               _f("Expected masks for arguments 1 and 2, but received %1% and %2%")
               % args[0].label() % args[1].label());
    }
    else if (args.size() == 0) {
      throw_(std::runtime_error, _("Too few arguments to function"));
    }
    else {
      throw_(std::runtime_error, _("Too many arguments to function"));
    }
    return false;
  }

  value_t get_tag(call_scope_t& args)
  {
    item_t& item(find_scope<item_t>(args));
    optional<value_t> val;

    if (args.size() == 1) {
      if (args[0].is_string())
        val = item.get_tag(args.get<string>(0));
      else if (args[0].is_mask())
        val = item.get_tag(args.get<mask_t>(0));
      else
        throw_(std::runtime_error,
               _f("Expected string or mask for argument 1, but received %1%")
               % args[0].label());
    }
    else if (args.size() == 2) {
      if (args[0].is_mask() && args[1].is_mask())
        val = item.get_tag(args.get<mask_t>(0), args.get<mask_t>(1));
      else
        throw_(std::runtime_error,
               _f("Expected masks for arguments 1 and 2, but received %1% and %2%")
               % args[0].label() % args[1].label());
    }
    else if (args.size() == 0) {
      throw_(std::runtime_error, _("Too few arguments to function"));
    }
    else {
      throw_(std::runtime_error, _("Too many arguments to function"));
    }

    return val ? *val : NULL_VALUE;
  }

  value_t get_pathname(item_t& item) {
    if (item.pos)
      return string_value(item.pos->pathname.string());
    else
      return NULL_VALUE;
  }

  value_t get_filebase(item_t& item) {
    if (item.pos)
      return string_value(item.pos->pathname.filename().string());
    else
      return NULL_VALUE;
  }

  value_t get_filepath(item_t& item) {
    if (item.pos)
      return string_value(item.pos->pathname.parent_path().string());
    else
      return NULL_VALUE;
  }


  value_t get_beg_pos(item_t& item) {
    return item.pos ? long(item.pos->beg_pos) : 0L;
  }

  value_t get_beg_line(item_t& item) {
    return item.pos ? long(item.pos->beg_line) : 0L;
  }

  value_t get_end_pos(item_t& item) {
    return item.pos ? long(item.pos->end_pos) : 0L;
  }

  value_t get_end_line(item_t& item) {
    return item.pos ? long(item.pos->end_line) : 0L;
  }

  value_t get_seq(item_t& item) {
    return long(item.seq());
  }
  value_t get_id(item_t& item) {
    return string_value(item.id());
  }

  value_t get_addr(item_t& item) {
    return long(&item);
  }

  value_t get_depth(item_t&) {
    return 0L;
  }

  value_t ignore(item_t&) {
    return false;
  }

  template <value_t (*Func)(item_t&)>
  value_t get_wrapper(call_scope_t& scope) {
    return (*Func)(find_scope<item_t>(scope));
  }
}

value_t get_comment(item_t& item)
{
  if (! item.note) {
    return string_value("");
  } else {
    std::ostringstream buf;
    if (item.note->length() > 15)
      buf << "\n    ;";
    else
      buf << "  ;";

    bool need_separator = false;
    for (const char * p = item.note->c_str(); *p; p++) {
      if (*p == '\n') {
        need_separator = true;
      } else {
        if (need_separator) {
          buf << "\n    ;";
          need_separator = false;
        }
        buf << *p;
      }
    }
    return string_value(buf.str());
  }
}

void item_t::define(const symbol_t::kind_t, const string& name,
                    expr_t::ptr_op_t def)
{
  bind_scope_t bound_scope(*scope_t::default_scope, *this);
  set_tag(name, def->calc(bound_scope));
}

expr_t::ptr_op_t item_t::lookup(const symbol_t::kind_t kind,
                                const string& name)
{
  if (kind != symbol_t::FUNCTION)
    return NULL;

  switch (name[0]) {
  case 'a':
    if (name == "actual")
      return WRAP_FUNCTOR(get_wrapper<&get_actual>);
    else if (name == "actual_date")
      return WRAP_FUNCTOR(get_wrapper<&get_primary_date>);
    else if (name == "addr")
      return WRAP_FUNCTOR(get_wrapper<&get_addr>);
    else if (name == "aux_date")
      return WRAP_FUNCTOR(get_wrapper<&get_aux_date>);
    break;

  case 'b':
    if (name == "beg_line")
      return WRAP_FUNCTOR(get_wrapper<&get_beg_line>);
    else if (name == "beg_pos")
      return WRAP_FUNCTOR(get_wrapper<&get_beg_pos>);
    break;

  case 'c':
    if (name == "cleared")
      return WRAP_FUNCTOR(get_wrapper<&get_cleared>);
    else if (name == "comment")
      return WRAP_FUNCTOR(get_wrapper<&get_comment>);
    break;

  case 'd':
    if (name[1] == '\0' || name == "date")
      return WRAP_FUNCTOR(get_wrapper<&get_date>);
    else if (name == "depth")
      return WRAP_FUNCTOR(get_wrapper<&get_depth>);
    break;

  case 'e':
    if (name == "end_line")
      return WRAP_FUNCTOR(get_wrapper<&get_end_line>);
    else if (name == "end_pos")
      return WRAP_FUNCTOR(get_wrapper<&get_end_pos>);
    else if (name == "effective_date")
      return WRAP_FUNCTOR(get_wrapper<&get_aux_date>);
    break;

  case 'f':
    if (name == "filename")
      return WRAP_FUNCTOR(get_wrapper<&get_pathname>);
    else if (name == "filebase")
      return WRAP_FUNCTOR(get_wrapper<&get_filebase>);
    else if (name == "filepath")
      return WRAP_FUNCTOR(get_wrapper<&get_filepath>);
     break;

  case 'h':
    if (name == "has_tag")
      return WRAP_FUNCTOR(ledger::has_tag);
    else if (name == "has_meta")
      return WRAP_FUNCTOR(ledger::has_tag);
    break;

  case 'i':
    if (name == "is_account")
      return WRAP_FUNCTOR(get_wrapper<&ignore>);
    else if (name == "id")
      return WRAP_FUNCTOR(get_wrapper<&get_id>);
    break;

  case 'm':
    if (name == "meta")
      return WRAP_FUNCTOR(ledger::get_tag);
    break;

  case 'n':
    if (name == "note")
      return WRAP_FUNCTOR(get_wrapper<&get_note>);
    break;

  case 'p':
    if (name == "pending")
      return WRAP_FUNCTOR(get_wrapper<&get_pending>);
    else if (name == "parent")
      return WRAP_FUNCTOR(get_wrapper<&ignore>);
    else if (name == "primary_date")
      return WRAP_FUNCTOR(get_wrapper<&get_primary_date>);
    break;

  case 's':
    if (name == "status" || name == "state")
      return WRAP_FUNCTOR(get_wrapper<&get_status>);
    else if (name == "seq")
      return WRAP_FUNCTOR(get_wrapper<&get_seq>);
    break;

  case 't':
    if (name == "tag")
      return WRAP_FUNCTOR(ledger::get_tag);
    break;

  case 'u':
    if (name == "uncleared")
      return WRAP_FUNCTOR(get_wrapper<&get_uncleared>);
    else if (name == "uuid")
      return WRAP_FUNCTOR(get_wrapper<&get_id>);
    break;

  case 'v':
    if (name == "value_date")
      return WRAP_FUNCTOR(get_wrapper<&get_date>);
    break;

  case 'L':
    if (name[1] == '\0')
      return WRAP_FUNCTOR(get_wrapper<&get_actual>);
    break;

  case 'X':
    if (name[1] == '\0')
      return WRAP_FUNCTOR(get_wrapper<&get_cleared>);
    break;

  case 'Y':
    if (name[1] == '\0')
      return WRAP_FUNCTOR(get_wrapper<&get_pending>);
    break;
  }

  return NULL;
}

bool item_t::valid() const
{
  if (_state != UNCLEARED && _state != CLEARED && _state != PENDING) {
    DEBUG("ledger.validate", "item_t: state is bad");
    return false;
  }

  return true;
}

void print_item(std::ostream& out, const item_t& item, const string& prefix)
{
  out << source_context(item.pos->pathname, item.pos->beg_pos,
                        item.pos->end_pos, prefix);
}

string item_context(const item_t& item, const string& desc)
{
  if (! item.pos)
    return empty_string;

  std::streamoff len = item.pos->end_pos - item.pos->beg_pos;
  if (! (len > 0))
    return empty_string;

  assert(len < 1024 * 1024);

  std::ostringstream out;

  if (item.pos->pathname.empty()) {
    out << desc << _(" from streamed input:");
    return out.str();
  }

  out << desc << _(" from \"") << item.pos->pathname.string() << "\"";

  if (item.pos->beg_line != item.pos->end_line)
    out << _(", lines ") << item.pos->beg_line << "-"
        << item.pos->end_line << ":\n";
  else
    out << _(", line ") << item.pos->beg_line << ":\n";

  print_item(out, item, "> ");

  return out.str();
}

void put_metadata(property_tree::ptree& st, const item_t::string_map& metadata)
{
  foreach (const item_t::string_map::value_type& pair, metadata) {
    if (pair.second.first) {
      property_tree::ptree& vt(st.add("value", ""));
      vt.put("<xmlattr>.key", pair.first);
      put_value(vt, *pair.second.first);
    } else {
      st.add("tag", pair.first);
    }
  }
}

} // namespace ledger
