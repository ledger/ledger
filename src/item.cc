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

#include "item.h"

namespace ledger {

bool item_t::use_effective_date = false;

bool item_t::has_tag(const string& tag) const
{
  if (! metadata)
    return false;
  string_map::const_iterator i = metadata->find(tag);
  return i != metadata->end();
}

optional<string> item_t::get_tag(const string& tag) const
{
  if (metadata) {
    string_map::const_iterator i = metadata->find(tag);
    if (i != metadata->end())
      return (*i).second;
  }
  return none;
}

void item_t::set_tag(const string&           tag,
		     const optional<string>& value)
{
  if (! metadata)
    metadata = string_map();

  DEBUG("item.meta", "Setting tag '" << tag << "' to value '"
	<< (value ? *value : string("<none>")) << "'");

  std::pair<string_map::iterator, bool> result
    = metadata->insert(string_map::value_type(tag, value));
  assert(result.second);
}

void item_t::parse_tags(const char * p, int current_year)
{
  if (char * b = std::strchr(p, '[')) {
    if (char * e = std::strchr(p, ']')) {
      char buf[256];
      std::strncpy(buf, b + 1, e - b - 1);
      buf[e - b - 1] = '\0';

      if (char * p = std::strchr(buf, '=')) {
	*p++ = '\0';
	_date_eff = parse_date(p, current_year);
      }
      if (buf[0])
	_date = parse_date(buf, current_year);
    }
  }

  if (! std::strchr(p, ':'))
    return;

  scoped_array<char> buf(new char[std::strlen(p) + 1]);

  std::strcpy(buf.get(), p);

  string tag;
  for (char * q = std::strtok(buf.get(), " \t");
       q;
       q = std::strtok(NULL, " \t")) {
    const std::size_t len = std::strlen(q);
    if (! tag.empty()) {
      set_tag(tag, string(p + (q - buf.get())));
      break;
    }
    else if (q[0] == ':' && q[len - 1] == ':') { // a series of tags
      for (char * r = std::strtok(q + 1, ":");
	   r;
	   r = std::strtok(NULL, ":"))
	set_tag(r);
    }
    else if (q[len - 1] == ':') { // a metadata setting
      tag = string(q, len - 1);
    }
  }
}

void item_t::append_note(const char * p, int current_year)
{
  if (note)
    *note += p;
  else
    note = p;
  *note += '\n';

  parse_tags(p, current_year);
}

namespace {
  value_t get_status(item_t& item) {
    return long(item.state());
  }
  value_t get_uncleared(item_t& item) {
    return item.state() == item_t::CLEARED;
  }
  value_t get_cleared(item_t& item) {
    return item.state() == item_t::CLEARED;
  }
  value_t get_pending(item_t& item) {
    return item.state() == item_t::PENDING;
  }

  value_t get_date(item_t& item) {
    if (optional<date_t> date = item.date())
      return *date;
    else
      return 0L;
  }

  value_t get_note(item_t& item) {
    return item.note ? string_value(*item.note) : value_t(false);
  }

  value_t has_tag(call_scope_t& args) {
    item_t& item(find_scope<item_t>(args));
    if (! item.metadata)
      return false;

    IF_DEBUG("item.meta") {
	foreach (const item_t::string_map::value_type& data, *item.metadata) {
	  *_log_stream << "  Tag: " << data.first << "\n";
	  *_log_stream << "Value: ";
	  if (data.second)
	    *_log_stream << *data.second << "\n";
	  else
	    *_log_stream << "<none>\n";
	}
    }

    value_t& arg(args[0]);

    if (arg.is_string()) {
      if (args.size() == 1) {
	return item.has_tag(args[0].as_string());
      }
      else if (optional<string> tag = item.get_tag(args[0].as_string())) {
	if (args[1].is_string()) {
	  return args[1].as_string() == *tag;
	}
	else if (args[1].is_mask()) {
	  return args[1].as_mask().match(*tag);
	}
      }
    }
    else if (arg.is_mask()) {
      foreach (const item_t::string_map::value_type& data, *item.metadata) {
	if (arg.as_mask().match(data.first)) {
	  if (args.size() == 1)
	    return true;
	  else if (data.second) {
	    if (args[1].is_string())
	      return args[1].as_string() == *data.second;
	    else if (args[1].is_mask())
	      return args[1].as_mask().match(*data.second);
	  }
	}
      }
    }
    return false;
  }

  value_t get_tag(call_scope_t& args) {
    item_t& item(find_scope<item_t>(args));
    if (optional<string> value = item.get_tag(args[0].as_string()))
      return string_value(*value);
    return false;
  }

  value_t get_beg_pos(item_t& item) {
    return long(item.beg_pos);
  }

  value_t get_beg_line(item_t& item) {
    return long(item.beg_line);
  }

  value_t get_end_pos(item_t& item) {
    return long(item.end_pos);
  }

  value_t get_end_line(item_t& item) {
    return long(item.end_line);
  }

  template <value_t (*Func)(item_t&)>
  value_t get_wrapper(call_scope_t& scope) {
    return (*Func)(find_scope<item_t>(scope));
  }
}

value_t get_comment(item_t& item)
{
  if (! item.note) {
    return false;
  } else {
    std::ostringstream buf;
    buf << "\n    ;";
    bool need_separator = false;
    for (const char * p = item.note->c_str(); *p; p++) {
      if (*p == '\n')
	need_separator = true;
      else {
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

expr_t::ptr_op_t item_t::lookup(const string& name)
{
  switch (name[0]) {
  case 'c':
    if (name == "cleared")
      return WRAP_FUNCTOR(get_wrapper<&get_cleared>);
    else if (name == "comment")
      return WRAP_FUNCTOR(get_wrapper<&get_comment>);
    break;

  case 'd':
    if (name[1] == '\0' || name == "date")
      return WRAP_FUNCTOR(get_wrapper<&get_date>);
    break;

  case 'h':
    if (name == "has_tag")
      return WRAP_FUNCTOR(ledger::has_tag);
    else if (name == "has_meta")
      return WRAP_FUNCTOR(ledger::has_tag);
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
    break;

  case 's':
    if (name == "status")
      return WRAP_FUNCTOR(get_wrapper<&get_status>);
    break;

  case 't':
    if (name == "tag")
      return WRAP_FUNCTOR(ledger::get_tag);
    break;

  case 'u':
    if (name == "uncleared")
      return WRAP_FUNCTOR(get_wrapper<&get_uncleared>);
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

  return expr_t::ptr_op_t();
}

bool item_t::valid() const
{
  if (_state != UNCLEARED && _state != CLEARED && _state != PENDING) {
    DEBUG("ledger.validate", "item_t: state is bad");
    return false;
  }

  return true;
}

string item_context(const item_t& item)
{
  std::size_t len = item.end_pos - item.beg_pos;
  assert(len > 0);
  assert(len < 2048);

  ifstream in(item.pathname);
  in.seekg(item.beg_pos, std::ios::beg);
      
  scoped_array<char> buf(new char[len + 1]);
  in.read(buf.get(), len);

  std::ostringstream out;
      
  out << "While balancing item from \"" << item.pathname.string()
      << "\"";

  if (item.beg_line != item.end_line)
    out << ", lines " << item.beg_line << "-"
	<< item.end_line << ":\n";
  else
    out << ", line " << item.beg_line << ":\n";

  bool first = true;
  for (char * p = std::strtok(buf.get(), "\n");
       p;
       p = std::strtok(NULL, "\n")) {
    if (first)
      first = false;
    else
      out << '\n';
    out << "> " << p;
  }
  return out.str();
}

} // namespace ledger
