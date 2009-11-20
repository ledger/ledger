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

#include <system.hh>

#include "item.h"
#include "interactive.h"

namespace ledger {

bool item_t::use_effective_date = false;

bool item_t::has_tag(const string& tag) const
{
  DEBUG("item.meta", "Checking if item has tag: " << tag);
  if (! metadata) {
    DEBUG("item.meta", "Item has no metadata at all");
    return false;
  }
  string_map::const_iterator i = metadata->find(tag);
#if defined(DEBUG_ON)
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
		     const optional<mask_t>& value_mask) const
{
  if (metadata) {
    foreach (const string_map::value_type& data, *metadata) {
      if (tag_mask.match(data.first)) {
	if (! value_mask)
	  return true;
	else if (data.second)
	  return value_mask->match(*data.second);
      }
    }
  }
  return false;
}

optional<string> item_t::get_tag(const string& tag) const
{
  DEBUG("item.meta", "Getting item tag: " << tag);
  if (metadata) {
    DEBUG("item.meta", "Item has metadata");
    string_map::const_iterator i = metadata->find(tag);
    if (i != metadata->end()) {
      DEBUG("item.meta", "Found the item!");
      return (*i).second;
    }
  }
  return none;
}

optional<string> item_t::get_tag(const mask_t& tag_mask,
				 const optional<mask_t>& value_mask) const
{
  if (metadata) {
    foreach (const string_map::value_type& data, *metadata) {
      if (tag_mask.match(data.first) &&
	  (! value_mask ||
	   (data.second && value_mask->match(*data.second))))
	return data.second;
    }
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

void item_t::parse_tags(const char * p, optional<date_t::year_type> current_year)
{
  if (const char * b = std::strchr(p, '[')) {
    if (*(b + 1) != '\0' &&
	(std::isdigit(*(b + 1)) || *(b + 1) == '=')) {
      if (const char * e = std::strchr(p, ']')) {
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
  }

  if (! std::strchr(p, ':'))
    return;

  scoped_array<char> buf(new char[std::strlen(p) + 1]);

  std::strcpy(buf.get(), p);

  string tag;
  for (char * q = std::strtok(buf.get(), " \t");
       q;
       q = std::strtok(NULL, " \t")) {
    const string::size_type len = std::strlen(q);
    if (! tag.empty()) {
      if (! has_tag(tag))
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

void item_t::append_note(const char * p, optional<date_t::year_type> current_year)
{
  if (note) {
    *note += '\n';
    *note += p;
  } else {
    note = p;
  }

  parse_tags(p, current_year);
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
  value_t get_effective_date(item_t& item) {
    if (optional<date_t> effective = item.effective_date())
      return *effective;
    return NULL_VALUE;
  }
  value_t get_note(item_t& item) {
    return string_value(item.note ? *item.note : empty_string);
  }

  value_t has_tag(call_scope_t& args) {
    item_t& item(find_scope<item_t>(args));

    if (args.size() == 1) {
      if (args[0].is_string())
	return item.has_tag(args[0].as_string());
      else if (args[0].is_mask())
	return item.has_tag(args[0].as_mask());
      else
	throw_(std::runtime_error,
	       _("Expected string or mask for argument 1, but received %1")
	       << args[0].label());
    }
    else if (args.size() == 2) {
      if (args[0].is_mask() && args[1].is_mask())
	return item.has_tag(args[0].to_mask(), args[1].to_mask());
      else
	throw_(std::runtime_error,
	       _("Expected masks for arguments 1 and 2, but received %1 and %2")
	       << args[0].label() << args[1].label());
    }
    else if (args.size() == 0) {
      throw_(std::runtime_error, _("Too few arguments to function"));
    }
    else {
      throw_(std::runtime_error, _("Too many arguments to function"));
    }
    return false;
  }

  value_t get_tag(call_scope_t& scope) {
    in_context_t<item_t> env(scope, "s");
    if (optional<string> value = env->get_tag(env.get<string>(0)))
      return string_value(*value);
    return string_value(empty_string);
  }

  value_t get_pathname(item_t& item) {
    if (item.pos)
      return string_value(item.pos->pathname.string());
    else
      return string_value(empty_string);
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

expr_t::ptr_op_t item_t::lookup(const symbol_t::kind_t kind,
				const string& name)
{
  if (kind != symbol_t::FUNCTION)
    return NULL;

  switch (name[0]) {
  case 'a':
    if (name == "actual")
      return WRAP_FUNCTOR(get_wrapper<&get_actual>);
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
      return WRAP_FUNCTOR(get_wrapper<&get_effective_date>);
    break;

  case 'f':
    if (name == "filename")
      return WRAP_FUNCTOR(get_wrapper<&get_pathname>);
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
  std::streamoff len = item.pos->end_pos - item.pos->beg_pos;
  if (! len)
    return _("<no item context>");

  assert(len > 0);
  assert(len < 2048);

  std::ostringstream out;
      
  if (item.pos->pathname == path("/dev/stdin")) {
    out << desc << _(" from standard input:");
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

} // namespace ledger
